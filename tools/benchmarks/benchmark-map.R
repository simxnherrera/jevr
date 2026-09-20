#!/usr/bin/env Rscript

# Development benchmark for bounded local HTTP execution.
# The endpoint is synthetic; no provider credentials or paid requests are used.

required <- c("devtools", "httr2", "webfakes")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop(
    "Install development packages before running this benchmark: ",
    paste(missing, collapse = ", ")
  )
}

devtools::load_all(normalizePath(getwd(), mustWork = TRUE), quiet = TRUE)
Sys.setenv(TYPESAFE_API_KEY = "benchmark-key")

app <- webfakes::new_app()
app$get("/decision/:size/:id", function(req, res) {
  delay <- if (identical(req$params$size, "small")) 0.02 else 0.06
  Sys.sleep(delay)
  res$send_json(list(
    id = paste0("decision-", req$params$size, "-", req$params$id),
    model = "jev-1.13.0",
    answers = list(urgent = list(type = "noul", noul = 0.8)),
    usage = list(
      input_tokens = if (identical(req$params$size, "small")) 8L else 80L,
      output_tokens = 2L,
      cost = if (identical(req$params$size, "small")) 0.0001 else 0.0005
    )
  ), auto_unbox = TRUE)
})
server <- webfakes::new_app_process(app, start = TRUE)
on.exit(server$stop(), add = TRUE)
base_url <- server$url("/decision/")

questions <- list(urgent = jev_noul("Is this urgent?"))
definition_hash <- jev_spec_hash(questions)

run_case <- function(size, concurrency, n_states = 12L) {
  items <- lapply(seq_len(n_states), function(index) {
    jev_map_item(
      state = list(
        id = index,
        text = if (identical(size, "small")) {
          "Short state"
        } else {
          paste(rep("Medium state text.", 40L), collapse = " ")
        }
      ),
      state_id = paste0(size, "-", index),
      input_index = index,
      durable_id = TRUE,
      definition_hash = definition_hash,
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })

  perform_parallel <- function(reqs, on_error, progress, max_active) {
    local_requests <- lapply(reqs, function(req) {
      id <- req$body$data$state$id
      request <- httr2::request(paste0(base_url, size, "/", id))
      request <- httr2::req_timeout(request, seconds = 30)
      request <- httr2::req_throttle(request, rate = 1000, fill_time_s = 1)
      httr2::req_error(request, is_error = function(response) FALSE)
    })
    httr2::req_perform_parallel(
      local_requests,
      on_error = on_error,
      progress = progress,
      max_active = max_active
    )
  }

  gc(reset = TRUE)
  elapsed <- system.time({
    execution <- jevr:::jev_execute_map(
      items = items,
      provider = "typesafe",
      questions = questions,
      model = "jev-1.13.0",
      concurrency = concurrency,
      timeout = 30,
      max_retries = 0L,
      rate_limit = 1000,
      retry_budget = 120,
      progress = FALSE,
      perform_parallel = perform_parallel
    )
  })[["elapsed"]]
  memory <- gc()
  statuses <- vapply(execution$items, function(item) item$status, character(1))
  durations <- execution$requests$duration_seconds
  finite_durations <- durations[is.finite(durations)]
  cost <- execution$requests$cost

  data.frame(
    size = size,
    states = n_states,
    concurrency = concurrency,
    throughput_states_per_second = sum(statuses == "success") / elapsed,
    elapsed_seconds = elapsed,
    latency_p50_seconds = if (length(finite_durations) == 0L) {
      NA_real_
    } else {
      stats::median(finite_durations)
    },
    timed_attempts = length(finite_durations),
    errors = sum(statuses != "success"),
    status_429 = sum(execution$requests$status == 429L, na.rm = TRUE),
    status_529 = sum(execution$requests$status == 529L, na.rm = TRUE),
    peak_memory_mb = max(memory[, 7]),
    result_size_bytes = as.numeric(object.size(execution$items)) +
      as.numeric(object.size(execution$requests)),
    cost_reported = if (all(is.na(cost))) NA_real_ else sum(cost, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

cases <- expand.grid(
  size = c("small", "medium"),
  concurrency = c(1L, 2L, 4L, 8L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
results <- do.call(
  rbind,
  lapply(seq_len(nrow(cases)), function(index) {
    run_case(cases$size[[index]], cases$concurrency[[index]])
  })
)
rownames(results) <- NULL
print(results)
cat("\nThe endpoint is synthetic and returns no 429/529 responses; those columns are control checks.\n")
