#!/usr/bin/env Rscript

# Development-only probe for real req_perform_parallel() interruption.
# It is intentionally outside the package test suite because it needs sockets
# and child R processes.

required <- c("callr", "devtools", "httr2", "webfakes")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop(
    "Install development packages before running this probe: ",
    paste(missing, collapse = ", ")
  )
}

repo <- normalizePath(getwd(), mustWork = TRUE)
active_path <- tempfile("jevr-transport-active-")
raw_ready_path <- tempfile("jevr-transport-raw-ready-")
raw_result_path <- tempfile("jevr-transport-raw-result-", fileext = ".rds")
package_ready_path <- tempfile("jevr-transport-package-ready-")
package_result_path <- tempfile("jevr-transport-package-result-", fileext = ".rds")
on.exit(unlink(c(
  active_path,
  raw_ready_path,
  raw_result_path,
  package_ready_path,
  package_result_path
)), add = TRUE)

app <- webfakes::new_app()
app$get("/decision/:id", function(req, res) {
  active_path <- req$app$get_config("active_path")
  if (!file.exists(active_path)) {
    writeLines("", active_path)
  }
  Sys.sleep(2)
  res$send_json(list(
    id = paste0("decision-", req$params$id),
    model = "jev-1.13.0",
    answers = list(urgent = list(type = "noul", noul = 0.8)),
    usage = list(input_tokens = 1L, output_tokens = 2L)
  ), auto_unbox = TRUE)
})
app$set_config("active_path", active_path)

server <- webfakes::new_app_process(app, start = TRUE)
on.exit(invisible(server$stop()), add = TRUE)
base_url <- server$url("/decision/")

wait_for_file <- function(path, child, label) {
  deadline <- Sys.time() + 10
  while (!file.exists(path) && Sys.time() < deadline) {
    if (!child$is_alive()) {
      stop(
        "The child process stopped before ", label, ": ",
        paste(child$read_all_error_lines(), collapse = "\n")
      )
    }
    Sys.sleep(0.05)
  }
  if (!file.exists(path)) {
    child$kill()
    stop("The child process did not reach ", label, ".")
  }
}

raw_child <- callr::r_bg(
  function(base_url, ready_path, result_path) {
    reqs <- lapply(seq_len(4L), function(index) {
      request <- httr2::request(paste0(base_url, index))
      request <- httr2::req_timeout(request, seconds = 30)
      httr2::req_throttle(request, rate = 100, fill_time_s = 1)
    })
    writeLines("", ready_path)
    value <- tryCatch(
      httr2::req_perform_parallel(
        reqs,
        on_error = "continue",
        progress = FALSE,
        max_active = 2L
      ),
      error = identity,
      interrupt = identity
    )
    saveRDS(value, result_path)
  },
  args = list(
    base_url = base_url,
    ready_path = raw_ready_path,
    result_path = raw_result_path
  ),
  supervise = TRUE
)

wait_for_file(raw_ready_path, raw_child, "the raw active-wave probe")
wait_for_file(active_path, raw_child, "a raw request")
raw_child$interrupt()
wait_for_file(raw_result_path, raw_child, "the raw interrupted result")
raw_child$wait(timeout = 30000)
raw_value <- readRDS(raw_result_path)
if (!is.list(raw_value) || length(raw_value) != 4L) {
  stop("The raw httr2 probe did not return a four-element result list.")
}
raw_nulls <- sum(vapply(raw_value, is.null, logical(1)))
if (raw_nulls == 0L) {
  stop("The raw httr2 probe did not expose any unfinished request as NULL.")
}

unlink(active_path)

package_child <- callr::r_bg(
  function(repo, base_url, ready_path, result_path) {
    devtools::load_all(repo, quiet = TRUE)
    Sys.setenv(TYPESAFE_API_KEY = "probe-key")
    questions <- list(urgent = jevr::jev_noul("Is this urgent?"))
    items <- lapply(seq_len(4L), function(index) {
      jevr:::jev_map_item(
        state = paste0("state-", index),
        state_id = paste0("state-", index),
        input_index = index,
        durable_id = TRUE,
        definition_hash = jevr::jev_spec_hash(questions),
        provider = "typesafe",
        model = "jev-1.13.0",
        questions = questions,
        definition = questions
      )
    })
    waves <- 0L
    perform_parallel <- function(reqs, on_error, progress, max_active) {
      waves <<- waves + 1L
      local_requests <- lapply(seq_along(reqs), function(index) {
        request <- httr2::request(paste0(base_url, index))
        request <- httr2::req_timeout(request, seconds = 30)
        httr2::req_error(request, is_error = function(response) FALSE)
      })
      httr2::req_perform_parallel(
        local_requests,
        on_error = on_error,
        progress = progress,
        max_active = max_active
      )
    }

    writeLines("", ready_path)
    execution <- tryCatch(
      jevr:::jev_execute_map(
        items = items,
        provider = "typesafe",
        questions = questions,
        model = "jev-1.13.0",
        concurrency = 2L,
        timeout = 30,
        max_retries = 0L,
        rate_limit = 100,
        retry_budget = 120,
        progress = FALSE,
        perform_parallel = perform_parallel
      ),
      error = identity,
      interrupt = identity
    )
    saveRDS(list(execution = execution, waves = waves), result_path)
  },
  args = list(
    repo = repo,
    base_url = base_url,
    ready_path = package_ready_path,
    result_path = package_result_path
  ),
  supervise = TRUE
)

wait_for_file(package_ready_path, package_child, "the package active-wave probe")
wait_for_file(active_path, package_child, "a package request")
package_child$interrupt()
wait_for_file(package_result_path, package_child, "the package interrupted result")
package_child$wait(timeout = 30000)

probe <- readRDS(package_result_path)
execution <- probe$execution
if (inherits(execution, "condition")) {
  stop("The package child returned an unexpected condition: ", conditionMessage(execution))
}
statuses <- vapply(execution$items, function(item) item$status, character(1))
attempts <- vapply(execution$items, function(item) item$attempts, integer(1))

package_gate_passed <- identical(probe$waves, 1L) &&
  identical(execution$run_status, "cancelled") &&
  any(statuses == "cancelled") &&
  !any(statuses == "not_started") &&
  nrow(execution$requests) < length(execution$items) &&
  all(attempts <= 1L)

cat("Real req_perform_parallel cancellation probe completed\n")
cat("raw result NULL responses:", raw_nulls, "of", length(raw_value), "\n")
cat("package waves:", probe$waves, "\n")
cat("package run_status:", execution$run_status, "\n")
cat("package statuses:", paste(statuses, collapse = ", "), "\n")
cat("package attempts recorded:", nrow(execution$requests), "\n")

if (package_gate_passed) {
  cat("Package cancellation gate passed: no later wave was admitted.\n")
} else {
  cat(
    "LIMITATION: httr2 drained an active wave and returned all responses ",
    "without a public cancellation marker; jevr could not distinguish that ",
    "from a normal wave and admitted another wave.\n"
  )
  cat(
    "The minimal adapter required to close this gate is a public-backend ",
    "wrapper that returns an explicit interrupted flag alongside responses; ",
    "the current httr2 API does not provide one.\n"
  )
}
