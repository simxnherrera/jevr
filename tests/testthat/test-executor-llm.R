test_that("sequential map keeps partial results, errors, callbacks and provenance", {
  skip_if_not_installed("ellmer", "0.5.0")
  factory_calls <- 0L
  completed <- list()
  backend <- jev_test_llm_backend(inference_options = list(reasoning_effort = "medium"))
  factory <- backend$chat
  original <- structure(list(message = "upstream failure"), class = c("codex_network_error", "error", "condition"))
  backend$chat <- function() {
    factory_calls <<- factory_calls + 1L
    if (factory_calls == 2L) stop(original)
    chat <- factory()
    if (factory_calls == 1L) {
      chat$chat_structured <- function(...) {
        raw <- jev_test_llm_raw()
        raw$answers$severity$probabilities <- list(`0` = 0.2, `1` = 0.2, `2` = 0.2)
        raw
      }
    }
    chat
  }
  questions <- jev_spec("routing", "1", jev_test_llm_questions())
  result <- jev_map(list(first = "A", failed = "B", last = "C", invalid = list(x = NA_real_)),
    questions,
    backend = backend, rate_limit = 1e6, progress = FALSE,
    on_result = function(item) completed[[item$state_id]] <<- item
  )

  expect_s3_class(result, "jev_result_set")
  expect_identical(names(result), c("first", "failed", "last", "invalid"))
  expect_identical(
    vapply(result, function(item) item$status, character(1)),
    c(first = "partial", failed = "transport_error", last = "success", invalid = "invalid_input")
  )
  expect_identical(factory_calls, 3L)
  expect_identical(names(result$first$response$answers), c("department", "urgent"))
  expect_identical(names(result$first$question_errors), "severity")
  expect_identical(result$failed$error$details$cause$class, "codex_network_error")
  expect_identical(result$failed$error$details$cause$message, "upstream failure")
  expect_identical(sort(names(completed)), sort(names(result)))
  expect_identical(completed$first$response, result$first$response)
  expect_identical(result$last$provenance$upstream_provider, "mock")
  expect_identical(result$last$provenance$inference_options, list(reasoning_effort = "medium"))
  expect_identical(result$last$provenance$reported_model, "reported-model")
  expect_identical(result$last$response$metadata$execution_id, result$last$provenance$execution_id)
  expect_identical(result$last$provenance$execution_id, jev_llm_execution_id("C", questions, backend, "mock"))
  ledger <- attr(result, "requests")
  expect_equal(nrow(ledger), 3L)
  expect_identical(ledger$upstream_provider, c("mock", NA_character_, "mock"))
  expect_identical(result$failed$provenance$execution_id, NA_character_)
  expect_identical(ledger$attempt, rep(1L, 3))
  expect_identical(ledger$status, rep(NA_integer_, 3))
  expect_equal(all(is.finite(ledger$duration_seconds)), TRUE)
  expect_identical(summary(result)$http_attempts, 0L)
  expect_identical(summary(result)$llm_invocations, 3L)
  expect_identical(summary(result["last"])$llm_invocations, 1L)
  expect_identical(attr(result, "run_status"), "completed")
  long <- as.data.frame(result)
  expect_equal(long$confidence, rep(NA_real_, 12L))
  expect_identical(
    result$first$response$raw$answers$severity$probabilities,
    list(`0` = 0.2, `1` = 0.2, `2` = 0.2)
  )
})

test_that("map missing answers are partial but unknown IDs invalidate the envelope", {
  skip_if_not_installed("ellmer", "0.5.0")
  raw <- jev_test_llm_raw()
  raw$answers$severity <- NULL
  result <- jev_map(c(doc = "state"), jev_test_llm_questions(),
    backend = jev_test_llm_backend(raw), progress = FALSE
  )
  expect_identical(result$doc$status, "partial")
  expect_identical(names(result$doc$question_errors), "severity")
  raw$answers$other <- list(noul = 0.4)
  result <- jev_map(c(doc = "state"), jev_test_llm_questions(),
    backend = jev_test_llm_backend(raw), progress = FALSE
  )
  expect_identical(result$doc$status, "error")
  expect_identical(result$doc$error$details$structured_result, raw)
})

test_that("callback failure stops new admission and keeps already completed results", {
  skip_if_not_installed("ellmer", "0.5.0")
  capture <- new.env()
  capture$calls <- capture$factories <- 0L
  result <- jev_map(c(first = "A", second = "B"), jev_test_llm_questions(),
    backend = jev_test_llm_backend(capture = capture), progress = FALSE,
    on_result = function(item) stop("save failed")
  )
  expect_identical(capture$calls, 1L)
  expect_identical(result$first$status, "success")
  expect_identical(result$second$status, "not_started")
  expect_identical(result$second$provenance$execution_id, NA_character_)
  expect_identical(attr(result, "run_status"), "callback_error")
  expect_identical(attr(result, "callback_error")$message, "save failed")
  expect_equal(nrow(result$second$attempts), 0L)
})

test_that("interrupts preserve completed results and record cancellation", {
  skip_if_not_installed("ellmer", "0.5.0")
  backend <- jev_test_llm_backend()
  factory <- backend$chat
  calls <- 0L
  backend$chat <- function() {
    calls <<- calls + 1L
    chat <- factory()
    if (calls == 2L) {
      chat$chat_structured <- function(...) {
        stop(structure(list(message = "cancelled"), class = c("interrupt", "condition")))
      }
    }
    chat
  }
  callbacks <- character()
  result <- jev_map(c(first = "A", second = "B", third = "C"), jev_test_llm_questions(),
    backend = backend, progress = FALSE, rate_limit = 1e6,
    on_result = function(item) callbacks <<- c(callbacks, item$state_id)
  )
  expect_identical(result$first$status, "success")
  expect_identical(result$second$status, "cancelled")
  expect_identical(result$second$provenance$upstream_provider, "mock")
  expect_identical(
    result$second$provenance$execution_id,
    jev_llm_execution_id("B", jev_test_llm_questions(), backend, "mock")
  )
  expect_identical(result$third$status, "cancelled")
  expect_identical(calls, 2L)
  expect_identical(callbacks, c("first", "second", "third"))
  expect_identical(attr(result, "run_status"), "cancelled")
  expect_equal(nrow(attr(result, "requests")), 2L)
})

test_that("backend map handles empty and invalid inputs without constructing chats", {
  backend <- jev_test_llm_backend()
  backend$chat <- function() stop("must not run")
  questions <- jev_test_llm_questions()
  empty <- jev_map(character(), questions, backend = backend, progress = FALSE)
  expect_length(empty, 0L)
  result <- jev_map(list(bad = list(value = NA_real_)), questions,
    backend = backend, progress = FALSE
  )
  expect_identical(result$bad$status, "invalid_input")
  expect_equal(nrow(attr(result, "requests")), 0L)
  expect_snapshot(error = TRUE, jev_map("A", questions, backend = backend, concurrency = 2))
  expect_snapshot(error = TRUE, jev_map("A", questions, backend = backend, retry_budget = 120))
  expect_snapshot(error = TRUE, jev_map("A", questions, backend = backend, max_retries = 2))
})

test_that("sequential admission rate uses the existing execution clock", {
  skip_if_not_installed("ellmer", "0.5.0")
  backend <- jev_test_llm_backend()
  questions <- jev_test_llm_questions()
  definition_hash <- jev_spec_hash(questions)
  items <- lapply(1:2, function(index) {
    jev_map_item(
      "state", as.character(index), index, FALSE, definition_hash,
      "ellmer", backend$model, questions, questions, backend
    )
  })
  elapsed <- 0
  waits <- numeric()
  execution <- jev_execute_llm_map(items, questions, backend,
    rate_limit = 2,
    on_result = NULL, progress = FALSE, now = function() elapsed,
    sleep = function(seconds) {
      waits <<- c(waits, seconds)
      elapsed <<- elapsed + seconds
    }
  )
  expect_equal(waits, 0.5)
  expect_equal(nrow(execution$requests), 2L)
})

test_that("a generation error retains the discovered provider and execution identity", {
  skip_if_not_installed("ellmer", "0.5.0")
  backend <- jev_test_llm_backend(provider_name = "codex")
  factory <- backend$chat
  original <- structure(list(message = "generation failed"),
    class = c("codex_generation_error", "error", "condition")
  )
  backend$chat <- function() {
    chat <- factory()
    chat$chat_structured <- function(...) stop(original)
    chat
  }
  questions <- jev_test_llm_questions()
  result <- jev_map(c(doc = "state"), questions, backend = backend, progress = FALSE)
  expect_identical(result$doc$status, "transport_error")
  expect_identical(result$doc$provenance$upstream_provider, "codex")
  expect_identical(
    result$doc$provenance$execution_id,
    jev_llm_execution_id("state", questions, backend, "codex")
  )
  expect_identical(attr(result, "requests")$upstream_provider, "codex")
  expect_identical(result$doc$error$details$cause$class, "codex_generation_error")
  expect_equal(nrow(attr(result, "requests")), 1L)
})
