test_that("executor uses one attempt per request in a wave", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  item <- jev_map_item(
    state = "text",
    state_id = "doc",
    input_index = 1L,
    durable_id = TRUE,
    definition_hash = jev_spec_hash(questions),
    provider = "typesafe",
    model = "jev-1.13.0",
    questions = questions,
    definition = questions
  )
  max_active_seen <- 0L
  response_body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul","noul":0.8}},',
    '"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    max_active_seen <<- max_active
    lapply(seq_along(reqs), function(index) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(response_body)
      )
    })
  }

  execution <- jevr:::jev_execute_map(
    items = list(item),
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 2L,
    timeout = 30,
    max_retries = 0L,
    rate_limit = 5,
    retry_budget = 120,
    progress = FALSE,
    perform_parallel = perform
  )

  expect_equal(max_active_seen, 2L)
  expect_identical(execution$items[[1L]]$status, "success")
  expect_equal(nrow(execution$requests), 1L)
})

test_that("executor does not treat incomplete parallel output as success", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  item <- jev_map_item(
    state = "text",
    state_id = "doc",
    input_index = 1L,
    durable_id = TRUE,
    definition_hash = jev_spec_hash(questions),
    provider = "typesafe",
    model = "jev-1.13.0",
    questions = questions,
    definition = questions
  )
  execution <- jevr:::jev_execute_map(
    items = list(item),
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 0L,
    rate_limit = 5,
    retry_budget = 120,
    progress = FALSE,
    perform_parallel = function(...) NULL
  )

  expect_identical(execution$run_status, "error")
  expect_identical(execution$items[[1L]]$status, "cancelled")
  expect_identical(nrow(execution$requests), 0L)
})

test_that("executor preserves cancellation and does not admit another wave", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  item <- jev_map_item(
    state = "text",
    state_id = "doc",
    input_index = 1L,
    durable_id = TRUE,
    definition_hash = jev_spec_hash(questions),
    provider = "typesafe",
    model = "jev-1.13.0",
    questions = questions,
    definition = questions
  )
  interruption <- structure(
    list(message = "user interrupted"),
    class = c("interrupt", "condition")
  )
  execution <- jevr:::jev_execute_map(
    items = list(item),
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 0L,
    rate_limit = 5,
    retry_budget = 120,
    progress = FALSE,
    perform_parallel = function(...) stop(interruption)
  )

  expect_identical(execution$run_status, "cancelled")
  expect_identical(execution$items[[1L]]$status, "cancelled")
})
