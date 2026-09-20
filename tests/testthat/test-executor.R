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

test_that("executor keeps state association when completions are out of order", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  states <- c("first", "second")
  items <- lapply(seq_along(states), function(index) {
    jev_map_item(
      state = states[[index]],
      state_id = states[[index]],
      input_index = index,
      durable_id = TRUE,
      definition_hash = jev_spec_hash(questions),
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })
  perform <- function(reqs, on_error, progress, max_active) {
    completion_order <- rev(seq_along(reqs))
    completed <- lapply(completion_order, function(index) {
      state <- reqs[[index]]$body$data$state
      body <- paste0(
        '{"id":"decision-', state, '","model":"jev-1.13.0",',
        '"answers":{"urgent":{"type":"noul","noul":0.8}},',
        '"usage":{"input_tokens":1,"output_tokens":2}}'
      )
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(body)
      )
    })
    responses <- vector("list", length(reqs))
    responses[completion_order] <- completed
    responses
  }

  execution <- jevr:::jev_execute_map(
    items = items,
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

  expect_identical(
    vapply(execution$items, function(item) item$response$raw$id, character(1)),
    c("decision-first", "decision-second")
  )
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

test_that("request ledger uses per-response HTTP timings", {
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
  body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul","noul":0.8}},',
    '"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    lapply(
      reqs,
      function(req) httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(body)
      )
    )
  }

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
    perform_parallel = perform,
    response_timing = function(response) c(total = 0.25)
  )

  expect_equal(execution$requests$duration_seconds, 0.25)
  expect_true(is.na(execution$requests$started_at[[1L]]))
  expect_true(is.na(execution$requests$finished_at[[1L]]))
})

test_that("provider cooldown survives item retry-budget exhaustion", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  items <- lapply(seq_along(c("first", "second")), function(index) {
    jev_map_item(
      state = "text",
      state_id = c("first", "second")[[index]],
      input_index = index,
      durable_id = TRUE,
      definition_hash = jev_spec_hash(questions),
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })
  clock <- 0
  waits <- numeric()
  calls <- 0L
  body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul","noul":0.8}},',
    '"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(list(httr2::response(
        status_code = 429,
        headers = list("Retry-After" = "5")
      )))
    }
    list(httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    ))
  }

  execution <- jevr:::jev_execute_map(
    items = items,
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 1L,
    rate_limit = 5,
    retry_budget = 1,
    progress = FALSE,
    perform_parallel = perform,
    now = function() clock,
    sleep = function(seconds) {
      waits <<- c(waits, seconds)
      clock <<- clock + seconds
    }
  )

  expect_equal(waits, 5)
  expect_equal(calls, 2L)
  expect_identical(execution$items[[1L]]$status, "error")
  expect_identical(execution$items[[2L]]$status, "success")
})

test_that("529 cooldown also survives item retry-budget exhaustion", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  items <- lapply(seq_along(c("first", "second")), function(index) {
    jev_map_item(
      state = "text",
      state_id = c("first", "second")[[index]],
      input_index = index,
      durable_id = TRUE,
      definition_hash = jev_spec_hash(questions),
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })
  clock <- 0
  waits <- numeric()
  calls <- 0L
  response_body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul",',
    '"noul":0.8}},"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(list(httr2::response(
        status_code = 529,
        headers = list("Retry-After" = "3")
      )))
    }
    list(httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_body)
    ))
  }

  execution <- jevr:::jev_execute_map(
    items = items,
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 1L,
    rate_limit = 5,
    retry_budget = 1,
    progress = FALSE,
    perform_parallel = perform,
    now = function() clock,
    sleep = function(seconds) {
      waits <<- c(waits, seconds)
      clock <<- clock + seconds
    }
  )

  expect_equal(waits, 3)
  expect_equal(calls, 2L)
  expect_identical(execution$items[[1L]]$status, "error")
  expect_identical(execution$items[[2L]]$status, "success")
})

test_that("provider cooldown keeps the longest valid Retry-After", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  items <- lapply(seq_along(c("first", "second")), function(index) {
    jev_map_item(
      state = "text",
      state_id = c("first", "second")[[index]],
      input_index = index,
      durable_id = TRUE,
      definition_hash = jev_spec_hash(questions),
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })
  clock <- 0
  waits <- numeric()
  calls <- 0L
  response_body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul",',
    '"noul":0.8}},"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(list(
        httr2::response(status_code = 429, headers = list("Retry-After" = "5")),
        httr2::response(status_code = 429, headers = list("Retry-After" = "1"))
      ))
    }
    lapply(seq_along(reqs), function(index) httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_body)
    ))
  }

  execution <- jevr:::jev_execute_map(
    items = items,
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 2L,
    timeout = 30,
    max_retries = 1L,
    rate_limit = 5,
    retry_budget = 20,
    progress = FALSE,
    perform_parallel = perform,
    now = function() clock,
    sleep = function(seconds) {
      waits <<- c(waits, seconds)
      clock <<- clock + seconds
    }
  )

  expect_equal(waits, 5)
  expect_equal(calls, 2L)
  expect_identical(
    vapply(execution$items, function(item) item$status, character(1)),
    c("success", "success")
  )
})

test_that("attempt timeout is bounded by the remaining item budget", {
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
  now_calls <- 0L
  captured_request <- NULL
  response_body <- paste0(
    '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul",',
    '"noul":0.8}},"usage":{"input_tokens":1,"output_tokens":2}}'
  )
  perform <- function(reqs, on_error, progress, max_active) {
    captured_request <<- reqs[[1L]]
    list(httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_body)
    ))
  }
  now <- function() {
    now_calls <<- now_calls + 1L
    if (now_calls <= 2L) 0 else 2
  }

  jevr:::jev_execute_map(
    items = list(item),
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 0L,
    rate_limit = 5,
    retry_budget = 5,
    progress = FALSE,
    perform_parallel = perform,
    now = now
  )

  expect_equal(captured_request$options$timeout_ms, 3000)
})

test_that("systemic stop does not relabel attempted states as not_started", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  states <- c("first", "second", "third", "fourth")
  items <- lapply(seq_along(states), function(index) {
    jev_map_item(
      state = states[[index]],
      state_id = states[[index]],
      input_index = index,
      durable_id = TRUE,
      definition_hash = jev_spec_hash(questions),
      provider = "typesafe",
      model = "jev-1.13.0",
      questions = questions,
      definition = questions
    )
  })
  perform <- function(reqs, on_error, progress, max_active) {
    list(httr2::response(status_code = 529, headers = list("Retry-After" = "0")))
  }

  execution <- jevr:::jev_execute_map(
    items = items,
    provider = "typesafe",
    questions = questions,
    model = "jev-1.13.0",
    concurrency = 1L,
    timeout = 30,
    max_retries = 10L,
    rate_limit = 5,
    retry_budget = 120,
    progress = FALSE,
    perform_parallel = perform,
    now = function() 0,
    sleep = function(seconds) NULL
  )

  expect_identical(execution$run_status, "stopped")
  expect_identical(execution$items[[1L]]$status, "error")
  expect_equal(execution$items[[1L]]$attempts, 3L)
  expect_identical(
    vapply(execution$items[-1L], function(item) item$status, character(1)),
    rep("not_started", 3L)
  )
})
