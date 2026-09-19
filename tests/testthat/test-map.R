jev_test_map_body <- function(value = 0.8) {
  paste0(
    '{"id":"decision-1","model":"jev-1.13.0","answers":{"urgent":{"type":"noul","noul":',
    value,
    '}},"usage":{"input_tokens":1,"output_tokens":2}}'
  )
}

test_that("jev_map preserves state IDs, order, and request provenance", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      headers = list(
        "Content-Type" = "application/json",
        "X-Request-ID" = "request-1"
      ),
      body = charToRaw(jev_test_map_body())
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(doc_a = "First", doc_b = "Second"),
      list(urgent = jev_noul("Is this urgent?")),
      max_retries = 0,
      progress = FALSE
    )
  )

  expect_s3_class(result, "jev_result_set")
  expect_identical(names(result), c("doc_a", "doc_b"))
  expect_identical(
    vapply(result, function(item) item$status, character(1)),
    c(doc_a = "success", doc_b = "success")
  )
  expect_equal(calls, 2L)
  expect_equal(nrow(attr(result, "requests")), 2L)
  expect_true(all(attr(result, "requests")$attempt == 1L))
  expect_identical(result$doc_a$response$metadata$request_id, "request-1")
  expect_true(all(attr(result, "requests")$request_id == "request-1"))
  expect_true(all(nzchar(attr(result, "definition")$hash)))
})

test_that("jev_map retains invalid states without sending them", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jev_test_map_body())
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      list(ok = "text", bad = list(value = NA_real_)),
      list(urgent = jev_noul("Is this urgent?")),
      max_retries = 0,
      progress = FALSE
    )
  )

  expect_equal(calls, 1L)
  expect_identical(result$ok$status, "success")
  expect_identical(result$bad$status, "invalid_input")
  expect_match(result$bad$error$message, "state")
})

test_that("jev_map retries transient responses and records every attempt", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(httr2::response(
        status_code = 429,
        headers = list("Retry-After" = "0")
      ))
    }
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jev_test_map_body())
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(doc = "text"),
      list(urgent = jev_noul("Is this urgent?")),
      max_retries = 1,
      progress = FALSE
    )
  )

  expect_identical(result$doc$status, "success")
  expect_equal(calls, 2L)
  expect_equal(nrow(attr(result, "requests")), 2L)
  expect_equal(attr(result, "summary")$retries, 1L)
})

test_that("jev_map stops before new admissions when on_result fails", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jev_test_map_body())
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(first = "one", second = "two"),
      list(urgent = jev_noul("Is this urgent?")),
      concurrency = 1,
      max_retries = 0,
      on_result = function(x) stop("sink failed"),
      progress = FALSE
    )
  )

  expect_equal(calls, 1L)
  expect_identical(attr(result, "run_status"), "callback_error")
  expect_identical(result$second$status, "not_started")
  expect_match(attr(result, "callback_error")$message, "sink failed")
})
