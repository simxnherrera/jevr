test_that("result sets expose long rows and preserve request metadata", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(
        paste0(
          '{"model":"jev-1.13.0","answers":{"choice":{"type":"choice",',
          '"choice":"yes","probabilities":{"yes":0.7,"no":0.3},',
          '"confidence":0.7},"urgent":{"type":"noul","noul":0.8}},',
          '"usage":{"input_tokens":2,"output_tokens":1}}'
        )
      )
    )
  }
  questions <- list(
    choice = jev_choice("Does it hold?", c(yes = "Yes", no = "No")),
    urgent = jev_noul("Is it urgent?")
  )
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(c(a = "one", b = "two"), questions, max_retries = 0, progress = FALSE)
  )

  long <- as.data.frame(result)
  expect_equal(nrow(long), 4L)
  expect_true(is.list(long$value))
  expect_equal(long$value[[1L]], "yes")
  expect_equal(long$confidence[[1L]], 0.7)
  expect_equal(long$value[[2L]], 0.8)
  expect_null(long$probabilities[[2L]])
  expect_null(long$legend[[2L]])
  expect_equal(nrow(attr(result, "requests")), 2L)
  expect_equal(length(result["a"]), 1L)
  expect_equal(summary(result["a"])$states, 1L)
  expect_equal(summary(result["a"])$http_attempts, 1L)
})

test_that("global HTTP stops keep unadmitted states in the result set", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(status_code = 401)
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(first = "one", second = "two"),
      list(urgent = jev_noul("Is it urgent?")),
      concurrency = 1,
      max_retries = 0,
      progress = FALSE
    )
  )

  expect_equal(calls, 1L)
  expect_identical(attr(result, "run_status"), "stopped")
  expect_identical(result$second$status, "not_started")
})

test_that("result sets reject combining different definitions", {
  first <- structure(
    list(),
    class = c("jev_result_set", "list"),
    definition = list(hash = "first"),
    definition_hash = "first",
    requests = jev_empty_attempt_ledger(),
    run_status = "completed",
    summary = list()
  )
  second <- structure(
    list(),
    class = c("jev_result_set", "list"),
    definition = list(hash = "second"),
    definition_hash = "second",
    requests = jev_empty_attempt_ledger(),
    run_status = "completed",
    summary = list()
  )

  expect_error(c(first, second), class = "jev_input_error")
})
