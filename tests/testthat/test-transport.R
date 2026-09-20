test_that("transient HTTP errors retry and honor Retry-After", {
  attempts <- 0L
  waits <- numeric()
  perform <- function(request) {
    attempts <<- attempts + 1L
    if (attempts < 3L) {
      return(httr2::response(
        status_code = 429,
        headers = list("Retry-After" = "0")
      ))
    }
    httr2::response(status_code = 200)
  }

  result <- jev_send_request(
    httr2::request("https://example.com"),
    provider = "typesafe",
    max_retries = 2,
    perform = perform,
    sleep = function(seconds) waits <<- c(waits, seconds)
  )

  expect_equal(httr2::resp_status(result), 200)
  expect_equal(attempts, 3L)
  expect_equal(waits, c(0, 0))
})

test_that("retry count is bounded", {
  attempts <- 0L
  waits <- numeric()
  perform <- function(request) {
    attempts <<- attempts + 1L
    httr2::response(status_code = 529)
  }

  expect_snapshot(
    error = TRUE,
    jev_send_request(
      httr2::request("https://example.com"),
      provider = "typesafe",
      max_retries = 2,
      perform = perform,
      sleep = function(seconds) waits <<- c(waits, seconds)
    )
  )

  expect_equal(attempts, 3L)
  expect_length(waits, 2L)
  expect_equal(jev_retry_delay(1L), 0.5)
  expect_equal(jev_retry_delay(2L), 1)
  expect_equal(
    jev_retry_delay(
      1L,
      httr2::response(status_code = 429, headers = list("Retry-After-Ms" = "1500"))
    ),
    1.5
  )
})

test_that("connection errors retry without making the loop unbounded", {
  attempts <- 0L
  waits <- numeric()
  perform <- function(request) {
    attempts <<- attempts + 1L
    if (attempts == 1L) {
      stop(structure(
        list(message = "connection failed"),
        class = c("curl_error", "error", "condition")
      ))
    }
    httr2::response(status_code = 200)
  }

  result <- jev_send_request(
    httr2::request("https://example.com"),
    provider = "typesafe",
    max_retries = 1,
    perform = perform,
    sleep = function(seconds) waits <<- c(waits, seconds)
  )

  expect_equal(httr2::resp_status(result), 200)
  expect_equal(attempts, 2L)
  expect_equal(waits, 0.5)
})

test_that("Retry-After HTTP-date is parsed without sleeping", {
  future <- format(
    as.POSIXct("2030-01-01 00:00:00", tz = "UTC"),
    "%a, %d %b %Y %H:%M:%S GMT",
    tz = "GMT"
  )
  delay <- jev_retry_delay(
    1L,
    httr2::response(status_code = 429, headers = list("Retry-After" = future))
  )

  expect_true(is.finite(delay))
  expect_gt(delay, 1e7)
})

test_that("unknown programming errors are not retried as transport errors", {
  attempts <- 0L
  expect_error(
    jev_send_request(
      httr2::request("https://example.com"),
      provider = "typesafe",
      max_retries = 3,
      perform = function(request) {
        attempts <<- attempts + 1L
        stop("programming failure")
      },
      sleep = function(seconds) NULL
    ),
    "programming failure"
  )
  expect_identical(attempts, 1L)
})

test_that("unavailable response timing is represented as NA", {
  expect_true(is.na(jev_http_duration(
    httr2::response(status_code = 200),
    timing = function(response) c(connect = 0.1)
  )))
  expect_true(is.na(jev_http_duration(
    simpleError("not an HTTP response")
  )))
})
