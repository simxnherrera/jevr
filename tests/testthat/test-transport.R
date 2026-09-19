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
      stop("connection failed")
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
