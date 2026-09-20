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

test_that("map preserves unnamed IDs, empty inputs, and invalid inputs without HTTP", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(urgent = jev_noul("Is this urgent?"))
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jev_test_map_body())
    )
  }

  empty <- httr2::with_mocked_responses(
    mock,
    jev_map(character(), questions, progress = FALSE)
  )
  expect_length(empty, 0L)
  expect_equal(summary(empty)$http_attempts, 0L)
  expect_equal(calls, 0L)

  unnamed <- httr2::with_mocked_responses(
    mock,
    jev_map(c("first", "second"), questions, max_retries = 0, progress = FALSE)
  )
  expect_identical(names(unnamed), c("1", "2"))
  expect_false(unnamed[[1L]]$provenance$state_id_durable)
  expect_equal(calls, 2L)

  invalid <- httr2::with_mocked_responses(
    mock,
    jev_map(
      list(bad = list(value = NA_real_)),
      questions,
      max_retries = 0,
      progress = FALSE
    )
  )
  expect_identical(invalid$bad$status, "invalid_input")
  expect_equal(summary(invalid)$http_attempts, 0L)
  expect_equal(calls, 2L)
})

test_that("map provenance and serialized errors do not contain API secrets", {
  secret <- "fake-typesafe-secret"
  withr::local_envvar(TYPESAFE_API_KEY = secret)
  mock <- function(req) {
    httr2::response(
      status_code = 401,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"error":{"message":"Invalid API key"}}')
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(doc = "text"),
      list(urgent = jev_noul("Is this urgent?")),
      max_retries = 0L,
      progress = FALSE
    )
  )
  printed <- paste(capture.output(str(result)), collapse = "\n")

  expect_false(grepl(secret, printed, fixed = TRUE))
  expect_false(grepl("Authorization", printed, fixed = TRUE))
  expect_false(grepl(secret, result$doc$provenance$execution_id, fixed = TRUE))
})

test_that("jev_map retains valid answers from a partial response", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  questions <- list(
    valid = jev_noul("Is the claim supported?"),
    invalid = jev_noul("Is the claim current?")
  )
  raw <- list(
    id = "decision-partial",
    model = "jev-1.13.0",
    answers = list(
      valid = list(type = "noul", noul = 0.8),
      invalid = list(type = "noul", noul = 1.2)
    ),
    usage = list(input_tokens = 10, output_tokens = 4)
  )
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null"))
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(doc = "text"),
      questions,
      max_retries = 0,
      progress = FALSE
    )
  )

  expect_identical(result$doc$status, "partial")
  expect_identical(names(result$doc$response$answers), "valid")
  expect_equal(result$doc$response$raw, raw)
  expect_identical(names(result$doc$question_errors), "invalid")
  expect_true(nzchar(result$doc$provenance$execution_id))
  expect_true(nzchar(result$doc$provenance$state_hash))
  expect_true(nzchar(result$doc$provenance$spec_hash))

  long <- as.data.frame(result)
  expect_identical(long$status, c("success", "error"))
  expect_equal(long$value[[1L]], 0.8)
  expect_null(long$value[[2L]])
  expect_identical(long$error_class[[2L]], "jev_response_error")
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

test_that("jev_map never exceeds one plus max_retries attempts", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 529,
      headers = list("Retry-After" = "0")
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(doc = "text"),
      list(urgent = jev_noul("Is this urgent?")),
      max_retries = 2,
      progress = FALSE
    )
  )

  expect_equal(calls, 3L)
  expect_equal(result$doc$attempts$attempt, c(1L, 2L, 3L))
  expect_identical(result$doc$status, "error")
})

test_that("HTTP 401, 402, and 403 stop new admissions", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  for (status in c(401L, 402L, 403L)) {
    calls <- 0L
    mock <- function(req) {
      calls <<- calls + 1L
      httr2::response(status_code = status)
    }
    result <- httr2::with_mocked_responses(
      mock,
      jev_map(
        c(first = "one", second = "two"),
        list(urgent = jev_noul("Is this urgent?")),
        concurrency = 1L,
        max_retries = 0L,
        progress = FALSE
      )
    )

    expect_equal(calls, 1L)
    expect_identical(attr(result, "run_status"), "stopped")
    expect_identical(result$first$status, "error")
    expect_identical(result$second$status, "not_started")
  }
})

test_that("ambiguous 404 responses do not stop global admission", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(httr2::response(
        status_code = 404,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw('{"error":{"message":"resource not found"}}')
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
      c(first = "one", second = "two"),
      list(urgent = jev_noul("Is it urgent?")),
      concurrency = 1,
      max_retries = 0,
      progress = FALSE
    )
  )

  expect_equal(calls, 2L)
  expect_identical(attr(result, "run_status"), "completed")
  expect_identical(result$first$status, "error")
  expect_identical(result$second$status, "success")
})

test_that("404 model errors stop global admission", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  expect_true(jevr:::jev_404_shared_configuration(
    list(error = list(code = "model_not_found"))
  ))
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 404,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"error":{"message":"model not found"}}')
    )
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
  expect_identical(result$first$status, "error")
  expect_identical(result$second$status, "not_started")
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
