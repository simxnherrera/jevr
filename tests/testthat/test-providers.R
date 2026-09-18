test_that("OpenRouter uses the Decisions endpoint and its model alias", {
  withr::local_envvar(OPENROUTER_API_KEY = "fake-openrouter-key")
  captured_request <- NULL
  response_body <- paste0(
    '{"id":"decision-1","model":"typesafe/jev-1.13",',
    '"provider":"TypeSafe","answers":{',
    '"urgent":{"type":"noul","noul":0.88}},',
    '"usage":{"input_tokens":21,"output_tokens":0,"cost":0.000001}}'
  )

  mock <- function(req) {
    captured_request <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_body)
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    jev_ask(
      state = list(message = "Payouts failed"),
      questions = list(
        urgent = jev_noul(
          "Does this request convey urgency?",
          criteria = c(true = "Time-sensitive", false = "Not urgent")
        )
      ),
      provider = "openrouter",
      max_retries = 0
    )
  )

  expect_equal(result$metadata$provider, "openrouter")
  expect_equal(result$metadata$id, "decision-1")
  expect_equal(result$metadata$upstream_provider, "TypeSafe")
  expect_equal(result$answers$urgent$noul, 0.88)
  expect_equal(
    captured_request$url,
    "https://openrouter.ai/api/alpha/decisions"
  )
  expect_contains(names(captured_request$headers), "Authorization")
  expect_equal(
    captured_request$headers[["HTTP-Referer"]],
    "https://github.com/simxnherrera/jevr"
  )
  expect_equal(captured_request$headers[["X-OpenRouter-Title"]], "jevr")
  expect_equal(captured_request$body$data$model, "~typesafe/jev-latest")
  expect_equal(
    captured_request$body$data$questions$urgent$criteria$true,
    "Time-sensitive"
  )
})

test_that("OpenRouter serializes choice criteria as a JSON object", {
  withr::local_envvar(OPENROUTER_API_KEY = "fake-openrouter-key")
  captured_request <- NULL
  mock <- function(req) {
    captured_request <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(
        paste0(
          '{"model":"typesafe/jev-1.13",',
          '"answers":{"department":{"type":"choice",',
          '"choice":"technical","probabilities":{"technical":1},',
          '"confidence":1}},',
          '"usage":{"input_tokens":1,"output_tokens":1}}'
        )
      )
    )
  }

  httr2::with_mocked_responses(
    mock,
    jev_ask(
      state = "A payout failed",
      questions = list(
        department = jev_choice(
          "Which team should handle this?",
          c(billing = "Payments", technical = "Bugs")
        )
      ),
      provider = "openrouter",
      max_retries = 0
    )
  )

  expect_type(captured_request$body$data$questions$department$criteria, "list")
  expect_equal(
    captured_request$body$data$questions$department$criteria$technical,
    "Bugs"
  )
})

test_that("OpenRouter reports its current string-only question constraint", {
  withr::local_envvar(OPENROUTER_API_KEY = "fake-openrouter-key")

  expect_snapshot(
    error = TRUE,
    jev_ask(
      state = "text",
      questions = list(
        choice = jev_choice(
          instructions = list(question = "Which option?"),
          criteria = c(first = "First", second = "Second")
        )
      ),
      provider = "openrouter",
      max_retries = 0
    )
  )
})

test_that("HTTP errors include status and corrective guidance", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  mock <- function(req) {
    httr2::response(
      status_code = 401,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"error":{"message":"Invalid API key"}}')
    )
  }

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      mock,
      jev_ask(
        state = "text",
        questions = list(urgent = jev_noul("Is this urgent?")),
        max_retries = 0
      )
    )
  )
})
