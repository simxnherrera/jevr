test_that("jev_ask batches questions and preserves answer IDs", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  captured_request <- NULL
  response_body <- paste0(
    '{"model":"jev-1.13.0","answers":{',
    '"department":{"type":"choice","choice":"billing",',
    '"probabilities":{"billing":0.8,"technical":0.2},"confidence":0.7},',
    '"urgent":{"type":"noul","noul":0.91},',
    '"severity":{"type":"score","score":1.4,',
    '"legend":{"0":"Cosmetic","1":"Degraded","2":"Blocking"},',
    '"probabilities":{"0":0.1,"1":0.4,"2":0.5},"confidence":0.6}',
    '},"usage":{"input_tokens":42,"output_tokens":18}}'
  )

  mock <- function(req) {
    captured_request <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_body)
    )
  }

  questions <- list(
    department = jev_choice(
      "Which team should handle this?",
      c(billing = "Payments", technical = "Bugs")
    ),
    urgent = jev_noul("Does this request convey urgency?"),
    severity = jev_score(
      "How severe is this?",
      c("Cosmetic", "Degraded", "Blocking")
    )
  )

  result <- httr2::with_mocked_responses(
    mock,
    jev_ask(
      state = list(message = "My payouts have failed"),
      questions = questions,
      timeout = 17,
      max_retries = 0
    )
  )

  expect_s3_class(result, "jev_response")
  expect_equal(names(result$answers), names(questions))
  expect_s3_class(result$answers$department, "jev_choice_answer")
  expect_equal(result$answers$department$choice, "billing")
  expect_equal(result$answers$department$probabilities[["billing"]], 0.8)
  expect_equal(result$answers$urgent$noul, 0.91)
  expect_equal(result$answers$severity$score, 1.4)
  expect_equal(result$usage$input_tokens, 42)
  expect_equal(result$metadata$provider, "typesafe")
  expect_equal(captured_request$url, "https://api.typesafe.ai/v1/systemone")
  expect_contains(names(captured_request$headers), "Authorization")
  expect_equal(captured_request$headers[["Content-Type"]], "application/json")
  expect_equal(captured_request$body$data$model, "jev-latest")
  expect_equal(
    names(captured_request$body$data$questions),
    c("department", "urgent", "severity")
  )
  expect_equal(captured_request$options$timeout_ms, 17000)
})

test_that("jev_ask validates questions before authenticating", {
  withr::local_envvar(TYPESAFE_API_KEY = "")

  expect_snapshot(
    error = TRUE,
    jev_ask("state", questions = list(bad = "not a question"))
  )
})
