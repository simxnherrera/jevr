test_that("response parsing keeps all typed answer fields", {
  raw <- list(
    id = "response-1",
    model = "jev-1.13.0",
    provider = "TypeSafe",
    answers = list(
      department = list(
        type = "choice",
        choice = "technical",
        probabilities = list(billing = 0.1, technical = 0.9),
        confidence = 0.8,
        extra = "preserved"
      ),
      severity = list(
        type = "score",
        score = 1.25,
        legend = setNames(
          list("Low", "Medium", "High"),
          c("0", "1", "2")
        ),
        probabilities = setNames(
          list(0.1, 0.55, 0.35),
          c("0", "1", "2")
        ),
        confidence = 0.62
      ),
      urgent = list(type = "noul", noul = 0.73)
    ),
    usage = list(input_tokens = 50, output_tokens = 12, cost = 0.001)
  )
  question_types <- c(
    department = "choice",
    severity = "score",
    urgent = "noul"
  )

  result <- jev_parse_response(raw, "typesafe", question_types)

  expect_s3_class(result, "jev_response")
  expect_s3_class(result$answers$department, "jev_choice_answer")
  expect_s3_class(result$answers$severity, "jev_score_answer")
  expect_s3_class(result$answers$urgent, "jev_noul_answer")
  expect_equal(result$answers$department$probabilities[["technical"]], 0.9)
  expect_equal(result$answers$department$extra, "preserved")
  expect_equal(result$answers$severity$legend[["2"]], "High")
  expect_equal(result$answers$severity$probabilities[["1"]], 0.55)
  expect_equal(result$answers$urgent$noul, 0.73)
  expect_equal(result$usage$cost, 0.001)
  expect_identical(result$raw, raw)
})

test_that("incomplete responses fail with the missing field", {
  incomplete <- list(
    model = "jev-latest",
    answers = list(
      department = list(type = "choice", choice = "billing")
    ),
    usage = list(input_tokens = 1, output_tokens = 1)
  )

  expect_snapshot(
    error = TRUE,
    jev_parse_response(
      incomplete,
      "typesafe",
      c(department = "choice")
    )
  )
})

test_that("invalid JSON responses use a package error", {
  response <- httr2::response(
    status_code = 200,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw("{not valid json")
  )

  expect_snapshot(error = TRUE, jev_body_json(response, "typesafe"))
})
