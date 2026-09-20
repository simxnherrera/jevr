expect_jev_response_error <- function(expr) {
  condition <- tryCatch(force(expr), error = identity)
  expect_s3_class(condition, "jev_response_error")
}

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
  questions <- list(
    department = jev_choice("Which department?", c(
      billing = "Billing",
      technical = "Technical"
    )),
    severity = jev_score("How severe?", c("Low", "Medium", "High")),
    urgent = jev_noul("Is it urgent?")
  )

  result <- jev_parse_response(raw, "typesafe", questions)

  expect_s3_class(result, "jev_response")
  expect_s3_class(result$answers$department, "jev_choice_answer")
  expect_s3_class(result$answers$severity, "jev_score_answer")
  expect_s3_class(result$answers$urgent, "jev_noul_answer")
  expect_equal(result$answers$department$probabilities[["technical"]], 0.9)
  expect_equal(result$answers$department$extra, "preserved")
  expect_type(result$answers$severity$legend, "character")
  expect_equal(result$answers$severity$legend[["2"]], "High")
  expect_equal(result$answers$severity$probabilities[["1"]], 0.55)
  expect_equal(result$answers$urgent$noul, 0.73)
  expect_equal(result$usage$cost, 0.001)
  expect_identical(result$raw, raw)
})

test_that("partial parsing keeps valid answers and records invalid questions", {
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

  result <- jevr:::jev_parse_response_partial(raw, "typesafe", questions)

  expect_identical(result$status, "partial")
  expect_identical(names(result$response$answers), "valid")
  expect_equal(result$response$answers$valid$noul, 0.8)
  expect_identical(result$response$raw, raw)
  expect_identical(result$response$metadata$provider, "typesafe")
  expect_identical(names(result$question_errors), "invalid")
  expect_identical(
    result$question_errors$invalid$class,
    "jev_response_error"
  )
  expect_match(result$question_errors$invalid$message, "invalid noul")
})

test_that("partial parsing can retain two question errors", {
  questions <- list(
    choice = jev_choice("Which option?", c(first = "First", second = "Second")),
    noul = jev_noul("Is it true?")
  )
  raw <- list(
    model = "jev-1.13.0",
    answers = list(
      choice = list(
        type = "choice",
        choice = "unknown",
        probabilities = list(first = 0.5, second = 0.5),
        confidence = 0.5
      ),
      noul = list(type = "noul", noul = -0.1)
    ),
    usage = list()
  )

  result <- jevr:::jev_parse_response_partial(raw, "typesafe", questions)

  expect_identical(result$status, "partial")
  expect_length(result$response$answers, 0L)
  expect_identical(names(result$question_errors), c("choice", "noul"))
})

test_that("an invalid response envelope still fails the state", {
  questions <- list(urgent = jev_noul("Is it urgent?"))
  invalid_envelope <- list(
    model = "jev-1.13.0",
    answers = "not an object",
    usage = list()
  )

  expect_jev_response_error(
    jevr:::jev_parse_response_partial(invalid_envelope, "typesafe", questions)
  )
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
    jev_parse_response(incomplete, "typesafe", c(department = "choice"))
  )
})

test_that("response parsing rejects an unknown choice", {
  response <- list(
    model = "jev-latest",
    answers = list(
      department = list(
        type = "choice",
        choice = "legal",
        probabilities = list(billing = 0.5, technical = 0.5)
      )
    ),
    usage = list()
  )

  expect_jev_response_error(
    jev_parse_response(
      response,
      "typesafe",
      list(department = jev_choice("Which department?", c(
        billing = "Billing", technical = "Technical"
      )))
    )
  )
})

test_that("response parsing checks probability keys and sums", {
  response <- list(
    model = "jev-latest",
    answers = list(
      department = list(
        type = "choice",
        choice = "billing",
        probabilities = list(billing = 0.2, technical = 0.2)
      )
    ),
    usage = list()
  )

  expect_jev_response_error(
    jev_parse_response(
      response,
      "typesafe",
      list(department = jev_choice("Which department?", c(
        billing = "Billing", technical = "Technical"
      )))
    )
  )
})

test_that("response parsing requires the complete answer ID set", {
  response <- list(
    model = "jev-latest",
    answers = list(
      department = list(
        type = "choice",
        choice = "billing",
        probabilities = list(billing = 0.5, technical = 0.5),
        confidence = 0.5
      ),
      extra = list(type = "noul", noul = 0.5)
    ),
    usage = list()
  )

  expect_jev_response_error(
    jev_parse_response(
      response,
      "typesafe",
      list(department = jev_choice("Which department?", c(
        billing = "Billing", technical = "Technical"
      )))
    )
  )
})

test_that("response parsing checks score scale and preserves structured legends", {
  response <- list(
    model = "jev-latest",
    answers = list(
      severity = list(
        type = "score",
        score = 3.1,
        legend = list(
          `0` = list(label = "Low"),
          `1` = list(label = "Medium"),
          `2` = list(label = "High")
        ),
        probabilities = list(`0` = 0.2, `1` = 0.3, `2` = 0.5),
        confidence = 0.7
      )
    ),
    usage = list()
  )

  expect_jev_response_error(
    jev_parse_response(
      response,
      "typesafe",
      list(severity = jev_score("How severe?", c("Low", "Medium", "High")))
    )
  )

  response$answers$severity$score <- 1.5
  result <- jev_parse_response(
    response,
    "typesafe",
    list(severity = jev_score("How severe?", c("Low", "Medium", "High")))
  )
  expect_type(result$answers$severity$legend, "list")
  expect_equal(result$answers$severity$legend[["1"]]$label, "Medium")
})

test_that("incomplete usage uses NA for unknown counters", {
  response <- list(
    model = "jev-latest",
    answers = list(urgent = list(type = "noul", noul = 0.4)),
    usage = list(input_tokens = 3, provider_note = "not reported")
  )

  result <- jev_parse_response(
    response,
    "typesafe",
    list(urgent = jev_noul("Is it urgent?"))
  )

  expect_equal(result$usage$input_tokens, 3)
  expect_true(is.na(result$usage$output_tokens))
  expect_equal(result$usage$provider_note, "not reported")
})

test_that("missing usage is valid and invalid usage is rejected", {
  response <- list(
    model = "jev-latest",
    answers = list(urgent = list(type = "noul", noul = 0.4))
  )

  result <- jev_parse_response(
    response,
    "typesafe",
    list(urgent = jev_noul("Is it urgent?"))
  )
  expect_true(is.na(result$usage$input_tokens))
  expect_true(is.na(result$usage$output_tokens))

  response$usage <- list(input_tokens = -1, output_tokens = 2)
  expect_jev_response_error(
    jev_parse_response(
      response,
      "typesafe",
      list(urgent = jev_noul("Is it urgent?"))
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
