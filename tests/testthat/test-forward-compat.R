expect_response_error <- function(expr) {
  condition <- tryCatch(force(expr), error = identity)
  expect_s3_class(condition, "jev_response_error")
}

test_that("unknown answer kinds are kept raw with one classed warning", {
  body <- list(
    model = "jev-latest",
    answers = list(
      urgent = list(type = "noul", noul = 0.4),
      tone = list(type = "ranking", order = list("a", "b")),
      mood = list(type = "embedding", vector = list(0.1, 0.2))
    ),
    usage = list()
  )
  questions <- list(
    urgent = jev_noul("Urgent?"),
    tone = jev_noul("Tone?"),
    mood = jev_noul("Mood?")
  )

  warnings <- list()
  result <- withCallingHandlers(
    jev_parse_response(body, "typesafe", questions),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "jev_unknown_answer_warning")
  expect_identical(warnings[[1L]]$question_ids, c("tone", "mood"))
  expect_s3_class(result$answers$tone, "jev_unknown_answer")
  expect_identical(result$answers$tone$reason, "unknown_type")
  expect_identical(result$answers$tone$raw$order, list("a", "b"))
  expect_s3_class(result$answers$urgent, "jev_noul_answer")
})

test_that("a known answer type that differs from the question is kept raw", {
  body <- list(
    model = "jev-latest",
    answers = list(urgent = list(type = "score", score = 1)),
    usage = list()
  )

  expect_warning(
    result <- jev_parse_response(
      body, "typesafe", list(urgent = jev_noul("Urgent?"))
    ),
    class = "jev_unknown_answer_warning"
  )
  expect_s3_class(result$answers$urgent, "jev_unknown_answer")
  expect_identical(result$answers$urgent$reason, "type_mismatch")
  expect_identical(result$answers$urgent$expected_type, "noul")
  expect_identical(result$answers$urgent$raw$score, 1)
})

test_that("answers without a type or non-object answers still abort", {
  questions <- list(urgent = jev_noul("Urgent?"))
  body <- list(
    model = "m", answers = list(urgent = list(noul = 0.5)), usage = list()
  )
  expect_response_error(jev_parse_response(body, "typesafe", questions))
  body$answers$urgent <- "oops"
  expect_response_error(jev_parse_response(body, "typesafe", questions))
})

test_that("extra answer fields are preserved without warning", {
  body <- list(
    model = "jev-latest",
    answers = list(urgent = list(type = "noul", noul = 0.4, rationale = "x")),
    usage = list()
  )
  expect_no_warning(
    result <- jev_parse_response(
      body, "typesafe", list(urgent = jev_noul("Urgent?"))
    )
  )
  expect_identical(result$answers$urgent$rationale, "x")
})

test_that("partial parsing keeps unknown answers and question errors separate", {
  body <- list(
    model = "jev-latest",
    answers = list(
      valid = list(type = "noul", noul = 0.4),
      future = list(type = "ranking", order = list()),
      invalid = list(type = "noul", noul = 2)
    ),
    usage = list()
  )
  questions <- list(
    valid = jev_noul("A?"), future = jev_noul("B?"), invalid = jev_noul("C?")
  )

  expect_warning(
    parsed <- jevr:::jev_parse_response_partial(body, "typesafe", questions),
    class = "jev_unknown_answer_warning"
  )
  expect_identical(parsed$status, "partial")
  expect_identical(names(parsed$question_errors), "invalid")
  expect_identical(names(parsed$response$answers), c("valid", "future"))
  expect_s3_class(parsed$response$answers$future, "jev_unknown_answer")

  body$answers$valid <- NULL
  body$answers$invalid <- NULL
  expect_warning(
    parsed <- jevr:::jev_parse_response_partial(body, "typesafe", questions),
    class = "jev_unknown_answer_warning"
  )
  expect_identical(sort(names(parsed$question_errors)), c("invalid", "valid"))
  expect_identical(parsed$status, "partial")
})

test_that("extra question fields are forwarded to the request body and hashed", {
  choice <- jev_choice(
    "Pick", c(a = "A", b = "B"), reasoning = "high", tags = list("x")
  )
  score <- jev_score("Rate", c("lo", "hi"), future = TRUE)
  noul <- jev_noul("Yes?", extra = list(k = 1))

  expect_identical(
    names(choice), c("type", "instructions", "criteria", "reasoning", "tags")
  )
  expect_identical(choice$reasoning, "high")
  expect_no_error(jev_validate_questions(list(c = choice, s = score, n = noul)))

  payload <- jevr:::jev_request_payload(
    list(m = "x"), list(c = choice, n = noul), "jev-latest"
  )
  json <- jsonlite::fromJSON(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  expect_identical(json$questions$c$reasoning, "high")
  expect_identical(json$questions$c$tags, list("x"))
  expect_identical(json$questions$n$extra$k, 1L)

  plain <- jev_choice("Pick", c(a = "A", b = "B"))
  expect_false(identical(
    jev_spec_hash(list(q = plain)),
    jev_spec_hash(list(
      q = jev_choice("Pick", c(a = "A", b = "B"), reasoning = "high")
    ))
  ))
  expect_false(identical(
    jev_spec_hash(list(q = jev_noul("Y?", extra = 1))),
    jev_spec_hash(list(q = jev_noul("Y?", extra = 2)))
  ))
})

test_that("extra question fields survive specs and are validated", {
  spec <- jev_spec("s", "1", list(n = jev_noul("Yes?", extra = "v")))
  expect_identical(spec$questions$n$extra, "v")
  expect_identical(jevr:::jev_questions_value(spec)$n$extra, "v")
  expect_no_error(capture.output(print(spec)))
  expect_identical(spec$hash, jev_spec_hash(spec))

  expect_error(jev_noul("Y?", NULL, 1), class = "jev_input_error")
  expect_error(jev_noul("Y?", type = "x"), class = "jev_input_error")
  expect_error(jev_noul("Y?", a = 1, a = 2), class = "jev_input_error")
  expect_error(jev_noul("Y?", a = NULL), class = "jev_input_error")
  expect_error(jev_noul("Y?", a = function() 1), class = "jev_input_error")
})
