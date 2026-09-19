test_that("question constructors preserve the System One schema", {
  choice <- jev_choice(
    instructions = "Which team should handle this?",
    criteria = c(billing = "Payments", technical = "Bugs")
  )
  score <- jev_score(
    instructions = "How severe is this?",
    criteria = c("Cosmetic", "Degraded", "Blocking")
  )
  noul <- jev_noul(
    instructions = "Does this convey urgency?",
    criteria = c(true = "Time-sensitive", false = "No urgency")
  )

  expect_s3_class(choice, "jev_choice_question")
  expect_equal(choice$type, "choice")
  expect_equal(choice$criteria$billing, "Payments")
  expect_s3_class(score, "jev_score_question")
  expect_equal(score$type, "score")
  expect_equal(score$criteria[[3L]], "Blocking")
  expect_s3_class(noul, "jev_noul_question")
  expect_equal(noul$type, "noul")
  expect_equal(names(noul$criteria), c("true", "false"))
})

test_that("choice and score validation gives actionable errors", {
  expect_snapshot(error = TRUE, jev_choice("Choose", c(only = "one")))
  expect_snapshot(error = TRUE, jev_choice("Choose", c("unnamed", "options")))
  expect_snapshot(error = TRUE, jev_score("Rate", "only one level"))
})

test_that("structured instructions and optional descriptions are preserved", {
  choice <- jev_choice(
    instructions = list(
      question = "Which option applies?",
      focus = list("message", "policy")
    ),
    criteria = list(
      first = list(covers = "The first case"),
      second = NULL
    )
  )

  expect_equal(choice$instructions$focus, list("message", "policy"))
  expect_equal(choice$criteria$first$covers, "The first case")
  expect_null(choice$criteria$second)
})

test_that("noul criteria must name both outcomes", {
  expect_snapshot(
    error = TRUE,
    jev_noul("Is this true?", criteria = c(true = "Yes"))
  )
})

test_that("structured question values are validated recursively", {
  question <- jev_choice(
    instructions = list(
      question = "Which option applies?",
      context = list(labels = list("first", "second"), note = NULL)
    ),
    criteria = list(
      first = list(description = "The first case"),
      second = list(description = "The second case")
    )
  )

  expect_identical(
    jev_validate_questions(list(choice = question)),
    list(choice = question)
  )
})

test_that("modified questions are checked beyond their class", {
  question <- jev_choice("Which option applies?", c(first = "One", second = "Two"))
  question$criteria$second <- NA_character_

  expect_error(
    jev_validate_questions(list(choice = question)),
    "questions\\$choice\\$criteria\\[\\[second\\]\\]",
    class = "jev_input_error"
  )

  class(question) <- c("custom_question", class(question))
  expect_error(
    jev_validate_questions(list(choice = question)),
    "ambiguous question class",
    class = "jev_input_error"
  )
})

test_that("question validation reports invalid nested names and numbers", {
  duplicate_names <- structure(
    list(first = "One", second = "Two"),
    names = c("option", "option")
  )
  expect_error(
    jev_choice("Which option applies?", duplicate_names),
    "duplicate option names",
    class = "jev_input_error"
  )

  partial_names <- structure(
    list(first = "One", second = "Two"),
    names = c("option", "")
  )
  expect_error(
    jev_choice("Which option applies?", partial_names),
    "non-empty name for every option",
    class = "jev_input_error"
  )

  expect_error(
    jev_score(
      instructions = list(question = "Rate", value = list(score = Inf)),
      criteria = c("low", "high")
    ),
    "instructions\\$value\\$score",
    class = "jev_input_error"
  )

  expect_error(
    jev_score("Rate", as.factor(c("low", "high"))),
    "criteria must",
    class = "jev_input_error"
  )
})
