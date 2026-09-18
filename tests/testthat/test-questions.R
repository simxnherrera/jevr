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
