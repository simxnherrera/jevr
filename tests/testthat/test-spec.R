test_that("specs preserve their data-only definition and recalculate hashes", {
  questions <- list(
    relation = jev_choice(
      "How does evidence relate to claim?",
      c(supports = "Supports", contradicts = "Contradicts")
    ),
    current = jev_noul("Does the relation currently hold?")
  )
  spec <- jev_spec("relation_validation", "1.2.0", questions)

  expect_s3_class(spec, "jev_spec")
  expect_equal(names(spec), c("schema_version", "name", "version", "questions", "hash"))
  expect_identical(spec$questions$relation$type, "choice")
  expect_identical(
    jev_definition_manifest(spec)$questions,
    spec$questions
  )
  expect_identical(spec$hash, jev_spec_hash(spec))

  changed <- spec
  changed$questions$relation$instructions <- "A different question"
  expect_false(identical(spec$hash, jev_spec_hash(changed)))
})

test_that("spec hashes preserve question, object, and array order", {
  first <- jev_spec(questions = list(
    one = jev_choice("Choose", c(first = "1", second = "2")),
    two = jev_noul("Is it true?")
  ))
  reversed_questions <- jev_spec(questions = list(
    two = jev_noul("Is it true?"),
    one = jev_choice("Choose", c(first = "1", second = "2"))
  ))
  reversed_criteria <- jev_spec(questions = list(
    one = jev_choice("Choose", c(second = "2", first = "1")),
    two = jev_noul("Is it true?")
  ))

  expect_false(identical(first$hash, reversed_questions$hash))
  expect_false(identical(first$hash, reversed_criteria$hash))
})

test_that("spec validation rejects operational and non-data inputs", {
  expect_error(
    jev_spec(name = "", questions = list(one = jev_noul("Is it true?"))),
    class = "jev_input_error"
  )
  expect_error(
    jev_spec(
      questions = list(one = jev_noul("Is it true?")),
      version = function() NULL
    ),
    class = "jev_input_error"
  )
})

test_that("spec manifests round-trip back to executable questions", {
  questions <- list(
    choice = jev_choice("Pick one", c(first = "First", second = "Second")),
    score = jev_score("How much?", c("Low", "High")),
    noul = jev_noul("Is it true?", c(true = "Yes", false = "No"))
  )
  spec <- jev_spec("round_trip", "1.0.0", questions)
  restored <- jevr:::jev_questions_from_manifest(spec$questions)

  expect_identical(
    unname(lapply(restored, unclass)),
    unname(lapply(questions, unclass))
  )
  expect_identical(jev_spec_hash(spec), spec$hash)
})

test_that("specs remain data-only after an attempted mutation", {
  spec <- jev_spec(questions = list(one = jev_noul("Is it true?")))
  mutated <- spec
  mutated$questions$one$instructions <- function() NULL

  expect_error(jev_spec_hash(mutated), class = "jev_input_error")
})
