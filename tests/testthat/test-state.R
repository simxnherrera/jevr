test_that("state validation keeps named and unnamed structures intact", {
  object_state <- list(
    message = "A payment failed",
    attempts = list(1L, 2L)
  )
  array_state <- list("first message", "second message")

  expect_identical(jev_validate_state(object_state), object_state)
  expect_identical(jev_validate_state(array_state), array_state)
  expect_identical(jev_validate_state("plain text"), "plain text")
})

test_that("state validation rejects empty or non-serializable values", {
  expect_snapshot(error = TRUE, jev_validate_state(character()))
  expect_error(
    jev_validate_state(integer()),
    "empty atomic vector",
    class = "jev_input_error"
  )
  expect_snapshot(error = TRUE, jev_validate_state(function() NULL))
})

test_that("state validation recurses through JSON objects and arrays", {
  state <- list(
    message = "A payment failed",
    optional = NULL,
    history = list(
      list(attempt = 1L, successful = FALSE),
      list(attempt = 2L, successful = TRUE)
    )
  )

  expect_identical(jev_validate_state(state), state)
  expect_identical(
    jev_validate_state(list("first", list("second", "third"))),
    list("first", list("second", "third"))
  )
})

test_that("state errors identify nested paths and non-finite values", {
  expect_error(
    jev_validate_state(list(payload = list(items = list("ok", NA_character_)))),
    "state\\$payload\\$items\\[\\[2\\]\\]",
    class = "jev_input_error"
  )
  expect_error(
    jev_validate_state(list(payload = list(score = NaN))),
    "state\\$payload\\$score",
    class = "jev_input_error"
  )
  expect_error(
    jev_validate_state(list(payload = list(score = -Inf))),
    "state\\$payload\\$score",
    class = "jev_input_error"
  )
})

test_that("state validation rejects missing, partial, and duplicate names", {
  expect_error(
    jev_validate_state(structure(list(1, 2), names = c("first", NA_character_))),
    "missing names",
    class = "jev_input_error"
  )
  expect_error(
    jev_validate_state(structure(list(1, 2), names = c("first", ""))),
    "missing or partial names",
    class = "jev_input_error"
  )
  expect_error(
    jev_validate_state(structure(list(1, 2), names = c("first", "first"))),
    "duplicate names",
    class = "jev_input_error"
  )
})

test_that("state validation rejects ambiguous R classes and functions recursively", {
  expect_error(
    jev_validate_state(list(date = as.Date("2026-01-01"))),
    "state\\$date",
    class = "jev_input_error"
  )
  expect_error(
    jev_validate_state(list(payload = list(callback = function() NULL))),
    "state\\$payload\\$callback",
    class = "jev_input_error"
  )
})
