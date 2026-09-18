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
  expect_snapshot(error = TRUE, jev_validate_state(function() NULL))
})
