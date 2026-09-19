test_that("state identity distinguishes Unicode and numeric types", {
  expect_false(identical(
    jev_state_hash("café"),
    jev_state_hash("cafe")
  ))
  expect_false(identical(jev_state_hash(1L), jev_state_hash(1)))
  expect_false(identical(jev_state_hash(list("one")), jev_state_hash("one")))
})

test_that("canonical encoding preserves object and array order", {
  object <- list(first = 1L, second = "á")
  reversed_object <- list(second = "á", first = 1L)
  array <- list("first", list(nested = 2))
  reversed_array <- rev(array)

  expect_false(identical(jev_state_hash(object), jev_state_hash(reversed_object)))
  expect_false(identical(jev_state_hash(array), jev_state_hash(reversed_array)))
  expect_identical(
    jevr:::jev_identity_hash(list(a = 1L, b = 1)),
    "912fb6b9777cc538591e69fb5c1c11be19521913548f3125f2ab20166484dd8c"
  )
})

test_that("execution identity excludes secrets and operational options", {
  definition <- list(urgent = jev_noul("Is this urgent?"))
  base <- jev_execution_id(
    "A payment failed",
    definition,
    provider = "typesafe",
    model = "jev-1.13.0",
    inference_options = list(temperature = 0),
    routing_options = list(timeout = 10, concurrency = 1, api_key = "first")
  )
  changed_operational <- jev_execution_id(
    "A payment failed",
    definition,
    provider = "typesafe",
    model = "jev-1.13.0",
    inference_options = list(temperature = 0),
    routing_options = list(timeout = 99, concurrency = 8, api_key = "second")
  )
  changed_semantic <- jev_execution_id(
    "A payment failed",
    definition,
    provider = "typesafe",
    model = "jev-1.13.0",
    inference_options = list(temperature = 1),
    routing_options = list(timeout = 99, concurrency = 8, api_key = "third")
  )

  expect_identical(base, changed_operational)
  expect_false(identical(base, changed_semantic))
  expect_false(grepl("first|second|third", base))
})

test_that("execution identity excludes all operational settings and secrets", {
  definition <- list(urgent = jev_noul("Is this urgent?"))
  id <- jev_execution_id(
    "state", definition, "typesafe", "jev-1.13.0",
    routing_options = list(
      timeout = 1, retries = 1, max_retries = 1, concurrency = 1,
      api.key = "not-in-the-id", progress = TRUE
    )
  )
  unchanged <- jev_execution_id(
    "state", definition, "typesafe", "jev-1.13.0",
    routing_options = list(
      timeout = 999, retries = 99, max_retries = 99, concurrency = 99,
      api.key = "a-different-secret", progress = FALSE
    )
  )

  expect_identical(id, unchanged)
  expect_false(grepl("not-in-the-id|a-different-secret", id))
})

test_that("execution identity changes with state, definition, provider, and model", {
  definition <- list(urgent = jev_noul("Is this urgent?"))
  id <- jev_execution_id("state", definition, "typesafe", "jev-1.13.0")

  expect_false(identical(id, jev_execution_id(
    "other state", definition, "typesafe", "jev-1.13.0"
  )))
  expect_false(identical(id, jev_execution_id(
    "state", list(urgent = jev_noul("Is this urgent now?")),
    "typesafe", "jev-1.13.0"
  )))
  expect_false(identical(id, jev_execution_id(
    "state", definition, "openrouter", "jev-1.13.0"
  )))
  expect_false(identical(id, jev_execution_id(
    "state", definition, "typesafe", "jev-1.14.0"
  )))
})
