test_that("optional direct TypeSafe integration request", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("TYPESAFE_API_KEY")),
    "TYPESAFE_API_KEY is not available"
  )

  response <- jev_ask(
    state = "A customer reports a delayed payout.",
    questions = list(
      urgent = jev_noul("Does this report convey urgency?")
    ),
    provider = "typesafe",
    max_retries = 0
  )

  expect_s3_class(response, "jev_response")
  expect_identical(response$metadata$provider, "typesafe")
  expect_true(is.numeric(response$answers$urgent$noul))
})

test_that("optional OpenRouter integration request", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("OPENROUTER_API_KEY")),
    "OPENROUTER_API_KEY is not available"
  )

  response <- jev_ask(
    state = "A customer reports a delayed payout.",
    questions = list(
      urgent = jev_noul("Does this report convey urgency?")
    ),
    provider = "openrouter",
    max_retries = 0
  )

  expect_s3_class(response, "jev_response")
  expect_identical(response$metadata$provider, "openrouter")
  expect_true(is.numeric(response$answers$urgent$noul))
})
