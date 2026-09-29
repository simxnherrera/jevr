test_that("optional direct TypeSafe integration request", {
  skip_on_cran()
  skip_if(
    !identical(Sys.getenv("JEVR_RUN_INTEGRATION_TESTS"), "true"),
    "Set JEVR_RUN_INTEGRATION_TESTS=true to run paid integration tests"
  )
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
    !identical(Sys.getenv("JEVR_RUN_INTEGRATION_TESTS"), "true"),
    "Set JEVR_RUN_INTEGRATION_TESTS=true to run paid integration tests"
  )
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

test_that("optional ellmercodex integration evaluates all question types", {
  skip_on_cran()
  skip_if(
    !identical(Sys.getenv("JEVR_RUN_ELLMERCODEX_INTEGRATION"), "true"),
    "Set JEVR_RUN_ELLMERCODEX_INTEGRATION=true to use the subscription"
  )
  skip_if_not_installed("ellmer", "0.5.0")
  skip_if_not_installed("ellmercodex", "0.1.64")
  backend <- jev_llm(
    chat = function() ellmercodex::chat_codex(model = "gpt-6-luna", effort = "medium"),
    model = "gpt-6-luna",
    inference_options = list(reasoning_effort = "medium")
  )
  result <- jev_ask("Payouts have failed for three days.", list(
    department = jev_choice("Which team?", c(billing = "Payments", technical = "Bugs")),
    severity = jev_score("How severe?", c("Cosmetic", "Degraded", "Blocking")),
    urgent = jev_noul("Does this report convey urgency?")
  ), backend = backend)
  expect_s3_class(result, "jev_response")
  expect_identical(names(result$answers), c("department", "severity", "urgent"))
  expect_identical(result$metadata$upstream_provider, "codex")
  expect_identical(result$metadata$probability_source, "llm_self_report")
  expect_null(result$answers$department$confidence)
})
