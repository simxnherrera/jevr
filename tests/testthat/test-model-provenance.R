test_that("alias detection recognises moving aliases", {
  expect_true(jev_model_is_alias("jev-latest"))
  expect_true(jev_model_is_alias("jev-preview"))
  expect_true(jev_model_is_alias("~typesafe/jev-latest"))
  expect_true(jev_model_is_alias("typesafe-ai/jev"))
  expect_false(jev_model_is_alias("jev-1.13.0"))
  expect_false(jev_model_is_alias("typesafe-ai/jev-1.13.0"))
  expect_true(is.na(jev_model_is_alias(NULL)))
})

test_that("jev_map records provenance and warns once on model drift", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  calls <- 0L
  mock <- function(req) {
    calls <<- calls + 1L
    model <- if (calls == 1L) "jev-1.13.0" else "jev-1.14.0"
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(paste0(
        '{"id":"d","model":"', model,
        '","answers":{"urgent":{"type":"noul","noul":0.8}},',
        '"usage":{"input_tokens":1,"output_tokens":2}}'
      ))
    )
  }
  expect_warning(
    result <- httr2::with_mocked_responses(
      mock,
      jev_map(
        c(a = "First", b = "Second", c = "Third"),
        list(urgent = jev_noul("Is this urgent?")),
        concurrency = 1, max_retries = 0, progress = FALSE
      )
    ),
    class = "jev_model_drift_warning"
  )
  expect_equal(summary(result)$answered_models, c("jev-1.13.0", "jev-1.14.0"))
  wide <- as.data.frame(result)
  expect_true(all(wide$requested_model == "jev-latest"))
  expect_true(all(wide$is_alias))
  expect_equal(wide$answered_model[[1]], "jev-1.13.0")
  expect_equal(result$a$provenance$answered_model, "jev-1.13.0")
})

test_that("jev_map does not warn when a pinned model answers consistently", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(paste0(
        '{"id":"d","model":"jev-1.13.0","answers":{"urgent":',
        '{"type":"noul","noul":0.8}},',
        '"usage":{"input_tokens":1,"output_tokens":2}}'
      ))
    )
  }
  expect_no_warning(
    result <- httr2::with_mocked_responses(
      mock,
      jev_map(
        c(a = "First", b = "Second"),
        list(urgent = jev_noul("Is this urgent?")),
        model = "jev-1.13.0", max_retries = 0, progress = FALSE
      )
    )
  )
  expect_false(any(as.data.frame(result)$is_alias))
})

test_that("results record requested and answered models", {
  mk <- function(model) {
    jev_result(
      "a", 1L, "success",
      response = structure(
        list(model = model, answers = list(urgent = list(type = "noul", noul = 0.8))),
        class = "jev_response"
      ),
      provenance = list(requested_model = "jev-latest", is_alias = TRUE)
    )
  }
  set <- jev_result_set(
    list(a = mk("jev-1.13.0"), b = mk("jev-1.14.0")),
    definition = jev_definition_manifest(
      list(urgent = jev_noul("Does this request convey urgency?"))
    ),
    requests = jev_empty_attempt_ledger()
  )
  wide <- as.data.frame(set)
  expect_equal(wide$requested_model, c("jev-latest", "jev-latest"))
  expect_equal(wide$answered_model, c("jev-1.13.0", "jev-1.14.0"))
  expect_true(all(wide$is_alias))
  long <- as.data.frame(set, format = "long")
  expect_true(all(long$requested_model == "jev-latest"))
  expect_setequal(long$model, c("jev-1.13.0", "jev-1.14.0"))
  expect_warning(
    jev_warn_model_drift(unclass(set)),
    class = "jev_model_drift_warning"
  )
  expect_no_warning(jev_warn_model_drift(unclass(set)[1]))
  expect_equal(
    jev_map_summary(unclass(set), jev_empty_attempt_ledger(), 0, 1)$answered_models,
    c("jev-1.13.0", "jev-1.14.0")
  )
})
