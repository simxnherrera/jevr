models_body <- list(models = list(
  list(name = "jev-latest", description = "Alias", release_date = "2026-09-01"),
  list(name = "jev-1.13", description = "Pinned")
))

models_response <- function(body = models_body, status = 200L) {
  httr2::response_json(status_code = status, body = body)
}

test_that("models requests are GET with bearer auth and endpoint headers", {
  withr::local_envvar(TYPESAFE_API_KEY = "k", TYPESAFE_BASE_URL = NA)
  request <- jev_build_models_request(jev_resolve_provider("typesafe"), 5)
  expect_equal(request$url, "https://api.typesafe.ai/v1/models")
  expect_null(request$body)
  expect_equal(httr2::req_get_method(request), "GET")
  expect_equal(httr2::req_get_headers(request, redacted = "reveal")$Authorization, "Bearer k")

  withr::local_envvar(X_KEY = "x")
  endpoint <- jev_endpoint(
    "https://gw.example.com/ts", "X_KEY", headers = list("X-Team" = "a")
  )
  request <- jev_build_models_request(endpoint, 5)
  expect_equal(request$url, "https://gw.example.com/ts/v1/models")
  expect_equal(httr2::req_get_headers(request)[["X-Team"]], "a")

  withr::local_envvar(AI_GATEWAY_API_KEY = "v")
  expect_equal(
    jev_build_models_request(jev_resolve_provider("vercel"), 5)$url,
    "https://ai-gateway.vercel.sh/typesafe/v1/models"
  )
})

test_that("jev_models() returns a data frame with metadata", {
  withr::local_envvar(TYPESAFE_API_KEY = "k")
  testthat::local_mocked_bindings(
    jev_send_request = function(request, provider, max_retries, ...) {
      expect_equal(provider, "typesafe")
      expect_equal(max_retries, 2L)
      models_response()
    }
  )
  models <- jev_models()
  expect_s3_class(models, "data.frame")
  expect_equal(names(models), c("name", "description", "release_date"))
  expect_equal(models$name, c("jev-latest", "jev-1.13"))
  expect_equal(models$release_date, c("2026-09-01", NA))
  expect_equal(attr(models, "metadata")$http_status, 200L)
})

test_that("empty model lists give a zero-row data frame", {
  withr::local_envvar(TYPESAFE_API_KEY = "k")
  testthat::local_mocked_bindings(
    jev_send_request = function(...) models_response(list(models = list()))
  )
  models <- jev_models()
  expect_equal(nrow(models), 0L)
  expect_equal(names(models), c("name", "description", "release_date"))
})

test_that("OpenRouter is rejected without a request", {
  testthat::local_mocked_bindings(
    jev_send_request = function(...) stop("should not be called")
  )
  expect_error(jev_models("openrouter"), class = "jev_unsupported_error")
  expect_error(
    jev_models("openrouter_decisions"), class = "jev_unsupported_error"
  )
  expect_error(jev_models("openrouter"), "openrouter.ai/typesafe")
})

test_that("malformed responses raise jev_response_error", {
  withr::local_envvar(TYPESAFE_API_KEY = "k")
  bad <- list(
    list(data = list()),
    list(models = "x"),
    list(models = list(list(description = "no name"))),
    list(models = list(list(name = 1))),
    list(models = list(list(name = "a", release_date = 5))),
    list(models = list("a"))
  )
  for (body in bad) {
    testthat::local_mocked_bindings(
      jev_send_request = function(...) models_response(body)
    )
    expect_error(jev_models(), class = "jev_response_error")
  }
})

test_that("HTTP errors and bad arguments use existing classes", {
  withr::local_envvar(TYPESAFE_API_KEY = "k")
  testthat::local_mocked_bindings(
    jev_send_request = function(request, provider, max_retries, ...) {
      jev_http_error(401L, provider, NULL, 1L)
    }
  )
  expect_error(jev_models(), class = "jev_http_error")
  expect_error(jev_models(timeout = -1), class = "jev_input_error")
  expect_error(jev_models("nope"), class = "jev_input_error")
  withr::local_envvar(TYPESAFE_API_KEY = NA)
  expect_error(jev_models(), class = "jev_auth_error")
})
