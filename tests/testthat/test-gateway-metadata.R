gateway_body <- function(usage = '"usage":{"input_tokens":3,"output_tokens":0}',
                         gateway = NULL) {
  if (is.null(gateway)) {
    gateway <- paste0(
      '{"routing":{"originalModelId":"typesafe-ai/jev","resolvedProvider":"typesafe",',
      '"canonicalSlug":"typesafe-ai/jev","finalProvider":"typesafe"},',
      '"cost":"0.00001155","marketCost":"0.00001155","gatewayCost":"0",',
      '"generationId":"gen_123"}'
    )
  }
  paste0(
    '{"id":"r1","model":"jev-1.13.0","answers":{"urgent":{"type":"noul","noul":0.8}},',
    usage, ',"provider_metadata":{"gateway":', gateway, '}}'
  )
}

gateway_parse <- function(json) {
  jev_parse_response(
    jsonlite::fromJSON(json, simplifyVector = FALSE), "vercel",
    list(urgent = jev_noul("Urgent?"))
  )
}

test_that("Vercel gateway cost string is normalized and metadata preserved", {
  result <- gateway_parse(gateway_body())
  expect_identical(result$usage$cost, 0.00001155)
  expect_identical(result$metadata$final_provider, "typesafe")
  expect_identical(result$metadata$generation_id, "gen_123")
  expect_identical(result$metadata$provider_metadata$gateway$cost, "0.00001155")
  expect_identical(
    result$metadata$provider_metadata$gateway$routing$resolvedProvider, "typesafe"
  )
})

test_that("usage.cost is preferred and invalid costs are dropped", {
  result <- gateway_parse(gateway_body('"usage":{"cost":0.5}'))
  expect_identical(result$usage$cost, 0.5)

  for (bad in c('"abc"', '""', "null", "-1", "[1,2]", '{"a":1}')) {
    result <- gateway_parse(gateway_body(gateway = paste0('{"cost":', bad, "}")))
    expect_null(result$usage$cost, info = bad)
  }
  result <- gateway_parse(gateway_body('"usage":{"cost":"oops"}'))
  expect_equal(result$usage$cost, 0.00001155)
})

test_that("responses without gateway data have no cost and no routing fields", {
  raw <- list(
    model = "jev-1.13.0",
    answers = list(urgent = list(type = "noul", noul = 0.8))
  )
  result <- jev_parse_response(raw, "typesafe", list(urgent = jev_noul("Urgent?")))
  expect_null(result$usage$cost)
  expect_null(result$metadata$final_provider)
  expect_null(result$metadata$provider_metadata)
})

test_that("jev_map ledger carries gateway cost without double counting", {
  withr::local_envvar(AI_GATEWAY_API_KEY = "k")
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(gateway_body())
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(c(a = "one", b = "two"), list(urgent = jev_noul("Urgent?")),
      provider = "vercel", max_retries = 0, progress = FALSE
    )
  )
  requests <- attr(result, "requests")
  expect_equal(requests$cost, c(0.00001155, 0.00001155))
  expect_equal(summary(result)$observed_cost, 2 * 0.00001155)
})

test_that("jev_ask records the requested model and long format reports it", {
  withr::local_envvar(TYPESAFE_API_KEY = "k")
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(gateway_body())
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_ask("x", list(urgent = jev_noul("Urgent?")), max_retries = 0)
  )
  expect_identical(result$metadata$requested_model, "jev-latest")
  long <- as.data.frame(result)
  expect_true(all(long$requested_model == "jev-latest"))
  expect_true(all(long$model == "jev-1.13.0"))
})
