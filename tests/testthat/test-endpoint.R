endpoint_questions <- function() {
  list(urgent = jev_noul("Does this request convey urgency?"))
}

endpoint_response <- function() {
  httr2::response(
    status_code = 200,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(paste0(
      '{"model":"jev-1.13","answers":{"urgent":{"type":"noul","noul":0.9}},',
      '"usage":{"input_tokens":3,"output_tokens":0,"cost":0.000002}}'
    ))
  )
}

test_that("presets build the documented URL, auth header, model and body", {
  withr::local_envvar(
    TYPESAFE_BASE_URL = NA,
    TYPESAFE_API_KEY = "k-typesafe",
    OPENROUTER_API_KEY = "k-openrouter",
    AI_GATEWAY_API_KEY = "k-vercel",
    PYDANTIC_AI_GATEWAY_API_KEY = "k-pydantic"
  )
  expected <- list(
    typesafe = c("https://api.typesafe.ai/v1/systemone", "k-typesafe", "jev-latest"),
    openrouter = c("https://openrouter.ai/api/v1/systemone", "k-openrouter", "~typesafe/jev-latest"),
    vercel = c("https://ai-gateway.vercel.sh/typesafe/v1/systemone", "k-vercel", "typesafe-ai/jev"),
    pydantic = c("https://gateway-us.pydantic.dev/proxy/typesafe/v1/systemone", "k-pydantic", "jev-latest")
  )

  for (preset in names(expected)) {
    captured <- NULL
    mock <- function(req) {
      captured <<- req
      endpoint_response()
    }
    result <- httr2::with_mocked_responses(
      mock,
      jev_ask("Payouts failed", endpoint_questions(),
        provider = preset, max_retries = 0
      )
    )
    expect_equal(captured$url, expected[[preset]][[1]], info = preset)
    expect_equal(
      httr2::req_get_headers(captured, redacted = "reveal")[["Authorization"]], paste("Bearer", expected[[preset]][[2]]),
      info = preset
    )
    expect_equal(captured$body$data$model, expected[[preset]][[3]], info = preset)
    expect_equal(captured$body$data$state, "Payouts failed", info = preset)
    expect_equal(captured$body$data$questions$urgent$type, "noul", info = preset)
    expect_equal(result$metadata$provider, preset)
    expect_false(grepl(expected[[preset]][[2]], paste(deparse(result), collapse = ""), fixed = TRUE))
  }
})

test_that("OpenRouter preset keeps attribution headers and surfaces usage cost", {
  withr::local_envvar(OPENROUTER_API_KEY = "k")
  captured <- NULL
  mock <- function(req) {
    captured <<- req
    endpoint_response()
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_ask("x", endpoint_questions(), provider = "openrouter", max_retries = 0)
  )
  expect_equal(captured$headers[["X-OpenRouter-Title"]], "jevr")
  expect_equal(result$raw$usage$cost, 2e-06)
})

test_that("TYPESAFE_BASE_URL is honored by the typesafe preset only", {
  withr::local_envvar(
    TYPESAFE_BASE_URL = "https://staging.example.com/",
    TYPESAFE_API_KEY = "k"
  )
  expect_equal(
    jev_build_provider_request(
      jev_resolve_provider("typesafe"), "s", endpoint_questions(), "m", 5
    )$request$url,
    "https://staging.example.com/v1/systemone"
  )
  withr::local_envvar(VERCEL_X = "1", AI_GATEWAY_API_KEY = "k")
  expect_equal(
    jev_build_provider_request(
      jev_resolve_provider("vercel"), "s", endpoint_questions(), "m", 5
    )$request$url,
    "https://ai-gateway.vercel.sh/typesafe/v1/systemone"
  )
})

test_that("legacy openrouter_decisions keeps the alpha endpoint", {
  withr::local_envvar(OPENROUTER_API_KEY = "k")
  built <- jev_build_provider_request(
    jev_resolve_provider("openrouter_decisions"), "s", endpoint_questions(),
    "~typesafe/jev-latest", 5
  )
  expect_equal(built$request$url, "https://openrouter.ai/api/alpha/decisions")
  expect_equal(built$provider, "openrouter_decisions")
})

test_that("custom endpoints are used by jev_ask() and jev_map()", {
  withr::local_envvar(MY_GATEWAY_KEY = "secret-gateway-key")
  endpoint <- jev_endpoint(
    "https://gw.example.com/typesafe/",
    "MY_GATEWAY_KEY",
    model = "custom-model",
    headers = list("X-Team" = "blue"),
    name = "gw"
  )
  expect_s3_class(endpoint, "jev_endpoint")
  expect_equal(endpoint$base_url, "https://gw.example.com/typesafe")
  expect_false(any(grepl("secret-gateway-key", unlist(endpoint))))

  captured <- list()
  mock <- function(req) {
    captured[[length(captured) + 1L]] <<- req
    endpoint_response()
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_ask("x", endpoint_questions(), provider = endpoint, max_retries = 0)
  )
  expect_equal(captured[[1]]$url, "https://gw.example.com/typesafe/v1/systemone")
  expect_equal(captured[[1]]$headers[["X-Team"]], "blue")
  expect_equal(captured[[1]]$body$data$model, "custom-model")
  expect_equal(result$metadata$provider, "gw")

  mapped <- httr2::with_mocked_responses(
    mock,
    jev_map(c("a", "b"), endpoint_questions(),
      provider = endpoint, max_retries = 0, progress = FALSE, rate_limit = 1000
    )
  )
  expect_equal(length(captured), 3L)
  expect_equal(captured[[3]]$url, "https://gw.example.com/typesafe/v1/systemone")
  expect_equal(unique(attr(mapped, "requests")$provider), "gw")
  expect_false(grepl("secret-gateway-key", paste(deparse(mapped), collapse = ""), fixed = TRUE))
})

test_that("endpoint without model requires an explicit model", {
  withr::local_envvar(MY_GATEWAY_KEY = "k")
  endpoint <- jev_endpoint("https://gw.example.com", "MY_GATEWAY_KEY")
  expect_equal(endpoint$name, "gw.example.com")
  expect_error(
    jev_ask("x", endpoint_questions(), provider = endpoint),
    class = "jev_input_error"
  )
})

test_that("missing API key names the endpoint's environment variable", {
  withr::local_envvar(MY_GATEWAY_KEY = NA)
  endpoint <- jev_endpoint("https://gw.example.com", "MY_GATEWAY_KEY", "m")
  expect_error(
    jev_ask("x", endpoint_questions(), provider = endpoint),
    "MY_GATEWAY_KEY",
    class = "jev_auth_error"
  )
})

test_that("jev_endpoint validates its arguments", {
  expect_error(jev_endpoint("not a url", "K"), class = "jev_input_error")
  expect_error(jev_endpoint("https://a.com/v1/systemone", "K"), class = "jev_input_error")
  expect_error(jev_endpoint("https://a.com", ""), class = "jev_input_error")
  expect_error(jev_endpoint("https://a.com", "K", model = 1), class = "jev_input_error")
  expect_error(
    jev_endpoint("https://a.com", "K", headers = list("Authorization" = "x")),
    class = "jev_input_error"
  )
  expect_error(
    jev_endpoint("https://a.com", "K", headers = list("x")),
    class = "jev_input_error"
  )
  expect_error(
    jev_ask("x", endpoint_questions(), provider = "nope"),
    "arg"
  )
})

test_that("jev_endpoint prints without secrets", {
  expect_snapshot(print(jev_endpoint(
    "https://gw.example.com/typesafe", "MY_GATEWAY_KEY", "m", name = "gw"
  )))
})
