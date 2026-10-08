fb_questions <- function() {
  list(
    intent = jev_choice("Which team?", c(billing = "Charges", account = "Sign-in")),
    quality = jev_score("Quality?", c("bad", "ok", "good")),
    refund = jev_noul("Refund?")
  )
}

fb_body <- function(answers) {
  jsonlite::toJSON(
    list(model = "openai/gpt-6-astra", answers = answers,
      usage = list(input_tokens = 3, output_tokens = 1)),
    auto_unbox = TRUE
  )
}

test_that("jev_fallback serialises the documented condition shapes", {
  fb <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
  wire <- jevr:::jev_fallback_provider_options(fb)
  expect_equal(
    as.character(jsonlite::toJSON(wire, auto_unbox = TRUE)),
    paste0('{"gateway":{"models":[{"model":"openai/gpt-6-astra",',
      '"when":{"question":"intent","confidenceBelow":0.6}}]}}')
  )

  fb <- jev_fallback(
    "openai/gpt-6-astra",
    when = jev_when_any(
      jev_when("intent", confidence_below = 0.6),
      jev_when_at_least(1, jev_when(probability_between = c(0.4, 0.6)))
    ),
    on_error = "anthropic/claude-sonnet-5"
  )
  json <- as.character(jsonlite::toJSON(
    jevr:::jev_fallback_provider_options(fb), auto_unbox = TRUE
  ))
  expect_match(json, '"any":[{"question":"intent","confidenceBelow":0.6},', fixed = TRUE)
  expect_match(json, '{"atLeast":{"count":1,"conditions":[{"probabilityBetween":[0.4,0.6]}]}}', fixed = TRUE)
  expect_match(json, '"anthropic/claude-sonnet-5"]', fixed = TRUE)
  expect_output(print(fb), "jev_fallback")
})

test_that("conditions are validated structurally", {
  expect_error(jev_when("a"), class = "jev_input_error")
  expect_error(jev_when("a", 0.5, c(0.1, 0.2)), class = "jev_input_error")
  expect_error(jev_when("a", confidence_below = 1.5), class = "jev_input_error")
  expect_error(jev_when("a", probability_between = c(0.7, 0.2)), class = "jev_input_error")
  expect_error(jev_when_any(), class = "jev_input_error")
  expect_error(jev_when_at_least(3, jev_when("a", 0.5)), class = "jev_input_error")
  expect_error(jev_fallback("m"), class = "jev_input_error")
  expect_error(
    jev_fallback("m", "a", 0.5, when = jev_when("a", 0.5)),
    class = "jev_input_error"
  )
})

test_that("fallback is checked against endpoint, questions and model", {
  withr::local_envvar(AI_GATEWAY_API_KEY = "k", TYPESAFE_API_KEY = "k")
  fb <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
  # No request is ever attempted for these.
  boom <- function(req) stop("request sent")
  httr2::with_mocked_responses(boom, {
    expect_error(
      jev_ask("s", fb_questions(), provider = "typesafe", fallback = fb),
      class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", fb_questions(), provider = "vercel",
        fallback = jev_fallback("openai/gpt-6-astra", "nope", confidence_below = 0.6)),
      "unknown question", class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", fb_questions(), provider = "vercel",
        fallback = jev_fallback("openai/gpt-6-astra", "refund", confidence_below = 0.6)),
      class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", fb_questions(), provider = "vercel",
        fallback = jev_fallback("openai/gpt-6-astra", "intent", probability_between = c(0.4, 0.6))),
      class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", fb_questions(), provider = "vercel", model = "openai/gpt-6-astra",
        fallback = fb),
      class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", list(r = jev_noul("R?")), provider = "vercel",
        fallback = jev_fallback("openai/gpt-6-astra", confidence_below = 0.6)),
      class = "jev_input_error"
    )
    expect_error(
      jev_map("s", fb_questions(), provider = "typesafe", fallback = fb, progress = FALSE),
      class = "jev_input_error"
    )
    expect_error(
      jev_ask("s", fb_questions(), provider = "vercel", fallback = list()),
      class = "jev_input_error"
    )
  })
})

test_that("custom endpoints opt in with capabilities", {
  expect_error(
    jev_endpoint("https://a.com", "K", capabilities = "bogus"),
    class = "jev_input_error"
  )
  endpoint <- jev_endpoint("https://a.com", "K", "m", capabilities = "fallback")
  withr::local_envvar(K = "k")
  fb <- jev_fallback("x/y", "intent", confidence_below = 0.6)
  sent <- NULL
  mock <- function(req) {
    sent <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(fb_body(list(
        intent = list(type = "choice", choice = "billing",
          probabilities = list(billing = 0.9, account = 0.1), confidence = 0.8)
      )))
    )
  }
  httr2::with_mocked_responses(
    mock,
    jev_ask("s", fb_questions()["intent"], provider = endpoint, fallback = fb, max_retries = 0)
  )
  body <- sent$body$data
  expect_equal(body$providerOptions$gateway$models[[1]]$when$confidenceBelow, 0.6)
})

test_that("jev_ask sends providerOptions and parses fallback headers", {
  withr::local_envvar(AI_GATEWAY_API_KEY = "k")
  fb <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
  sent <- NULL
  mock <- function(req) {
    sent <<- req
    httr2::response(
      status_code = 200,
      headers = list(
        "Content-Type" = "application/json",
        "x-ai-gateway-decision-fallback-triggered" = "true",
        "x-ai-gateway-decision-fallback-final-model" = "openai/gpt-6-astra",
        "x-ai-gateway-decision-fallback-primary-model" = "typesafe-ai/jev",
        "x-ai-gateway-decision-fallback-triggering-questions" =
          utils::URLencode('["intent","café"]', reserved = TRUE),
        "x-ai-gateway-decision-fallback-triggering-questions-truncated" = "true"
      ),
      body = charToRaw(fb_body(list(
        intent = list(type = "choice", choice = "billing",
          probabilities = setNames(list(), character()), confidence = 0)
      )))
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_ask("s", fb_questions()["intent"], provider = "vercel", fallback = fb, max_retries = 0)
  )
  models <- sent$body$data$providerOptions$gateway$models
  expect_equal(models[[1]]$model, "openai/gpt-6-astra")
  expect_equal(models[[1]]$when, list(question = "intent", confidenceBelow = 0.6))

  meta <- result$metadata$fallback
  expect_true(meta$triggered)
  expect_identical(meta$final_model, "openai/gpt-6-astra")
  expect_identical(meta$primary_model, "typesafe-ai/jev")
  expect_identical(meta$triggering_questions, c("intent", "café"))
  expect_true(meta$triggering_questions_truncated)
  expect_true("x-ai-gateway-decision-fallback-triggered" %in% names(result$metadata$headers))

  answer <- result$answers$intent
  expect_true(is.na(answer$confidence))
  expect_identical(answer$choice, "billing")
  expect_identical(names(answer$probabilities), c("billing", "account"))
  expect_true(all(is.na(answer$probabilities)))
})

test_that("no fallback metadata when the condition did not match", {
  withr::local_envvar(AI_GATEWAY_API_KEY = "k")
  fb <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(fb_body(list(
        intent = list(type = "choice", choice = "billing",
          probabilities = list(billing = 0.9, account = 0.1), confidence = 0.8)
      )))
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_ask("s", fb_questions()["intent"], provider = "vercel", fallback = fb, max_retries = 0)
  )
  expect_null(result$metadata$fallback)
  expect_equal(result$answers$intent$confidence, 0.8)
})

test_that("sentinel answers are NA in long format, bands and score answers", {
  questions <- fb_questions()
  raw <- list(
    model = "openai/gpt-6-astra",
    answers = list(
      intent = list(type = "choice", choice = "billing",
        probabilities = setNames(list(), character()), confidence = 0),
      quality = list(type = "score", score = 2,
        probabilities = setNames(list(), character()), confidence = 0),
      refund = list(type = "noul", noul = 0.5)
    )
  )
  response <- jev_parse_response(raw, "vercel", questions)
  expect_true(is.na(response$answers$quality$confidence))
  expect_true(isTRUE(response$answers$quality$confidence_unavailable))

  long <- as.data.frame(response, format = "long")
  intent <- long[long$question_id == "intent", ]
  expect_equal(nrow(intent), 2L)
  expect_true(all(is.na(intent$probability)))
  expect_true(all(is.na(intent$confidence)))
  expect_identical(intent$selected, c(TRUE, FALSE))
  expect_equal(sum(long$question_id == "quality"), 3L)

  expect_true(is.na(jev_band(response, "intent", breaks = c(0, 0.5, 1))))
  expect_false(is.na(jev_band(response, "refund", breaks = c(0, 0.6, 1))))
})

test_that("zero confidence with real probabilities is still validated", {
  raw <- list(
    model = "m",
    answers = list(intent = list(type = "choice", choice = "billing",
      probabilities = list(billing = 0.5, account = 0.5), confidence = 0))
  )
  response <- jev_parse_response(raw, "vercel", fb_questions()["intent"])
  expect_identical(response$answers$intent$confidence, 0)
  expect_null(response$answers$intent$confidence_unavailable)
})

test_that("fallback is part of the execution identity", {
  q <- fb_questions()
  fb <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
  fb2 <- jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.7)
  base <- jev_execution_id("s", q, "vercel", "typesafe-ai/jev")
  expect_identical(
    base,
    jev_execution_id("s", q, "vercel", "typesafe-ai/jev", fallback = NULL)
  )
  with_fb <- jev_execution_id("s", q, "vercel", "typesafe-ai/jev", fallback = fb)
  expect_false(identical(base, with_fb))
  expect_false(identical(
    with_fb, jev_execution_id("s", q, "vercel", "typesafe-ai/jev", fallback = fb2)
  ))
})

test_that("jev_map sends fallback and surfaces metadata per result", {
  withr::local_envvar(AI_GATEWAY_API_KEY = "k")
  fb <- jev_fallback("openai/gpt-6-astra", "refund", probability_between = c(0.4, 0.6))
  bodies <- list()
  mock <- function(req) {
    bodies[[length(bodies) + 1L]] <<- req$body$data
    httr2::response(
      status_code = 200,
      headers = list(
        "Content-Type" = "application/json",
        "x-ai-gateway-decision-fallback-triggered" = "true",
        "x-ai-gateway-decision-fallback-final-model" = "openai/gpt-6-astra"
      ),
      body = charToRaw(fb_body(list(refund = list(type = "noul", noul = 0.5))))
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(c(a = "x"), fb_questions()["refund"], provider = "vercel",
      fallback = fb, max_retries = 0, progress = FALSE)
  )
  expect_equal(bodies[[1]]$providerOptions$gateway$models[[1]]$when$probabilityBetween, c(0.4, 0.6))
  item <- result[["a"]]
  expect_identical(item$response$metadata$fallback$final_model, "openai/gpt-6-astra")
  plain <- jev_execution_id("x", fb_questions()["refund"], "vercel", "typesafe-ai/jev")
  expect_false(identical(item$provenance$execution_id, plain))
})
