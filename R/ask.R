jev_request_payload <- function(state, questions, model) {
  serialized_questions <- lapply(questions, unclass)
  names(serialized_questions) <- names(questions)

  list(
    model = model,
    state = state,
    questions = serialized_questions
  )
}

jev_questions_value <- function(questions) {
  if (inherits(questions, "jev_spec")) {
    jev_validate_spec(questions)
    return(jev_questions_from_manifest(questions$questions))
  }

  questions
}

jev_build_provider_request <- function(
  provider,
  state,
  questions,
  model,
  timeout,
  rate_limit = NULL,
  fallback = NULL
) {
  questions <- jev_questions_value(questions)
  provider <- jev_resolve_provider(provider)

  if (identical(provider$protocol, "decisions")) {
    return(jev_build_openrouter_request(
      state = state,
      questions = questions,
      model = model,
      timeout = timeout,
      rate_limit = rate_limit,
      endpoint = provider
    ))
  }

  jev_build_systemone_request(
    state = state,
    questions = questions,
    model = model,
    timeout = timeout,
    rate_limit = rate_limit,
    endpoint = provider,
    fallback = fallback
  )
}

jev_validate_request_options <- function(timeout, max_retries) {
  if (!is.numeric(timeout) || length(timeout) != 1L ||
    !is.finite(timeout) || timeout <= 0) {
    jev_abort(
      "timeout must be one positive, finite number of seconds.",
      class = "jev_input_error"
    )
  }

  if (!is.numeric(max_retries) || length(max_retries) != 1L ||
    !is.finite(max_retries) || max_retries < 0 ||
    max_retries != as.integer(max_retries)) {
    jev_abort(
      "max_retries must be one non-negative integer.",
      class = "jev_input_error"
    )
  }

  invisible(NULL)
}

#' Evaluate one state with one or more JEV questions
#'
#' `jev_ask()` represents one evaluation: it sends one state and a named set
#' of independent typed questions to a System One provider. Questions are
#' evaluated together, and the answer IDs are the names supplied in questions.
#'
#' @param state Content for the evaluation. Use a length-one character string
#'   for simple text, a named list for a JSON object, or an unnamed list for a
#'   JSON array. Other JSON-serializable R values are passed to the provider.
#' @param questions A non-empty named list of questions created with
#'   jev_choice(), jev_score(), or jev_noul(), or a `jev_spec()` object. The
#'   names become answer IDs.
#' @param provider Provider to use: a preset name or a [jev_endpoint()] object.
#'   Presets are `"typesafe"` (direct TypeSafe API), `"openrouter"` (OpenRouter's
#'   native System One endpoint), `"vercel"` (Vercel AI Gateway), `"pydantic"`
#'   (Pydantic AI Gateway), and the legacy `"openrouter_decisions"` (OpenRouter's
#'   alpha Decisions endpoint, which drops extra question fields). A custom
#'   [jev_endpoint()] is used as given and forwards extra question fields. This
#'   is distinct from `backend`, which selects an ellmer chat.
#' @param model Model name. Defaults to the endpoint's model: `jev-latest` for
#'   TypeSafe and Pydantic, `~typesafe/jev-latest` for OpenRouter, and
#'   `typesafe-ai/jev` for Vercel.
#' @param timeout Maximum time in seconds for each HTTP attempt.
#' @param max_retries Number of retries after the first failed or transient
#'   response. Retries use exponential backoff and honor Retry-After.
#' @param fallback Optional [jev_fallback()] describing a Vercel AI Gateway
#'   decision fallback. Only endpoints with the `"fallback"` capability (the
#'   `"vercel"` preset) accept it; otherwise a `jev_input_error` is raised
#'   before any request. See [jev_fallback()].
#' @param backend Optional `jev_llm()` backend. Cannot be combined with explicit
#'   `provider`, `model`, `timeout`, or positive `max_retries`. Timeout and
#'   retries belong to the chat transport. Omitted native defaults do not apply.
#'
#' @return An object of class jev_response with model, typed answers, usage,
#'   provider metadata, and the unmodified response in raw. `usage$cost` is the
#'   normalized USD cost (`usage.cost`, else Vercel
#'   `provider_metadata.gateway.cost`; omitted when absent or invalid; the map ledger records `NA`).
#'   `metadata$provider_metadata` keeps gateway metadata verbatim, with
#'   `metadata$final_provider` and `metadata$generation_id` when present; `metadata$requested_model` is the
#'   model asked for. With an ellmer
#'   backend, raw is the received structured object; answers are the validated
#'   jevr representation with locally calculated decisions.
#' @details `jev_ask()` keeps strict response parsing for compatibility: an
#'   invalid envelope or answer raises an error for the evaluation. Use
#'   `jev_map()` when collection-level partial results are needed.
#'
#' @section Authentication:
#' Set TYPESAFE_API_KEY for the direct TypeSafe provider or
#' OPENROUTER_API_KEY for OpenRouter. Keys are read at request time and are
#' never stored in the returned object.
#'
#' @section Request limits:
#' Choice questions are limited to 255 options and Score questions to 2-10
#' levels; violations are input errors. System One also documents 64k tokens
#' per request and 32k tokens for the state plus the longest question. Before
#' sending, jevr estimates size from the serialized JSON (characters / 4, an
#' approximation) and emits a warning of class `jev_preflight_warning` if a
#' limit looks exceeded; the request is still sent. Disable with
#' `options(jevr.preflight = FALSE)`.
#'
#' @section Errors:
#' Input errors are raised before any request. HTTP, authentication, transport,
#' JSON, and response-shape failures use classes beginning with jev_ and
#' include a corrective message without printing the API key.
#'
#' @export
#' @examplesIf identical(Sys.getenv("JEVR_RUN_EXAMPLES"), "true") && nzchar(Sys.getenv("TYPESAFE_API_KEY"))
#' questions <- list(
#'   department = jev_choice(
#'     "Which team should handle this?",
#'     c(billing = "Payments", technical = "Bugs")
#'   ),
#'   urgent = jev_noul("Does this request convey urgency?")
#' )
#' result <- jev_ask("My payouts have been failing for three days.", questions)
jev_ask <- function(
  state,
  questions,
  provider = c("typesafe", "openrouter", "vercel", "pydantic", "openrouter_decisions"),
  model = NULL,
  timeout = 30,
  max_retries = 3,
  backend = NULL,
  fallback = NULL
) {
  definition <- questions
  if (!is.null(backend)) {
    if (!is.null(fallback)) {
      jev_abort("fallback cannot be combined with an ellmer backend.", class = "jev_input_error")
    }
    jev_validate_llm_options(
      backend, !missing(provider), !missing(model),
      !missing(timeout), if (missing(max_retries)) 0 else max_retries
    )
    state <- jev_validate_state(state)
    questions <- jev_validate_questions(jev_questions_value(questions))
    evaluation <- jev_llm_evaluate(state, questions, backend, definition)
    result <- jev_llm_parse(evaluation, questions, backend)$response
    result$metadata$spec_hash <- jev_spec_hash(definition)
    result$metadata$state_hash <- jev_state_hash(state)
    return(result)
  }
  if (!inherits(provider, "jev_endpoint")) provider <- match.arg(provider)
  provider <- jev_resolve_provider(provider)
  state <- jev_validate_state(state)
  questions <- jev_questions_value(questions)
  questions <- jev_validate_questions(questions)
  jev_validate_request_options(timeout, max_retries)

  if (is.null(model)) {
    model <- jev_default_model(provider)
  }

  if (!is.character(model) || length(model) != 1L || is.na(model) ||
    !nzchar(model)) {
    jev_abort(
      "model must be one non-empty character string.",
      class = "jev_input_error"
    )
  }

  jev_validate_fallback(fallback, questions, provider, model)
  jev_preflight_ask(state, questions)

  provider_name <- jev_provider_name(provider)
  provider_request <- jev_build_provider_request(
    provider = provider,
    state = state,
    questions = questions,
    model = model,
    timeout = timeout,
    fallback = fallback
  )
  response <- jev_send_request(
    provider_request$request,
    provider = provider_name,
    max_retries = as.integer(max_retries)
  )
  body <- jev_body_json(response, provider_name)

  result <- jev_parse_response(
    body,
    provider_name,
    questions
  )
  result$metadata <- utils::modifyList(
    result$metadata,
    jev_response_metadata(response, provider_name)
  )
  result$metadata$requested_model <- model
  result
}
