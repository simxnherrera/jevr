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
  rate_limit = NULL
) {
  questions <- jev_questions_value(questions)

  switch(
    provider,
    typesafe = jev_build_typesafe_request(
      state = state,
      questions = questions,
      model = model,
      timeout = timeout,
      rate_limit = rate_limit
    ),
    openrouter = jev_build_openrouter_request(
      state = state,
      questions = questions,
      model = model,
      timeout = timeout,
      rate_limit = rate_limit
    )
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
#' @param provider Provider to use: "typesafe" for the direct TypeSafe API or
#'   "openrouter" for OpenRouter's Decisions endpoint.
#' @param model Model name. Defaults to the stable jev-latest alias for
#'   TypeSafe and ~typesafe/jev-latest for OpenRouter.
#' @param timeout Maximum time in seconds for each HTTP attempt.
#' @param max_retries Number of retries after the first failed or transient
#'   response. Retries use exponential backoff and honor Retry-After.
#'
#' @return An object of class jev_response with model, typed answers, usage,
#'   provider metadata, and the unmodified response in raw.
#' @details `jev_ask()` keeps strict response parsing for compatibility: an
#'   invalid envelope or answer raises an error for the evaluation. Use
#'   `jev_map()` when collection-level partial results are needed.
#'
#' @section Authentication:
#' Set TYPESAFE_API_KEY for the direct TypeSafe provider or
#' OPENROUTER_API_KEY for OpenRouter. Keys are read at request time and are
#' never stored in the returned object.
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
  provider = c("typesafe", "openrouter"),
  model = NULL,
  timeout = 30,
  max_retries = 3
) {
  provider <- match.arg(provider)
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

  provider_request <- jev_build_provider_request(
    provider = provider,
    state = state,
    questions = questions,
    model = model,
    timeout = timeout
  )
  response <- jev_send_request(
    provider_request$request,
    provider = provider,
    max_retries = as.integer(max_retries)
  )
  body <- jev_body_json(response, provider)

  result <- jev_parse_response(
    body,
    provider,
    questions
  )
  result$metadata <- utils::modifyList(
    result$metadata,
    jev_response_metadata(response, provider)
  )
  result
}
