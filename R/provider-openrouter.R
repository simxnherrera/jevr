jev_openrouter_value <- function(value, argument, allow_null = FALSE) {
  tryCatch(
    jev_validate_json_value(value, argument, allow_null = allow_null),
    jev_input_error = function(error) {
      jev_abort(
        paste0(
          "OpenRouter Decisions cannot encode ", argument,
          "; use a JSON value supported by the provider."
        ),
        class = "jev_provider_error",
        details = list(provider = "openrouter", argument = argument, cause = error)
      )
    }
  )
}

jev_openrouter_question <- function(question, id) {
  instructions <- jev_openrouter_value(
    question$instructions,
    paste0("questions$", id, "$instructions")
  )

  if (identical(question$type, "choice")) {
    criteria <- lapply(
      names(question$criteria),
      function(name) {
        jev_openrouter_value(
          question$criteria[[name]],
          paste0("questions$", id, "$criteria$", name),
          allow_null = TRUE
        )
      }
    )
    names(criteria) <- names(question$criteria)

    return(list(
      type = "choice",
      instructions = instructions,
      criteria = criteria
    ))
  }

  if (identical(question$type, "score")) {
    criteria <- lapply(
      seq_along(question$criteria),
      function(index) {
        jev_openrouter_value(
          question$criteria[[index]],
          paste0("questions$", id, "$criteria[[", index, "]]")
        )
      }
    )

    return(list(
      type = "score",
      instructions = instructions,
      criteria = unname(criteria)
    ))
  }

  if (is.null(question$criteria)) {
    return(list(type = "noul", instructions = instructions))
  }

  list(
    type = "noul",
    instructions = instructions,
    criteria = list(
      true = jev_openrouter_value(
        question$criteria$true,
        paste0("questions$", id, "$criteria$true"),
        allow_null = TRUE
      ),
      false = jev_openrouter_value(
        question$criteria$false,
        paste0("questions$", id, "$criteria$false"),
        allow_null = TRUE
      )
    )
  )
}

jev_openrouter_questions <- function(questions) {
  result <- lapply(
    names(questions),
    function(id) jev_openrouter_question(questions[[id]], id)
  )
  names(result) <- names(questions)
  result
}

jev_build_openrouter_request <- function(
  state,
  questions,
  model,
  timeout,
  rate_limit = NULL,
  endpoint = jev_endpoint_preset("openrouter_decisions"),
  api_key = jev_api_key(endpoint)
) {
  payload <- list(
    model = model,
    state = state,
    questions = jev_openrouter_questions(questions)
  )
  request <- jev_build_request(
    url = jev_endpoint_url(endpoint),
    payload = payload,
    api_key = api_key,
    timeout = timeout,
    rate_limit = rate_limit,
    throttle_realm = paste0("jevr-", endpoint$name),
    headers = endpoint$headers
  )
  list(request = request, provider = endpoint$name)
}
