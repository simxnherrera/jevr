jev_openrouter_text <- function(value, argument) {
  if (is.null(value)) {
    return("")
  }

  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }

  jev_abort(
    paste0(
      "OpenRouter Decisions currently accepts string values for ", argument,
      "; use a string or call jev_ask() with provider = typesafe."
    ),
    class = "jev_provider_error"
  )
}

jev_openrouter_question <- function(question, id) {
  instructions <- jev_openrouter_text(
    question$instructions,
    paste0("questions$", id, "$instructions")
  )

  if (identical(question$type, "choice")) {
    criteria <- vapply(
      names(question$criteria),
      function(name) {
        jev_openrouter_text(
          question$criteria[[name]],
          paste0("questions$", id, "$criteria$", name)
        )
      },
      character(1)
    )

    return(list(
      type = "choice",
      instructions = instructions,
      criteria = criteria
    ))
  }

  if (identical(question$type, "score")) {
    criteria <- vapply(
      seq_along(question$criteria),
      function(index) {
        jev_openrouter_text(
          question$criteria[[index]],
          paste0("questions$", id, "$criteria[[", index, "]]")
        )
      },
      character(1)
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
      true = jev_openrouter_text(
        question$criteria$true,
        paste0("questions$", id, "$criteria$true")
      ),
      false = jev_openrouter_text(
        question$criteria$false,
        paste0("questions$", id, "$criteria$false")
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

jev_request_openrouter <- function(
  state,
  questions,
  model,
  timeout,
  max_retries
) {
  api_key <- jev_api_key("openrouter")
  payload <- list(
    model = model,
    state = state,
    questions = jev_openrouter_questions(questions)
  )
  request <- jev_build_request(
    url = "https://openrouter.ai/api/alpha/decisions",
    payload = payload,
    api_key = api_key,
    timeout = timeout,
    headers = stats::setNames(
      c(
        "https://github.com/simxnherrera/jevr",
        "jevr"
      ),
      c("HTTP-Referer", "X-OpenRouter-Title")
    )
  )
  response <- jev_send_request(
    request,
    provider = "openrouter",
    max_retries = max_retries
  )

  list(body = jev_body_json(response, "openrouter"), response = response)
}
