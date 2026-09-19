jev_abort <- function(message, class = "jev_error", details = list()) {
  condition <- structure(
    c(list(message = as.character(message)), details),
    class = unique(c(class, "jev_error", "error", "condition"))
  )
  stop(condition)
}

jev_server_message <- function(body) {
  if (!is.list(body)) {
    return(NULL)
  }

  if (is.list(body$error) && is.character(body$error$message)) {
    return(body$error$message[[1L]])
  }

  if (is.character(body$error) && length(body$error) == 1L) {
    return(body$error)
  }

  if (is.character(body$message) && length(body$message) == 1L) {
    return(body$message)
  }

  NULL
}

jev_http_error <- function(status, provider, body, attempts) {
  detail <- jev_server_message(body)
  hint <- switch(as.character(status),
    "401" = "Check the provider API key environment variable.",
    "402" = "Check the provider account credits or quota.",
    "422" = "Check the state and question definitions.",
    "429" = "The retry limit was reached after rate limiting.",
    "529" = "The retry limit was reached while the provider was overloaded.",
    NULL
  )

  message <- paste0(
    provider,
    " request failed with HTTP ",
    status,
    " after ",
    attempts,
    " attempt",
    if (attempts == 1L) "" else "s",
    "."
  )

  if (!is.null(detail)) {
    message <- paste(message, detail)
  }

  if (!is.null(hint)) {
    message <- paste(message, hint)
  }

  jev_abort(
    message,
    class = "jev_http_error",
    details = list(
      status = status,
      provider = provider,
      attempts = attempts,
      body = body
    )
  )
}

jev_json_error <- function(provider, error) {
  message <- paste0(
    provider,
    " returned a response that is not valid JSON: ",
    conditionMessage(error)
  )

  jev_abort(
    message,
    class = "jev_parse_error",
    details = list(provider = provider, cause = error)
  )
}
