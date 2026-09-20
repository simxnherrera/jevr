jev_abort <- function(message, class = "jev_error", details = list()) {
  condition <- structure(
    c(list(message = as.character(message)), details),
    class = unique(c(class, "jev_error", "error", "condition"))
  )
  stop(condition)
}

jev_error_record <- function(error) {
  if (is.null(error)) {
    return(NULL)
  }

  if (is.list(error) && !inherits(error, "condition") &&
    !is.null(error$class) && !is.null(error$message)) {
    return(error)
  }

  details <- if (is.list(error)) {
    error[names(error) %in% c(
      "status", "provider", "attempts", "retryable", "argument", "retry_at",
      "scope", "field_path", "request_id"
    )]
  } else {
    list()
  }

  list(
    class = class(error)[[1L]],
    message = conditionMessage(error),
    details = details
  )
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

jev_404_shared_configuration <- function(body) {
  values <- c(
    jev_server_message(body),
    if (is.list(body)) unlist(body, recursive = TRUE, use.names = FALSE)
  )
  text <- tolower(paste(values, collapse = " "))
  shared_terms <- paste(
    c(
      "model", "endpoint", "route", "deployment", "provider",
      "configuration", "api[ _-]?version", "systemone"
    ),
    collapse = "|"
  )
  missing_terms <- paste(
    c(
      "not[ _-]?found", "unknown", "invalid",
      "does[ _-]?not[ _-]?exist", "unavailable"
    ),
    collapse = "|"
  )

  grepl(
    paste0("(", shared_terms, ").*(", missing_terms, ")|(", missing_terms,
      ").*(", shared_terms, ")"),
    text,
    perl = TRUE
  )
}

jev_http_global_stop <- function(status, body) {
  status %in% c(401L, 402L, 403L) ||
    (identical(as.integer(status), 404L) && jev_404_shared_configuration(body))
}

jev_http_error <- function(status, provider, body, attempts) {
  detail <- jev_server_message(body)
  hint <- switch(as.character(status),
    "401" = "Check the provider API key environment variable.",
    "402" = "Check the provider account credits or quota.",
    "404" = if (jev_404_shared_configuration(body)) {
      "Check the provider endpoint and model configuration."
    } else {
      NULL
    },
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
    details = list(provider = provider, cause_message = conditionMessage(error))
  )
}
