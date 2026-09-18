jev_default_model <- function(provider) {
  c(
    typesafe = "jev-latest",
    openrouter = "~typesafe/jev-latest"
  )[[provider]]
}

jev_api_key_name <- function(provider) {
  c(
    typesafe = "TYPESAFE_API_KEY",
    openrouter = "OPENROUTER_API_KEY"
  )[[provider]]
}

jev_api_key <- function(provider) {
  name <- jev_api_key_name(provider)
  key <- Sys.getenv(name, unset = "")

  if (!nzchar(key)) {
    jev_abort(
      paste0(
        "No API key was found for ", provider, ". Set the ", name,
        " environment variable before calling jev_ask()."
      ),
      class = "jev_auth_error"
    )
  }

  key
}

jev_build_request <- function(
  url,
  payload,
  api_key,
  timeout,
  headers = list()
) {
  request_headers <- c(
    list(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    headers
  )

  request <- httr2::request(url)
  request <- do.call(httr2::req_headers, c(list(request), request_headers))
  request <- httr2::req_body_json(
    request,
    payload,
    auto_unbox = TRUE,
    null = "null"
  )
  httr2::req_timeout(request, seconds = timeout)
}

jev_retryable_status <- function(status) {
  status %in% c(408L, 429L, 500L, 502L, 503L, 504L, 524L, 529L)
}

jev_retry_delay <- function(attempt, response = NULL) {
  retry_after <- if (is.null(response)) {
    NA_real_
  } else {
    tryCatch(
      httr2::resp_retry_after(response),
      error = function(error) NA_real_
    )
  }

  if (length(retry_after) == 1L && is.finite(retry_after)) {
    return(max(0, retry_after))
  }

  min(0.5 * 2^(attempt - 1L), 30)
}

jev_send_request <- function(
  request,
  provider,
  max_retries,
  perform = httr2::req_perform,
  sleep = Sys.sleep
) {
  attempt <- 0L

  repeat {
    attempt <- attempt + 1L
    result <- tryCatch(
      perform(request),
      error = identity
    )

    if (inherits(result, "httr2_http")) {
      result <- result$resp
    }

    if (inherits(result, "condition")) {
      if (attempt <= max_retries) {
        sleep(jev_retry_delay(attempt))
        next
      }

      jev_abort(
        paste0(
          provider,
          " request could not connect after ",
          attempt,
          " attempt",
          if (attempt == 1L) "" else "s",
          ": ",
          conditionMessage(result)
        ),
        class = "jev_transport_error",
        details = list(provider = provider, attempts = attempt, cause = result)
      )
    }

    status <- httr2::resp_status(result)
    if (jev_retryable_status(status) && attempt <= max_retries) {
      sleep(jev_retry_delay(attempt, result))
      next
    }

    if (status >= 400L) {
      body <- tryCatch(
        httr2::resp_body_json(result, simplifyVector = FALSE),
        error = function(error) NULL
      )
      jev_http_error(status, provider, body, attempt)
    }

    return(result)
  }
}

jev_body_json <- function(response, provider) {
  tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(error) jev_json_error(provider, error)
  )
}
