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
  headers = list(),
  rate_limit = NULL,
  throttle_realm = NULL
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
  request <- httr2::req_timeout(request, seconds = timeout)
  request <- httr2::req_error(request, is_error = function(response) FALSE)

  if (!is.null(rate_limit)) {
    request <- httr2::req_throttle(
      request,
      rate = rate_limit,
      fill_time_s = 1,
      realm = throttle_realm
    )
  }

  request
}

jev_retryable_status <- function(status) {
  status %in% c(408L, 429L, 500L, 502L, 503L, 504L, 524L, 529L)
}

jev_retry_delay <- function(
  attempt,
  response = NULL,
  jitter = 0,
  random = stats::runif
) {
  retry_after <- if (is.null(response)) {
    NA_real_
  } else {
    tryCatch(
      httr2::resp_retry_after(response),
      error = function(error) NA_real_
    )
  }

  if (is.null(response) ||
    !length(retry_after) || is.na(retry_after) || !is.finite(retry_after)) {
    headers <- tryCatch(
      httr2::resp_headers(response),
      error = function(error) list()
    )
    header_names <- tolower(names(headers))
    retry_after_ms_position <- match("retry-after-ms", header_names)
    retry_after_ms <- tryCatch(
      if (is.na(retry_after_ms_position)) {
        NULL
      } else {
        headers[[retry_after_ms_position]]
      },
      error = function(error) NULL
    )
    retry_after_ms_numeric <- suppressWarnings(as.numeric(retry_after_ms))
    if (length(retry_after_ms_numeric) == 1L &&
      !is.na(retry_after_ms_numeric) && is.finite(retry_after_ms_numeric)) {
      retry_after <- retry_after_ms_numeric / 1000
    }
  }

  if (length(retry_after) == 1L && is.finite(retry_after)) {
    return(max(0, retry_after))
  }

  delay <- min(0.5 * 2^(attempt - 1L), 30)
  if (jitter > 0) {
    delay <- delay * (1 + random(1L, min = -jitter, max = jitter))
  }

  max(0, delay)
}

jev_is_retryable_condition <- function(condition) {
  inherits(
    condition,
    c(
      "httr2_failure",
      "curl_error",
      "curl_fetch_error",
      "timeout_error",
      "connection_error"
    )
  ) || grepl(
    "connection|timed? ?out|timeout|could not resolve|reset by peer",
    conditionMessage(condition),
    ignore.case = TRUE
  )
}

jev_send_request <- function(
  request,
  provider,
  max_retries,
  perform = httr2::req_perform,
  sleep = Sys.sleep,
  retry_budget = Inf,
  jitter = 0,
  random = stats::runif,
  now = function() unname(proc.time()[["elapsed"]]),
  on_attempt = NULL
) {
  attempt <- 0L
  started_at <- now()

  repeat {
    attempt <- attempt + 1L
    result <- tryCatch(
      perform(request),
      error = identity
    )

    if (is.function(on_attempt)) {
      on_attempt(attempt, result)
    }

    if (inherits(result, "httr2_http")) {
      result <- result$resp
    }

    if (inherits(result, "condition")) {
      retryable <- jev_is_retryable_condition(result)
      if (retryable && attempt <= max_retries) {
        delay <- jev_retry_delay(attempt, jitter = jitter, random = random)
        if (is.finite(retry_budget) && now() + delay - started_at > retry_budget) {
          jev_abort(
            paste0(
              provider,
              " request exceeded its retry budget after ",
              attempt,
              " attempt",
              if (attempt == 1L) "" else "s",
              "."
            ),
            class = "jev_transport_error",
            details = list(
              provider = provider,
              attempts = attempt,
              retryable = TRUE,
              cause = result
            )
          )
        }
        sleep(delay)
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
        details = list(
          provider = provider,
          attempts = attempt,
          retryable = retryable,
          cause = result
        )
      )
    }

    status <- httr2::resp_status(result)
    if (jev_retryable_status(status) && attempt <= max_retries) {
      delay <- jev_retry_delay(
        attempt,
        result,
        jitter = jitter,
        random = random
      )
      if (is.finite(retry_budget) && now() + delay - started_at > retry_budget) {
        body <- tryCatch(
          httr2::resp_body_json(result, simplifyVector = FALSE),
          error = function(error) NULL
        )
        jev_http_error(status, provider, body, attempt)
      }
      sleep(delay)
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

jev_response_metadata <- function(response, provider) {
  headers <- tryCatch(
    httr2::resp_headers(response),
    error = function(error) list()
  )
  header_names <- tolower(names(headers))
  allowed_headers <- c(
    "content-length", "content-type", "date", "retry-after", "retry-after-ms",
    "server", "x-openrouter-request-id", "x-ratelimit-limit",
    "x-ratelimit-remaining", "x-ratelimit-reset", "x-ratelimit-request-id",
    "x-request-id"
  )
  headers <- headers[header_names %in% allowed_headers]
  header_names <- tolower(names(headers))
  status <- tryCatch(httr2::resp_status(response), error = function(error) NA_integer_)
  timing <- tryCatch(httr2::resp_timing(response), error = function(error) NULL)
  request_id <- NULL
  for (header_name in c(
    "x-request-id",
    "x-ratelimit-request-id",
    "x-openrouter-request-id"
  )) {
    position <- match(header_name, header_names)
    if (!is.na(position)) {
      request_id <- headers[[position]]
      break
    }
  }

  list(
    provider = provider,
    http_status = status,
    headers = headers,
    request_id = request_id,
    timing = timing
  )
}
