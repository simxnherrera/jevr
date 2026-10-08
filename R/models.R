#' List models served by an endpoint
#'
#' Calls `GET <base_url>/v1/models` on a TypeSafe-format endpoint and returns
#' the models it advertises. It uses the same authentication, timeout, retry
#' and error handling as [jev_ask()].
#'
#' OpenRouter serves its own model catalogue at `/api/v1/models` in a
#' different format, so `jev_models()` raises a `jev_unsupported_error`
#' without making a request for the `"openrouter"` and
#' `"openrouter_decisions"` presets and for any endpoint hosted on
#' `openrouter.ai`. Browse <https://openrouter.ai/typesafe> instead.
#'
#' @param provider A preset name (`"typesafe"`, `"vercel"`, `"pydantic"`) or a
#'   [jev_endpoint()] object.
#' @param timeout Request timeout in seconds.
#' @param max_retries Maximum number of retries for transient failures.
#'
#' @return A data frame with character columns `name`, `description` and
#'   `release_date` (as reported by the server; dates are not parsed). The raw
#'   response metadata (status, headers, request id, timing) is attached as the
#'   `"metadata"` attribute.
#' @export
#' @examples
#' \dontrun{
#' jev_models()
#' jev_models("vercel")
#' }
jev_models <- function(provider = "typesafe", timeout = 30, max_retries = 2) {
  jev_validate_request_options(timeout, max_retries)
  endpoint <- jev_resolve_provider(provider)

  if (identical(endpoint$protocol, "decisions") ||
    grepl("^https?://([^/]*\\.)?openrouter\\.ai(/|$)", endpoint$base_url)) {
    jev_abort(
      paste0(
        "jev_models() does not support the ", endpoint$name,
        " endpoint: OpenRouter lists models in its own catalogue format. ",
        "Browse https://openrouter.ai/typesafe instead."
      ),
      class = "jev_unsupported_error",
      details = list(provider = endpoint$name)
    )
  }

  request <- jev_build_models_request(endpoint, timeout)
  response <- jev_send_request(
    request,
    provider = endpoint$name,
    max_retries = as.integer(max_retries)
  )
  body <- jev_body_json(response, endpoint$name)
  models <- jev_parse_models(body, endpoint$name)
  attr(models, "metadata") <- jev_response_metadata(response, endpoint$name)
  models
}

jev_build_models_request <- function(
  endpoint,
  timeout,
  api_key = jev_api_key(endpoint)
) {
  request_headers <- c(
    list(Authorization = paste("Bearer", api_key)),
    endpoint$headers
  )
  request <- httr2::request(paste0(endpoint$base_url, "/v1/models"))
  request <- do.call(httr2::req_headers, c(list(request), request_headers))
  request <- httr2::req_timeout(request, seconds = timeout)
  httr2::req_error(request, is_error = function(response) FALSE)
}

jev_parse_models <- function(body, provider) {
  fail <- function(message) {
    jev_abort(
      paste0(provider, " models response ", message),
      class = "jev_response_error"
    )
  }

  if (!is.list(body) || is.null(names(body)) || !is.list(body$models) ||
    !is.null(names(body$models))) {
    fail("must be an object with a models array.")
  }

  field <- function(item, key, required) {
    value <- item[[key]]
    if (is.null(value)) {
      if (required) fail(paste0("has a model without ", key, "."))
      return(NA_character_)
    }
    if (!is.character(value) || length(value) != 1L ||
      (required && (is.na(value) || !nzchar(value)))) {
      fail(paste0("has a model whose ", key, " is not a string."))
    }
    value
  }

  items <- body$models
  for (item in items) {
    if (!is.list(item) || is.null(names(item))) {
      fail("has a models entry that is not an object.")
    }
  }

  data.frame(
    name = vapply(items, field, character(1), key = "name", required = TRUE),
    description = vapply(
      items, field, character(1), key = "description", required = FALSE
    ),
    release_date = vapply(
      items, field, character(1), key = "release_date", required = FALSE
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
