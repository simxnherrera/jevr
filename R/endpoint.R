#' Describe a TypeSafe-format System One endpoint
#'
#' A `jev_endpoint` describes any server that speaks the TypeSafe System One
#' wire format. Requests are sent as `POST <base_url>/v1/systemone` with a
#' bearer token read from an environment variable when the request is built.
#' The API key itself is never stored in the endpoint or in returned results.
#'
#' Pass an endpoint as the `provider` argument of [jev_ask()] or [jev_map()].
#' The preset names accepted there correspond to these endpoints:
#'
#' * `"typesafe"`: `https://api.typesafe.ai` (override with the
#'   `TYPESAFE_BASE_URL` environment variable), `TYPESAFE_API_KEY`,
#'   model `jev-latest`.
#' * `"openrouter"`: `https://openrouter.ai/api`, `OPENROUTER_API_KEY`, model
#'   `~typesafe/jev-latest`.
#' * `"vercel"`: `https://ai-gateway.vercel.sh/typesafe`, `AI_GATEWAY_API_KEY`,
#'   model `typesafe-ai/jev`.
#' * `"pydantic"`: `https://gateway-us.pydantic.dev/proxy/typesafe`,
#'   `PYDANTIC_AI_GATEWAY_API_KEY`, model `jev-latest`.
#'
#' @param base_url Server base URL without the `/v1/systemone` suffix.
#' @param api_key_env Name of the environment variable holding the API key.
#' @param model Default model for this endpoint, or `NULL` to require an
#'   explicit `model` in each call.
#' @param headers Named list of extra HTTP headers sent with every request.
#'   `Authorization` cannot be set here.
#' @param name Short name recorded in result metadata and used in execution
#'   identity. Defaults to the host and path of `base_url`.
#'
#' @return An object of class `jev_endpoint`.
#' @export
#' @examples
#' endpoint <- jev_endpoint(
#'   base_url = "https://gateway.example.com/typesafe",
#'   api_key_env = "EXAMPLE_GATEWAY_API_KEY",
#'   model = "jev-latest"
#' )
#' endpoint
#'
#' \dontrun{
#' jev_ask(
#'   "Payouts fail",
#'   list(urgent = jev_noul("Is this urgent?")),
#'   provider = endpoint
#' )
#' }
jev_endpoint <- function(
  base_url,
  api_key_env,
  model = NULL,
  headers = list(),
  name = NULL
) {
  jev_endpoint_new(
    base_url, api_key_env, model, headers, name,
    protocol = "systemone"
  )
}

jev_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

jev_endpoint_new <- function(
  base_url,
  api_key_env,
  model,
  headers,
  name,
  protocol
) {
  fail <- function(message) {
    jev_abort(paste0("jev_endpoint(): ", message), class = "jev_input_error")
  }

  if (!jev_scalar_string(base_url) || !grepl("^https?://[^/]", base_url)) {
    fail("base_url must be one http(s) URL string.")
  }
  base_url <- sub("/+$", "", base_url)
  if (grepl("/v1/systemone$", base_url)) {
    fail("base_url must not include the /v1/systemone suffix.")
  }
  if (!jev_scalar_string(api_key_env)) {
    fail("api_key_env must be one non-empty environment variable name.")
  }
  if (!is.null(model) && !jev_scalar_string(model)) {
    fail("model must be NULL or one non-empty string.")
  }
  if (!is.list(headers) ||
    (length(headers) > 0L &&
      (is.null(names(headers)) || anyNA(names(headers)) ||
        any(!nzchar(names(headers))) || anyDuplicated(names(headers)) > 0L ||
        !all(vapply(headers, jev_scalar_string, logical(1)))))) {
    fail("headers must be a named list of non-empty strings.")
  }
  if ("authorization" %in% tolower(names(headers))) {
    fail("headers cannot set Authorization; use api_key_env.")
  }
  if (is.null(name)) {
    name <- sub("^https?://", "", base_url)
  }
  if (!jev_scalar_string(name)) {
    fail("name must be NULL or one non-empty string.")
  }

  structure(
    list(
      name = name,
      base_url = base_url,
      api_key_env = api_key_env,
      model = model,
      headers = headers,
      protocol = protocol
    ),
    class = "jev_endpoint"
  )
}

#' @export
print.jev_endpoint <- function(x, ...) {
  cat("<jev_endpoint> ", x$name, "\n", sep = "")
  cat("URL:     ", jev_endpoint_url(x), "\n", sep = "")
  cat("API key: $", x$api_key_env, "\n", sep = "")
  cat("Model:   ", if (is.null(x$model)) "<none>" else x$model, "\n", sep = "")
  invisible(x)
}

jev_endpoint_url <- function(endpoint) {
  if (identical(endpoint$protocol, "decisions")) {
    return(paste0(endpoint$base_url, "/alpha/decisions"))
  }
  paste0(endpoint$base_url, "/v1/systemone")
}

jev_provider_name <- function(provider) {
  if (inherits(provider, "jev_endpoint")) provider$name else provider
}

jev_openrouter_headers <- function() {
  list(
    "HTTP-Referer" = "https://github.com/simxnherrera/jevr",
    "X-OpenRouter-Title" = "jevr"
  )
}

jev_endpoint_preset <- function(name) {
  switch(
    name,
    typesafe = {
      base_url <- Sys.getenv("TYPESAFE_BASE_URL", unset = "")
      jev_endpoint_new(
        if (nzchar(base_url)) base_url else "https://api.typesafe.ai",
        "TYPESAFE_API_KEY", "jev-latest", list(), "typesafe", "systemone"
      )
    },
    openrouter = jev_endpoint_new(
      "https://openrouter.ai/api", "OPENROUTER_API_KEY",
      "~typesafe/jev-latest", jev_openrouter_headers(), "openrouter",
      "systemone"
    ),
    openrouter_decisions = jev_endpoint_new(
      "https://openrouter.ai/api", "OPENROUTER_API_KEY",
      "~typesafe/jev-latest", jev_openrouter_headers(),
      "openrouter_decisions", "decisions"
    ),
    vercel = jev_endpoint_new(
      "https://ai-gateway.vercel.sh/typesafe", "AI_GATEWAY_API_KEY",
      "typesafe-ai/jev", list(), "vercel", "systemone"
    ),
    pydantic = jev_endpoint_new(
      "https://gateway-us.pydantic.dev/proxy/typesafe",
      "PYDANTIC_AI_GATEWAY_API_KEY", "jev-latest", list(), "pydantic",
      "systemone"
    ),
    jev_abort(
      paste0("Unknown provider preset: ", name, "."),
      class = "jev_input_error"
    )
  )
}

jev_resolve_provider <- function(provider) {
  if (inherits(provider, "jev_endpoint")) {
    return(provider)
  }
  if (!jev_scalar_string(provider)) {
    jev_abort(
      "provider must be a preset name or a jev_endpoint() object.",
      class = "jev_input_error"
    )
  }
  jev_endpoint_preset(provider)
}
