jev_build_systemone_request <- function(
  state,
  questions,
  model,
  timeout,
  rate_limit = NULL,
  endpoint,
  api_key = jev_api_key(endpoint),
  fallback = NULL
) {
  payload <- jev_request_payload(state, questions, model)
  if (!is.null(fallback)) {
    payload$providerOptions <- jev_fallback_provider_options(fallback)
  }
  request <- jev_build_request(
    url = jev_endpoint_url(endpoint),
    payload = payload,
    api_key = api_key,
    timeout = timeout,
    headers = endpoint$headers,
    rate_limit = rate_limit,
    throttle_realm = paste0("jevr-", endpoint$name)
  )
  list(request = request, provider = endpoint$name)
}
