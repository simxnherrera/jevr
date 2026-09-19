jev_build_typesafe_request <- function(
  state,
  questions,
  model,
  timeout,
  rate_limit = NULL,
  api_key = jev_api_key("typesafe")
) {
  payload <- jev_request_payload(state, questions, model)
  request <- jev_build_request(
    url = "https://api.typesafe.ai/v1/systemone",
    payload = payload,
    api_key = api_key,
    timeout = timeout,
    rate_limit = rate_limit,
    throttle_realm = "jevr-typesafe"
  )
  list(request = request, provider = "typesafe")
}

jev_request_typesafe <- function(
  state,
  questions,
  model,
  timeout,
  max_retries,
  retry_budget = Inf
) {
  built <- jev_build_typesafe_request(
    state = state,
    questions = questions,
    model = model,
    timeout = timeout
  )
  response <- jev_send_request(
    built$request,
    provider = "typesafe",
    max_retries = max_retries,
    retry_budget = retry_budget
  )

  list(body = jev_body_json(response, "typesafe"), response = response)
}
