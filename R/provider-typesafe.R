jev_request_typesafe <- function(
  state,
  questions,
  model,
  timeout,
  max_retries
) {
  api_key <- jev_api_key("typesafe")
  payload <- jev_request_payload(state, questions, model)
  request <- jev_build_request(
    url = "https://api.typesafe.ai/v1/systemone",
    payload = payload,
    api_key = api_key,
    timeout = timeout
  )
  response <- jev_send_request(
    request,
    provider = "typesafe",
    max_retries = max_retries
  )

  list(body = jev_body_json(response, "typesafe"), response = response)
}
