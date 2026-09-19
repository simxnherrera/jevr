# HTTP errors include status and corrective guidance

    Code
      httr2::with_mocked_responses(mock, jev_ask(state = "text", questions = list(
        urgent = jev_noul("Is this urgent?")), max_retries = 0))
    Condition
      Error:
      ! typesafe request failed with HTTP 401 after 1 attempt. Invalid API key Check the provider API key environment variable.

