# retry count is bounded

    Code
      jev_send_request(httr2::request("https://example.com"), provider = "typesafe",
      max_retries = 2, perform = perform, sleep = function(seconds) waits <<- c(waits,
        seconds))
    Condition
      Error:
      ! typesafe request failed with HTTP 529 after 3 attempts. The retry limit was reached while the provider was overloaded.
