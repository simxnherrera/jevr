# incomplete responses fail with the missing field

    Code
      jev_parse_response(incomplete, "typesafe", c(department = "choice"))
    Condition
      Error:
      ! Answer department is missing the required field probabilities.

# invalid JSON responses use a package error

    Code
      jev_body_json(response, "typesafe")
    Condition
      Error:
      ! typesafe returned a response that is not valid JSON: lexical error: invalid string in json text.
                                            {not valid json
                           (right here) ------^

