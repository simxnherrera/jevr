# invalid probabilities and envelopes fail with strict native confidence unchanged

    Code
      for (name in names(invalid)) {
        error <- tryCatch(jev_ask("state", questions, backend = jev_test_llm_backend(
          invalid[[name]])), error = identity)
        expect_s3_class(error, "jev_response_error")
        cat(name, ":", conditionMessage(error), "\n")
      }
    Output
      missing : Response is missing answer(s) for question ID(s): urgent. 
      extra : Response answers do not match the requested question IDs. 
      duplicate : Response answers do not have valid unique question IDs. 
      keys : Response field department$probabilities has unexpected probability keys. 
      sum : Response field department$probabilities probabilities must sum to 1. 
      range : Response field department$probabilities must be a named probability map. 
      infinite : Answer urgent has an invalid noul field. 
      nan : Answer urgent has an invalid noul field. 
      nonnumeric : Answer urgent has an invalid noul field. 
      unknown_field : Answer urgent must contain only noul. 
    Code
      answer <- list(type = "choice", choice = "billing", probabilities = c(billing = 0.5,
        technical = 0.5), confidence = NULL)
      error <- tryCatch(jev_parse_answer(answer, "department", questions$department),
      error = identity)
      expect_s3_class(error, "jev_response_error")
      cat("native:", conditionMessage(error), "\n")
    Output
      native: Answer department is missing the required field confidence. 
    Code
      answer <- list(type = "score", score = 1, legend = c(`0` = "A", `1` = "B"),
      probabilities = c(`0` = 0, `1` = 1), confidence = NULL)
      error <- tryCatch(jev_parse_answer(answer, "score", jev_score("Score", c("A",
        "B"))), error = identity)
      expect_s3_class(error, "jev_response_error")
      cat("native score:", conditionMessage(error), "\n")
    Output
      native score: Answer score is missing the required field confidence. 

# rounding tolerance is explicit and accepted values are not renormalized

    Code
      jev_ask("state", jev_test_llm_questions(), backend = jev_test_llm_backend(raw,
        probability_tolerance = 0))
    Condition
      Error:
      ! Response field department$probabilities probabilities must sum to 1.

# semantic configuration defines identity without factory inspection

    Code
      jev_test_llm_backend(configuration = list(headers = list(Authorization = "secret")))
    Condition
      Error:
      ! Backend descriptions must not contain credential-named fields.

---

    Code
      jev_test_llm_backend(configuration = list(factory = function() NULL))
    Condition
      Error:
      ! Identity inputs must use plain JSON-compatible R values.

---

    Code
      jev_test_llm_backend(configuration = list(chat = new.env()))
    Condition
      Error:
      ! Identity inputs must use plain JSON-compatible R values.

# transport errors keep their original cause and metadata can be unavailable

    Code
      jev_ask("state", jev_test_llm_questions(), backend = backend)
    Condition
      Error:
      ! jevr ellmer evaluation failed: The backend factory must return a new Chat without prior turns.

# backend rejects contradictory native controls before calling the factory

    Code
      jev_ask("state", questions, backend = backend, provider = "typesafe")
    Condition
      Error:
      ! backend cannot be combined with provider or model; configure the Chat factory and declare its model in jev_llm().

---

    Code
      jev_ask("state", questions, backend = backend, model = NULL)
    Condition
      Error:
      ! backend cannot be combined with provider or model; configure the Chat factory and declare its model in jev_llm().

---

    Code
      jev_ask("state", questions, backend = backend, timeout = 30)
    Condition
      Error:
      ! The ellmer backend cannot apply timeout; configure it in the transport.

---

    Code
      jev_ask("state", questions, backend = backend, max_retries = 1)
    Condition
      Error:
      ! The ellmer backend requires max_retries = 0; retries belong to the transport.

