jev_probability_map <- function(value, argument) {
  if (is.list(value) && !is.null(names(value))) {
    value <- vapply(
      value,
      function(item) {
        if (is.numeric(item) && length(item) == 1L) {
          return(item)
        }
        NA_real_
      },
      numeric(1)
    )
  }

  if (!is.numeric(value) || is.null(names(value)) || length(value) == 0L ||
    anyNA(value) || any(!is.finite(value)) || any(value < 0 | value > 1)) {
    jev_abort(
      paste0("Response field ", argument, " must be a named probability map."),
      class = "jev_response_error"
    )
  }

  value
}

jev_confidence <- function(value, argument) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
    !is.finite(value) || value < 0 || value > 1) {
    jev_abort(
      paste0("Response field ", argument, " must be between 0 and 1."),
      class = "jev_response_error"
    )
  }

  value
}

jev_required_response_field <- function(answer, field, question_id) {
  if (is.null(answer[[field]])) {
    jev_abort(
      paste0(
        "Answer ", question_id, " is missing the required field ", field, "."
      ),
      class = "jev_response_error"
    )
  }

  answer[[field]]
}

jev_parse_answer <- function(answer, question_id, expected_type) {
  if (!is.list(answer) || !identical(answer$type, expected_type)) {
    jev_abort(
      paste0(
        "Answer ", question_id,
        " does not match its question type (", expected_type, ")."
      ),
      class = "jev_response_error"
    )
  }

  if (identical(expected_type, "choice")) {
    choice <- jev_required_response_field(answer, "choice", question_id)
    probabilities <- jev_required_response_field(
      answer, "probabilities", question_id
    )
    confidence <- jev_required_response_field(answer, "confidence", question_id)

    if (!is.character(choice) || length(choice) != 1L || is.na(choice)) {
      jev_abort(
        paste0("Answer ", question_id, " has an invalid choice field."),
        class = "jev_response_error"
      )
    }

    answer$probabilities <- jev_probability_map(
      probabilities,
      paste0(question_id, "$probabilities")
    )
    answer$confidence <- jev_confidence(
      confidence,
      paste0(question_id, "$confidence")
    )
    class(answer) <- c("jev_choice_answer", "jev_answer", "list")
    return(answer)
  }

  if (identical(expected_type, "score")) {
    score <- jev_required_response_field(answer, "score", question_id)
    legend <- jev_required_response_field(answer, "legend", question_id)
    probabilities <- jev_required_response_field(
      answer, "probabilities", question_id
    )
    confidence <- jev_required_response_field(answer, "confidence", question_id)

    if (!is.numeric(score) || length(score) != 1L || is.na(score) ||
      !is.finite(score)) {
      jev_abort(
        paste0("Answer ", question_id, " has an invalid score field."),
        class = "jev_response_error"
      )
    }

    if (is.list(legend) && !is.null(names(legend))) {
      legend <- vapply(
        legend,
        function(item) {
          if (is.character(item) && length(item) == 1L) {
            return(item)
          }
          NA_character_
        },
        character(1)
      )
    }

    if (!is.character(legend) || is.null(names(legend)) || anyNA(legend)) {
      jev_abort(
        paste0("Answer ", question_id, " has an invalid legend field."),
        class = "jev_response_error"
      )
    }

    answer$legend <- legend
    answer$probabilities <- jev_probability_map(
      probabilities,
      paste0(question_id, "$probabilities")
    )
    answer$confidence <- jev_confidence(
      confidence,
      paste0(question_id, "$confidence")
    )
    class(answer) <- c("jev_score_answer", "jev_answer", "list")
    return(answer)
  }

  noul <- jev_required_response_field(answer, "noul", question_id)
  if (!is.numeric(noul) || length(noul) != 1L || is.na(noul) ||
    !is.finite(noul) || noul < 0 || noul > 1) {
    jev_abort(
      paste0("Answer ", question_id, " has an invalid noul field."),
      class = "jev_response_error"
    )
  }

  class(answer) <- c("jev_noul_answer", "jev_answer", "list")
  answer
}

jev_parse_response <- function(body, provider, question_types) {
  if (!is.list(body)) {
    jev_abort(
      paste0(provider, " returned a JSON value instead of an object."),
      class = "jev_response_error"
    )
  }

  model <- body$model
  answers <- body$answers
  usage <- body$usage

  if (!is.character(model) || length(model) != 1L || is.na(model) ||
    !nzchar(model)) {
    jev_abort(
      "Response is missing a non-empty model field.",
      class = "jev_response_error"
    )
  }

  if (!is.list(answers) || is.null(names(answers))) {
    jev_abort(
      "Response is missing its named answers object.",
      class = "jev_response_error"
    )
  }

  missing_ids <- setdiff(names(question_types), names(answers))
  if (length(missing_ids) > 0L) {
    jev_abort(
      paste0(
        "Response is missing answer(s) for question ID(s): ",
        paste(missing_ids, collapse = ", "),
        "."
      ),
      class = "jev_response_error"
    )
  }

  if (!is.list(usage)) {
    jev_abort(
      "Response is missing its usage object.",
      class = "jev_response_error"
    )
  }

  if (!is.numeric(usage$input_tokens) || length(usage$input_tokens) != 1L ||
    !is.numeric(usage$output_tokens) || length(usage$output_tokens) != 1L) {
    jev_abort(
      "Response usage must contain numeric input_tokens and output_tokens.",
      class = "jev_response_error"
    )
  }

  parsed_answers <- lapply(
    names(question_types),
    function(id) jev_parse_answer(answers[[id]], id, question_types[[id]])
  )
  names(parsed_answers) <- names(question_types)

  structure(
    list(
      model = model,
      answers = parsed_answers,
      usage = usage,
      metadata = list(
        provider = provider,
        id = if (is.null(body$id)) NULL else body$id,
        upstream_provider = if (is.null(body$provider)) NULL else body$provider
      ),
      raw = body
    ),
    class = "jev_response"
  )
}

#' @export
print.jev_response <- function(x, ...) {
  cat("<jev_response>\n")
  cat("Provider: ", x$metadata$provider, "\n", sep = "")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Answers: ", length(x$answers), "\n", sep = "")
  invisible(x)
}
