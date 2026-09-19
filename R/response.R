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
    anyNA(names(value)) || any(!nzchar(names(value))) ||
    anyDuplicated(names(value)) || anyNA(value) || any(!is.finite(value)) ||
    any(value < 0 | value > 1)) {
    jev_abort(
      paste0("Response field ", argument, " must be a named probability map."),
      class = "jev_response_error"
    )
  }

  value
}

jev_validate_probability_map <- function(value, argument, expected_names = NULL) {
  value <- jev_probability_map(value, argument)

  if (!is.null(expected_names) && !setequal(names(value), expected_names)) {
    jev_abort(
      paste0("Response field ", argument, " has unexpected probability keys."),
      class = "jev_response_error"
    )
  }

  probability_tolerance <- 1e-6
  if (abs(sum(value) - 1) > probability_tolerance) {
    jev_abort(
      paste0("Response field ", argument, " probabilities must sum to 1."),
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

jev_validate_legend <- function(value, argument, expected_names) {
  if (!is.character(value) && !is.list(value)) {
    jev_abort(
      paste0("Response field ", argument, " must be a named legend."),
      class = "jev_response_error"
    )
  }

  if (is.null(names(value)) || anyNA(names(value)) ||
    any(!nzchar(names(value))) || anyDuplicated(names(value)) ||
    !setequal(names(value), expected_names)) {
    jev_abort(
      paste0("Response field ", argument, " has unexpected legend keys."),
      class = "jev_response_error"
    )
  }

  if (is.character(value) && (anyNA(value) || any(!nzchar(value)))) {
    jev_abort(
      paste0("Response field ", argument, " must contain non-empty text."),
      class = "jev_response_error"
    )
  }

  if (is.list(value)) {
    string_items <- vapply(
      value,
      function(item) is.character(item) && length(item) == 1L && !is.na(item),
      logical(1)
    )
    if (all(string_items)) {
      value <- vapply(value, identity, character(1))
    }
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

jev_parse_answer <- function(answer, question_id, question) {
  expected_type <- question$type

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
    if (!is.character(choice) || length(choice) != 1L || is.na(choice)) {
      jev_abort(
        paste0("Answer ", question_id, " has an invalid choice field."),
        class = "jev_response_error"
      )
    }

    if (!is.null(question$criteria) &&
      !identical(choice %in% names(question$criteria), TRUE)) {
      jev_abort(
        paste0("Answer ", question_id, " has an unknown choice."),
        class = "jev_response_error"
      )
    }

    expected_names <- if (is.null(question$criteria)) NULL else names(question$criteria)

    answer$probabilities <- jev_validate_probability_map(
      probabilities,
      paste0(question_id, "$probabilities"),
      expected_names
    )
    confidence <- jev_required_response_field(answer, "confidence", question_id)
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
    if (!is.numeric(score) || length(score) != 1L || is.na(score) ||
      !is.finite(score) ||
      (!is.null(question$criteria) &&
        (score < 0 || score > length(question$criteria) - 1L))) {
      jev_abort(
        paste0("Answer ", question_id, " has an invalid score field."),
        class = "jev_response_error"
      )
    }

    expected_names <- if (is.null(question$criteria)) names(probabilities) else {
      as.character(seq_along(question$criteria) - 1L)
    }
    answer$legend <- jev_validate_legend(
      legend,
      paste0(question_id, "$legend"),
      expected_names
    )
    answer$probabilities <- jev_validate_probability_map(
      probabilities,
      paste0(question_id, "$probabilities"),
      expected_names
    )
    confidence <- jev_required_response_field(answer, "confidence", question_id)
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

jev_parse_response <- function(body, provider, questions) {
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

  if (!is.character(questions) && !is.list(questions)) {
    jev_abort(
      "questions must be a named vector of types or a named list of questions.",
      class = "jev_input_error"
    )
  }

  if (is.character(questions)) {
    question_ids <- names(questions)
    question_definitions <- lapply(
      questions,
      function(type) list(type = type, criteria = NULL)
    )
  } else {
    question_ids <- names(questions)
    question_definitions <- questions
  }

  if (is.null(question_ids) || anyNA(question_ids) ||
    any(!nzchar(question_ids)) || anyDuplicated(question_ids)) {
    jev_abort(
      "questions must have unique, non-empty IDs.",
      class = "jev_input_error"
    )
  }

  if (length(question_definitions) == 0L ||
    !all(vapply(question_definitions, is.list, logical(1)))) {
    jev_abort(
      "questions must contain complete question definitions or response types.",
      class = "jev_input_error"
    )
  }

  question_types <- vapply(
    question_definitions,
    function(question) {
      type <- question$type
      if (!is.character(type) || length(type) != 1L || is.na(type)) {
        return(NA_character_)
      }
      type
    },
    character(1)
  )
  if (anyNA(question_types) ||
    any(!question_types %in% c("choice", "score", "noul"))) {
    jev_abort(
      "questions contain an unsupported question type.",
      class = "jev_input_error"
    )
  }

  missing_ids <- setdiff(question_ids, names(answers))
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

  extra_ids <- setdiff(names(answers), question_ids)
  if (length(extra_ids) > 0L || anyNA(names(answers)) ||
    any(!nzchar(names(answers))) || anyDuplicated(names(answers))) {
    jev_abort(
      "Response answers do not match the requested question IDs.",
      class = "jev_response_error"
    )
  }

  if (!is.null(usage) && !is.list(usage)) {
    jev_abort(
      "Response is missing its usage object.",
      class = "jev_response_error"
    )
  }

  if (is.null(usage)) usage <- list()
  for (field in c("input_tokens", "output_tokens")) {
    if (is.null(usage[[field]])) {
      usage[[field]] <- NA_real_
    } else if (!is.numeric(usage[[field]]) || length(usage[[field]]) != 1L ||
      !is.finite(usage[[field]]) || usage[[field]] < 0) {
      jev_abort(
        paste0("Response usage field ", field, " must be non-negative and finite."),
        class = "jev_response_error"
      )
    }
  }

  parsed_answers <- lapply(
    question_ids,
    function(id) jev_parse_answer(answers[[id]], id, question_definitions[[id]])
  )
  names(parsed_answers) <- question_ids

  metadata <- if (is.null(body$metadata)) {
    list()
  } else if (is.list(body$metadata)) {
    body$metadata
  } else {
    list(provider_metadata = body$metadata)
  }
  metadata$provider <- provider
  metadata$id <- if (is.null(body$id)) NULL else body$id
  metadata$upstream_provider <- if (is.null(body$provider)) NULL else body$provider

  structure(
    list(
      model = model,
      answers = parsed_answers,
      usage = usage,
      metadata = metadata,
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
