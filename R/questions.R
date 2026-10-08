jev_validate_json_value <- function(x, argument, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) {
      return(x)
    }

    jev_abort(
      paste0(argument, " must not be NULL."),
      class = "jev_input_error"
    )
  }

  if (is.function(x) || is.environment(x) || inherits(x, "formula")) {
    jev_abort(
      paste0(argument, " must be JSON-serializable."),
      class = "jev_input_error"
    )
  }

  if (is.atomic(x) && length(x) == 0L) {
    jev_abort(
      paste0(argument, " must not be an empty atomic vector."),
      class = "jev_input_error"
    )
  }

  if (typeof(x) == "list") {
    item_names <- names(x)
    item_attributes <- attributes(x)
    item_attributes[names(item_attributes) == "names"] <- NULL

    if (length(item_attributes) > 0L) {
      jev_abort(
        paste0(argument, " must be a plain JSON object or array."),
        class = "jev_input_error"
      )
    }

    if (!is.null(item_names) && anyNA(item_names)) {
      jev_abort(
        paste0(argument, " has missing names."),
        class = "jev_input_error"
      )
    }

    if (!is.null(item_names) && any(!nzchar(item_names))) {
      jev_abort(
        paste0(argument, " has missing or partial names."),
        class = "jev_input_error"
      )
    }

    if (!is.null(item_names) && anyDuplicated(item_names)) {
      jev_abort(
        paste0(argument, " has duplicate names."),
        class = "jev_input_error"
      )
    }

    for (index in seq_along(x)) {
      child <- if (is.null(item_names)) {
        paste0(argument, "[[", index, "]]")
      } else {
        paste0(argument, "$", item_names[[index]])
      }

      jev_validate_json_value(x[[index]], child, allow_null = TRUE)
    }

    return(x)
  }

  item_names <- names(x)
  if (!is.null(item_names) && (anyNA(item_names) ||
    any(!nzchar(item_names)) || anyDuplicated(item_names))) {
    jev_abort(
      paste0(argument, " has missing, partial, or duplicate names."),
      class = "jev_input_error"
    )
  }

  item_attributes <- attributes(x)
  item_attributes[names(item_attributes) == "names"] <- NULL
  if (length(dim(x)) > 0L || length(item_attributes) > 0L) {
    jev_abort(
      paste0(argument, " must be a plain JSON value."),
      class = "jev_input_error"
    )
  }

  if (typeof(x) %in% c("character", "logical", "integer", "double")) {
    if (anyNA(x) || (is.double(x) && any(!is.finite(x)))) {
      jev_abort(
        paste0(argument, " must not contain NA, NaN, or Inf."),
        class = "jev_input_error"
      )
    }
    return(x)
  }

  jev_abort(
    paste0(argument, " must be JSON-serializable."),
    class = "jev_input_error"
  )
}

jev_validate_instructions <- function(instructions, argument = "instructions") {
  if (is.character(instructions) && length(instructions) == 1L) {
    if (is.na(instructions) || !nzchar(instructions)) {
      jev_abort(
        paste0(
          argument,
          " must be one non-empty string or a JSON value."
        ),
        class = "jev_input_error"
      )
    }
  }

  jev_validate_json_value(instructions, argument)
}

jev_validate_named_criteria <- function(criteria, argument, maximum) {
  if (!is.character(criteria) && typeof(criteria) != "list") {
    jev_abort(
      paste0(argument, " must be a named character vector or list."),
      class = "jev_input_error"
    )
  }

  attributes <- attributes(criteria)
  attributes[names(attributes) == "names"] <- NULL
  if (length(attributes) > 0L) {
    jev_abort(
      paste0(argument, " must be a plain named character vector or list."),
      class = "jev_input_error"
    )
  }

  option_names <- names(criteria)
  if (is.null(option_names) || anyNA(option_names) ||
    any(!nzchar(option_names))) {
    jev_abort(
      paste0(argument, " must have a non-empty name for every option."),
      class = "jev_input_error"
    )
  }

  if (anyDuplicated(names(criteria))) {
    jev_abort(
      paste0(argument, " cannot contain duplicate option names."),
      class = "jev_input_error"
    )
  }

  if (length(criteria) < 2L || length(criteria) > maximum) {
    jev_abort(
      paste0(
        argument, " must contain between 2 and ", maximum, " options."
      ),
      class = "jev_input_error"
    )
  }

  criteria <- as.list(criteria)
  invisible(lapply(
    seq_along(criteria),
    function(index) {
      jev_validate_json_value(
        criteria[[index]],
        paste0(argument, "[[", names(criteria)[[index]], "]]"),
        allow_null = TRUE
      )
    }
  ))

  criteria
}

jev_validate_score_criteria <- function(criteria, argument = "criteria") {
  if (!is.character(criteria) && typeof(criteria) != "list") {
    jev_abort(
      paste0(argument, " must be an ordered character vector or list."),
      class = "jev_input_error"
    )
  }

  if (length(attributes(criteria)) > 0L) {
    jev_abort(
      paste0(argument, " must be a plain unnamed character vector or list."),
      class = "jev_input_error"
    )
  }

  if (length(criteria) < 2L || length(criteria) > 10L) {
    jev_abort(
      "Score criteria must contain between 2 and 10 ordered levels.",
      class = "jev_input_error"
    )
  }

  if (!is.null(names(criteria))) {
    jev_abort(
      "Score criteria must be an unnamed ordered vector or list.",
      class = "jev_input_error"
    )
  }

  criteria <- unname(as.list(criteria))
  invisible(lapply(
    seq_along(criteria),
    function(index) {
      jev_validate_json_value(
        criteria[[index]],
        paste0(argument, "[[", index, "]]"),
        allow_null = FALSE
      )
    }
  ))

  criteria
}

jev_validate_noul_criteria <- function(criteria, argument = "criteria") {
  if (is.null(criteria)) {
    return(NULL)
  }

  if (!is.character(criteria) && typeof(criteria) != "list") {
    jev_abort(
      paste0(
        argument,
        " must be NULL or a named list with true and false."
      ),
      class = "jev_input_error"
    )
  }

  attributes <- attributes(criteria)
  attributes[names(attributes) == "names"] <- NULL
  if (length(attributes) > 0L) {
    jev_abort(
      paste0(argument, " must be a plain named character vector or list."),
      class = "jev_input_error"
    )
  }

  option_names <- names(criteria)
  if (is.null(option_names) || anyNA(option_names) ||
    any(!nzchar(option_names)) || anyDuplicated(option_names) ||
    !setequal(option_names, c("false", "true"))) {
    message <- if (identical(argument, "criteria")) {
      "Noul criteria must contain exactly the names true and false."
    } else {
      paste0(argument, " must contain exactly the names true and false.")
    }
    jev_abort(
      message,
      class = "jev_input_error"
    )
  }

  criteria <- as.list(criteria)[c("true", "false")]
  invisible(lapply(
    names(criteria),
    function(name) {
      jev_validate_json_value(
        criteria[[name]],
        paste0(argument, "$", name),
        allow_null = TRUE
      )
    }
  ))

  criteria
}

jev_question_core_names <- c("type", "instructions", "criteria")

# Extra named fields are forwarded to the request body unchanged (forward
# compatibility with fields this version of jevr does not know about).
jev_validate_question_extras <- function(extras, argument = "...") {
  if (length(extras) == 0L) {
    return(list())
  }

  extra_names <- names(extras)
  if (is.null(extra_names) || anyNA(extra_names) || any(!nzchar(extra_names))) {
    jev_abort(
      paste0("All extra question fields in ", argument, " must be named."),
      class = "jev_input_error"
    )
  }

  if (anyDuplicated(extra_names)) {
    jev_abort(
      paste0("Extra question fields in ", argument, " must have unique names."),
      class = "jev_input_error"
    )
  }

  reserved <- intersect(extra_names, jev_question_core_names)
  if (length(reserved) > 0L) {
    jev_abort(
      paste0(
        "Extra question fields cannot reuse reserved names: ",
        paste(reserved, collapse = ", "), "."
      ),
      class = "jev_input_error"
    )
  }

  for (name in extra_names) {
    jev_validate_json_value(extras[[name]], paste0(argument, "$", name))
  }

  as.list(extras)
}

# TRUE when `question` has the core fields first, followed only by valid extras.
jev_question_has_valid_shape <- function(question, argument = "question") {
  question_names <- names(question)
  if (length(question_names) < 3L ||
    !identical(question_names[1:3], jev_question_core_names)) {
    return(FALSE)
  }

  extras <- question[-(1:3)]
  if (length(extras) == 0L) {
    return(TRUE)
  }
  result <- tryCatch(
    jev_validate_question_extras(unclass(extras), argument),
    jev_input_error = function(error) NULL
  )
  !is.null(result)
}

jev_validate_questions <- function(questions) {
  if (!is.list(questions) || length(questions) == 0L) {
    jev_abort(
      "questions must be a non-empty named list of JEV questions.",
      class = "jev_input_error"
    )
  }

  ids <- names(questions)
  if (is.null(ids) || anyNA(ids) || any(!nzchar(ids))) {
    jev_abort(
      "questions must have a non-empty ID for every question.",
      class = "jev_input_error"
    )
  }

  if (anyDuplicated(ids)) {
    jev_abort(
      "questions cannot contain duplicate question IDs.",
      class = "jev_input_error"
    )
  }

  for (index in seq_along(questions)) {
    question <- questions[[index]]
    id <- ids[[index]]
    argument <- paste0("questions$", id)

    if (typeof(question) != "list" ||
      !jev_question_has_valid_shape(question, argument) ||
      !inherits(question, "jev_question")) {
      jev_abort(
        paste0(
          "Question ", id,
          " is not a jev_choice(), jev_score(), or jev_noul() object."
        ),
        class = "jev_input_error"
      )
    }

    if (inherits(question, "jev_choice_question")) {
      expected_class <- c("jev_choice_question", "jev_question")
      if (!identical(class(question), expected_class)) {
        jev_abort(
          paste0(argument, " has an ambiguous question class."),
          class = "jev_input_error"
        )
      }
      if (!identical(question$type, "choice")) {
        jev_abort(
          paste0(argument, "$type must be \"choice\"."),
          class = "jev_input_error"
        )
      }
      jev_validate_json_value(question$type, paste0(argument, "$type"))
      jev_validate_instructions(
        question$instructions,
        paste0(argument, "$instructions")
      )
      jev_validate_named_criteria(
        question$criteria,
        paste0(argument, "$criteria"),
        255L
      )
    } else if (inherits(question, "jev_score_question")) {
      expected_class <- c("jev_score_question", "jev_question")
      if (!identical(class(question), expected_class)) {
        jev_abort(
          paste0(argument, " has an ambiguous question class."),
          class = "jev_input_error"
        )
      }
      if (!identical(question$type, "score")) {
        jev_abort(
          paste0(argument, "$type must be \"score\"."),
          class = "jev_input_error"
        )
      }
      jev_validate_json_value(question$type, paste0(argument, "$type"))
      jev_validate_instructions(
        question$instructions,
        paste0(argument, "$instructions")
      )
      jev_validate_score_criteria(
        question$criteria,
        paste0(argument, "$criteria")
      )
    } else if (inherits(question, "jev_noul_question")) {
      expected_class <- c("jev_noul_question", "jev_question")
      if (!identical(class(question), expected_class)) {
        jev_abort(
          paste0(argument, " has an ambiguous question class."),
          class = "jev_input_error"
        )
      }
      if (!identical(question$type, "noul")) {
        jev_abort(
          paste0(argument, "$type must be \"noul\"."),
          class = "jev_input_error"
        )
      }
      jev_validate_json_value(question$type, paste0(argument, "$type"))
      jev_validate_instructions(
        question$instructions,
        paste0(argument, "$instructions")
      )
      jev_validate_noul_criteria(
        question$criteria,
        paste0(argument, "$criteria")
      )
    } else {
      jev_abort(
        paste0(argument, " has an unsupported question class."),
        class = "jev_input_error"
      )
    }
  }

  questions
}

#' Define a Choice question
#'
#' A Choice asks JEV to select one option from a fixed set. The returned
#' answer preserves the selected option, the full probability distribution, and
#' confidence.
#'
#' @param instructions The question or judgment to evaluate. A string is the
#'   usual form; TypeSafe also accepts JSON objects and arrays for structured
#'   instructions.
#' @param criteria A named character vector or named list mapping each option
#'   to an optional description. Use NULL for an option that needs no extra
#'   description.
#'
#' @param ... Optional extra named fields (JSON-serializable) forwarded to
#'   the request body unchanged and included in the spec hash. Use them for
#'   question fields that newer TypeSafe API versions accept but this version
#'   of jevr does not model. Names must be unique and cannot be `type`,
#'   `instructions`, or `criteria`. The `ellmer` and legacy OpenRouter
#'   backends ignore them.
#'
#' @return An object of class jev_choice_question and jev_question.
#' @export
#' @examples
#' jev_choice(
#'   instructions = "Which team should handle this?",
#'   criteria = c(
#'     billing = "Payments, invoicing, refunds",
#'     technical = "Bugs, outages, integrations",
#'     sales = "Pricing, upgrades, new accounts"
#'   )
#' )
jev_choice <- function(instructions, criteria, ...) {
  structure(
    c(
      list(
        type = "choice",
        instructions = jev_validate_instructions(instructions),
        criteria = jev_validate_named_criteria(criteria, "criteria", 255L)
      ),
      jev_validate_question_extras(list(...))
    ),
    class = c("jev_choice_question", "jev_question")
  )
}

#' Define a Score question
#'
#' A Score asks JEV to place the state along ordered levels. The response is a
#' probability-weighted position, which can fall between two levels.
#'
#' @param instructions The question or judgment to evaluate. A string is the
#'   usual form; TypeSafe also accepts JSON objects and arrays for structured
#'   instructions.
#' @param criteria An ordered character vector or list of level descriptions.
#'   It must contain between two and ten levels, from low to high.
#'
#' @param ... Optional extra named fields (JSON-serializable) forwarded to
#'   the request body unchanged and included in the spec hash. Use them for
#'   question fields that newer TypeSafe API versions accept but this version
#'   of jevr does not model. Names must be unique and cannot be `type`,
#'   `instructions`, or `criteria`. The `ellmer` and legacy OpenRouter
#'   backends ignore them.
#'
#' @return An object of class jev_score_question and jev_question.
#' @export
#' @examples
#' jev_score(
#'   instructions = "How severe is the reported issue?",
#'   criteria = c(
#'     "Cosmetic; no impact to functionality",
#'     "Broken, but a workaround exists",
#'     "Blocking; no workaround exists"
#'   )
#' )
jev_score <- function(instructions, criteria, ...) {
  structure(
    c(
      list(
        type = "score",
        instructions = jev_validate_instructions(instructions),
        criteria = jev_validate_score_criteria(criteria)
      ),
      jev_validate_question_extras(list(...))
    ),
    class = c("jev_score_question", "jev_question")
  )
}

#' Define a Noul question
#'
#' A Noul asks whether a statement is true and returns the continuous
#' probability of yes. The value is not converted to TRUE or FALSE.
#'
#' @param instructions The yes/no question or statement to evaluate. Phrase it
#'   so a high value means yes.
#' @param criteria Optional named character vector or list with exactly true
#'   and false descriptions. Omit it when the instruction is sufficient.
#'
#' @param ... Optional extra named fields (JSON-serializable) forwarded to
#'   the request body unchanged and included in the spec hash. Use them for
#'   question fields that newer TypeSafe API versions accept but this version
#'   of jevr does not model. Names must be unique and cannot be `type`,
#'   `instructions`, or `criteria`. The `ellmer` and legacy OpenRouter
#'   backends ignore them.
#'
#' @return An object of class jev_noul_question and jev_question.
#' @export
#' @examples
#' jev_noul(
#'   instructions = "Does this request convey urgency?",
#'   criteria = c(true = "Explicitly time-sensitive", false = "No urgency")
#' )
jev_noul <- function(instructions, criteria = NULL, ...) {
  structure(
    c(
      list(
        type = "noul",
        instructions = jev_validate_instructions(instructions),
        criteria = jev_validate_noul_criteria(criteria)
      ),
      jev_validate_question_extras(list(...))
    ),
    class = c("jev_noul_question", "jev_question")
  )
}
