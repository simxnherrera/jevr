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

  if (is.character(x)) {
    if (anyNA(x) || length(x) == 0L) {
      jev_abort(
        paste0(argument, " must contain non-missing text."),
        class = "jev_input_error"
      )
    }
    return(x)
  }

  if (is.list(x)) {
    item_names <- names(x)
    if (!is.null(item_names) && anyNA(item_names)) {
      jev_abort(
        paste0(argument, " has missing list names."),
        class = "jev_input_error"
      )
    }
    return(x)
  }

  if (is.atomic(x) && length(x) > 0L) {
    return(x)
  }

  jev_abort(
    paste0(argument, " must be JSON-serializable."),
    class = "jev_input_error"
  )
}

jev_validate_instructions <- function(instructions) {
  if (is.character(instructions) && length(instructions) == 1L) {
    if (is.na(instructions) || !nzchar(instructions)) {
      jev_abort(
        "instructions must be one non-empty string or a JSON value.",
        class = "jev_input_error"
      )
    }
  }

  jev_validate_json_value(instructions, "instructions")
}

jev_validate_named_criteria <- function(criteria, argument, maximum) {
  if (!is.character(criteria) && !is.list(criteria)) {
    jev_abort(
      paste0(argument, " must be a named character vector or list."),
      class = "jev_input_error"
    )
  }

  if (is.null(names(criteria)) || any(!nzchar(names(criteria)))) {
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

jev_validate_score_criteria <- function(criteria) {
  if (!is.character(criteria) && !is.list(criteria)) {
    jev_abort(
      "criteria must be an ordered character vector or list.",
      class = "jev_input_error"
    )
  }

  if (length(criteria) < 2L || length(criteria) > 10L) {
    jev_abort(
      "Score criteria must contain between 2 and 10 ordered levels.",
      class = "jev_input_error"
    )
  }

  if (!is.null(names(criteria)) && any(nzchar(names(criteria)))) {
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
        paste0("criteria[[", index, "]]"),
        allow_null = FALSE
      )
    }
  ))

  criteria
}

jev_validate_noul_criteria <- function(criteria) {
  if (is.null(criteria)) {
    return(NULL)
  }

  if (!is.character(criteria) && !is.list(criteria)) {
    jev_abort(
      "Noul criteria must be NULL or a named list with true and false.",
      class = "jev_input_error"
    )
  }

  if (!identical(sort(names(criteria)), c("false", "true"))) {
    jev_abort(
      "Noul criteria must contain exactly the names true and false.",
      class = "jev_input_error"
    )
  }

  criteria <- as.list(criteria)[c("true", "false")]
  invisible(lapply(
    names(criteria),
    function(name) {
      jev_validate_json_value(
        criteria[[name]],
        paste0("criteria$", name),
        allow_null = TRUE
      )
    }
  ))

  criteria
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
jev_choice <- function(instructions, criteria) {
  structure(
    list(
      type = "choice",
      instructions = jev_validate_instructions(instructions),
      criteria = jev_validate_named_criteria(criteria, "criteria", 255L)
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
jev_score <- function(instructions, criteria) {
  structure(
    list(
      type = "score",
      instructions = jev_validate_instructions(instructions),
      criteria = jev_validate_score_criteria(criteria)
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
#' @return An object of class jev_noul_question and jev_question.
#' @export
#' @examples
#' jev_noul(
#'   instructions = "Does this request convey urgency?",
#'   criteria = c(true = "Explicitly time-sensitive", false = "No urgency")
#' )
jev_noul <- function(instructions, criteria = NULL) {
  structure(
    list(
      type = "noul",
      instructions = jev_validate_instructions(instructions),
      criteria = jev_validate_noul_criteria(criteria)
    ),
    class = c("jev_noul_question", "jev_question")
  )
}
