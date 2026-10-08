jev_long_empty <- function() {
  data.frame(
    state_id = character(),
    question_id = character(),
    type = character(),
    option = character(),
    probability = numeric(),
    selected = logical(),
    confidence = numeric(),
    score = numeric(),
    noul = numeric(),
    model = character(),
    status = character(),
    error_class = character(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
}

# One question's rows. `answer` NULL (failed/missing) yields a single NA row.
jev_long_question_rows <- function(
  state_id, question_id, type, answer, model, status, error
) {
  na_chr <- NA_character_
  na_num <- NA_real_
  options <- na_chr
  probability <- na_num
  selected <- NA
  confidence <- na_num
  score <- na_num
  noul <- na_num

  if (!is.null(answer)) {
    if (!inherits(answer, "jev_unknown_answer")) {
      type <- answer$type
    }
    if (inherits(answer, "jev_unknown_answer")) {
      status <- "unknown_answer"
      if (!is.null(answer$expected_type)) type <- answer$expected_type
    } else if (identical(type, "noul")) {
      options <- c("true", "false")
      probability <- c(answer$noul, 1 - answer$noul)
      selected <- c(NA, NA)
      noul <- answer$noul
    } else if (isTRUE(type %in% c("choice", "score"))) {
      probs <- answer$probabilities
      options <- names(probs)
      probability <- unname(as.numeric(probs))
      confidence <- if (is.null(answer$confidence)) na_num else answer$confidence
      if (identical(type, "choice")) {
        selected <- options == answer$choice
      } else {
        score <- as.numeric(answer$score)
        selected <- probability == max(probability)
      }
    } else {
      status <- "unknown_answer"
    }
  }

  n <- length(options)
  data.frame(
    state_id = rep(as.character(state_id), n),
    question_id = rep(as.character(question_id), n),
    type = rep(if (is.null(type)) na_chr else as.character(type), n),
    option = options,
    probability = probability,
    selected = rep_len(selected, n),
    confidence = rep_len(confidence, n),
    score = rep_len(score, n),
    noul = rep_len(noul, n),
    model = rep(if (is.null(model)) na_chr else as.character(model), n),
    status = rep(status, n),
    error_class = rep(if (is.null(error)) na_chr else error$class, n),
    error_message = rep(if (is.null(error)) na_chr else error$message, n),
    stringsAsFactors = FALSE
  )
}

jev_long_bind <- function(rows) {
  if (length(rows) == 0L) {
    return(jev_long_empty())
  }
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  out
}

jev_long_response <- function(response, state_id = NA_character_) {
  rows <- lapply(names(response$answers), function(id) {
    answer <- response$answers[[id]]
    jev_long_question_rows(
      state_id, id, answer$type, answer, response$model, "success", NULL
    )
  })
  jev_long_bind(rows)
}

jev_long_result <- function(x, questions = NULL) {
  ids <- if (!is.null(questions)) {
    names(questions)
  } else {
    unique(c(names(x$response$answers), names(x$question_errors)))
  }
  model <- if (is.null(x$response)) NULL else x$response$model
  rows <- lapply(ids, function(id) {
    answer <- if (is.null(x$response)) NULL else x$response$answers[[id]]
    question_error <- x$question_errors[[id]]
    error <- if (!is.null(question_error)) question_error else x$error
    status <- if (!is.null(answer)) {
      "success"
    } else if (!is.null(question_error)) {
      "error"
    } else {
      x$status
    }
    type <- if (!is.null(questions)) questions[[id]]$type else NULL
    jev_long_question_rows(
      x$state_id, id, type, answer, model, status,
      if (is.null(answer)) error else NULL
    )
  })
  jev_long_bind(rows)
}

jev_long_result_set <- function(x) {
  questions <- jev_result_question_definitions(x)
  rows <- lapply(jev_result_set_items(x), jev_long_result, questions = questions)
  jev_long_bind(rows)
}

jev_long_reject_wide <- function(format) {
  if (identical(format, "wide")) {
    jev_abort(
      "Wide format is only available for jev_result_set objects.",
      class = "jev_input_error"
    )
  }
}

#' Convert a response or result to long format
#'
#' `as.data.frame()` methods for `jev_response` and `jev_result`, and the
#' `format = "long"` option of [as.data.frame.jev_result_set()], return one
#' row per state, question, and option (choice), level (score), or truth value
#' (noul).
#'
#' @param x A `jev_response` from [jev_ask()] or a `jev_result`.
#' @param row.names,optional Ignored; present for the generic.
#' @param format `"long"` (the only format for these methods).
#' @param ... Unused arguments.
#' @return A data frame with columns `state_id`, `question_id`, `type`,
#'   `option`, `probability`, `selected`, `confidence`, `score`, `noul`,
#'   `model`, `status`, `error_class`, and `error_message`.
#' @details
#' Choice and score rows carry the answer's probability for each option or
#' level; probabilities per question sum to 1. For choice, `selected` marks
#' the chosen option. For score, the `score` column is the probability-weighted
#' expectation (it can be fractional) while `selected` marks the mode, the
#' highest-probability level (all levels are marked on exact ties). Noul
#' questions yield the options `"true"` and `"false"` with probabilities
#' `noul` and `1 - noul`; `selected` is `NA` because jevr never chooses a
#' threshold, so callers apply their own (for example with `jev_band()`).
#' Answers of an unrecognised type produce one NA row with
#' `status = "unknown_answer"`. `confidence` is NA for noul, `score` is NA for
#' non-score questions, and `noul` is NA for other
#' questions. Failed or missing questions produce a single row with `NA`
#' option, probability, and selected values, the state status (or `"error"`
#' for a question-level error), and the error class and message. `state_id`
#' is `NA` for a single response from [jev_ask()].
#' @examples
#' response <- structure(
#'   list(
#'     model = "jev-1.13.0",
#'     answers = list(urgent = list(type = "noul", noul = 0.8))
#'   ),
#'   class = "jev_response"
#' )
#' as.data.frame(response)
#' @export
as.data.frame.jev_response <- function(
  x, row.names = NULL, optional = FALSE, format = "long", ...
) {
  jev_long_reject_wide(format)
  jev_long_response(x)
}

#' @rdname as.data.frame.jev_response
#' @export
as.data.frame.jev_result <- function(
  x, row.names = NULL, optional = FALSE, format = "long", ...
) {
  jev_long_reject_wide(format)
  jev_long_result(x)
}
