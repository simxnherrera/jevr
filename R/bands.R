#' Turn confidence or noul probabilities into routing bands
#'
#' Maps the confidence of a Choice or Score answer, or the `noul`
#' probability of a Noul answer, onto caller-defined labelled bands such as
#' `"auto"`, `"review"` and `"escalate"`.
#'
#' The package never chooses thresholds: `breaks` is required and has no
#' default. Confidence measures how concentrated the answer distribution is,
#' not whether the answer is correct, so suitable thresholds should be
#' calibrated against your own labelled data. A `noul` probability near 0.5
#' means the model is uncertain, so symmetric breaks such as
#' `c(0, 0.3, 0.7, 1)` with labels `c("no", "uncertain", "yes")` are typical.
#'
#' @param x A `jev_result_set`, a `jev_result`, a `jev_response`, a single
#'   answer from a response, or a plain numeric vector of values to band.
#' @param question Question ID to band. Required for result sets, results and
#'   responses unless they contain exactly one question. Ignored for answers
#'   and numeric vectors.
#' @param breaks Strictly increasing numeric cut points (at least two,
#'   `-Inf`/`Inf` allowed), as in [base::cut()]. Required.
#' @param labels Character labels, one per band (`length(breaks) - 1`).
#'   If `NULL`, [base::cut()] interval labels are used.
#' @param right If `TRUE`, bands are `(a, b]`; otherwise `[a, b)`. The
#'   outermost band always includes its end point, so `breaks = c(0, 0.5, 1)`
#'   covers every probability from 0 to 1.
#' @return A factor with the bands as levels (all levels are kept, even when
#'   empty). For result sets it is named by state ID. The value is `NA`
#'   for failed states, missing or invalid answers, answers without
#'   confidence, and values outside the outermost breaks.
#' @export
#' @examples
#' answer <- list(type = "choice", choice = "yes", confidence = 0.62)
#' class(answer) <- c("jev_choice_answer", "jev_answer", "list")
#' jev_band(answer, breaks = c(0, 0.5, 0.8, 1),
#'   labels = c("escalate", "review", "auto"))
#'
#' # Noul: a symmetric "uncertain" band around 0.5
#' jev_band(c(0.1, 0.5, 0.9), breaks = c(0, 0.3, 0.7, 1),
#'   labels = c("no", "uncertain", "yes"))
jev_band <- function(x, question = NULL, breaks, labels = NULL, right = FALSE) {
  if (missing(breaks) || is.null(breaks)) {
    jev_abort(
      paste0(
        "`breaks` is required: jevr does not choose confidence thresholds. ",
        "Supply cut points, for example breaks = c(0, 0.5, 0.8, 1)."
      ),
      class = "jev_input_error"
    )
  }
  if (!is.numeric(breaks) || length(breaks) < 2L || anyNA(breaks) ||
    is.unsorted(breaks, strictly = TRUE)) {
    jev_abort(
      "`breaks` must be a strictly increasing numeric vector of length >= 2.",
      class = "jev_input_error"
    )
  }
  if (!is.null(labels) && (!is.character(labels) || anyNA(labels) ||
    length(labels) != length(breaks) - 1L || anyDuplicated(labels))) {
    jev_abort(
      "`labels` must be unique character values, one per band.",
      class = "jev_input_error"
    )
  }
  if (!is.null(question) && (!is.character(question) ||
    length(question) != 1L || is.na(question))) {
    jev_abort("`question` must be a single question ID.", class = "jev_input_error")
  }

  values <- jev_band_values(x, question)
  out <- cut(
    values,
    breaks = breaks,
    labels = if (is.null(labels)) NULL else labels,
    right = right,
    include.lowest = TRUE
  )
  names(out) <- names(values)
  out
}

jev_band_answer_value <- function(answer) {
  if (is.null(answer)) {
    return(NA_real_)
  }
  value <- if (identical(answer$type, "noul")) answer$noul else answer$confidence
  if (is.numeric(value) && length(value) == 1L) as.numeric(value) else NA_real_
}

jev_band_question <- function(question, ids, what) {
  if (is.null(question)) {
    if (length(ids) == 1L) {
      return(ids)
    }
    jev_abort(
      paste0("`question` is required when ", what, " has several questions."),
      class = "jev_input_error"
    )
  }
  if (length(ids) > 0L && !question %in% ids) {
    jev_abort(
      paste0("Unknown question ID: ", question, "."),
      class = "jev_input_error"
    )
  }
  question
}

jev_band_values <- function(x, question) {
  if (inherits(x, "jev_answer")) {
    return(jev_band_answer_value(x))
  }
  if (inherits(x, "jev_response")) {
    id <- jev_band_question(question, names(x$answers), "the response")
    return(jev_band_answer_value(x$answers[[id]]))
  }
  if (inherits(x, "jev_result")) {
    ids <- names(x$response$answers)
    id <- jev_band_question(question, ids, "the result")
    value <- jev_band_answer_value(x$response$answers[[id]])
    names(value) <- x$state_id
    return(value)
  }
  if (inherits(x, "jev_result_set")) {
    items <- jev_result_set_items(x)
    ids <- names(jev_result_question_definitions(x))
    if (length(ids) == 0L) {
      ids <- unique(unlist(lapply(items, function(i) names(i$response$answers))))
    }
    id <- jev_band_question(question, ids, "the result set")
    values <- vapply(
      items,
      function(item) jev_band_answer_value(item$response$answers[[id]]),
      numeric(1)
    )
    names(values) <- vapply(items, function(item) item$state_id, character(1))
    return(values)
  }
  if (is.numeric(x)) {
    return(x)
  }
  jev_abort(
    "`x` must be a jev_result_set, jev_result, jev_response, answer or numeric vector.",
    class = "jev_input_error"
  )
}
