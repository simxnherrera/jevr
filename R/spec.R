#' Define a versioned set of JEV questions
#'
#' A spec is a data-only contract for a decision. It contains the questions,
#' an optional human-readable name and version, and a recalculable hash.
#'
#' @param name Optional non-empty name for the definition.
#' @param version Optional non-empty version label for the definition.
#' @param questions A non-empty named list of JEV questions.
#'
#' @return An object of class `jev_spec`.
#' @export
jev_spec <- function(name = NULL, version = NULL, questions) {
  spec <- structure(
    list(
      schema_version = 1L,
      name = jev_validate_spec_label(name, "name"),
      version = jev_validate_spec_label(version, "version"),
      questions = jev_normalize_questions(questions)
    ),
    class = "jev_spec"
  )
  spec$hash <- jev_spec_hash_unchecked(spec)
  spec
}

jev_validate_spec_label <- function(value, argument) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.character(value) || length(value) != 1L || is.na(value) ||
    !nzchar(value)) {
    jev_abort(
      paste0(argument, " must be NULL or one non-empty string."),
      class = "jev_input_error"
    )
  }

  value
}

jev_question_class_from_manifest <- function(question, argument) {
  if (!is.list(question) || is.null(question$type) ||
    !identical(names(question), c("type", "instructions", "criteria"))) {
    jev_abort(
      paste0(argument, " must contain type, instructions, and criteria."),
      class = "jev_input_error"
    )
  }

  class <- switch(
    question$type,
    choice = "jev_choice_question",
    score = "jev_score_question",
    noul = "jev_noul_question",
    NULL
  )
  if (is.null(class)) {
    jev_abort(
      paste0(argument, "$type is not a supported JEV question type."),
      class = "jev_input_error"
    )
  }

  structure(question, class = c(class, "jev_question"))
}

jev_questions_from_manifest <- function(questions) {
  if (!is.list(questions)) {
    jev_abort(
      "spec$questions must be a named list.",
      class = "jev_input_error"
    )
  }

  ids <- names(questions)
  if (is.null(ids) || anyNA(ids) || any(!nzchar(ids)) ||
    anyDuplicated(ids)) {
    jev_abort(
      "spec$questions must have unique, non-empty IDs.",
      class = "jev_input_error"
    )
  }

  result <- lapply(seq_along(questions), function(index) {
    jev_question_class_from_manifest(
      questions[[index]],
      paste0("spec$questions$", ids[[index]])
    )
  })
  names(result) <- ids
  result
}

jev_normalize_questions <- function(questions) {
  questions <- jev_validate_questions(questions)
  normalized <- lapply(questions, function(question) {
    unclass(question)
  })
  names(normalized) <- names(questions)
  jev_validate_questions(jev_questions_from_manifest(normalized))
  normalized
}

jev_validate_spec <- function(spec) {
  if (!inherits(spec, "jev_spec") || !is.list(spec)) {
    jev_abort(
      "spec must be a jev_spec() object.",
      class = "jev_input_error"
    )
  }

  required <- c("schema_version", "name", "version", "questions", "hash")
  if (!identical(names(spec), required) ||
    !identical(spec$schema_version, 1L)) {
    jev_abort(
      "spec has an unsupported or malformed schema.",
      class = "jev_input_error"
    )
  }

  jev_validate_spec_label(spec$name, "spec$name")
  jev_validate_spec_label(spec$version, "spec$version")
  jev_validate_questions(jev_questions_from_manifest(spec$questions))
  if (!is.character(spec$hash) || length(spec$hash) != 1L ||
    is.na(spec$hash) || !nzchar(spec$hash)) {
    jev_abort(
      "spec$hash must be one non-empty string.",
      class = "jev_input_error"
    )
  }
  invisible(spec)
}

jev_spec_hash_unchecked <- function(spec) {
  jev_identity_hash(list(
    schema_version = spec$schema_version,
    name = spec$name,
    version = spec$version,
    questions = spec$questions
  ))
}

#' @export
print.jev_spec <- function(x, ...) {
  jev_validate_spec(x)
  cat("JEV specification\n")
  cat("  schema version:", x$schema_version, "\n")
  if (!is.null(x$name)) {
    cat("  name:", x$name, "\n")
  }
  if (!is.null(x$version)) {
    cat("  version:", x$version, "\n")
  }
  cat("  questions:", length(x$questions), "\n")
  cat("  hash:", jev_spec_hash_unchecked(x), "\n")
  invisible(x)
}
