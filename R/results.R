jev_empty_attempt_ledger <- function() {
  data.frame(
    request_ref = character(),
    execution_id = character(),
    state_id = character(),
    input_index = integer(),
    attempt = integer(),
    status = integer(),
    outcome = character(),
    provider = character(),
    upstream_provider = character(),
    requested_model = character(),
    actual_model = character(),
    response_id = character(),
    request_id = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    input_tokens = numeric(),
    output_tokens = numeric(),
    cost = numeric(),
    stringsAsFactors = FALSE
  )
}

jev_result <- function(
  state_id,
  input_index,
  status,
  response = NULL,
  question_errors = list(),
  provenance = list(),
  attempts = jev_empty_attempt_ledger(),
  error = NULL,
  request_ref = NULL
) {
  structure(
    list(
      state_id = state_id,
      input_index = input_index,
      status = status,
      response = response,
      question_errors = question_errors,
      provenance = provenance,
      attempts = attempts,
      error = jev_error_record(error),
      request_ref = request_ref
    ),
    class = c("jev_result", "list")
  )
}

jev_result_set <- function(
  results,
  definition,
  requests,
  run_status = "completed",
  summary = list(),
  callback_error = NULL
) {
  structure(
    results,
    class = c("jev_result_set", "list"),
    schema_version = 1L,
    run_status = run_status,
    definition = definition,
    definition_hash = if (is.null(definition$hash)) {
      NA_character_
    } else {
      definition$hash
    },
    requests = requests,
    summary = summary,
    callback_error = if (is.null(callback_error) ||
      (is.list(callback_error) && !inherits(callback_error, "condition") &&
        !is.null(callback_error$class) && !is.null(callback_error$message))) {
      callback_error
    } else {
      jev_error_record(callback_error)
    }
  )
}

jev_result_set_items <- function(x) {
  unclass(x)
}

jev_result_item_key <- function(item) {
  paste(
    item$provenance$execution_id,
    item$state_id,
    item$input_index,
    sep = "\r"
  )
}

jev_request_item_key <- function(requests) {
  paste(
    requests$execution_id,
    requests$state_id,
    requests$input_index,
    sep = "\r"
  )
}

jev_result_set_summary <- function(items, requests, summary) {
  if (length(summary) == 0L ||
    is.null(summary$started_at) || is.null(summary$finished_at)) {
    return(summary)
  }

  jev_map_summary(
    items,
    requests,
    summary$started_at,
    summary$finished_at
  )
}

jev_result_set_from <- function(x, items, requests = attr(x, "requests")) {
  if (is.null(names(items))) {
    names(items) <- names(jev_result_set_items(x))
  }

  jev_result_set(
    results = items,
    definition = attr(x, "definition"),
    requests = requests,
    run_status = attr(x, "run_status"),
    summary = jev_result_set_summary(
      items,
      requests,
      attr(x, "summary")
    ),
    callback_error = attr(x, "callback_error")
  )
}

#' @export
print.jev_result <- function(x, ...) {
  cat(
    "<jev_result>",
    x$state_id,
    "status=",
    x$status,
    "\n",
    sep = " "
  )
  invisible(x)
}

#' @export
print.jev_result_set <- function(x, ...) {
  summary <- attr(x, "summary")
  cat("<jev_result_set>\n")
  cat("Run status: ", attr(x, "run_status"), "\n", sep = "")
  cat("States: ", length(x), "\n", sep = "")
  if (length(summary) > 0L) {
    cat("Successes: ", summary$successes, "\n", sep = "")
    cat("Failures: ", summary$failures, "\n", sep = "")
    cat("HTTP attempts: ", summary$http_attempts, "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.jev_result_set <- function(object, ...) {
  attr(object, "summary")
}

#' @export
`[.jev_result_set` <- function(x, i, ..., drop = TRUE) {
  items <- if (missing(i)) {
    jev_result_set_items(x)
  } else {
    jev_result_set_items(x)[i]
  }

  requests <- attr(x, "requests")
  if (is.null(requests)) {
    requests <- jev_empty_attempt_ledger()
  }
  if (nrow(requests) > 0L && length(items) > 0L) {
    keys <- unique(vapply(items, jev_result_item_key, character(1)))
    requests <- requests[jev_request_item_key(requests) %in% keys, , drop = FALSE]
  } else {
    requests <- jev_empty_attempt_ledger()
  }

  jev_result_set_from(x, items, requests)
}

#' @export
c.jev_result_set <- function(..., recursive = FALSE) {
  sets <- list(...)
  sets <- sets[!vapply(sets, is.null, logical(1))]
  if (length(sets) == 0L) {
    return(NULL)
  }

  if (any(!vapply(sets, inherits, logical(1), what = "jev_result_set"))) {
    jev_abort(
      "jev_result_set objects can only be combined with other result sets.",
      class = "jev_input_error"
    )
  }

  definition_hashes <- vapply(
    sets,
    function(set) attr(set, "definition_hash"),
    character(1)
  )
  if (length(unique(definition_hashes)) > 1L) {
    jev_abort(
      "Cannot combine result sets with different definitions.",
      class = "jev_input_error"
    )
  }

  items <- list()
  for (set in sets) {
    items <- c(items, jev_result_set_items(set))
  }
  requests <- do.call(
    rbind,
    lapply(sets, function(set) {
      requests <- attr(set, "requests")
      if (nrow(requests) == 0L) NULL else requests
    })
  )
  if (is.null(requests)) {
    requests <- jev_empty_attempt_ledger()
  }

  summaries <- lapply(sets, attr, which = "summary")
  summary <- if (all(vapply(
    summaries,
    function(value) length(value) > 0L &&
      !is.null(value$started_at) && !is.null(value$finished_at),
    logical(1)
  ))) {
    jev_map_summary(
      items,
      requests,
      min(vapply(summaries, function(value) value$started_at, numeric(1))),
      max(vapply(summaries, function(value) value$finished_at, numeric(1)))
    )
  } else {
    list()
  }

  jev_result_set(
    results = items,
    definition = attr(sets[[1L]], "definition"),
    requests = requests,
    run_status = if (all(vapply(
      sets,
      function(set) identical(attr(set, "run_status"), "completed"),
      logical(1)
    ))) "completed" else "partial",
    summary = summary
  )
}

jev_result_value <- function(answer) {
  if (is.null(answer)) {
    return(NULL)
  }

  switch(
    answer$type,
    choice = answer$choice,
    score = answer$score,
    noul = answer$noul,
    NULL
  )
}

jev_result_question_definitions <- function(x) {
  definition <- attr(x, "definition")
  if (is.null(definition$questions)) {
    return(list())
  }

  if (exists("jev_questions_from_manifest", mode = "function")) {
    return(jev_questions_from_manifest(definition$questions))
  }

  definition$questions
}

#' Convert a result set to one row per state and requested question
#'
#' @param x A `jev_result_set`.
#' @param row.names Passed to `data.frame()`.
#' @param optional Passed to `data.frame()`.
#' @param ... Unused arguments.
#' @return A data frame with one row per state-question pair. `value`,
#'   `probabilities`, and `legend` remain list-columns; provenance and error
#'   fields are returned as scalar columns.
#' @details
#' Partial states produce one row for every requested question. Valid answers
#' have `status = "success"`; invalid or missing individual answers have
#' `status = "error"` and their question-level error in `error_class` and
#' `error_message`.
#' @export
#' @examplesIf identical(Sys.getenv("JEVR_RUN_EXAMPLES"), "true") && nzchar(Sys.getenv("TYPESAFE_API_KEY"))
#' states <- list(
#'   incident_a = list(message = "Payouts fail", days = 3),
#'   incident_b = list(message = "Refund pending", days = 1)
#' )
#' questions <- list(
#'   urgent = jev_noul("Does this request convey urgency?")
#' )
#' results <- jev_map(states, questions, provider = "typesafe")
#' as.data.frame(results)
as.data.frame.jev_result_set <- function(x, row.names = NULL, optional = FALSE, ...) {
  questions <- jev_result_question_definitions(x)
  items <- jev_result_set_items(x)
  question_ids <- names(questions)
  n <- length(items) * length(question_ids)

  state_id <- character(n)
  input_index <- integer(n)
  question_id <- character(n)
  question_type <- character(n)
  value <- vector("list", n)
  probabilities <- vector("list", n)
  confidence <- rep(NA_real_, n)
  legend <- vector("list", n)
  status <- character(n)
  execution_id <- character(n)
  request_ref <- character(n)
  spec_hash <- character(n)
  state_hash <- character(n)
  error_class <- rep(NA_character_, n)
  error_message <- rep(NA_character_, n)

  position <- 0L
  for (item in items) {
    for (id in question_ids) {
      position <- position + 1L
      answer <- if (!is.null(item$response)) item$response$answers[[id]] else NULL
      question_error <- if (is.null(item$question_errors)) {
        NULL
      } else {
        item$question_errors[[id]]
      }
      row_error <- if (!is.null(question_error)) question_error else item$error
      question <- questions[[id]]
      state_id[[position]] <- item$state_id
      input_index[[position]] <- item$input_index
      question_id[[position]] <- id
      question_type[[position]] <- question$type
      value[position] <- list(jev_result_value(answer))
      probabilities[position] <- list(
        if (is.null(answer)) NULL else answer$probabilities
      )
      confidence[[position]] <- if (is.null(answer) || is.null(answer$confidence)) {
        NA_real_
      } else {
        answer$confidence
      }
      legend[position] <- list(if (is.null(answer)) NULL else answer$legend)
      status[[position]] <- if (!is.null(answer)) {
        "success"
      } else if (!is.null(question_error)) {
        "error"
      } else {
        item$status
      }
      execution_id[[position]] <- if (is.null(item$provenance$execution_id)) {
        NA_character_
      } else {
        item$provenance$execution_id
      }
      request_ref[[position]] <- if (is.null(item$request_ref)) {
        NA_character_
      } else {
        item$request_ref
      }
      spec_hash[[position]] <- if (is.null(item$provenance$spec_hash)) {
        NA_character_
      } else {
        item$provenance$spec_hash
      }
      state_hash[[position]] <- if (is.null(item$provenance$state_hash)) {
        NA_character_
      } else {
        item$provenance$state_hash
      }
      error_class[[position]] <- if (is.null(row_error)) {
        NA_character_
      } else {
        row_error$class
      }
      error_message[[position]] <- if (is.null(row_error)) {
        NA_character_
      } else {
        row_error$message
      }
    }
  }

  result <- data.frame(
    state_id = state_id,
    input_index = input_index,
    question_id = question_id,
    question_type = question_type,
    status = status,
    execution_id = execution_id,
    request_ref = request_ref,
    spec_hash = spec_hash,
    state_hash = state_hash,
    error_class = error_class,
    error_message = error_message,
    stringsAsFactors = FALSE,
    row.names = row.names,
    check.names = FALSE
  )
  result$value <- I(value)
  result$probabilities <- I(probabilities)
  result$confidence <- confidence
  result$legend <- I(legend)
  result
}
