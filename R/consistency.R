#' Reorder Choice options to check first-option bias
#'
#' JEV leans slightly toward the first option of a Choice question. Asking the
#' same Choice several times with its options in different orders, in one
#' request, shows how much an answer depends on option order. Use
#' `jev_permute_choices()` to build the expanded questions, send them with
#' [jev_ask()] or [jev_map()], and collapse the answers with
#' [jev_consistency()].
#'
#' @param questions A named list of questions or a [jev_spec()] object.
#' @param n Number of reordered copies of every Choice question, at least 2.
#'   When `n` does not exceed the number of options, each copy starts with a
#'   different option, so every option leads at most once. Larger values, or
#'   Choice questions with few options, can repeat an order.
#' @param seed Optional single number making the orders reproducible. The
#'   caller's random number state is restored afterward. With `NULL`, orders
#'   are drawn from the current random number state.
#'
#' @return The same kind of object as `questions`: a named list of questions,
#'   or a new `jev_spec` with the same `name` and `version`. Each Choice
#'   question is replaced, in place, by `n` copies with identical
#'   instructions and descriptions but reordered options. Copy IDs are the
#'   original ID plus a suffix such as `__perm1`; the suffix is lengthened if
#'   needed so no generated ID equals an existing ID. Score and Noul
#'   questions pass through unchanged. The mapping back to the original IDs
#'   is stored in the `"jev_permutation"` attribute, which
#'   [jev_consistency()] reads.
#'
#' @details
#' The original Choice question is not sent: all `n` copies are reordered.
#' A permuted spec is a different definition from the original, so it has a
#' different [jev_spec_hash()]; results from it are not interchangeable with
#' results from the unpermuted spec. The attribute is not part of the hash;
#' the expanded questions are. Keep the returned object and pass it to
#' [jev_consistency()]: a result set stores only the expanded definition.
#' @seealso [jev_consistency()]
#' @export
#' @examples
#' questions <- list(
#'   team = jev_choice(
#'     "Which team should handle this?",
#'     c(billing = "Payments", technical = "Bugs", sales = "Pricing")
#'   ),
#'   urgent = jev_noul("Does this request convey urgency?")
#' )
#' permuted <- jev_permute_choices(questions, n = 3, seed = 1)
#' names(permuted)
jev_permute_choices <- function(questions, n = 2, seed = NULL) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
    n < 2 || n != as.integer(n)) {
    jev_abort("n must be one integer of at least 2.", class = "jev_input_error")
  }
  n <- as.integer(n)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
    is.na(seed) || !is.finite(seed))) {
    jev_abort("seed must be NULL or one finite number.", class = "jev_input_error")
  }

  spec <- NULL
  if (inherits(questions, "jev_spec")) {
    jev_validate_spec(questions)
    spec <- questions
    questions <- jev_questions_from_manifest(spec$questions)
  } else {
    questions <- jev_validate_questions(questions)
  }

  ids <- names(questions)
  is_choice <- vapply(
    questions, inherits, logical(1), what = "jev_choice_question"
  )

  orders <- jev_with_seed(seed, {
    lapply(which(is_choice), function(index) {
      jev_choice_orders(length(questions[[index]]$criteria), n)
    })
  })
  names(orders) <- ids[is_choice]

  separator <- "__perm"
  repeat {
    copy_ids <- lapply(ids[is_choice], function(id) {
      paste0(id, separator, seq_len(n))
    })
    names(copy_ids) <- ids[is_choice]
    all_ids <- c(ids, unlist(copy_ids, use.names = FALSE))
    if (!anyDuplicated(all_ids)) break
    separator <- paste0(separator, "_")
  }

  expanded <- list()
  entries <- list()
  for (index in seq_along(questions)) {
    id <- ids[[index]]
    question <- questions[[index]]
    if (!is_choice[[index]]) {
      expanded[id] <- list(question)
      entries[id] <- list(list(type = question$type))
      next
    }
    options <- names(question$criteria)
    option_orders <- lapply(orders[[id]], function(o) options[o])
    for (copy in seq_len(n)) {
      copy_question <- question
      copy_question$criteria <- question$criteria[orders[[id]][[copy]]]
      expanded[copy_ids[[id]][[copy]]] <- list(copy_question)
    }
    entries[id] <- list(list(
      type = "choice",
      options = options,
      copy_ids = copy_ids[[id]],
      orders = option_orders
    ))
  }

  plan <- structure(
    list(original_ids = ids, entries = entries, n = n),
    class = "jev_permutation_plan"
  )

  result <- if (is.null(spec)) {
    expanded
  } else {
    jev_spec(name = spec$name, version = spec$version, questions = expanded)
  }
  attr(result, "jev_permutation") <- plan
  result
}

# Orders as integer index vectors over k options. Leading options differ across
# copies while n <= k; later orders avoid duplicates when enough exist.
jev_choice_orders <- function(k, n) {
  firsts <- sample.int(k)
  distinct_possible <- k <= 10L && factorial(k) >= n
  orders <- list()
  for (copy in seq_len(n)) {
    first <- firsts[[(copy - 1L) %% k + 1L]]
    others <- setdiff(seq_len(k), first)
    for (attempt in 1:50) {
      candidate <- c(first, others[sample.int(length(others))])
      duplicated_order <- any(vapply(
        orders, identical, logical(1), y = candidate
      ))
      if (!distinct_possible || !duplicated_order) break
    }
    orders[[copy]] <- candidate
  }
  orders
}

jev_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit(
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    },
    add = TRUE
  )
  set.seed(seed, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expr
}

#' Collapse reordered Choice answers to the original questions
#'
#' Maps answers to the copies made by [jev_permute_choices()] back to the
#' original question IDs and summarizes how consistent they are.
#'
#' @param x A `jev_response` from [jev_ask()], a `jev_result`, or a
#'   `jev_result_set` from [jev_map()], produced with `permuted`.
#' @param permuted The object returned by [jev_permute_choices()] that was
#'   sent. It carries the mapping, so it must be passed even for result sets.
#'
#' @return A data frame with one row per state and original Choice question
#'   (Score and Noul answers are not summarized; read them from `x`):
#'   `state_id`, `input_index` (`NA` for a single response), `question_id`,
#'   `status`, `n_permutations`, `n_answered`, `choice`, `agreement`,
#'   `confidence`, and list-columns `probabilities`, `choices` and `orders`.
#'
#'   `probabilities` is the mean over answered copies, named and ordered as in
#'   the original question. `choice` is the option with the highest mean
#'   probability (ties go to the earlier original option). `agreement` is the
#'   share of answered copies whose own choice equals `choice`, so 1 means
#'   the answer did not depend on option order. `confidence` is the mean
#'   confidence. `choices` is the per-permutation choice, named by copy ID,
#'   and `orders` the option order each copy used.
#'
#'   `status` is `"success"` when all copies were answered, `"partial"` when
#'   some were not (statistics use the answered copies), and `"error"` when
#'   none were; the statistics are then `NA`.
#' @seealso [jev_permute_choices()]
#' @export
#' @examples
#' questions <- list(
#'   team = jev_choice(
#'     "Which team should handle this?",
#'     c(billing = "Payments", technical = "Bugs")
#'   )
#' )
#' permuted <- jev_permute_choices(questions, n = 2, seed = 1)
#' # After `response <- jev_ask(state, permuted)`:
#' # jev_consistency(response, permuted)
jev_consistency <- function(x, permuted) {
  plan <- jev_permutation_plan(permuted)

  items <- if (inherits(x, "jev_response")) {
    list(list(
      state_id = NA_character_, input_index = NA_integer_,
      response = x, status = "success"
    ))
  } else if (inherits(x, "jev_result_set")) {
    unname(jev_result_set_items(x))
  } else if (inherits(x, "jev_result")) {
    list(x)
  } else {
    jev_abort(
      "x must be a jev_response, jev_result, or jev_result_set.",
      class = "jev_input_error"
    )
  }

  rows <- list()
  for (item in items) {
    answers <- if (is.null(item$response)) list() else item$response$answers
    unknown <- setdiff(names(answers), jev_plan_answer_ids(plan))
    if (length(unknown) > 0L) {
      jev_abort(
        paste0(
          "x contains answers that are not part of permuted: ",
          paste(unknown, collapse = ", "), "."
        ),
        class = "jev_input_error"
      )
    }
    for (id in plan$original_ids) {
      entry <- plan$entries[[id]]
      if (!identical(entry$type, "choice")) next
      rows[[length(rows) + 1L]] <- jev_consistency_row(
        item$state_id, item$input_index, id, entry, answers
      )
    }
  }

  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  rows
}

jev_plan_answer_ids <- function(plan) {
  unlist(lapply(plan$original_ids, function(id) {
    entry <- plan$entries[[id]]
    if (identical(entry$type, "choice")) entry$copy_ids else id
  }), use.names = FALSE)
}

jev_permutation_plan <- function(permuted) {
  plan <- attr(permuted, "jev_permutation")
  if (!inherits(plan, "jev_permutation_plan")) {
    jev_abort(
      "permuted must be the object returned by jev_permute_choices().",
      class = "jev_input_error"
    )
  }
  question_ids <- if (inherits(permuted, "jev_spec")) {
    names(permuted$questions)
  } else {
    names(permuted)
  }
  if (!all(jev_plan_answer_ids(plan) %in% question_ids)) {
    jev_abort(
      "permuted no longer matches its permutation mapping.",
      class = "jev_input_error"
    )
  }
  if (!any(vapply(plan$entries, function(e) identical(e$type, "choice"), logical(1)))) {
    jev_abort(
      "permuted contains no permuted Choice questions.",
      class = "jev_input_error"
    )
  }
  plan
}

jev_consistency_row <- function(state_id, input_index, id, entry, answers) {
  options <- entry$options
  answered <- intersect(entry$copy_ids, names(answers))
  # Unknown or mismatched answers are kept raw by the parser; treat them as
  # unanswered copies rather than failing the whole summary.
  answered <- answered[!vapply(
    answers[answered],
    function(answer) {
      inherits(answer, "jev_unknown_answer") ||
        isTRUE(answer$confidence_unavailable)
    },
    logical(1)
  )]
  probabilities <- NULL
  choices <- character()
  confidences <- numeric()
  for (copy_id in answered) {
    answer <- answers[[copy_id]]
    p <- answer$probabilities
    if (!inherits(answer, "jev_choice_answer") ||
      !setequal(names(p), options)) {
      jev_abort(
        paste0("Answer ", copy_id, " is not a Choice answer for ", id, "."),
        class = "jev_input_error"
      )
    }
    p <- p[options]
    probabilities <- rbind(probabilities, p)
    choices[[copy_id]] <- answer$choice
    confidences <- c(
      confidences,
      if (is.null(answer$confidence)) NA_real_ else answer$confidence
    )
  }

  n_answered <- length(answered)
  mean_p <- NULL
  choice <- NA_character_
  agreement <- NA_real_
  confidence <- NA_real_
  if (n_answered > 0L) {
    mean_p <- colMeans(probabilities)
    names(mean_p) <- options
    choice <- options[[which.max(mean_p)]]
    agreement <- mean(choices == choice)
    confidence <- if (all(is.na(confidences))) {
      NA_real_
    } else {
      mean(confidences, na.rm = TRUE)
    }
  }

  row <- data.frame(
    state_id = if (is.null(state_id)) NA_character_ else state_id,
    input_index = if (is.null(input_index)) NA_integer_ else as.integer(input_index),
    question_id = id,
    status = if (n_answered == length(entry$copy_ids)) {
      "success"
    } else if (n_answered > 0L) {
      "partial"
    } else {
      "error"
    },
    n_permutations = length(entry$copy_ids),
    n_answered = n_answered,
    choice = choice,
    agreement = agreement,
    confidence = confidence,
    stringsAsFactors = FALSE
  )
  row$probabilities <- I(list(mean_p))
  row$choices <- I(list(if (n_answered > 0L) choices else NULL))
  row$orders <- I(list(stats::setNames(entry$orders, entry$copy_ids)))
  row
}
