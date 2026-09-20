jev_validate_map_options <- function(
  concurrency,
  rate_limit,
  retry_budget,
  on_result,
  progress
) {
  if (!is.numeric(concurrency) || length(concurrency) != 1L ||
    !is.finite(concurrency) || concurrency < 1 ||
    concurrency != as.integer(concurrency)) {
    jev_abort(
      "concurrency must be one positive integer.",
      class = "jev_input_error"
    )
  }

  if (!is.numeric(rate_limit) || length(rate_limit) != 1L ||
    !is.finite(rate_limit) || rate_limit <= 0) {
    jev_abort(
      "rate_limit must be one positive, finite number of requests per second.",
      class = "jev_input_error"
    )
  }

  if (!is.numeric(retry_budget) || length(retry_budget) != 1L ||
    !is.finite(retry_budget) || retry_budget <= 0) {
    jev_abort(
      "retry_budget must be one positive, finite number of seconds.",
      class = "jev_input_error"
    )
  }

  if (!is.null(on_result) && !is.function(on_result)) {
    jev_abort(
      "on_result must be NULL or a function.",
      class = "jev_input_error"
    )
  }

  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    jev_abort(
      "progress must be one TRUE or FALSE value.",
      class = "jev_input_error"
    )
  }

  invisible(NULL)
}

jev_map_states <- function(states) {
  if (is.data.frame(states) || is.function(states) ||
    inherits(states, "formula")) {
    jev_abort(
      "states must be a character vector or list; convert tabular data explicitly to a named list.",
      class = "jev_input_error"
    )
  }

  if (!is.character(states) && !is.list(states)) {
    jev_abort(
      "states must be a character vector or list of complete states.",
      class = "jev_input_error"
    )
  }

  if (length(states) == 0L) {
    return(list(values = list(), ids = character(), durable_ids = logical()))
  }

  ids <- names(states)
  durable_ids <- !is.null(ids)
  if (is.null(ids)) {
    ids <- as.character(seq_along(states))
    durable_ids <- rep(FALSE, length(states))
  } else if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    jev_abort(
      "states names must be complete, non-empty, and unique.",
      class = "jev_input_error"
    )
  } else {
    durable_ids <- rep(TRUE, length(states))
  }

  values <- if (is.character(states)) as.list(states) else states
  list(values = values, ids = ids, durable_ids = durable_ids)
}

jev_map_definition <- function(questions) {
  if (inherits(questions, "jev_spec")) {
    jev_validate_spec(questions)
    return(questions)
  }

  jev_validate_questions(questions)
  questions
}

jev_map_item <- function(
  state,
  state_id,
  input_index,
  durable_id,
  definition_hash,
  provider,
  model,
  questions,
  definition
) {
  provenance <- list(
    spec_name = if (inherits(definition, "jev_spec")) definition$name else NULL,
    spec_version = if (inherits(definition, "jev_spec")) definition$version else NULL,
    spec_hash = definition_hash,
    state_hash = NA_character_,
    execution_id = NA_character_,
    state_id_durable = durable_id,
    provider = provider,
    requested_model = model
  )

  item <- list(
    state = NULL,
    state_id = state_id,
    input_index = input_index,
    status = "pending",
    response = NULL,
    question_errors = list(),
    provenance = provenance,
    attempts = 0L,
    attempts_log = jev_empty_attempt_ledger(),
    error = NULL,
    request_ref = NULL,
    next_eligible_at = 0,
    started_at = NA_real_,
    provider = provider,
    model = model
  )

  validated <- tryCatch(jev_validate_state(state), error = identity)
  if (inherits(validated, "condition")) {
    item$status <- "invalid_input"
    item$error <- jev_map_as_error(validated)
    return(item)
  }

  item$state <- validated
  item$provenance$state_hash <- jev_state_hash(validated)
  item$provenance$execution_id <- jev_execution_id(
    state = validated,
    definition = definition,
    provider = provider,
    model = model
  )
  item
}

jev_map_as_result <- function(item) {
  jev_result(
    state_id = item$state_id,
    input_index = item$input_index,
    status = item$status,
    response = item$response,
    question_errors = item$question_errors,
    provenance = item$provenance,
    attempts = item$attempts_log,
    error = item$error,
    request_ref = item$request_ref
  )
}

jev_map_summary <- function(items, requests, started_at, finished_at) {
  statuses <- vapply(items, function(item) item$status, character(1))
  attempts <- if (nrow(requests) == 0L) integer() else requests$attempt
  list(
    states = length(items),
    successes = sum(statuses == "success"),
    failures = sum(statuses %in% c(
      "error", "transport_error", "invalid_input", "cancelled"
    )),
    partial = sum(statuses == "partial"),
    not_started = sum(statuses == "not_started"),
    http_requests = nrow(unique(requests[c(
      "execution_id", "state_id", "input_index"
    )])),
    http_attempts = nrow(requests),
    retries = sum(attempts > 1L),
    status_429 = if (nrow(requests) == 0L) 0L else sum(requests$status == 429L, na.rm = TRUE),
    status_529 = if (nrow(requests) == 0L) 0L else sum(requests$status == 529L, na.rm = TRUE),
    transport_errors = if (nrow(requests) == 0L) 0L else sum(requests$outcome == "transport_error"),
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = max(0, finished_at - started_at),
    attempts_without_usage = if (nrow(requests) == 0L) {
      0L
    } else {
      sum(is.na(requests$input_tokens) | is.na(requests$output_tokens))
    },
    observed_input_tokens = if (nrow(requests) == 0L || all(is.na(requests$input_tokens))) {
      NA_real_
    } else {
      sum(requests$input_tokens, na.rm = TRUE)
    },
    observed_output_tokens = if (nrow(requests) == 0L || all(is.na(requests$output_tokens))) {
      NA_real_
    } else {
      sum(requests$output_tokens, na.rm = TRUE)
    },
    observed_cost = if (nrow(requests) == 0L || all(is.na(requests$cost))) {
      NA_real_
    } else {
      sum(requests$cost, na.rm = TRUE)
    }
  )
}

#' Evaluate shared questions for multiple independent states
#'
#' @param states A character vector or list of complete states. A named input
#'   supplies durable state IDs; unnamed inputs receive positional IDs.
#' @param questions A named list of questions or a `jev_spec()` object.
#' @param provider Provider to use.
#' @param model Requested model, or the provider default when `NULL`.
#' @param concurrency Maximum number of active HTTP requests per wave.
#' @param timeout Maximum seconds for one HTTP attempt.
#' @param max_retries Number of retries after the first attempt.
#' @param rate_limit Preventive requests-per-second throttle.
#' @param retry_budget Maximum seconds for retries and waits for one state.
#' @param on_result Optional caller-owned callback for each terminal result.
#' @param progress Display the httr2 progress indicator.
#' @return A `jev_result_set` preserving input order and IDs.
#' @details
#' `jev_map()` represents many independent evaluations. It does not combine
#' multiple states into one model request. Requests are admitted in bounded
#' waves, while `rate_limit` controls preventive admission separately from
#' `concurrency`.
#'
#' For a valid response envelope with invalid individual answers, the result
#' has `status = "partial"`, keeps only validated answers in `response$answers`,
#' and records per-question errors in `question_errors`. The request ledger's
#' `duration_seconds` is the per-response HTTP total reported by
#' `httr2::resp_timing()`; it is `NA` when that timing is unavailable. The
#' ledger timestamps are `NA` because `req_perform_parallel()` returns after
#' the HTTP work and does not expose reliable per-request wall-clock stamps.
#' @export
#' @examplesIf identical(Sys.getenv("JEVR_RUN_EXAMPLES"), "true") && nzchar(Sys.getenv("TYPESAFE_API_KEY"))
#' states <- list(
#'   incident_a = list(message = "Payouts fail", days = 3),
#'   incident_b = list(message = "Refund pending", days = 1)
#' )
#' questions <- list(
#'   urgent = jev_noul("Does this request convey urgency?")
#' )
#' results <- jev_map(
#'   states = states,
#'   questions = questions,
#'   provider = "typesafe",
#'   concurrency = 2
#' )
#' results
jev_map <- function(
  states,
  questions,
  provider = c("typesafe", "openrouter"),
  model = NULL,
  concurrency = 4L,
  timeout = 30,
  max_retries = 3L,
  rate_limit = 5,
  retry_budget = 120,
  on_result = NULL,
  progress = interactive()
) {
  provider <- match.arg(provider)
  definition <- jev_map_definition(questions)
  questions <- jev_questions_value(definition)
  jev_validate_request_options(timeout, max_retries)
  jev_validate_map_options(
    concurrency,
    rate_limit,
    retry_budget,
    on_result,
    progress
  )

  if (is.null(model)) {
    model <- jev_default_model(provider)
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
    !nzchar(model)) {
    jev_abort(
      "model must be one non-empty character string.",
      class = "jev_input_error"
    )
  }

  state_input <- jev_map_states(states)
  definition_hash <- jev_spec_hash(definition)
  definition_manifest <- jev_definition_manifest(definition)
  definition_manifest$hash <- definition_hash
  started_at <- jev_executor_now()

  items <- lapply(seq_along(state_input$values), function(index) {
    jev_map_item(
      state = state_input$values[[index]],
      state_id = state_input$ids[[index]],
      input_index = index,
      durable_id = state_input$durable_ids[[index]],
      definition_hash = definition_hash,
      provider = provider,
      model = model,
      questions = questions,
      definition = definition
    )
  })

  if (length(items) == 0L) {
    finished_at <- jev_executor_now()
    return(jev_result_set(
      results = list(),
      definition = definition_manifest,
      requests = jev_empty_attempt_ledger(),
      run_status = "completed",
      summary = jev_map_summary(list(), jev_empty_attempt_ledger(), started_at, finished_at)
    ))
  }

  valid_indices <- which(vapply(items, function(item) item$status == "pending", logical(1)))
  if (length(valid_indices) > 0L) {
    jev_api_key(provider)
  }

  invalid_indices <- which(vapply(items, function(item) item$status != "pending", logical(1)))
  callback_error <- NULL
  delivered <- integer()
  if (length(invalid_indices) > 0L && is.function(on_result)) {
    delivery <- jev_map_deliver(items, invalid_indices, on_result)
    items <- delivery$items
    callback_error <- delivery$error
    delivered <- invalid_indices
  }

  if (!is.null(callback_error)) {
    pending_indices <- which(vapply(
      items,
      function(item) item$status == "pending",
      logical(1)
    ))
    for (index in pending_indices) {
      items[[index]] <- jev_map_terminal(
        items[[index]],
        "not_started",
        jev_map_condition(
          "Execution stopped because on_result failed.",
          "jev_execution_error"
        )
      )
    }
  }

  execution <- if (is.null(callback_error) && length(valid_indices) > 0L) {
    jev_execute_map(
      items = items,
      provider = provider,
      questions = questions,
      model = model,
      concurrency = as.integer(concurrency),
      timeout = timeout,
      max_retries = as.integer(max_retries),
      rate_limit = rate_limit,
      retry_budget = retry_budget,
      on_result = on_result,
      progress = progress,
      delivered = delivered
    )
  } else {
    list(
      items = items,
      requests = jev_empty_attempt_ledger(),
      run_status = if (is.null(callback_error)) "completed" else "callback_error",
      callback_error = callback_error
    )
  }

  items <- execution$items
  finished_at <- jev_executor_now()
  results <- lapply(items, jev_map_as_result)
  names(results) <- state_input$ids
  run_status <- execution$run_status
  if (!is.null(callback_error)) {
    run_status <- "callback_error"
  }

  jev_result_set(
    results = results,
    definition = definition_manifest,
    requests = execution$requests,
    run_status = run_status,
    summary = jev_map_summary(
      items,
      execution$requests,
      started_at,
      finished_at
    ),
    callback_error = if (is.null(execution$callback_error)) {
      callback_error
    } else {
      execution$callback_error
    }
  )
}
