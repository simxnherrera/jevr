jev_execute_llm_map <- function(
  items, questions, backend, rate_limit, on_result, progress,
  delivered = integer(), now = jev_executor_now, sleep = Sys.sleep,
  definition = questions
) {
  requests <- jev_empty_attempt_ledger()
  run_status <- "completed"
  callback_error <- NULL
  last_start <- -Inf
  pending <- which(vapply(items, function(item) item$status == "pending", logical(1)))

  for (index in pending) {
    item <- items[[index]]
    evaluation <- NULL
    elapsed_start <- NA_real_
    wall_start <- NA_character_
    interrupted <- FALSE
    parsed <- tryCatch(
      {
        wait <- max(0, last_start + 1 / rate_limit - now())
        if (wait > 0) sleep(wait)
        elapsed_start <- now()
        last_start <- elapsed_start
        wall_start <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        item$attempts <- 1L
        evaluation <- jev_llm_evaluate(item$state, questions, backend, definition)
        jev_llm_parse(evaluation, questions, backend, partial = TRUE)
      },
      error = identity,
      interrupt = function(condition) {
        interrupted <<- TRUE
        condition
      }
    )

    if (item$attempts > 0L) {
      identity_metadata <- if (is.null(evaluation)) parsed else evaluation$metadata
      item$provenance$execution_id <- if (is.null(identity_metadata$execution_id)) {
        NA_character_
      } else {
        identity_metadata$execution_id
      }
      item$provenance["upstream_provider"] <- list(identity_metadata$upstream_provider)
      outcome <- if (interrupted) {
        "cancelled"
      } else if (inherits(parsed, "condition")) {
        if (inherits(parsed, "jev_response_error")) "response_error" else "transport_error"
      } else {
        parsed$status
      }
      usage <- if (is.null(evaluation)) list() else evaluation$usage
      usage$upstream_provider <- identity_metadata$upstream_provider
      reported_model <- if (is.null(evaluation)) NULL else evaluation$metadata$reported_model
      item$request_ref <- paste0("llm-", index, "-1")
      row <- jev_map_request_row(item, item$request_ref,
        status = NA_integer_,
        outcome = outcome,
        actual_model = if (is.null(reported_model)) NA_character_ else reported_model,
        started_at = wall_start,
        finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        duration_seconds = max(0, now() - elapsed_start), usage = usage
      )
      item$attempts_log <- row
      requests <- rbind(requests, row)
    }
    if (interrupted) {
      items[[index]] <- jev_map_terminal(item, "cancelled", parsed)
      run_status <- "cancelled"
      break
    }
    if (inherits(parsed, "condition")) {
      if (!is.null(evaluation)) parsed$structured_result <- evaluation$structured
      item <- jev_map_terminal(
        item,
        if (inherits(parsed, "jev_response_error")) "error" else "transport_error", parsed
      )
    } else {
      parsed$response$metadata$execution_id <- item$provenance$execution_id
      parsed$response$metadata$spec_hash <- item$provenance$spec_hash
      parsed$response$metadata$state_hash <- item$provenance$state_hash
      item$provenance$reported_model <- parsed$response$metadata$reported_model
      item <- jev_map_terminal(item, parsed$status,
        response = parsed$response,
        question_errors = parsed$question_errors
      )
    }
    items[[index]] <- item
    if (progress) message("Evaluated ", index, "/", length(items), ": ", item$status)
    delivery <- jev_map_deliver(items, index, on_result)
    delivered <- c(delivered, index)
    if (!is.null(delivery$error)) {
      callback_error <- delivery$error
      run_status <- "callback_error"
      break
    }
  }

  remaining <- which(vapply(items, function(item) item$status == "pending", logical(1)))
  for (index in remaining) {
    items[[index]] <- jev_map_terminal(
      items[[index]],
      if (run_status == "cancelled") "cancelled" else "not_started",
      jev_map_condition("Execution stopped before this state was admitted.", "jev_execution_error")
    )
  }
  if (is.null(callback_error)) {
    terminal <- setdiff(seq_along(items), delivered)
    delivery <- jev_map_deliver(items, terminal, on_result)
    callback_error <- delivery$error
    if (!is.null(callback_error)) run_status <- "callback_error"
  }
  list(
    items = items, requests = requests, run_status = run_status,
    callback_error = callback_error
  )
}
