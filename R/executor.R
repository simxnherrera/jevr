jev_executor_now <- function() {
  unname(proc.time()[["elapsed"]])
}

jev_map_condition <- function(message, class, details = list()) {
  structure(
    c(list(message = message), details),
    class = unique(c(class, "jev_error", "error", "condition"))
  )
}

jev_map_condition_class <- function(error) {
  if (inherits(error, "jev_error")) {
    return(class(error)[[1L]])
  }

  if (jev_is_retryable_condition(error)) {
    return("jev_transport_error")
  }

  "jev_error"
}

jev_map_condition_message <- function(error) {
  if (inherits(error, "condition")) {
    return(conditionMessage(error))
  }

  if (is.null(error$message)) {
    return("Unknown jev_map error")
  }

  as.character(error$message)
}

jev_map_condition_details <- function(error) {
  if (!is.list(error)) {
    return(list())
  }

  error[names(error) %in% c(
    "status", "provider", "attempts", "retryable", "retry_at", "scope",
    "field_path", "request_id"
  )]
}

jev_map_as_error <- function(error) {
  if (is.null(error)) {
    return(NULL)
  }

  list(
    class = jev_map_condition_class(error),
    message = jev_map_condition_message(error),
    details = jev_map_condition_details(error)
  )
}

jev_map_request_row <- function(
  item,
  request_ref,
  status,
  outcome,
  actual_model = NA_character_,
  response_id = NA_character_,
  request_id = NA_character_,
  started_at,
  finished_at,
  duration_seconds,
  usage = list()
) {
  data.frame(
    request_ref = request_ref,
    execution_id = item$provenance$execution_id,
    state_id = item$state_id,
    input_index = item$input_index,
    attempt = item$attempts,
    status = as.integer(status),
    outcome = outcome,
    provider = item$provider,
    upstream_provider = if (is.null(usage$upstream_provider)) {
      NA_character_
    } else {
      usage$upstream_provider
    },
    requested_model = item$model,
    actual_model = actual_model,
    response_id = response_id,
    request_id = request_id,
    started_at = started_at,
    finished_at = finished_at,
    duration_seconds = duration_seconds,
    input_tokens = if (is.null(usage$input_tokens)) NA_real_ else usage$input_tokens,
    output_tokens = if (is.null(usage$output_tokens)) NA_real_ else usage$output_tokens,
    cost = if (is.null(usage$cost)) NA_real_ else usage$cost,
    stringsAsFactors = FALSE
  )
}

jev_map_terminal <- function(
  item,
  status,
  error = NULL,
  response = NULL,
  question_errors = item$question_errors
) {
  item$status <- status
  item$error <- jev_map_as_error(error)
  item$response <- response
  item$question_errors <- question_errors
  item
}

jev_map_retry_due <- function(item, now) {
  isTRUE(item$status == "pending") && item$next_eligible_at <= now
}

jev_map_deliver <- function(items, indices, on_result) {
  if (!is.function(on_result) || length(indices) == 0L) {
    return(list(items = items, error = NULL))
  }

  callback_error <- NULL
  for (index in indices) {
    item <- items[[index]]
    result <- jev_result(
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
    callback_error <- tryCatch(
      {
        on_result(result)
        NULL
      },
      error = identity
    )
    if (!is.null(callback_error)) {
      break
    }
  }

  list(items = items, error = callback_error)
}

jev_execute_map <- function(
  items,
  provider,
  questions,
  model,
  concurrency,
  timeout,
  max_retries,
  rate_limit,
  retry_budget,
  on_result = NULL,
  progress = FALSE,
  perform_parallel = httr2::req_perform_parallel,
  sleep = Sys.sleep,
  now = jev_executor_now,
  random = stats::runif,
  response_timing = httr2::resp_timing,
  delivered = integer()
) {
  requests <- jev_empty_attempt_ledger()
  pending <- which(vapply(items, function(item) item$status == "pending", logical(1)))
  run_status <- "completed"
  callback_error <- NULL
  systemic_waves <- 0L
  stop_admission <- FALSE
  cooldown_until <- 0

  while (length(pending) > 0L && !stop_admission) {
    current_time <- now()
    eligible <- pending[vapply(
      pending,
      function(index) jev_map_retry_due(items[[index]], current_time),
      logical(1)
    ) & current_time >= cooldown_until]

    if (length(eligible) == 0L) {
      next_time <- max(cooldown_until, min(vapply(
        pending,
        function(index) items[[index]]$next_eligible_at,
        numeric(1)
      )))
      wait_seconds <- max(0, next_time - current_time)
      if (wait_seconds > 0) {
        sleep_result <- tryCatch(
          {
            sleep(wait_seconds)
            NULL
          },
          interrupt = identity
        )
        if (inherits(sleep_result, "interrupt")) {
          run_status <- "cancelled"
          stop_admission <- TRUE
          break
        }
      }
      next
    }

    wave <- eligible[seq_len(min(concurrency, length(eligible)))]
    request_list <- list()
    request_indices <- integer()
    terminal_indices <- integer()
    wave_outcomes <- character()

    for (index in wave) {
      item <- items[[index]]
      if (is.na(item$started_at)) {
        item$started_at <- now()
      }
      items[[index]] <- item
      remaining <- retry_budget - (now() - item$started_at)
      if (remaining <= 0) {
        items[[index]] <- jev_map_terminal(
          item,
          "error",
          jev_map_condition(
            "The state exceeded its retry budget before the next attempt.",
            "jev_transport_error",
            list(retryable = TRUE)
          )
        )
        terminal_indices <- c(terminal_indices, index)
        wave_outcomes <- c(wave_outcomes, "transient_error")
        next
      }

      built <- tryCatch(
        jev_build_provider_request(
          provider = provider,
          state = item$state,
          questions = questions,
          model = model,
          timeout = min(timeout, remaining),
          rate_limit = rate_limit
        ),
        error = identity
      )

      if (inherits(built, "condition")) {
        if (!inherits(built, c("jev_input_error", "jev_provider_error"))) {
          stop(built)
        }
        items[[index]] <- jev_map_terminal(item, "invalid_input", built)
        terminal_indices <- c(terminal_indices, index)
        wave_outcomes <- c(wave_outcomes, "terminal_error")
        next
      }

      request_list <- c(request_list, list(built$request))
      request_indices <- c(request_indices, index)
    }

    if (length(request_list) > 0L) {
      responses <- tryCatch(
        perform_parallel(
          request_list,
          on_error = "continue",
          progress = progress,
          max_active = concurrency
        ),
        interrupt = identity,
        error = identity
      )

      if (inherits(responses, "condition")) {
        if (!inherits(responses, c("interrupt", "httr2_failure"))) {
          stop(responses)
        }

        run_status <- if (inherits(responses, "interrupt")) {
          "cancelled"
        } else {
          "error"
        }
        for (index in request_indices) {
          terminal_status <- if (inherits(responses, "interrupt")) {
            "cancelled"
          } else {
            "transport_error"
          }
          items[[index]] <- jev_map_terminal(
            items[[index]],
            terminal_status,
            responses
          )
          terminal_indices <- c(terminal_indices, index)
          wave_outcomes <- c(wave_outcomes, "cancelled")
        }
        stop_admission <- TRUE
      } else if (!is.list(responses) ||
        length(responses) != length(request_indices)) {
        run_status <- "error"
        incomplete_error <- jev_map_condition(
          "The parallel transport backend returned incomplete responses.",
          "jev_executor_error"
        )
        for (index in request_indices) {
          items[[index]] <- jev_map_terminal(
            items[[index]],
            "cancelled",
            incomplete_error
          )
          terminal_indices <- c(terminal_indices, index)
          wave_outcomes <- c(wave_outcomes, "terminal_error")
        }
        stop_admission <- TRUE
      } else {
        null_positions <- which(vapply(responses, is.null, logical(1)))
        if (length(null_positions) > 0L) {
          run_status <- "cancelled"
          stop_admission <- TRUE
        }

        for (position in seq_along(request_indices)) {
          index <- request_indices[[position]]
          item <- items[[index]]
          response <- responses[[position]]

          if (is.null(response)) {
            items[[index]] <- jev_map_terminal(
              item,
              "cancelled",
              jev_map_condition(
                "The parallel transport backend returned no response; execution was stopped.",
                "jev_executor_error"
              )
            )
            terminal_indices <- c(terminal_indices, index)
            wave_outcomes <- c(wave_outcomes, "cancelled")
            next
          }

          item$attempts <- item$attempts + 1L
          request_ref <- paste0(
            item$provenance$execution_id,
            "-input-",
            item$input_index,
            "-attempt-",
            item$attempts
          )
          item$request_ref <- request_ref
          status <- NA_integer_
          body <- NULL
          outcome <- "transport_error"
          error <- NULL
          parsed <- NULL
          parsed_response <- NULL
          question_errors <- list()
          actual_model <- NA_character_
          response_id <- NA_character_
          request_id <- NA_character_
          usage <- list()

          if (inherits(response, "httr2_response")) {
            http_metadata <- jev_response_metadata(response, provider)
            request_id <- if (is.null(http_metadata$request_id)) {
              NA_character_
            } else {
              http_metadata$request_id
            }
            status <- httr2::resp_status(response)
            if (status >= 200L && status < 300L) {
              body <- tryCatch(
                jev_body_json(response, provider),
                error = identity
              )
              if (inherits(body, "condition")) {
                error <- body
                outcome <- "parse_error"
              } else {
                parsed <- tryCatch(
                  jev_parse_response_partial(body, provider, questions),
                  error = identity
                )
                if (inherits(parsed, "condition")) {
                  if (!inherits(parsed, "jev_response_error")) {
                    stop(parsed)
                  }
                  error <- parsed
                  outcome <- "response_error"
                } else {
                  parsed_response <- parsed$response
                  parsed_response$metadata <- utils::modifyList(
                    parsed_response$metadata,
                    http_metadata
                  )
                  question_errors <- parsed$question_errors
                  actual_model <- parsed_response$model
                  response_id <- if (is.null(body$id)) NA_character_ else body$id
                  usage <- utils::modifyList(
                    parsed_response$usage,
                    parsed_response$metadata
                  )
                  outcome <- parsed$status
                }
              }
            } else {
              body <- tryCatch(
                httr2::resp_body_json(response, simplifyVector = FALSE),
                error = function(error) NULL
              )
              error <- tryCatch(
                jev_http_error(status, provider, body, item$attempts),
                error = identity
              )
              outcome <- "http_error"
            }
          } else {
            error <- if (inherits(response, "condition")) {
              response
            } else {
              jev_map_condition(
                "The provider did not return a response.",
                "jev_transport_error"
              )
            }
          }

          row <- jev_map_request_row(
            item = item,
            request_ref = request_ref,
            status = status,
            outcome = outcome,
            actual_model = actual_model,
            response_id = response_id,
            request_id = request_id,
            started_at = NA_character_,
            finished_at = NA_character_,
            duration_seconds = jev_http_duration(
              response,
              timing = response_timing
            ),
            usage = usage
          )
          item$attempts_log <- rbind(item$attempts_log, row)
          requests <- rbind(requests, row)

          retryable <- if (!is.na(status)) {
            jev_retryable_status(status)
          } else {
            jev_is_retryable_condition(error)
          }
          retry_delay <- if (retryable) {
            jev_retry_delay(
              item$attempts,
              response = if (inherits(response, "httr2_response")) {
                response
              } else {
                NULL
              },
              jitter = 0.1,
              random = random
            )
          } else {
            NA_real_
          }
          if (!is.na(status) && status %in% c(429L, 529L)) {
            cooldown_until <- max(cooldown_until, now() + retry_delay)
          }

          global_stop <- !is.na(status) && jev_http_global_stop(status, body)
          remaining <- retry_budget - (now() - item$started_at)
          can_retry <- retryable && item$attempts <= max_retries && remaining > 0

          if (outcome %in% c("success", "partial")) {
            items[[index]] <- jev_map_terminal(
              item,
              outcome,
              response = parsed_response,
              question_errors = question_errors
            )
            terminal_indices <- c(terminal_indices, index)
            wave_outcomes <- c(wave_outcomes, outcome)
          } else if (can_retry && !global_stop) {
            if (retry_delay >= remaining) {
              items[[index]] <- jev_map_terminal(item, "error", error)
              terminal_indices <- c(terminal_indices, index)
              wave_outcomes <- c(wave_outcomes, "transient_error")
            } else {
              item$status <- "pending"
              item$next_eligible_at <- now() + retry_delay
              items[[index]] <- item
              wave_outcomes <- c(wave_outcomes, "retry")
            }
          } else {
            final_status <- if (outcome == "transport_error") {
              "transport_error"
            } else if (global_stop) {
              "error"
            } else {
              "error"
            }
            items[[index]] <- jev_map_terminal(item, final_status, error)
            terminal_indices <- c(terminal_indices, index)
            wave_outcomes <- c(
              wave_outcomes,
              if (retryable) "transient_error" else "terminal_error"
            )
          }

          if (global_stop) {
            stop_admission <- TRUE
          }
        }
      }
    }

    pending <- which(vapply(items, function(item) item$status == "pending", logical(1)))

    if (length(wave_outcomes) > 0L && all(wave_outcomes %in% c(
      "retry", "transient_error"
    ))) {
      systemic_waves <- systemic_waves + 1L
    } else {
      systemic_waves <- 0L
    }
    if (systemic_waves >= 3L) {
      stop_admission <- TRUE
    }

    terminal_indices <- setdiff(terminal_indices, delivered)
    delivery <- jev_map_deliver(items, terminal_indices, on_result)
    items <- delivery$items
    delivered <- unique(c(delivered, terminal_indices))
    if (!is.null(delivery$error)) {
      callback_error <- delivery$error
      run_status <- "callback_error"
      stop_admission <- TRUE
    }
  }

  pending <- which(vapply(items, function(item) item$status == "pending", logical(1)))
  if (length(pending) > 0L) {
    for (index in pending) {
      attempted <- items[[index]]$attempts > 0L
      pending_status <- if (attempted) {
        if (identical(run_status, "cancelled")) "cancelled" else "error"
      } else if (identical(run_status, "cancelled")) {
        "cancelled"
      } else {
        "not_started"
      }
      pending_error <- if (attempted) {
        jev_map_condition(
          "Execution stopped after this state had already been attempted.",
          "jev_transport_error",
          list(retryable = TRUE)
        )
      } else {
        jev_map_condition(
          "Execution stopped before this state was admitted.",
          "jev_execution_error"
        )
      }
      items[[index]] <- jev_map_terminal(
        items[[index]],
        pending_status,
        pending_error
      )
    }
  }

  remaining_terminal <- setdiff(
    which(vapply(items, function(item) item$status != "pending", logical(1))),
    delivered
  )
  if (is.null(callback_error) && length(remaining_terminal) > 0L) {
    delivery <- jev_map_deliver(items, remaining_terminal, on_result)
    items <- delivery$items
    callback_error <- delivery$error
    if (!is.null(callback_error)) {
      run_status <- "callback_error"
    }
  }

  if (stop_admission && identical(run_status, "completed")) {
    run_status <- "stopped"
  }

  list(
    items = items,
    requests = requests,
    run_status = if (!is.null(callback_error)) "callback_error" else run_status,
    callback_error = callback_error
  )
}
