# Preflight checks for documented System One request limits.
#
# Hard limits (Choice <= 255 options, Score 2-10 levels) are enforced by the
# question constructors and validators. The size check here is a soft,
# approximate estimate (serialized JSON characters / 4) that only warns.

jev_preflight_limits <- function() {
  list(total_tokens = 64000, state_question_tokens = 32000)
}

jev_preflight_enabled <- function() {
  !identical(getOption("jevr.preflight", TRUE), FALSE)
}

jev_preflight_tokens <- function(x) {
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    ensure_ascii = FALSE
  )
  ceiling(nchar(as.character(json), type = "chars") / 4)
}

# Estimated tokens for the whole request, and for state plus the longest
# single question.
jev_preflight_estimate <- function(state, questions) {
  payload_questions <- lapply(questions, unclass)
  names(payload_questions) <- names(questions)
  state_tokens <- jev_preflight_tokens(state)
  question_tokens <- vapply(payload_questions, jev_preflight_tokens, numeric(1))
  list(
    total = jev_preflight_tokens(list(
      state = state, questions = payload_questions
    )),
    state_and_longest_question = state_tokens +
      if (length(question_tokens)) max(question_tokens) else 0
  )
}

# Character vector describing which limits are (approximately) exceeded.
jev_preflight_issues <- function(state, questions) {
  limits <- jev_preflight_limits()
  estimate <- jev_preflight_estimate(state, questions)
  issues <- character()
  if (estimate$total > limits$total_tokens) {
    issues <- c(issues, sprintf(
      "estimated request size ~%s tokens exceeds the 64k total limit",
      format(estimate$total, big.mark = ",", scientific = FALSE)
    ))
  }
  if (estimate$state_and_longest_question > limits$state_question_tokens) {
    issues <- c(issues, sprintf(
      paste0(
        "estimated state plus longest question ~%s tokens exceeds the ",
        "32k limit"
      ),
      format(estimate$state_and_longest_question,
        big.mark = ",", scientific = FALSE
      )
    ))
  }
  issues
}

jev_preflight_warn <- function(message, state_ids = NULL, issues = character()) {
  condition <- structure(
    class = c("jev_preflight_warning", "warning", "condition"),
    list(message = message, state_ids = state_ids, issues = issues, call = NULL)
  )
  warning(condition)
}

jev_preflight_note <- paste(
  "Sizes are approximate (serialized JSON characters / 4) and the request",
  "is still sent; set options(jevr.preflight = FALSE) to silence this check."
)

# Single evaluation: warn once when the request may exceed documented limits.
jev_preflight_ask <- function(state, questions) {
  if (!jev_preflight_enabled()) {
    return(invisible(NULL))
  }
  issues <- jev_preflight_issues(state, questions)
  if (length(issues) > 0L) {
    jev_preflight_warn(
      paste0(
        "Request may exceed System One limits: ",
        paste(issues, collapse = "; "), ". ", jev_preflight_note
      ),
      issues = issues
    )
  }
  invisible(NULL)
}

# Many states: one aggregated warning listing affected state ids.
jev_preflight_map <- function(items, questions) {
  if (!jev_preflight_enabled()) {
    return(invisible(NULL))
  }
  affected <- character()
  issues <- character()
  for (item in items) {
    if (!identical(item$status, "pending")) next
    found <- jev_preflight_issues(item$state, questions)
    if (length(found) > 0L) {
      affected <- c(affected, item$state_id)
      issues <- union(issues, sub(" ~[0-9,]+ tokens", "", found))
    }
  }
  if (length(affected) > 0L) {
    shown <- utils::head(affected, 10L)
    more <- length(affected) - length(shown)
    jev_preflight_warn(
      paste0(
        length(affected), " state(s) may exceed System One limits (",
        paste(issues, collapse = "; "), "): ",
        paste(shown, collapse = ", "),
        if (more > 0L) paste0(", and ", more, " more") else "",
        ". ", jev_preflight_note
      ),
      state_ids = affected,
      issues = issues
    )
  }
  invisible(NULL)
}
