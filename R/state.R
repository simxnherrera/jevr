jev_validate_state <- function(state) {
  if (is.null(state) || is.function(state) || is.environment(state)) {
    jev_abort(
      "state must be a string, list, or other JSON-serializable value.",
      class = "jev_input_error"
    )
  }

  if (is.character(state) && length(state) == 0L) {
    jev_abort(
      "state cannot be an empty character vector.",
      class = "jev_input_error"
    )
  }

  if (is.list(state)) {
    state_names <- names(state)
    if (!is.null(state_names) && anyNA(state_names)) {
      jev_abort(
        "state has missing list names and cannot be serialized reliably.",
        class = "jev_input_error"
      )
    }
    if (!is.null(state_names) &&
      any(nzchar(state_names)) && any(!nzchar(state_names))) {
      jev_abort(
        "state must use names for every list element or for none of them.",
        class = "jev_input_error"
      )
    }
  }

  jev_validate_json_value(state, "state")
}
