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

  jev_validate_json_value(state, "state")
}
