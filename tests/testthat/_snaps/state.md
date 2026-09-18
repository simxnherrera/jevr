# state validation rejects empty or non-serializable values

    Code
      jev_validate_state(character())
    Condition
      Error:
      ! state cannot be an empty character vector.

---

    Code
      jev_validate_state(function() NULL)
    Condition
      Error:
      ! state must be a string, list, or other JSON-serializable value.
