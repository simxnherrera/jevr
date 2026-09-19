# jevr 0.1.1

* Added `jev_ask()` for batched, provider-independent System One decisions.
* Added `jev_choice()`, `jev_score()`, and `jev_noul()` constructors with
  typed response parsing, probabilities, confidence, and score legends.
* Added direct 'TypeSafe' and 'OpenRouter' providers with secure environment-
  variable authentication, bounded retries, and informative errors.
* Added `jev_spec()` for versioned, hashed decision definitions and `jev_map()`
  for bounded multi-state execution with per-state results, provenance, and a
  caller-owned incremental result callback.
