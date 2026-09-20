# jevr 0.1.2

* `jev_map()` now retains valid answers when an otherwise valid response has
  invalid individual answers, records those failures in `question_errors`, and
  reports the state as `partial`.
* The map request ledger now records per-response HTTP duration from
  `httr2::resp_timing()` when available; unavailable timings are `NA` rather
  than wave-level estimates. Retry history is preserved when subsetting or
  combining result sets.
* Provider cooldowns now honor `Retry-After` for 429 and 529 independently of
  whether the affected state still has retry budget. Ambiguous 404 responses
  remain local failures; shared model or endpoint failures stop new admission.
* `httr2 (>= 1.2.0)` is now the supported transport floor because the map
  ledger uses the public response-timing API.
* Added a development-only local-server cancellation probe. It verifies the
  observable partial-result behavior of `req_perform_parallel()` and records
  the remaining limitation when an interrupted active wave drains completely
  without a public cancellation marker.

# jevr 0.1.1

* Added `jev_ask()` for provider-independent System One decisions with multiple
  questions per evaluation.
* Added `jev_choice()`, `jev_score()`, and `jev_noul()` constructors with
  typed response parsing, probabilities, confidence, and score legends.
* Added direct 'TypeSafe' and 'OpenRouter' providers with secure environment-
  variable authentication, bounded retries, and informative errors.
* Added `jev_spec()` for versioned, hashed decision definitions and `jev_map()`
  for bounded multi-state execution with per-state results, provenance, and a
  caller-owned incremental result callback.
