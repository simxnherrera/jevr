# jevr 0.2.0

* `jev_ask()` accepts an optional `jev_llm()` backend, preserving JEV question
  definitions and answer classes while making one ellmer structured call per
  evaluation. Choice and Score are calculated locally, probabilities are
  validated and marked as LLM self-reports, and confidence is explicitly absent.
* `jev_llm()` describes a chat factory with explicit model, inference options
  and semantic configuration. Provenance and execution identity include
  this description and the upstream provider name obtained from the public Chat
  interface, without inspecting or hashing the factory or Chat object.
  ellmer and ellmercodex remain optional integrations.
* `jev_map()` supports sequential ellmer evaluation with partial results,
  callbacks, cancellation and an invocation ledger. Concurrency above one and
  unsupported timeout/retry controls are rejected. The summary distinguishes
  LLM invocations from observed native HTTP attempts.

## Endpoints and gateways

* New `jev_endpoint()` describes any server that speaks the TypeSafe System
  One protocol (`<base_url>/v1/systemone`). `provider` in `jev_ask()` and
  `jev_map()` accepts an endpoint or a preset name: `"typesafe"` (honours
  `TYPESAFE_BASE_URL`), `"openrouter"`, `"vercel"` (Vercel AI Gateway) and
  `"pydantic"` (Pydantic AI Gateway). The Vercel and Pydantic presets are
  tested against their documented request and response shapes only.
* Noul questions without criteria now omit `criteria` from the request body
  instead of sending `null`, which OpenRouter's System One endpoint rejects.
* `provider = "openrouter"` now uses OpenRouter's native
  `/api/v1/systemone` endpoint. The previous alpha Decisions mapping remains
  available as `provider = "openrouter_decisions"`; execution ids and
  provenance for that path now report `openrouter_decisions`.
* New `jev_models()` lists the models an endpoint serves via
  `GET /v1/models` (not supported on OpenRouter, which uses a different
  catalogue format).
* Responses keep gateway `provider_metadata` verbatim, expose
  `final_provider` and `generation_id` when present, and normalise cost into
  `response$usage$cost` from OpenRouter `usage.cost` or Vercel's gateway cost.
* New `jev_fallback()` and the `jev_when()`, `jev_when_any()`,
  `jev_when_all()` and `jev_when_at_least()` condition builders configure
  Vercel AI Gateway decision fallbacks. Triggered fallbacks are reported in
  `response$metadata$fallback`; fallback answers without native confidence
  report `NA` confidence and probabilities. A fallback is part of the
  execution id.

## Provenance and forward compatibility

* Results record both the requested and the answering (versioned) model and
  flag alias requests with `is_alias`. `jev_map()` warns with
  `jev_model_drift_warning` when the answering model changes within a run, and
  `summary()` reports the models seen. `jev_ask()` responses record
  `metadata$requested_model`.
* Unknown answer types, and answers whose type does not match their question,
  are kept raw as `jev_unknown_answer` objects with a single
  `jev_unknown_answer_warning` instead of failing the response.
* `jev_choice()`, `jev_score()` and `jev_noul()` accept extra named fields in
  `...`, forwarded unchanged to TypeSafe-format endpoints and included in spec
  hashes.

## Analysis helpers

* `as.data.frame()` gains `format = "long"` for responses, results and result
  sets: one row per state, question and option or level, with status and
  error columns. The default wide format for result sets is unchanged.
* New `jev_permute_choices()` and `jev_consistency()` check Choice answers for
  option-order sensitivity by asking reordered copies in the same request and
  summarising agreement.
* New `jev_band()` maps confidence or Noul probabilities onto caller-defined
  bands. jevr never chooses thresholds.
* `jev_ask()` and `jev_map()` warn with `jev_preflight_warning` when a
  request likely exceeds the model's context limits (approximate estimate;
  disable with `options(jevr.preflight = FALSE)`).

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
