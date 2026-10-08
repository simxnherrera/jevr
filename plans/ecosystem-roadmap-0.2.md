# jevr 0.2 ecosystem roadmap

Source: ecosystem research on 2026-10-08 (TypeSafe docs, OpenRouter, Vercel AI
Gateway, Pydantic AI Gateway, Cloudflare Clef, OpenAI Decisions). The
TypeSafe `/v1/systemone` wire format is now served by several gateways, so
jevr becomes a client of that protocol with configurable endpoints.

Tier 4 items (Clef/OpenAI adapters, HTTP/2, renaming) are deferred.

## Shared rules for every task

- Work only in the task's own branch/worktree. Keep public API backwards
  compatible unless the task says otherwise.
- Do not edit `NEWS.md` (the integrator writes it). Run
  `devtools::document()` so `NAMESPACE` and `man/` match.
- Mock all HTTP in tests (match existing test style, testthat 3e, snapshots
  where existing tests use them). Never run paid integration tests
  (`JEVR_RUN_INTEGRATION_TESTS` must stay unset).
- Done means: `devtools::test()` passes and `devtools::check(error_on =
  "warning")` reports 0 errors / 0 warnings; new exports have roxygen docs
  with examples wrapped so they do not hit the network.

## Wave 1 (independent, run in parallel)

### T1. Endpoint abstraction and OpenRouter System One migration
- Add exported `jev_endpoint(base_url, api_key_env, model = NULL, headers =
  list(), name = NULL)` describing any TypeSafe-format server. Requests go to
  `<base_url>/v1/systemone`.
- Presets: `typesafe` (`https://api.typesafe.ai`, `TYPESAFE_API_KEY`,
  `jev-latest`), `openrouter` (`https://openrouter.ai/api`,
  `OPENROUTER_API_KEY`, `~typesafe/jev-latest`), `vercel`
  (`https://ai-gateway.vercel.sh/typesafe`, `AI_GATEWAY_API_KEY`,
  `typesafe-ai/jev`), `pydantic`
  (`https://gateway-us.pydantic.dev/proxy/typesafe`, `PYDANTIC_AI_GATEWAY_API_KEY`,
  `jev-latest`). Honor `TYPESAFE_BASE_URL` when the typesafe preset is used.
- `jev_ask()` / `jev_map()` accept `provider =` either a preset name (as
  today) or a `jev_endpoint` object.
- OpenRouter preset uses `/api/v1/systemone` with the native TypeSafe body.
  Keep the alpha Decisions mapping reachable as `provider =
  "openrouter_decisions"` (legacy), not the default.
- Acceptance: request builders produce the documented URL, auth header,
  model and body for each preset; existing tests still pass.

### T4. Forward compatibility
- Answers with an unknown `type`, or with extra fields, must not abort the
  response: keep them raw in the answer (e.g. `answer$raw`, class
  `jev_unknown_answer`), emit one classed warning per response.
- Question constructors accept extra named fields via `...` that are passed
  through to the request body unchanged (and included in the spec hash).
- Acceptance: tests for unknown answer kind, extra answer fields, extra
  question fields round-tripping into JSON.

### T7. Choice option-order consistency check
- New exported helper (suggested `jev_permute_choices(questions, n = 2,
  seed = NULL)`) that expands each Choice into `n` reordered copies in the
  same request, plus `jev_consistency()` that collapses a response/result
  back to the original question ids with mean probabilities, agreement rate
  and per-permutation choices.
- Motivation: Jev 1.13 jaggedness #8 (first-option bias).
- Acceptance: deterministic with seed; works with `jev_ask()` and
  `jev_map()` results; question ids map back exactly.

### T8. Preflight request limits
- Validate before sending: Choice ≤ 255 options, Score 2–10 levels (already
  enforced — keep), optional soft check of request size using a cheap
  token estimate (chars / 4) against 64k total and 32k for state + longest
  question. Soft check emits a classed warning; disable via
  `options(jevr.preflight = FALSE)`.
- Acceptance: tests for each limit; no false failures on current examples.

### T9. Long-format results
- `as.data.frame(x, format = "long")` (or exported `jev_long()`) on
  `jev_response`, `jev_result` and `jev_result_set`: one row per state ×
  question × option/level with columns `state_id, question_id, type, option,
  probability, selected, confidence, score, noul, model`.
- Acceptance: probabilities per question sum to ~1; noul yields
  `option = c("true","false")`; existing wide format unchanged by default.

### T10. Confidence band helpers
- Exported `jev_band(x, thresholds, labels)` style helper(s) that turn
  confidence (choice/score) or noul probability into caller-defined routing
  labels. No default thresholds — caller must supply them.
- Acceptance: vectorised over result sets; NA handling documented.

## Wave 2 (after T1 merged)

### T2. Gateway metadata and normalized cost
- Preserve top-level `provider_metadata` (Vercel) alongside `metadata`.
- Normalize cost into the request ledger from `usage.cost` (OpenRouter) or
  `provider_metadata.gateway.cost` (Vercel, string → numeric).
- Record routing info (`finalProvider`, `generationId`) when present.

### T3. Model provenance and alias drift
- Store the versioned `model` returned by the server on every result and in
  long/wide data frames; include it in result identity where the plan says
  aliases are not reusable.
- `jev_map()` warns (classed) if the answering model version changes within
  a run; summary reports the set of models seen.

### T5. `jev_models()`
- Wrap `GET <base_url>/v1/models`, return a data frame (`name, description,
  release_date`). For OpenRouter, give an informative classed error
  (different response shape).

## Wave 3 (after T1, T2 merged)

### T6. Gateway decision fallbacks
- `jev_fallback(model, question = NULL, confidence_below = NULL,
  probability_between = NULL)` passed as `fallback =` to `jev_ask()` /
  `jev_map()`; serialised into `providerOptions.gateway.models`. Only
  allowed on endpoints flagged as supporting it (vercel preset).
- Parse `x-ai-gateway-decision-fallback-*` headers into result metadata;
  treat `confidence: 0` with empty probabilities from an LLM fallback as
  unavailable (NA), not zero.

## Wave 4 (after everything)

### T11. Articles
- pkgdown articles: gateways/endpoints, reranking, date extraction (choices
  in model, arithmetic in code), composite scoring, consistency checks.
  Code chunks `eval = FALSE`.
