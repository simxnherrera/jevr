
- [jevr](#jevr)
  - [Installation](#installation)
  - [Authentication](#authentication)
  - [Basic usage](#basic-usage)
  - [Question types](#question-types)
  - [Providers](#providers)
  - [Versioned specifications and multiple
    states](#versioned-specifications-and-multiple-states)
  - [Response object](#response-object)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# jevr

<!-- badges: start -->

[![R-CMD-check](https://github.com/simxnherrera/jevr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simxnherrera/jevr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`jevr` is an R client for [JEV / TypeSafe System
One](https://docs.typesafe.ai/concepts/system-one). It sends a state and
a set of typed questions to a model and returns structured answers that
can be used directly in R.

JEV is designed for structured decisions rather than conversational
chat. The package supports batched questions, probabilities, confidence,
usage, and provider metadata.

## Installation

Install the development version from GitHub:

``` r
install.packages("pak")
pak::pak("simxnherrera/jevr")
```

Once `jevr` is available on CRAN, the stable version can be installed
with:

``` r
install.packages("jevr")
```

## Authentication

Set the API key for the provider you want to use in your `.Renviron`
file:

``` r
usethis::edit_r_environ()
```

``` text
# Direct TypeSafe requests
TYPESAFE_API_KEY=your_typesafe_key

# OpenRouter requests
OPENROUTER_API_KEY=your_openrouter_key
```

Keys are read when `jev_ask()` is called and are not included in
returned objects.

## Basic usage

Define one or more questions, then pass them together with the state to
`jev_ask()`:

``` r
library(jevr)

questions <- list(
  department = jev_choice(
    instructions = "Which team should handle this?",
    criteria = c(
      billing = "Payments, invoicing, refunds",
      technical = "Bugs, outages, integrations",
      sales = "Pricing, upgrades, new accounts"
    )
  ),
  urgent = jev_noul(
    instructions = "Does this request convey urgency?"
  )
)

result <- jev_ask(
  state = "Help! My payouts have been failing for three days.",
  questions = questions
)

result$answers$department$choice
result$answers$department$probabilities
result$answers$urgent$noul
```

The names in `questions` become the IDs in `result$answers`. This makes
it possible to send several questions in one request without losing the
mapping between each question and its answer.

`state` can be a character string, a named list representing a JSON
object, or an unnamed list representing a JSON array.

## Question types

### Choice

`jev_choice()` selects one option from a named set. The answer contains
the selected option, the probability for each option, and model
confidence.

``` r
jev_choice(
  instructions = "Which priority applies?",
  criteria = c(
    low = "Can wait",
    high = "Needs attention soon"
  )
)
```

### Score

`jev_score()` places the state along an ordered scale of two to ten
levels. The answer contains the score, its legend, probabilities, and
confidence.

``` r
jev_score(
  instructions = "How severe is the issue?",
  criteria = c(
    "Minor inconvenience",
    "Material disruption",
    "Service unavailable"
  )
)
```

### Noul

`jev_noul()` estimates how strongly a statement is true and returns a
numeric value between 0 and 1. The value is not converted to `TRUE` or
`FALSE`.

``` r
jev_noul(
  instructions = "Does the request require immediate action?",
  criteria = list(
    true = "The request describes an immediate operational risk",
    false = "The request can wait without immediate harm"
  )
)
```

## Providers

### TypeSafe

TypeSafe is the default provider. It uses the `jev-latest` model alias
unless a different `model` is supplied:

``` r
result <- jev_ask(
  state = list(
    customer = "Acme",
    message = "My payouts have been failing for three days."
  ),
  questions = questions,
  provider = "typesafe"
)
```

The direct TypeSafe provider supports structured JSON values for
`state`, `instructions`, and criteria.

### OpenRouter

OpenRouter can be selected without changing the question definitions:

``` r
result <- jev_ask(
  state = "Help! My payouts have been failing for three days.",
  questions = questions,
  provider = "openrouter"
)
```

It uses `~typesafe/jev-latest` by default and authenticates with
`OPENROUTER_API_KEY`. Structured JSON instructions and criteria are
preserved when the provider contract accepts them.

## Versioned specifications and multiple states

Use `jev_spec()` when a definition must be identified and compared over
time:

``` r
spec <- jev_spec(
  name = "claim_validation",
  version = "1.0.0",
  questions = questions
)
```

The spec contains only data and a recalculable SHA-256 hash. It does not
store credentials, provider clients, callbacks, or operational settings.

`jev_map()` evaluates explicit states with bounded concurrency and
returns a named `jev_result_set`. Failures stay attached to their state
instead of aborting the whole collection:

``` r
states <- c(
  document_001 = "First document",
  document_002 = "Second document"
)

results <- jev_map(
  states,
  spec,
  concurrency = 4,
  on_result = function(result) {
    # Persist the terminal result in caller-owned storage.
  }
)

as.data.frame(results)
attr(results, "requests")
```

The callback is the persistence boundary: jevr does not manage a
database, checkpoint files, or a distributed job queue. For large
sources, read chunks outside the package and call `jev_map()` for each
chunk. `concurrency` limits requests in flight; `rate_limit` controls
admission separately.

## Response object

`jev_ask()` returns an object of class `jev_response` with these main
fields:

- `model`: model used for the request.
- `answers`: named answers corresponding to the supplied questions.
- `usage`: input and output token counts returned by the provider.
- `metadata`: provider and upstream request metadata.
- `raw`: the unmodified provider response.

Use `?jev_ask`, `?jev_map`, `?jev_spec`, `?jev_choice`, `?jev_score`,
and `?jev_noul` for the complete argument reference.
