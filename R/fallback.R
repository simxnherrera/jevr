#' Describe a Vercel AI Gateway decision fallback
#'
#' A decision fallback reruns a successful but uncertain decision with another
#' model. It is an AI Gateway extension to the TypeSafe request, sent as one
#' conditional object in `providerOptions.gateway.models`. Pass the result as
#' `fallback =` to [jev_ask()] or [jev_map()].
#'
#' The primary model answers first. If `when` matches its answers, the gateway
#' reruns the original state and **all** questions with `model` and returns
#' that complete result (answers are not merged). Both stages are billed;
#' `usage` and `usage$cost` already report the combined totals, and latency is
#' the sum of both calls. The condition is not checked again on the fallback
#' result.
#'
#' Conditions follow the AI Gateway contract:
#'
#' * `confidence_below` (sent as `confidenceBelow`) applies to Choice and Score
#'   questions. It also matches when the primary confidence is unavailable.
#' * `probability_between` (sent as `probabilityBetween`) is an inclusive
#'   `c(lower, upper)` band on `P(true)` and applies to Noul questions only.
#' * Without `question`, the condition checks every question of the matching
#'   type and matches if any answer crosses the threshold.
#' * Compound conditions are built with [jev_when_any()], [jev_when_all()] and
#'   [jev_when_at_least()] and passed as `when`.
#'
#' jevr validates the structure before sending: thresholds in 0 to 1, ordered
#' bands, 1 to 20 conditions per compound, nesting depth at most 5, referenced
#' question IDs that exist, predicates that match the question type, and a
#' fallback `model` different from the primary model. Violations raise a
#' `jev_input_error`.
#'
#' @section Endpoint support:
#' Only endpoints with the `"fallback"` capability accept `fallback`: the
#' `"vercel"` preset, or a custom [jev_endpoint()] created with
#' `capabilities = "fallback"`. Other endpoints raise a `jev_input_error`
#' before any request is made.
#'
#' @section Results:
#' When a condition matches, response headers
#' `x-ai-gateway-decision-fallback-*` are parsed into
#' `response$metadata$fallback`, a list with `triggered`, `final_model`,
#' `primary_model`, `triggering_questions` (character) and
#' `triggering_questions_truncated`. The field is absent when the fallback did
#' not trigger. The top-level `model` names the fallback model whenever it
#' answered.
#'
#' If a language model produces a final Choice or Score answer, the gateway
#' returns `confidence: 0` with empty `probabilities` to mean "unavailable".
#' jevr converts this sentinel to `confidence = NA` and `NA` probabilities
#' (one `NA` per criteria option when known) instead of treating it as zero
#' confidence. Such answers carry `confidence_unavailable = TRUE`; in long
#' format they have `NA` probability and confidence, and [jev_band()] returns
#' `NA`. Noul answers are unaffected.
#'
#' @param model Fallback model, as an AI Gateway slug such as
#'   `"openai/gpt-6-astra"`. It must differ from the primary model.
#' @param question Optional question ID the condition applies to.
#' @param confidence_below Number in 0 to 1: match when Choice/Score confidence
#'   is below it.
#' @param probability_between Length-two numeric `c(lower, upper)` in 0 to 1:
#'   match when a Noul `P(true)` lies inside the inclusive range.
#' @param when A condition built with [jev_when()], [jev_when_any()],
#'   [jev_when_all()] or [jev_when_at_least()]. Mutually exclusive with
#'   `question`, `confidence_below` and `probability_between`.
#' @param on_error Optional character vector of model IDs appended after the
#'   conditional object as execution-error fallbacks. They run only if a
#'   model fails to execute.
#'
#' @return An object of class `jev_fallback`.
#' @export
#' @examples
#' jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
#'
#' jev_fallback(
#'   "openai/gpt-6-astra",
#'   when = jev_when_any(
#'     jev_when("intent", confidence_below = 0.6),
#'     jev_when("refund", probability_between = c(0.4, 0.6))
#'   ),
#'   on_error = "anthropic/claude-sonnet-5"
#' )
#'
#' \dontrun{
#' jev_ask(
#'   "I was charged twice and cannot sign in.",
#'   list(intent = jev_choice("Which team?", c(billing = "Charges", account = "Sign-in"))),
#'   provider = "vercel",
#'   fallback = jev_fallback("openai/gpt-6-astra", "intent", confidence_below = 0.6)
#' )
#' }
jev_fallback <- function(
  model,
  question = NULL,
  confidence_below = NULL,
  probability_between = NULL,
  when = NULL,
  on_error = character()
) {
  fail <- function(message) {
    jev_abort(paste0("jev_fallback(): ", message), class = "jev_input_error")
  }
  if (!jev_scalar_string(model)) {
    fail("model must be one non-empty string.")
  }
  if (!is.character(on_error) || anyNA(on_error) || any(!nzchar(on_error))) {
    fail("on_error must be a character vector of model IDs.")
  }
  shorthand <- !is.null(question) || !is.null(confidence_below) ||
    !is.null(probability_between)
  if (!is.null(when)) {
    if (shorthand) {
      fail("supply either `when` or question/confidence_below/probability_between, not both.")
    }
    if (!inherits(when, "jev_condition")) {
      fail("when must be built with jev_when(), jev_when_any(), jev_when_all() or jev_when_at_least().")
    }
  } else {
    if (is.null(confidence_below) && is.null(probability_between)) {
      fail("supply `when`, confidence_below or probability_between.")
    }
    when <- jev_when(question, confidence_below, probability_between)
  }
  if (model %in% on_error) {
    fail("on_error must not repeat the conditional model.")
  }

  structure(
    list(model = model, when = when, on_error = unname(on_error)),
    class = "jev_fallback"
  )
}

#' Build a decision-fallback condition
#'
#' Building blocks for the `when` argument of [jev_fallback()]. `jev_when()` is
#' a direct condition on one question (or on every question of the matching
#' type when `question` is `NULL`); the others combine conditions.
#'
#' @inheritParams jev_fallback
#' @param ... Conditions created by these functions (1 to 20, nesting at most
#'   5 levels deep).
#' @param count Integer: how many nested conditions must match (1 up to the
#'   number of conditions).
#' @return An object of class `jev_condition`.
#' @seealso [jev_fallback()]
#' @export
#' @examples
#' jev_when("intent", confidence_below = 0.6)
#' jev_when_all(
#'   jev_when("intent", confidence_below = 0.6),
#'   jev_when("severity", confidence_below = 0.7)
#' )
#' jev_when_at_least(
#'   2,
#'   jev_when("intent", confidence_below = 0.6),
#'   jev_when("severity", confidence_below = 0.7),
#'   jev_when("refund", probability_between = c(0.4, 0.6))
#' )
jev_when <- function(
  question = NULL,
  confidence_below = NULL,
  probability_between = NULL
) {
  fail <- function(message) {
    jev_abort(paste0("jev_when(): ", message), class = "jev_input_error")
  }
  if (is.null(confidence_below) == is.null(probability_between)) {
    fail("supply exactly one of confidence_below or probability_between.")
  }
  if (!is.null(question) &&
    (!jev_scalar_string(question) || nchar(question) > 256L)) {
    fail("question must be NULL or a string of 1 to 256 characters.")
  }
  unit <- function(x) {
    is.numeric(x) && all(is.finite(x)) && all(x >= 0 & x <= 1)
  }
  if (!is.null(confidence_below) &&
    !(length(confidence_below) == 1L && unit(confidence_below))) {
    fail("confidence_below must be one number from 0 to 1.")
  }
  if (!is.null(probability_between)) {
    if (!(length(probability_between) == 2L && unit(probability_between))) {
      fail("probability_between must be c(lower, upper) with values from 0 to 1.")
    }
    if (probability_between[[1L]] > probability_between[[2L]]) {
      fail("the lower bound of probability_between cannot exceed the upper bound.")
    }
    probability_between <- as.numeric(unname(probability_between))
  }

  structure(
    list(
      kind = "direct", question = question,
      confidence_below = if (is.null(confidence_below)) NULL else as.numeric(confidence_below),
      probability_between = probability_between
    ),
    class = "jev_condition"
  )
}

jev_when_compound <- function(kind, conditions, count = NULL, caller) {
  fail <- function(message) {
    jev_abort(paste0(caller, "(): ", message), class = "jev_input_error")
  }
  if (!all(vapply(conditions, inherits, logical(1), "jev_condition"))) {
    fail("all arguments must be conditions built with jev_when() or jev_when_*().")
  }
  if (length(conditions) < 1L || length(conditions) > 20L) {
    fail("supply 1 to 20 conditions.")
  }
  if (!is.null(count) &&
    (!is.numeric(count) || length(count) != 1L || !is.finite(count) ||
      count != as.integer(count) || count < 1 || count > length(conditions))) {
    fail("count must be an integer from 1 to the number of conditions.")
  }
  structure(
    list(
      kind = kind, conditions = unname(conditions),
      count = if (is.null(count)) NULL else as.integer(count)
    ),
    class = "jev_condition"
  )
}

#' @rdname jev_when
#' @export
jev_when_any <- function(...) {
  jev_when_compound("any", list(...), caller = "jev_when_any")
}

#' @rdname jev_when
#' @export
jev_when_all <- function(...) {
  jev_when_compound("all", list(...), caller = "jev_when_all")
}

#' @rdname jev_when
#' @export
jev_when_at_least <- function(count, ...) {
  jev_when_compound("at_least", list(...), count, caller = "jev_when_at_least")
}

jev_condition_depth <- function(condition) {
  if (identical(condition$kind, "direct")) {
    return(1L)
  }
  1L + max(vapply(condition$conditions, jev_condition_depth, integer(1)))
}

# Plain-list wire form of a condition (AI Gateway key names).
jev_condition_wire <- function(condition) {
  switch(
    condition$kind,
    direct = {
      out <- list()
      if (!is.null(condition$question)) out$question <- condition$question
      if (!is.null(condition$confidence_below)) {
        out$confidenceBelow <- condition$confidence_below
      }
      if (!is.null(condition$probability_between)) {
        out$probabilityBetween <- condition$probability_between
      }
      out
    },
    any = list(any = lapply(condition$conditions, jev_condition_wire)),
    all = list(all = lapply(condition$conditions, jev_condition_wire)),
    at_least = list(atLeast = list(
      count = condition$count,
      conditions = lapply(condition$conditions, jev_condition_wire)
    ))
  )
}

jev_fallback_provider_options <- function(fallback) {
  models <- c(
    list(list(
      model = fallback$model,
      when = jev_condition_wire(fallback$when)
    )),
    as.list(fallback$on_error)
  )
  list(gateway = list(models = models))
}

jev_validate_condition <- function(condition, questions, fail) {
  if (!identical(condition$kind, "direct")) {
    for (nested in condition$conditions) {
      jev_validate_condition(nested, questions, fail)
    }
    return(invisible(NULL))
  }
  id <- condition$question
  if (!is.null(id)) {
    if (!id %in% names(questions)) {
      fail(paste0("fallback references unknown question ID: ", id, "."))
    }
    type <- questions[[id]]$type
    if (!is.null(condition$confidence_below) && !type %in% c("choice", "score")) {
      fail(paste0(
        "confidence_below applies to Choice and Score questions, but ",
        id, " is ", type, "."
      ))
    }
    if (!is.null(condition$probability_between) && !identical(type, "noul")) {
      fail(paste0(
        "probability_between applies to Noul questions, but ", id, " is ",
        type, "."
      ))
    }
  } else {
    types <- vapply(questions, function(q) q$type, character(1))
    wanted <- if (is.null(condition$confidence_below)) "noul" else c("choice", "score")
    if (!any(types %in% wanted)) {
      fail(paste0(
        "fallback condition checks every ",
        paste(wanted, collapse = "/"), " question, but there are none."
      ))
    }
  }
  invisible(NULL)
}

jev_validate_fallback <- function(fallback, questions, endpoint, model) {
  if (is.null(fallback)) {
    return(invisible(NULL))
  }
  fail <- function(message) {
    jev_abort(message, class = "jev_input_error")
  }
  if (!inherits(fallback, "jev_fallback")) {
    fail("fallback must be created with jev_fallback().")
  }
  if (!jev_endpoint_supports(endpoint, "fallback")) {
    fail(paste0(
      "The ", endpoint$name, " endpoint does not support decision fallbacks. ",
      "Use provider = \"vercel\" or a jev_endpoint() created with ",
      "capabilities = \"fallback\"."
    ))
  }
  if (identical(fallback$model, model)) {
    fail("The fallback model must differ from the primary model.")
  }
  if (jev_condition_depth(fallback$when) > 5L) {
    fail("fallback conditions can nest at most 5 levels deep.")
  }
  jev_validate_condition(fallback$when, questions, fail)
  invisible(NULL)
}

#' @export
print.jev_fallback <- function(x, ...) {
  cat("<jev_fallback> ", x$model, "\n", sep = "")
  cat(
    "when: ",
    as.character(jsonlite::toJSON(
      jev_condition_wire(x$when), auto_unbox = TRUE, digits = NA
    )),
    "\n",
    sep = ""
  )
  if (length(x$on_error)) {
    cat("on error: ", paste(x$on_error, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.jev_condition <- function(x, ...) {
  cat("<jev_condition> ", as.character(jsonlite::toJSON(
    jev_condition_wire(x), auto_unbox = TRUE, digits = NA
  )), "\n", sep = "")
  invisible(x)
}

jev_parse_fallback_headers <- function(headers) {
  names(headers) <- tolower(names(headers))
  get <- function(name) {
    value <- headers[[paste0("x-ai-gateway-decision-fallback-", name)]]
    if (is.null(value) || length(value) != 1L) NULL else as.character(value)
  }
  if (!identical(tolower(get("triggered")), "true")) {
    return(NULL)
  }
  questions <- get("triggering-questions")
  parsed <- if (is.null(questions)) {
    character()
  } else {
    tryCatch(
      {
        decoded <- utils::URLdecode(questions)
        Encoding(decoded) <- "UTF-8"
        as.character(unlist(jsonlite::fromJSON(decoded, simplifyVector = FALSE)))
      },
      error = function(error) NA_character_
    )
  }
  list(
    triggered = TRUE,
    final_model = if (is.null(get("final-model"))) NA_character_ else get("final-model"),
    primary_model = if (is.null(get("primary-model"))) NA_character_ else get("primary-model"),
    triggering_questions = parsed,
    triggering_questions_truncated =
      identical(tolower(get("triggering-questions-truncated")), "true")
  )
}
