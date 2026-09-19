jev_identity_secret_names <- c(
  "api_key", "apikey", "authorization", "credential", "credentials",
  "password", "secret", "secrets", "token", "access_token",
  "refresh_token"
)

jev_identity_operational_names <- c(
  "timeout", "retries", "max_retries", "concurrency", "chunk_size",
  "rate_limit", "retry_budget", "progress", "callback", "callbacks",
  "on_result", "on_error"
)

jev_identity_node <- function(x, excluded_names = character()) {
  if (is.null(x)) {
    return(list(type = "null"))
  }

  if (is.function(x) || is.environment(x) || inherits(x, "formula") ||
    is.object(x) || is.complex(x) || is.raw(x)) {
    jev_abort(
      "Identity inputs must use plain JSON-compatible R values.",
      class = "jev_input_error"
    )
  }

  if (!is.atomic(x) && !is.list(x)) {
    jev_abort(
      "Identity inputs must use plain JSON-compatible R values.",
      class = "jev_input_error"
    )
  }
  if (anyNA(x) || (is.double(x) && any(!is.finite(x)))) {
    jev_abort(
      "Identity inputs must contain finite, non-missing values.",
      class = "jev_input_error"
    )
  }
  if (length(dim(x)) > 0L) {
    jev_abort(
      "Identity inputs must not contain matrix dimensions.",
      class = "jev_input_error"
    )
  }

  item_names <- names(x)
  if (!is.null(item_names) &&
    (anyNA(item_names) || any(!nzchar(item_names)) ||
      anyDuplicated(item_names))) {
    jev_abort(
      "Identity objects must have unique, non-empty names.",
      class = "jev_input_error"
    )
  }

  if (!is.null(item_names)) {
    items <- lapply(seq_along(x), function(index) {
      normalized_name <- tolower(gsub(
        "[-.]", "_", item_names[[index]], fixed = FALSE
      ))
      if (normalized_name %in% excluded_names) {
        return(NULL)
      }
      list(
        name = item_names[[index]],
        value = jev_identity_node(x[[index]], excluded_names)
      )
    })
    return(list(type = "object", items = Filter(Negate(is.null), items)))
  }

  if (is.list(x)) {
    return(list(
      type = "array",
      items = lapply(x, jev_identity_node, excluded_names = excluded_names)
    ))
  }

  if (length(x) == 1L) {
    return(list(
      type = "scalar",
      value_type = typeof(x),
      value = x[[1L]]
    ))
  }

  list(
    type = "array",
    value_type = typeof(x),
    items = lapply(x, function(value) jev_identity_node(value))
  )
}

jev_identity_json <- function(x, excluded_names = character()) {
  jsonlite::toJSON(
    jev_identity_node(x, excluded_names),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = 17,
    pretty = FALSE,
    ensure_ascii = FALSE
  )
}

jev_identity_hash <- function(x, excluded_names = character()) {
  unclass(as.character(openssl::sha256(charToRaw(
    jev_identity_json(x, excluded_names)
  ))))
}

jev_normalize_definition <- function(definition) {
  if (inherits(definition, "jev_spec")) {
    jev_validate_spec(definition)
    return(list(
      schema_version = definition$schema_version,
      name = definition$name,
      version = definition$version,
      questions = definition$questions
    ))
  }

  list(
    schema_version = 1L,
    name = NULL,
    version = NULL,
    questions = jev_normalize_questions(definition)
  )
}

#' Return the data-only manifest for a JEV definition
#'
#' @param definition A `jev_spec` or a named list of JEV questions.
#' @return A list containing the canonical definition fields, without its hash.
#' @export
jev_definition_manifest <- function(definition) {
  jev_normalize_definition(definition)
}

#' Hash a JEV definition
#'
#' @param definition A `jev_spec` or a named list of JEV questions.
#' @return A lowercase SHA-256 hexadecimal hash.
#' @export
jev_spec_hash <- function(definition) {
  jev_identity_hash(jev_normalize_definition(definition))
}

#' Hash a state using its deterministic identity representation
#'
#' @param state A JSON-compatible state value.
#' @return A lowercase SHA-256 hexadecimal hash.
#' @export
jev_state_hash <- function(state) {
  jev_validate_state(state)
  jev_identity_hash(state)
}

#' Build a deterministic identity for an effective JEV execution
#'
#' @param state A JSON-compatible state value.
#' @param definition A `jev_spec` or a named list of JEV questions.
#' @param provider Provider name.
#' @param model Requested model name.
#' @param endpoint_contract_version Version of the provider payload contract.
#' @param inference_options Options that affect inference semantics.
#' @param routing_options Options that affect request routing semantics.
#' @return A lowercase SHA-256 hexadecimal hash.
#' @export
jev_execution_id <- function(
  state,
  definition,
  provider,
  model,
  endpoint_contract_version = 1L,
  inference_options = list(),
  routing_options = list()
) {
  jev_validate_state(state)
  if (!is.character(provider) || length(provider) != 1L || is.na(provider) ||
    !nzchar(provider) || !is.character(model) || length(model) != 1L ||
    is.na(model) || !nzchar(model)) {
    jev_abort(
      "provider and model must be one non-empty string each.",
      class = "jev_input_error"
    )
  }

  definition <- jev_normalize_definition(definition)
  excluded_names <- c(
    jev_identity_secret_names,
    jev_identity_operational_names
  )
  jev_identity_hash(list(
    identity_schema_version = 1L,
    state_wire = state,
    spec_manifest = definition,
    effective_questions_wire = definition$questions,
    provider = provider,
    endpoint_contract_version = endpoint_contract_version,
    requested_model = model,
    inference_options = inference_options,
    routing_options = routing_options
  ), excluded_names = excluded_names)
}
