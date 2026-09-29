jev_test_llm_questions <- function() {
  list(
    department = jev_choice(
      list(task = "Route", rules = list("Use evidence")),
      list(billing = list(topic = "Payments"), technical = NULL)
    ),
    severity = jev_score("How severe?", list(list(impact = "none"), "Degraded", "Blocking")),
    urgent = jev_noul("Urgent?", c(true = "Time sensitive", false = "Can wait"))
  )
}

jev_test_llm_raw <- function() {
  list(answers = list(
    urgent = list(noul = 0.8),
    severity = list(probabilities = list(`2` = 0.5, `0` = 0.1, `1` = 0.4)),
    department = list(probabilities = list(technical = 0.5, billing = 0.5))
  ))
}

jev_test_llm_backend <- function(raw = jev_test_llm_raw(), capture = NULL, provider_name = "mock", ...) {
  jev_llm(chat = function() {
    if (!is.null(capture)) capture$factories <- capture$factories + 1L
    structure(list(
      get_turns = function() list(),
      get_model = function() "reported-model",
      get_provider = function() {
        ellmer::Provider(
          name = provider_name, base_url = "https://example.invalid",
          extra_headers = c(Authorization = "mock-private-header"),
          credentials = function() stop("Credentials must not be read")
        )
      },
      get_tokens = function() data.frame(input = 42, output = 8, cost = NA_real_),
      chat_structured = function(..., type, echo, convert) {
        if (!is.null(capture)) {
          capture$calls <- capture$calls + 1L
          capture$prompt <- list(...)[[1L]]
          capture$type <- type
          capture$convert <- convert
        }
        raw
      }
    ), class = "Chat")
  }, model = "requested-model", ...)
}
