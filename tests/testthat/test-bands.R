make_answer <- function(type, value) {
  answer <- switch(
    type,
    choice = list(type = "choice", choice = "yes",
      probabilities = c(yes = 0.7, no = 0.3), confidence = value),
    noul = list(type = "noul", noul = value)
  )
  class(answer) <- c(paste0("jev_", type, "_answer"), "jev_answer", "list")
  answer
}

make_response <- function(conf, noul) {
  structure(
    list(model = "m", answers = list(
      c = make_answer("choice", conf), u = make_answer("noul", noul)
    )),
    class = "jev_response"
  )
}

make_set <- function() {
  questions <- list(
    c = jev_choice("Q?", c(yes = "Yes", no = "No")),
    u = jev_noul("Urgent?")
  )
  definition <- list(questions = lapply(questions, unclass))
  ok <- function(id, conf, noul) {
    jev_result(id, 1L, "success", response = make_response(conf, noul))
  }
  failed <- jev_result("c", 3L, "error", error = simpleError("boom"))
  jev_result_set(
    list(a = ok("a", 0.9, 0.1), b = ok("b", 0.55, 0.5), c = failed),
    definition = definition,
    requests = jev_empty_attempt_ledger()
  )
}

test_that("breaks are required and validated", {
  expect_error(jev_band(0.5), class = "jev_input_error")
  expect_error(jev_band(0.5, breaks = c(1, 0)), class = "jev_input_error")
  expect_error(jev_band(0.5, breaks = 0.5), class = "jev_input_error")
  expect_error(
    jev_band(0.5, breaks = c(0, 1), labels = c("a", "b")),
    class = "jev_input_error"
  )
  expect_error(jev_band("a", breaks = c(0, 1)), class = "jev_input_error")
})

test_that("numeric vectors and answers are banded, edges included", {
  b <- jev_band(c(0, 0.3, 0.7, 1, NA, 2), breaks = c(0, 0.3, 0.7, 1),
    labels = c("no", "uncertain", "yes"))
  expect_equal(as.character(b), c("no", "uncertain", "yes", "yes", NA, NA))
  expect_equal(levels(b), c("no", "uncertain", "yes"))
  expect_equal(
    as.character(jev_band(make_answer("choice", 0.9), breaks = c(0, 0.5, 1),
      labels = c("low", "high"))),
    "high"
  )
  expect_equal(
    as.character(jev_band(c(0.3, 0.7), breaks = c(0, 0.3, 0.7, 1), right = TRUE,
      labels = c("a", "b", "c"))),
    c("a", "b")
  )
})

test_that("responses and results need a question when ambiguous", {
  response <- make_response(0.9, 0.2)
  brk <- c(0, 0.5, 1)
  lab <- c("low", "high")
  expect_error(jev_band(response, breaks = brk), class = "jev_input_error")
  expect_error(jev_band(response, "zzz", breaks = brk), class = "jev_input_error")
  expect_equal(as.character(jev_band(response, "c", breaks = brk, labels = lab)), "high")
  expect_equal(as.character(jev_band(response, "u", breaks = brk, labels = lab)), "low")
  result <- jev_result("s1", 1L, "success", response = response)
  expect_equal(names(jev_band(result, "c", breaks = brk)), "s1")
})

test_that("result sets are vectorised, named, and NA for failures", {
  set <- make_set()
  b <- jev_band(set, "c", breaks = c(0, 0.6, 1), labels = c("review", "auto"))
  expect_equal(names(b), c("a", "b", "c"))
  expect_equal(as.character(b), c("auto", "review", NA))
  n <- jev_band(set, "u", breaks = c(0, 0.3, 0.7, 1),
    labels = c("no", "uncertain", "yes"))
  expect_equal(as.character(n), c("no", "uncertain", NA))
  expect_error(jev_band(set, breaks = c(0, 1)), class = "jev_input_error")
  expect_error(jev_band(set, "nope", breaks = c(0, 1)), class = "jev_input_error")
})

test_that("answers without confidence give NA", {
  answer <- make_answer("choice", NULL)
  answer["confidence"] <- list(NULL)
  expect_true(is.na(jev_band(answer, breaks = c(0, 1))))
})
