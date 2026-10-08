test_that("long format has one row per option and sums to one", {
  choice <- structure(
    list(type = "choice", choice = "yes",
      probabilities = c(yes = 0.7, no = 0.3), confidence = 0.7),
    class = c("jev_choice_answer", "jev_answer", "list")
  )
  score <- structure(
    list(type = "score", score = 1,
      probabilities = c("0" = 0.2, "1" = 0.5, "2" = 0.3), confidence = 0.5,
      legend = c("0" = "a", "1" = "b", "2" = "c")),
    class = c("jev_score_answer", "jev_answer", "list")
  )
  noul <- structure(
    list(type = "noul", noul = 0.8),
    class = c("jev_noul_answer", "jev_answer", "list")
  )
  response <- structure(
    list(model = "jev-1.13.0",
      answers = list(c = choice, s = score, n = noul)),
    class = "jev_response"
  )

  long <- as.data.frame(response)
  expect_named(long, c(
    "state_id", "question_id", "type", "option", "probability", "selected",
    "confidence", "score", "noul", "model", "status", "error_class",
    "error_message"
  ))
  expect_equal(nrow(long), 7L)
  expect_true(all(is.na(long$state_id)))
  expect_equal(long$model, rep("jev-1.13.0", 7L))
  sums <- tapply(long$probability, long$question_id, sum)
  expect_equal(as.numeric(sums), c(1, 1, 1))
  expect_equal(long$option[long$question_id == "n"], c("true", "false"))
  expect_equal(long$option[long$selected & long$question_id == "c"], "yes")
  expect_equal(long$option[long$selected & long$question_id == "s"], "1")
  expect_equal(unique(long$score[long$question_id == "s"]), 1)
  expect_true(all(is.na(long$selected[long$question_id == "n"])))
  expect_equal(unique(long$noul[long$question_id == "n"]), 0.8)
  expect_error(as.data.frame(response, format = "wide"), class = "jev_input_error")
})

test_that("failed questions stay in long format as NA rows", {
  failed <- jev_result(
    "bad", 2L, "partial", response = NULL,
    question_errors = list(
      c = list(class = "jev_response_error", message = "boom")
    )
  )
  questions <- list(c = jev_choice("q", c(yes = "Yes", no = "No")))
  rows <- jev_long_result(failed, questions)
  expect_equal(nrow(rows), 1L)
  expect_true(is.na(rows$option))
  expect_equal(rows$status, "error")
  expect_equal(rows$error_class, "jev_response_error")
  expect_equal(rows$state_id, "bad")
  expect_equal(rows$type, "choice")
})

test_that("long result sets cover failed states and wide is unchanged", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(paste0(
        '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul",',
        '"noul":0.8}},"usage":{"input_tokens":2,"output_tokens":1}}'
      ))
    )
  }
  result <- httr2::with_mocked_responses(
    mock,
    jev_map(
      c(a = "one", b = "two"),
      list(urgent = jev_noul("Is it urgent?")),
      max_retries = 0, progress = FALSE
    )
  )
  wide <- as.data.frame(result)
  expect_identical(as.data.frame(result, format = "wide"), wide)
  expect_equal(nrow(wide), 2L)
  long <- as.data.frame(result, format = "long")
  expect_equal(nrow(long), 4L)
  expect_equal(long$state_id, c("a", "a", "b", "b"))
  expect_equal(long$option, rep(c("true", "false"), 2L))
  expect_equal(long$model, rep("jev-1.13.0", 4L))

  items <- unclass(result)
  items$b <- jev_result("b", 2L, "failed", error = simpleError("down"))
  broken <- jev_result_set_from(result, items)
  long <- as.data.frame(broken, format = "long")
  expect_equal(nrow(long), 3L)
  expect_equal(long$status[3L], "failed")
  expect_true(is.na(long$probability[3L]))
  expect_false(is.na(long$error_message[3L]))
})

test_that("score selected is the mode even when the score is fractional", {
  score <- list(
    type = "score", score = 1.05,
    probabilities = c("0" = 0, "1" = 0.95, "2" = 0.05), confidence = 0.9
  )
  tie <- list(
    type = "score", score = 0.5,
    probabilities = c("0" = 0.5, "1" = 0.5), confidence = 0.9
  )
  unknown <- jev_unknown_answer(list(type = "mystery"), "choice", "unknown_type")
  response <- structure(
    list(model = "m", answers = list(s = score, t = tie, u = unknown)),
    class = "jev_response"
  )
  long <- as.data.frame(response)
  expect_equal(long$option[long$selected %in% TRUE & long$question_id == "s"], "1")
  expect_equal(unique(long$score[long$question_id == "s"]), 1.05)
  expect_equal(sum(long$selected[long$question_id == "t"]), 2L)
  expect_equal(long$status[long$question_id == "u"], "unknown_answer")
  expect_equal(long$status[long$question_id == "s"][1L], "success")
})
