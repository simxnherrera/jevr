consistency_questions <- function() {
  list(
    team = jev_choice(
      list(task = "Route"),
      list(billing = "Payments", technical = NULL, sales = "Pricing")
    ),
    urgent = jev_noul("Urgent?"),
    severity = jev_score("How severe?", c("Low", "High")),
    tone = jev_choice("Tone?", c(calm = "Calm", angry = "Angry"))
  )
}

# Every Choice copy puts extra mass on its first listed option, mimicking
# first-option bias.
consistency_response <- function(permuted, truth = "technical", bias = 0.2) {
  questions <- if (inherits(permuted, "jev_spec")) {
    jev_questions_from_manifest(permuted$questions)
  } else {
    permuted
  }
  answers <- lapply(questions, function(q) {
    if (q$type == "noul") {
      return(list(type = "noul", noul = 0.5))
    }
    if (q$type == "score") {
      return(list(
        type = "score", score = 0.5,
        legend = list(`0` = "Low", `1` = "High"),
        probabilities = list(`0` = 0.5, `1` = 0.5), confidence = 0.5
      ))
    }
    options <- names(q$criteria)
    p <- rep(0.1, length(options))
    names(p) <- options
    p[[if (truth %in% options) truth else options[[1]]]] <- 0.6
    p[[options[[1]]]] <- p[[options[[1]]]] + bias
    p <- p / sum(p)
    list(
      type = "choice", choice = names(p)[which.max(p)],
      probabilities = as.list(p), confidence = max(p)
    )
  })
  jev_parse_response(
    list(
      model = "jev-1.13.0", answers = answers,
      usage = list(input_tokens = 1, output_tokens = 1)
    ),
    "typesafe", questions
  )
}

test_that("permuting is deterministic with a seed and restores the RNG", {
  set.seed(42)
  before <- .Random.seed
  a <- jev_permute_choices(consistency_questions(), n = 3, seed = 7)
  expect_identical(.Random.seed, before)
  b <- jev_permute_choices(consistency_questions(), n = 3, seed = 7)
  expect_identical(a, b)
  other <- jev_permute_choices(consistency_questions(), n = 3, seed = 8)
  expect_false(identical(a, other))
})

test_that("only Choice questions are expanded, in place, with exact options", {
  questions <- consistency_questions()
  permuted <- jev_permute_choices(questions, n = 3, seed = 1)
  expect_equal(names(permuted), c(
    paste0("team__perm", 1:3), "urgent", "severity",
    paste0("tone__perm", 1:3)
  ))
  expect_identical(permuted$urgent, questions$urgent)
  expect_identical(permuted$severity, questions$severity)
  for (id in paste0("team__perm", 1:3)) {
    copy <- permuted[[id]]
    expect_s3_class(copy, "jev_choice_question")
    expect_identical(copy$instructions, questions$team$instructions)
    expect_setequal(names(copy$criteria), names(questions$team$criteria))
    for (option in names(copy$criteria)) {
      expect_identical(copy$criteria[[option]], questions$team$criteria[[option]])
    }
  }
  firsts <- vapply(
    paste0("team__perm", 1:3),
    function(id) names(permuted[[id]]$criteria)[[1]],
    character(1)
  )
  expect_length(unique(firsts), 3L)
  expect_silent(jev_validate_questions(permuted))
})

test_that("generated ids never collide with user ids", {
  questions <- list(
    a = jev_choice("A?", c(x = "x", y = "y")),
    a__perm1 = jev_noul("User question named like a copy"),
    a__perm2_1 = jev_noul("Another")
  )
  permuted <- jev_permute_choices(questions, n = 2, seed = 1)
  expect_false(anyDuplicated(names(permuted)) > 0L)
  expect_true(all(c("a__perm1", "a__perm2_1") %in% names(permuted)))
  expect_identical(permuted$a__perm1, questions$a__perm1)
  copies <- attr(permuted, "jev_permutation")$entries$a$copy_ids
  expect_false(any(copies %in% names(questions)))
  expect_length(copies, 2L)
})

test_that("specs are permuted into a new spec with a different hash", {
  spec <- jev_spec("routing", "1.0.0", consistency_questions())
  permuted <- jev_permute_choices(spec, n = 2, seed = 3)
  expect_s3_class(permuted, "jev_spec")
  expect_identical(permuted$name, "routing")
  expect_identical(permuted$version, "1.0.0")
  expect_false(identical(permuted$hash, spec$hash))
  expect_silent(jev_validate_spec(permuted))
  expect_identical(
    permuted$hash,
    jev_permute_choices(spec, n = 2, seed = 3)$hash
  )
})

test_that("input validation is classed", {
  q <- consistency_questions()
  expect_error(jev_permute_choices(q, n = 1), class = "jev_input_error")
  expect_error(jev_permute_choices(q, n = 2.5), class = "jev_input_error")
  expect_error(jev_permute_choices(q, seed = "a"), class = "jev_input_error")
  expect_error(jev_permute_choices(list(1)), class = "jev_input_error")
  expect_error(
    jev_consistency(consistency_response(q), q),
    class = "jev_input_error"
  )
  expect_error(
    jev_consistency(1, jev_permute_choices(q, seed = 1)),
    class = "jev_input_error"
  )
  noch <- jev_permute_choices(list(u = jev_noul("U?")), seed = 1)
  expect_error(jev_consistency(NULL, noch), class = "jev_input_error")
})

test_that("jev_consistency collapses a response to original ids", {
  questions <- consistency_questions()
  permuted <- jev_permute_choices(questions, n = 3, seed = 1)
  response <- consistency_response(permuted)
  out <- jev_consistency(response, permuted)

  expect_equal(out$question_id, c("team", "tone"))
  expect_true(all(is.na(out$state_id)))
  expect_equal(out$n_permutations, c(3L, 3L))
  expect_equal(out$n_answered, c(3L, 3L))
  team <- out[1, ]
  expect_equal(names(team$probabilities[[1]]), names(questions$team$criteria))
  expect_equal(sum(team$probabilities[[1]]), 1)
  expect_equal(names(team$choices[[1]]), paste0("team__perm", 1:3))
  expect_equal(team$choice, "technical")
  expect_equal(team$agreement, mean(team$choices[[1]] == "technical"))
  expect_equal(
    team$orders[[1]][["team__perm2"]],
    names(permuted$team__perm2$criteria)
  )
  expect_s3_class(out, "data.frame")
})

test_that("mean probabilities and agreement are exact on a hand-made case", {
  questions <- list(c1 = jev_choice("Pick", c(a = "A", b = "B")))
  permuted <- jev_permute_choices(questions, n = 2, seed = 1)
  answers <- list(
    list(
      type = "choice", choice = "a",
      probabilities = list(a = 0.9, b = 0.1), confidence = 0.9
    ),
    list(
      type = "choice", choice = "b",
      probabilities = list(b = 0.6, a = 0.4), confidence = 0.6
    )
  )
  names(answers) <- names(permuted)
  response <- jev_parse_response(
    list(model = "m", answers = answers), "typesafe", permuted
  )
  out <- jev_consistency(response, permuted)
  expect_equal(unname(out$probabilities[[1]]), c(0.65, 0.35))
  expect_equal(names(out$probabilities[[1]]), c("a", "b"))
  expect_equal(out$choice, "a")
  expect_equal(out$agreement, 0.5)
  expect_equal(out$confidence, 0.75)
})

test_that("jev_consistency works with jev_ask and jev_map results", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-typesafe-key")
  permuted <- jev_permute_choices(consistency_questions(), n = 2, seed = 5)
  body <- jsonlite::toJSON(
    consistency_response(permuted)$raw,
    auto_unbox = TRUE, null = "null"
  )
  mock <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(body))
    )
  }

  asked <- httr2::with_mocked_responses(
    mock, jev_ask("hi", permuted, max_retries = 0)
  )
  expect_equal(jev_consistency(asked, permuted)$question_id, c("team", "tone"))

  states <- c(a = "one", b = "two")
  mapped <- httr2::with_mocked_responses(
    mock, jev_map(states, permuted, max_retries = 0, progress = FALSE)
  )
  out <- jev_consistency(mapped, permuted)
  expect_equal(out$state_id, c("a", "a", "b", "b"))
  expect_equal(out$input_index, c(1L, 1L, 2L, 2L))
  expect_equal(out$question_id, rep(c("team", "tone"), 2))
  expect_equal(nrow(jev_consistency(mapped[[1]], permuted)), 2L)

  spec <- jev_permute_choices(
    jev_spec("s", "1", consistency_questions()), n = 2, seed = 5
  )
  body <- jsonlite::toJSON(
    consistency_response(spec)$raw,
    auto_unbox = TRUE, null = "null"
  )
  mapped_spec <- httr2::with_mocked_responses(
    mock, jev_map(states, spec, max_retries = 0, progress = FALSE)
  )
  expect_equal(nrow(jev_consistency(mapped_spec, spec)), 4L)
})

test_that("failed and partial states yield NA or partial rows", {
  permuted <- jev_permute_choices(consistency_questions(), n = 2, seed = 5)
  failed <- jev_result("s1", 1L, "error", response = NULL)
  out <- jev_consistency(failed, permuted)
  expect_equal(out$status, c("error", "error"))
  expect_true(all(is.na(out$choice)))
  expect_equal(out$n_answered, c(0L, 0L))

  response <- consistency_response(permuted)
  response$answers$team__perm2 <- NULL
  partial <- jev_consistency(response, permuted)
  expect_equal(partial$status, c("partial", "success"))
  expect_equal(partial$n_answered[[1]], 1L)
})

test_that("answers from another question set are rejected", {
  permuted <- jev_permute_choices(consistency_questions(), n = 2, seed = 5)
  other <- jev_permute_choices(consistency_questions(), n = 3, seed = 5)
  expect_error(
    jev_consistency(consistency_response(other), permuted),
    class = "jev_input_error"
  )
})
