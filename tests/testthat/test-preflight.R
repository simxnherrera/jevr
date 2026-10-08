big_state <- function(chars) strrep("a", chars)
q_noul <- function() list(urgent = jev_noul("Is it urgent?"))
ok_body <- paste0(
  '{"model":"jev-1.13.0","answers":{"urgent":{"type":"noul",',
  '"noul":0.5}},"usage":{"input_tokens":1,"output_tokens":1}}'
)
ok_mock <- function(req) {
  httr2::response(
    status_code = 200,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(ok_body)
  )
}

test_that("hard question limits are enforced", {
  opts <- function(n) {
    stats::setNames(as.character(seq_len(n)), paste0("o", seq_len(n)))
  }
  expect_no_error(jev_choice("Pick", opts(255)))
  expect_error(
    jev_choice("Pick", opts(256)),
    class = "jev_input_error"
  )
  expect_no_error(jev_score("Rate", as.character(1:10)))
  expect_no_error(jev_score("Rate", c("a", "b")))
  expect_error(jev_score("Rate", as.character(1:11)), class = "jev_input_error")
  expect_error(jev_score("Rate", "a"), class = "jev_input_error")
})

test_that("small requests do not warn", {
  expect_no_warning(jev_preflight_ask("hello", q_noul()))
  expect_equal(jev_preflight_tokens("abcd"), ceiling(6 / 4))
})

test_that("total request limit warns with a classed warning", {
  expect_warning(
    jev_preflight_ask(
      list(a = big_state(130000), b = big_state(130000)),
      q_noul()
    ),
    class = "jev_preflight_warning"
  )
})

test_that("state plus longest question limit warns", {
  w <- expect_warning(
    jev_preflight_ask(big_state(140000), q_noul()),
    class = "jev_preflight_warning"
  )
  expect_match(conditionMessage(w), "32k")
  expect_false(grepl("64k", conditionMessage(w)))
})

test_that("the longest question, not the sum, counts toward the 32k limit", {
  qs <- list(
    a = jev_noul(strrep("x", 60000)),
    b = jev_noul(strrep("y", 60000))
  )
  expect_no_warning(jev_preflight_ask(big_state(1000), qs))
  expect_warning(
    jev_preflight_ask(big_state(80000), qs),
    class = "jev_preflight_warning"
  )
})

test_that("options(jevr.preflight = FALSE) disables the check", {
  withr::local_options(jevr.preflight = FALSE)
  expect_no_warning(jev_preflight_ask(big_state(300000), q_noul()))
})

test_that("jev_ask warns but still sends oversized requests", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-key")
  sent <- FALSE
  mock <- function(req) {
    sent <<- TRUE
    ok_mock(req)
  }
  expect_warning(
    httr2::with_mocked_responses(
      mock, jev_ask(big_state(140000), q_noul())
    ),
    class = "jev_preflight_warning"
  )
  expect_true(sent)
})

test_that("jev_map aggregates one warning listing affected state ids", {
  withr::local_envvar(TYPESAFE_API_KEY = "fake-key")
  states <- list(
    small = "hi", big1 = big_state(140000), big2 = big_state(140000)
  )
  warnings <- list()
  withCallingHandlers(
    httr2::with_mocked_responses(
      ok_mock, jev_map(states, q_noul(), progress = FALSE)
    ),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "jev_preflight_warning")
  expect_equal(warnings[[1L]]$state_ids, c("big1", "big2"))
  expect_match(conditionMessage(warnings[[1L]]), "big1, big2")

  withr::local_options(jevr.preflight = FALSE)
  expect_no_warning(
    httr2::with_mocked_responses(
      ok_mock, jev_map(states, q_noul(), progress = FALSE)
    )
  )
})
