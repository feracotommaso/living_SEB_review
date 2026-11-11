source(here::here("R", "title_abstract_screening.R"))

policy <- screening_load_policy()

test_that("stable IDs are deterministic and DOI-based", {
  x <- data.frame(
    title = c("Example A", "Example B"),
    doi = c("https://doi.org/10.1000/ABC", "10.1000/xyz"),
    author = c("A", "B"),
    year = c("2026", "2026"),
    stringsAsFactors = FALSE
  )

  first <- screening_make_record_ids(x)
  second <- screening_make_record_ids(x)
  expect_identical(first, second)
  expect_length(unique(first), 2L)
})

test_that("operational policy is conservative", {
  decisions <- data.frame(
    screening_record_id = paste0("id", 1:4),
    ai_decision = c("include", "uncertain", "exclude", "exclude"),
    ai_exclusion_reason = c("", "", "BOOK", "OUT_OF_SCOPE"),
    ai_confidence = c("high", "low", "high", "medium"),
    ai_rationale = rep("rationale", 4),
    ai_evidence = c("", "", "document type: book", "related inference"),
    policy_version = rep(policy$version, 4),
    prompt_version = rep(policy$prompt_version, 4),
    model = rep("test-model", 4),
    screened_at_utc = rep("2026-11-03T10:00:00Z", 4),
    stringsAsFactors = FALSE
  )

  result <- screening_apply_operational_policy(decisions, policy)
  expect_identical(
    result$automatic_action,
    c("KEEP", "KEEP_UNCERTAIN", "EXCLUDE", "KEEP_UNCERTAIN")
  )
})

test_that("decision validation requires exact coverage and exclusion evidence", {
  input <- data.frame(
    screening_record_id = c("id1", "id2"),
    title = c("A", "B"),
    stringsAsFactors = FALSE
  )

  decisions <- data.frame(
    screening_record_id = c("id1", "id2"),
    ai_decision = c("include", "exclude"),
    ai_exclusion_reason = c("", "BOOK"),
    ai_confidence = c("high", "high"),
    ai_rationale = c("relevant", "book"),
    ai_evidence = c("title", "document type: book"),
    policy_version = rep(policy$version, 2),
    prompt_version = rep(policy$prompt_version, 2),
    model = rep("test-model", 2),
    screened_at_utc = rep("2026-11-03T10:00:00Z", 2),
    stringsAsFactors = FALSE
  )

  expect_silent(screening_validate_decisions(decisions, input, policy))

  decisions$ai_evidence[2] <- ""
  expect_error(
    screening_validate_decisions(decisions, input, policy),
    "requires explicit evidence"
  )
})
