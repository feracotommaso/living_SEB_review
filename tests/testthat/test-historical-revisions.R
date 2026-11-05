withr::local_dir(here::here())
source("R/coding_compatibility.R")

test_that("exact revisions preserve the frozen baseline and reject further changes", {
  old <- data.frame(paper_id = c("0100", "0101"), matrix_id = c("0100a", "0101a"),
    n = c(100, 200), selfmanagement_innovation = c(.2, .3))
  revised <- old; revised$selfmanagement_innovation[1] <- .25
  baseline <- coding_numeric_contract(old)
  target <- coding_numeric_contract(revised)
  path <- tempfile(fileext = ".csv"); readr::write_csv(baseline, path)
  revision <- data.frame(revision_id = "test-0100a", revision_date = "2026-11-05",
    paper_id = "0100", matrix_id = "0100a", previous_n = "100",
    previous_n_effects = "1", previous_effects_sha256 = baseline$effects_sha256[1],
    n = "100", n_effects = "1", effects_sha256 = target$effects_sha256[1],
    reason = "Confirmed correction", notes = "Documented correction")
  expected <- coding_historical_contract(path, revision)
  expect_identical(readLines(path), c(paste(names(baseline), collapse = ","),
    paste(unlist(baseline[1, ]), collapse = ","), paste(unlist(baseline[2, ]), collapse = ",")))
  expect_equal(expected, target)
  expect_silent(coding_validate_historical(revised, expected))
  expect_error(coding_validate_historical(old, expected), "0100a")
  changed <- revised; changed$selfmanagement_innovation[1] <- .26
  expect_error(coding_validate_historical(changed, expected), "0100a")
  changed <- revised; changed$selfmanagement_innovation[2] <- .4
  expect_error(coding_validate_historical(changed, expected), "0101a")
  changed <- revised; changed$n[1] <- 101
  expect_error(coding_validate_historical(changed, expected), "0100a")
  expect_error(coding_validate_historical(revised[-1, ], expected), "Missing historical")
  invalid <- revision; invalid$previous_effects_sha256 <- target$effects_sha256[1]
  expect_error(coding_historical_contract(path, invalid), "previous contract")
  expect_error(coding_historical_contract(path, rbind(revision, revision)), "previous contract")
  old$notes <- NA_character_
  notes <- coding_apply_historical_revisions(old, revision)
  expect_identical(notes$notes, c("Documented correction", NA_character_))
})

test_that("0132 is the sole registered numerical revision and its N caveat reaches outputs", {
  revisions <- coding_read_historical_revisions()
  expect_identical(revisions$matrix_id, "0132a")
  baseline <- coding_historical_contract(revisions = revisions[FALSE, ])
  expected <- coding_historical_contract()
  other <- expected$matrix_id != "0132a"
  expect_identical(expected[other, ], baseline[other, ])
  expect_identical(expected$n, baseline$n)
  metadata <- as.data.frame(readxl::read_excel("data/3.meta_data/meta_data/metadata.xlsx"))
  expect_silent(coding_validate_historical(metadata))
  row <- metadata[metadata$matrix_id == "0132a", ]
  expect_match(row$notes, "N=315")
  expect_match(row$notes, "one N per matrix")
  review <- readxl::read_excel("data/3.meta_data/review_data/review_data.xlsx")
  expect_identical(review$notes[review$paper_id == "0132"], row$notes)
  invalid <- revisions; invalid$effects_sha256 <- "not-a-hash"
  path <- tempfile(fileext = ".csv"); readr::write_csv(invalid, path)
  expect_error(coding_read_historical_revisions(path), "signature")
})
