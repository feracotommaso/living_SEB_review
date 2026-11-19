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

test_that("only approved historical matrices are revised and N caveats reach outputs", {
  revisions <- coding_read_historical_revisions()
  expect_identical(revisions$matrix_id, c("0132a","0120a","0120b","0034a","0035a","0043a","0045a","0049a","0050a","0050b","0050a","0050a","0068a","0070a","0100a","0107a","0108a","0118a","0126a","0138a","0053e","0068a","0070a","0026a","0049a"))
  baseline <- coding_historical_contract(revisions = revisions[FALSE, ])
  expected <- coding_historical_contract()
  other <- !expected$matrix_id %in% revisions$matrix_id
  expect_identical(expected[other, ], baseline[other, ])
  same_n <- !expected$matrix_id %in% c("0068a","0126a")
  expect_identical(expected$n[same_n], baseline$n[same_n])
  expect_identical(expected$n[expected$matrix_id=="0068a"], "356")
  expect_identical(expected$n[expected$matrix_id=="0126a"], "1917")
  expect_false("0013a"%in%expected$matrix_id)
  restored <- revisions[revisions$revision_id=="2026-11-14-0050a-two-samples",]
  expect_identical(restored$previous_n, "1855")
  expect_identical(restored$n, "940")
  expect_identical(restored$effects_sha256,
    revisions$effects_sha256[match("2026-11-13-0050a", revisions$revision_id)])
  expect_true("0050b"%in%expected$matrix_id)
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
