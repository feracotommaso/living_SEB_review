withr::local_dir(here::here())
source("workflow.R")

copy_workflow_fixture <- function() {
  root <- tempfile("human_workflow_"); dir.create(root)
  files <- c(list.files("data/3.meta_data/coding", pattern = "^(records|sources|evidence)\\.csv$", full.names = TRUE),
    "config/coding/legacy_31_fields.csv", "data/topics_codebook.csv")
  for (path in files) {
    target <- file.path(root, path); dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(path, target))
  }
  # These synthetic edit tests neither require nor copy private originals.
  sources <- wf_read(file.path(root, wf_coding_dir, "sources.csv"))
  readr::write_csv(sources[FALSE, ], file.path(root, wf_coding_dir, "sources.csv"))
  root
}

test_that("human coding edits are imported by ID and unsaved changes are preserved", {
  root <- copy_workflow_fixture(); withr::local_dir(root)
  wf_export_coding(force = TRUE)
  before <- wf_read(wf_records_path)
  expect_false(wf_book_has_edits(wf_coding_path))
  expect_equal(nrow(wf_import_coding(save = FALSE)$changes), 0L)
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  papers <- wf_read_book(wf_coding_path, "articoli")
  row <- match("0066", papers$paper_id) + 1L
  openxlsx::writeData(wb, "articoli", "skillassessment; nomologicalnet", startRow = row,
    startCol = match("review_topics", names(papers)), colNames = FALSE)
  openxlsx::writeData(wb, "articoli", "Synthetic correction test", startRow = row,
    startCol = match("change_note", names(papers)), colNames = FALSE)
  openxlsx::writeData(wb, "articoli", as.Date("2026-11-09"), startRow = row,
    startCol = match("download_date", names(papers)), colNames = FALSE)
  openxlsx::saveWorkbook(wb, wf_coding_path, overwrite = TRUE)
  expect_true(wf_book_has_edits(wf_coding_path))
  expect_error(wf_export_coding(), "modifiche")
  file_before <- wf_hash(wf_coding_path)
  expect_identical(wf_hash(wf_coding_path), file_before)
  result <- wf_import_coding()
  after <- wf_read(wf_records_path)
  expect_identical(after$review_topics[after$paper_id == "0066"], "skillassessment; nomologicalnet")
  expect_identical(after$download_date[after$paper_id == "0066"], "2026-11-09")
  expect_equal(before[before$paper_id != "0066", ], after[after$paper_id != "0066", ])
  expect_false(wf_book_has_edits(wf_coding_path))
  expect_equal(nrow(wf_import_coding(save = FALSE)$changes), 0L)
})

test_that("invalid edits and stale Excel cannot overwrite canonical coding", {
  root <- copy_workflow_fixture(); withr::local_dir(root)
  wf_export_coding(force = TRUE)
  before <- wf_hash(wf_records_path)
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  samples <- wf_read_book(wf_coding_path, "campioni")
  row <- match("0150a", samples$matrix_id) + 1L
  openxlsx::writeData(wb, "campioni", -3, startRow = row, startCol = match("n", names(samples)), colNames = FALSE)
  openxlsx::saveWorkbook(wb, wf_coding_path, overwrite = TRUE)
  expect_error(wf_import_coding(), "intero positivo")
  expect_identical(wf_hash(wf_records_path), before)
  wf_export_coding(force = TRUE)
  x <- wf_read(wf_records_path); x$notes[1] <- "Synthetic concurrent change"
  readr::write_csv(x, wf_records_path, na = "")
  expect_error(wf_import_coding(), "tabelle precedenti")
})

test_that("simplified preview preserves every current scientific value", {
  result <- wf_build_results()
  expect_equal(nrow(result$metadata), 39L)
  expect_equal(nrow(result$review), 51L)
  expect_equal(nrow(wf_diff(wf_read("data/3.meta_data/meta_data/metadata.csv"), result$metadata, "matrix_id", "metadata")), 0L)
  expect_equal(nrow(wf_diff(wf_read("data/3.meta_data/review_data/review_data.csv"), result$review, "paper_id", "review")), 0L)
  expect_equal(result$metadata$n[result$metadata$matrix_id == "0150a"], 30719)
  expect_equal(result$metadata$n[result$metadata$matrix_id == "0132a"], 598)
})

test_that("comparison detects changed, removed and newly missing correlations", {
  old <- data.frame(matrix_id = c("a", "b"), n = c(10,20), x_y = c(.2,.4), x_z = c(NA,.3))
  new <- old; new$x_y[1] <- NA; new$n[2] <- 19; new$x_z[1] <- .5
  d <- wf_diff(old, new, "matrix_id", "metadata")
  expect_equal(nrow(d), 3L)
  expect_setequal(d$field, c("x_y", "x_z", "n"))
  expect_equal(sum(wf_diff(old, new[1, ], "matrix_id", "metadata")$field == ".record"), 1L)
})

test_that("review membership preserves history but respects explicit reassessment", {
  records <- wf_read(wf_records_path)
  expect_true("0107" %in% wf_current(records)$review$paper_id)
  records$final_decision[records$paper_id == "0107"] <- "exclude"
  expect_false("0107" %in% wf_current(records)$review$paper_id)
  result <- wf_build_results(records)
  expect_false("0107" %in% result$review$paper_id)
  expect_false("0107" %in% result$metadata$paper_id)
  records$final_decision[records$paper_id == "0107"] <- "meta"
  records$batch_id[records$paper_id == "0107"] <- "2026-11"
  expect_false("0107" %in% wf_current(records)$review$paper_id)
  records$final_decision[records$paper_id == "0107"] <- "both"
  expect_true("0107" %in% wf_current(records)$review$paper_id)
})

test_that("adding a source to an initially empty sheet is an unsaved edit", {
  root <- copy_workflow_fixture(); withr::local_dir(root)
  wf_export_coding(force = TRUE)
  expect_false(wf_book_has_edits(wf_coding_path))
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  openxlsx::writeData(wb, "fonti", "0150", startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, wf_coding_path, overwrite = TRUE)
  expect_true(wf_book_has_edits(wf_coding_path))
  expect_error(wf_export_coding(), "modifiche")
})
