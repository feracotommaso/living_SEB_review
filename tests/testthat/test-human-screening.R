withr::local_dir(here::here())
source("workflow.R")

copy_screen_fixture <- function(batch) {
  root <- tempfile("screening_workflow_"); dir.create(root)
  p <- wf_screen_paths(batch)
  paths <- unlist(p[setdiff(names(p), "book")], use.names = FALSE)
  paths <- c(paths[file.exists(paths)], "config/screening/title_abstract.yml",
    list.files("schemas", full.names = TRUE), "config/screening/full_text_eligibility.yml")
  for (path in paths) {
    target <- file.path(root, path); dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(path, target))
  }
  root
}

test_that("human abstract corrections preserve AI proposals and conservative retention", {
  batch <- "2026-11-external-01"; root <- copy_screen_fixture(batch)
  withr::local_dir(root)
  p <- wf_screen_paths(batch); wf_export_screening(batch, force = TRUE)
  original <- wf_read(p$ta)
  wb <- openxlsx::loadWorkbook(p$book); tab <- wf_read_book(p$book, "abstract")
  openxlsx::writeData(wb, "abstract", "uncertain", startRow = 2, startCol = match("human_decision", names(tab)), colNames = FALSE)
  openxlsx::writeData(wb, "abstract", "Synthetic human correction for testing", startRow = 2, startCol = match("human_rationale", names(tab)), colNames = FALSE)
  openxlsx::saveWorkbook(wb, p$book, overwrite = TRUE)
  expect_error(wf_export_screening(batch), "Modifiche Excel")
  wf_import_screening(batch)
  result <- wf_read(p$ta)
  expect_identical(result$ai_decision, original$ai_decision)
  expect_identical(result$human_decision, "uncertain")
  expect_identical(result$automatic_action, "KEEP_UNCERTAIN")
  expect_equal(nrow(wf_read(p$included)), 1L)
  expect_false(wf_book_has_edits(p$book))
})

test_that("resolved full text can be reopened and adjudicated through the common workbook", {
  batch <- "2026-11-external-01"; root <- copy_screen_fixture(batch)
  withr::local_dir(root)
  p <- wf_screen_paths(batch); wf_export_screening(batch, force = TRUE)
  before <- wf_read(p$ft)
  wf_reopen_full_text(batch, "0150", "Synthetic reassessment test")
  human <- wf_read(p$human)
  expect_identical(human$adjudication_status, "OPEN")
  expect_identical(human$human_meta_status, "eligible")
  wb <- openxlsx::loadWorkbook(p$book)
  for (pair in list(c("adjudication_status", "RESOLVED"), c("adjudicated_by", "Synthetic tester"),
      c("adjudication_date", "2026-11-06"), c("adjudication_note", "Synthetic verification; prior eligibility confirmed")))
    openxlsx::writeData(wb, "decisioni_full_text", pair[2], startRow = 2, startCol = match(pair[1], names(human)), colNames = FALSE)
  openxlsx::saveWorkbook(wb, p$book, overwrite = TRUE)
  wf_import_screening(batch)
  after <- wf_read(p$ft)
  expect_identical(after$final_route, before$final_route)
  expect_identical(after$review_evidence, before$review_evidence)
  expect_identical(after$meta_evidence, before$meta_evidence)
  expect_identical(after$decision_source, "human_adjudicated")
  expect_identical(wf_read(p$human)$adjudication_status, "RESOLVED")
})

test_that("a batch with no retained articles needs no invented full-text rows", {
  root <- tempfile(); dir.create(root); withr::local_dir(root)
  p <- wf_screen_paths("2026-12")
  dir.create(dirname(p$included), recursive = TRUE)
  readr::write_csv(data.frame(screening_record_id = character()), p$included)
  expect_message(prepara_full_text("2026-12"), "Nessun articolo")
  expect_false(file.exists(p$ft))
})
