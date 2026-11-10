withr::local_dir(here::here())
source('R/report_inputs.R')

test_that('report counts reconcile modern batches without counting October twice', {
  x <- report_input_counts()
  expect_equal(x$identified, 614)
  expect_equal(x$external, 1)
  expect_equal(x$registry, 387)
  expect_equal(x$legacy_screened, 352)
  expect_equal(x$screened, 367)
  expect_equal(x$assessed, 150)
  expect_equal(x$screening_gap, 20)
  expect_equal(x$duplicates + x$screening_gap + x$screened, x$identified)
})

test_that('an unresolved full text cannot silently appear as an exclusion in the report', {
  root <- tempfile('report_inputs_'); dir.create(root)
  # Copy only the report's small non-individual input tables, never private data.
  paths <- c(list.files('data/1.abstracting', pattern='_title_abstract_decisions\\.csv$', recursive=TRUE, full.names=TRUE),
    list.files('data/2.full_abstracting', pattern='_full_text_decisions\\.csv$', full.names=TRUE),
    list.files('data/0.deduplication', pattern='(history|search)_import_manifest\\.csv$', recursive=TRUE, full.names=TRUE),
    list.files('data/0.registry', pattern='\\.csv$', full.names=TRUE))
  for (p in paths) {
    dest <- file.path(root,p); dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
    stopifnot(file.copy(p,dest))
  }
  p <- file.path(root,'data/2.full_abstracting/2026-11-external-01_full_text_decisions.csv')
  x <- readr::read_csv(p,col_types=readr::cols(.default='c'))
  x$workflow_status <- 'PENDING_DATA'; readr::write_csv(x,p,na='')
  expect_error(report_input_counts(root), 'unresolved')
})
