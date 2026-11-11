withr::local_dir(here::here())
source('R/report_inputs.R')

test_that('report counts reconcile modern batches without counting October twice', {
  x <- report_input_counts()
  expect_equal(x$identified, 614)
  expect_equal(x$external, 1)
  expect_equal(x$registry, 385)
  expect_equal(x$legacy_screened, 370)
  expect_equal(x$screened, 385)
  expect_equal(x$assessed, 149)
  expect_equal(x$screening_gap, 0)
  expect_equal(x$duplicates + x$screening_gap + x$screened, x$identified)
})

copy_report_fixture <- function() {
  root <- tempfile('report_inputs_'); dir.create(root)
  # Copy only the report's small non-individual input tables, never private data.
  paths <- c('R/dedup_helpers.R',
    list.files('data/1.abstracting', pattern='_deduplicated_processed\\.xls[xm]$', full.names=TRUE),
    list.files('data/2.full_abstracting', pattern='_fullabstracting\\.xls[xm]$', full.names=TRUE),
    list.files('data/1.abstracting', pattern='_title_abstract_decisions\\.csv$', recursive=TRUE, full.names=TRUE),
    list.files('data/2.full_abstracting', pattern='_full_text_decisions\\.csv$', full.names=TRUE),
    list.files('data/0.deduplication', pattern='(history|search)_import_manifest\\.csv$', recursive=TRUE, full.names=TRUE),
    list.files('data/0.registry', pattern='\\.csv$', full.names=TRUE))
  for (p in paths) {
    dest <- file.path(root,p); dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
    stopifnot(file.copy(p,dest))
  }
  root
}

test_that('an unresolved full text cannot silently appear as an exclusion in the report', {
  root <- copy_report_fixture()
  p <- file.path(root,'data/2.full_abstracting/2026-11-external-01_full_text_decisions.csv')
  x <- readr::read_csv(p,col_types=readr::cols(.default='c'))
  x$workflow_status <- 'PENDING_DATA'; readr::write_csv(x,p,na='')
  expect_error(report_input_counts(root), 'unresolved')
})

test_that('duplicate exports count once and an omitted historical XLSM blocks the report', {
  root <- copy_report_fixture()
  file.copy(file.path(root,'data/1.abstracting/25_01_31_deduplicated_processed.xlsx'),
    file.path(root,'data/1.abstracting/copy_deduplicated_processed.xlsx'))
  expect_equal(report_input_counts(root)$screened, 385)
  unlink(file.path(root,'data/1.abstracting/26_05_01_deduplicated_processed.xlsm'))
  expect_error(report_input_counts(root), 'do not reconcile')
})

test_that('a shared title cannot override conflicting DOI evidence', {
  source('R/dedup_helpers.R', local = TRUE)
  x <- data.frame(title = 'Same title', doi = '10.1/a')
  registry <- data.frame(title = 'Same title', doi = '10.1/b', duplicate_id = '1')
  expect_error(report_registry_ids(x, registry, normalise_doi), 'Conflicting DOIs')
  x$doi <- ''
  expect_error(report_registry_ids(x, rbind(registry, registry), normalise_doi), 'uniquely')
})
