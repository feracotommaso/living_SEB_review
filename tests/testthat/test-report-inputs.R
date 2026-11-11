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

test_that('acquisition history reconciles article counts and records external intake', {
  x <- report_search_history()
  totals <- colSums(x[-1])
  expect_equal(unname(totals[c('identified', 'duplicates_removed', 'screened', 'full_text_assessed', 'included', 'meta')]),
    c(614, 229, 385, 149, 51, 31))
  expect_equal(x$external[x$batch_id == '2026-11-external-01'], 1)
  expect_true(all(x$identified == x$duplicates_removed + x$screened))
  expect_true(all(x$screened == x$abstract_excluded + x$full_text_assessed))
  expect_true(all(x$full_text_assessed == x$not_in_current_datasets + x$included))
})

test_that('a future completed batch extends the history without editing its code', {
  root <- copy_report_fixture()
  read <- function(p) as.data.frame(readr::read_csv(p, col_types = readr::cols(.default = 'c')))
  save <- function(x, p) { dest <- file.path(root,p); dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE); readr::write_csv(x,dest,na='') }
  p <- 'data/0.registry/records_master_full.csv'
  registry <- read(file.path(root,p)); extra <- registry[1, ]
  extra$duplicate_id <- extra$record_ids <- extra$record_id <- 'synthetic_new'
  extra$doi <- '10.9999/synthetic-history-test'; extra$title <- 'Synthetic future abstract'
  extra$cite_string <- '2026-11'
  save(rbind(registry,extra),p)
  save(rbind(registry,extra),'data/0.registry/records_master_full_through_2026-11.csv')
  p <- 'data/1.abstracting/2026-10/title_abstract/2026-10_title_abstract_decisions.csv'
  ta <- read(file.path(root,p)); row <- ta[ta$final_screening_decision == 'exclude', ][1, ]
  row$title <- extra$title; row$doi <- extra$doi
  row$screening_record_id <- 'synthetic_new'; row$screening_batch <- '2026-11'
  save(row,'data/1.abstracting/2026-11/title_abstract/2026-11_title_abstract_decisions.csv')
  imp <- read('data/0.deduplication/2026-10/search_import_manifest.csv')[1, ]
  imp$source_path <- 'synthetic/new.bib'; imp$batch_id <- '2026-11'
  imp$database <- 'Scopus'; imp$imported_records <- '1'
  save(imp,'data/0.deduplication/2026-11/search_import_manifest.csv')
  results <- list(metadata = read('data/3.meta_data/meta_data/metadata.csv'), review = read('data/3.meta_data/review_data/review_data.csv'))
  x <- report_search_history(root, results)
  new <- x[x$batch_id == '2026-11', ]
  expect_equal(new$identified, 1)
  expect_equal(new$abstract_excluded, 1)
  expect_equal(sum(x$screened), 386)
  expect_equal(sum(x$included), 51)
})
