source(here::here('R/dedup_helpers.R'))

test_that('DOI formatting does not create distinct publications', {
  expect_equal(normalise_doi(c('10.1/example\\_32', '10.1/example; 10.1/example')),
    c('10.1/example_32', '10.1/example'))
  expect_equal(normalise_doi('10.1/one;10.1/two'), '10.1/one;10.1/two')
})

test_that('reviewed reconciliation preserves sources and rejects stale or unapproved edits', {
  root <- tempfile(); dir.create(root)
  registry <- file.path(root, 'registry.csv')
  file.copy(here::here('data/0.registry/records_master_full_through_2026-11-external-01.csv'), registry)
  edits <- readr::read_csv(here::here('data/0.deduplication/bootstrap/registry_corrections.csv'), show_col_types = FALSE)
  # The temporary fixture may have different Git checkout line endings.
  edits$registry_md5 <- md5_file(registry)
  corrections <- file.path(root, 'corrections.csv')
  readr::write_csv(edits, corrections)
  expect_error(reconcile_registry(registry, corrections, '2026-11-10', ''), 'not confirmed')
  x <- reconcile_registry(registry, corrections, '2026-11-10', 'RECONCILE 2026-11-10')
  expect_equal(nrow(x), 385)
  expect_equal(x$record_ids[x$duplicate_id == '36'], '36, 154')
  expect_equal(x$record_ids[x$duplicate_id == '451'], '451, 513')
  expect_equal(x$cite_source[x$duplicate_id == '451'], 'Scopus, Web of Science')
  expect_error(reconcile_registry(registry, corrections, '2026-11-10', 'RECONCILE 2026-11-10'), 'changed since')
})
