withr::local_dir(here::here())
source("R/dedup_helpers.R")
source("R/external_intake.R")

test_that("external BibTeX retains complete records including abstracts and raw bytes", {
  path <- tempfile(fileext = ".bib")
  writeLines(c('@article{a,title={First skills paper},author={Doe, Jane},journal={Skills Journal},year={2026},doi={10.1000/a},abstract={First complete abstract.}}',
    '@article{b,title={Second skills paper},author={Roe, John},journal={Skills Journal},year={2025},doi={10.1000/b},abstract={Second complete abstract.}}'), path)
  hash <- digest::digest(path, algo = "sha256", file = TRUE)
  imported <- read_one_citation_file(path, path, "External:author_suggestion", "2026-11-external-01")
  expect_identical(imported$abstract, c("First complete abstract.", "Second complete abstract."))
  expect_true(all(imported$cite_source == "External:author_suggestion"))
  expect_true(all(imported$cite_label == "search"))
  expect_identical(imported$doi, c("10.1000/a", "10.1000/b"))
  expect_identical(imported$year, c("2026", "2025"))
  expect_true(all(imported$type == "Article"))
  expect_identical(digest::digest(path, algo = "sha256", file = TRUE), hash)
  book <- tempfile(fileext = ".bib")
  writeLines('@book{b,title={Skills book},author={Doe, Jane},publisher={Publisher},year={2025}}', book)
  expect_identical(external_read_bibtex(book, "External:manual_search", "2026-11-external-01")$type, "Book")
})

test_that("external manifests separate receipt from publication and reject changed originals", {
  batch <- "2026-11-external-01"; root <- tempfile(); dir.create(root)
  dir.create(file.path(root, "config", "external"), recursive = TRUE)
  file.copy("config/external_source_types.csv", file.path(root, "config", "external_source_types.csv"))
  relative <- paste0("data/0.bib_download/external/", batch, "/2026-11-05_author_suggestion_01.bib")
  path <- file.path(root, relative); dir.create(dirname(path), recursive = TRUE)
  writeLines('@article{old,title={Older skills paper},author={Doe, Jane},year={2017}}', path)
  manifest <- data.frame(path = relative, source_type = "author_suggestion", received_date = "2026-11-05",
    download_date = "2026-11-04", source_detail = "Author referral; publisher export", source_url = "https://example.org/paper",
    original_filename = "publisher.bib", sha256 = digest::digest(path, algo = "sha256", file = TRUE))
  ledger <- file.path(root, "config", "external", paste0(batch, ".csv"))
  readr::write_csv(manifest, ledger)
  parsed <- external_manifest(batch, root)
  expect_identical(parsed$batch_id, batch)
  expect_identical(parsed$database, "External:author_suggestion")
  expect_silent(validate_file_manifest(parsed, root, expected_batch = batch))
  invalid <- manifest; invalid$source_type <- "invented_source"; readr::write_csv(invalid, ledger)
  expect_error(external_manifest(batch, root), "provenance")
  invalid <- manifest; invalid$received_date <- "2017-01-05"; readr::write_csv(invalid, ledger)
  expect_error(external_manifest(batch, root), "receipt month")
  invalid <- manifest; invalid$path <- paste0(dirname(relative), "/../elsewhere.bib"); readr::write_csv(invalid, ledger)
  expect_error(external_manifest(batch, root), "outside")
  readr::write_csv(manifest, ledger); cat("changed", file = path, append = TRUE)
  expect_error(external_manifest(batch, root), "missing or changed")
  expect_error(external_manifest("2026-11", root), "batch ID")
})

test_that("a later database sighting deduplicates while retaining external provenance", {
  path <- tempfile(fileext = ".bib")
  writeLines('@article{a,title={The same SEB article},author={Doe, Jane},journal={Skills Journal},year={2026},doi={10.1000/shared}}', path)
  external <- read_one_citation_file(path, path, "External:manual_search", "2026-11-external-01")
  database <- external; database$cite_source <- "Scopus"; database$cite_string <- "2026-12"
  external$record_id <- 1L; database$record_id <- 2L
  existing <- suppressWarnings(CiteSource::dedup_citations(external, manual = TRUE))$unique
  result <- suppressWarnings(CiteSource::dedup_citations_add_sources(existing, database, manual = TRUE))
  expect_equal(nrow(result$unique), 1L)
  expect_equal(nrow(result$manual_dedup), 0L)
  expect_match(result$unique$cite_source, "External:manual_search", fixed = TRUE)
  expect_match(result$unique$cite_source, "Scopus", fixed = TRUE)
})
