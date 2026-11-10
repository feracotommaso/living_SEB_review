# Read-only counts for the report: historical manual exports plus canonical batches.
# Never count both a legacy workbook and its replacement CSV.
report_input_counts <- function(root = here::here()) {
  path <- function(...) file.path(root, ...)
  csv <- function(p) as.data.frame(readr::read_csv(p, col_types = readr::cols(.default = 'c'), na = ''))
  keys <- function(x) {
    doi <- tolower(trimws(x$doi)); doi[is.na(doi)] <- ''
    doi <- sub('^https?://(dx\\.)?doi\\.org/', '', doi)
    # Scopus may append a translated title in square brackets; Rayyan omits it.
    title <- sub(';[[:space:]]*\\[.*\\]$', '', x$title)
    title <- tolower(gsub('[[:space:]]+', ' ', trimws(title)))
    title[is.na(title)] <- ''
    data.frame(doi = doi, title = title)
  }
  ta_paths <- list.files(path('data/1.abstracting'), pattern = '_title_abstract_decisions\\.csv$', recursive = TRUE, full.names = TRUE)
  if (!length(ta_paths)) stop('No canonical screening batches found.')
  ta <- dplyr::bind_rows(lapply(ta_paths, csv))
  if (anyDuplicated(ta$screening_record_id)) stop('Duplicate screening IDs across batches.')
  batches <- unique(ta$screening_batch)
  for (batch in batches) if (!file.exists(path('data/0.registry', paste0('records_master_full_through_', batch, '.csv'))))
    stop('Report batch has not been promoted: ', batch)
  manifests <- c(path('data/0.deduplication/bootstrap/history_import_manifest.csv'),
    vapply(batches, function(b) path('data/0.deduplication', b, 'search_import_manifest.csv'), character(1)))
  imports <- dplyr::bind_rows(lapply(manifests, csv))
  if (anyDuplicated(imports$source_path)) stop('Bibliographic input counted twice.')
  n <- as.integer(imports$imported_records)
  stopifnot(!anyNA(n))
  ft_paths <- vapply(batches, function(b) path('data/2.full_abstracting', paste0(b, '_full_text_decisions.csv')), character(1))
  ft <- dplyr::bind_rows(lapply(ft_paths[file.exists(ft_paths)], csv))
  if (anyDuplicated(ft$screening_record_id) || any(ft$workflow_status != 'RESOLVED')) stop('Full-text decisions duplicated or unresolved.')
  kept <- ta$screening_record_id[ta$final_screening_decision != 'exclude']
  if (!setequal(kept, ft$screening_record_id)) stop('Retained abstracts and resolved full texts do not match.')
  historical_count <- function(folder, pattern, modern) {
    files <- list.files(path(folder), pattern = pattern, full.names = TRUE)
    sum(vapply(files, function(p) {
      x <- as.data.frame(readxl::read_excel(p))
      a <- keys(x); b <- keys(modern)
      covered <- (nzchar(a$doi) & a$doi %in% b$doi) | (nzchar(a$title) & a$title %in% b$title)
      sum(!covered)
    }, numeric(1)))
  }
  legacy_ta <- historical_count('data/1.abstracting', '_deduplicated_processed\\.xlsx$', ta)
  legacy_ft <- historical_count('data/2.full_abstracting', '_fullabstracting\\.xlsx$', ft)
  registry_n <- nrow(csv(path('data/0.registry/records_master_full.csv')))
  historical_registry_n <- as.integer(csv(path('data/0.deduplication/bootstrap/history_finalize_summary.csv'))$final_unique_records)
  if (registry_n != historical_registry_n + nrow(ta)) stop('Registry includes batches without completed abstract screening.')
  screened <- legacy_ta + nrow(ta)
  assessed <- legacy_ft + nrow(ft)
  if (registry_n < screened || screened < assessed || sum(n) < registry_n) stop('Inconsistent report counts.')
  list(scopus = sum(n[imports$database == 'Scopus']), wos = sum(n[imports$database == 'Web of Science']),
    external = sum(n[grepl('^External:', imports$database)]), identified = sum(n),
    registry = registry_n, duplicates = sum(n) - registry_n,
    screened = screened, assessed = assessed, screening_gap = registry_n - screened,
    legacy_screened = legacy_ta, legacy_assessed = legacy_ft, batches = batches)
}
