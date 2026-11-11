# Read-only counts for the report: historical manual exports plus canonical batches.
# Count publications, not export rows. Original manual workbooks remain untouched.
report_title_key <- function(x) {
  x[is.na(x)] <- ''
  # A few Rayyan exports contain UTF-8 text decoded as Windows-1252.
  bad <- grepl('\u00e2|\u00c3|\u00cd\u00b3', x)
  for (i in which(bad)) {
    fixed <- iconv(x[i], from = 'UTF-8', to = 'CP1252', sub = NA)
    if (!is.na(fixed)) {
      Encoding(fixed) <- 'UTF-8'
      if (validUTF8(fixed)) x[i] <- fixed
    }
  }
  # Scopus can append a translated title, with or without square brackets.
  x <- sub(';[[:space:]].*$', '', x)
  gsub('[^a-z0-9]', '', tolower(stringi::stri_trans_general(x, 'Latin-ASCII')))
}

report_registry_ids <- function(x, registry, normalise_doi) {
  # Historical coding also stores publisher resolver URLs (e.g. psycnet/doi/).
  key <- function(z) normalise_doi(sub('^https?://[^/]+/doi/(abs/|full/)?', '', z, ignore.case = TRUE))
  doi <- key(x$doi); rd <- key(registry$doi)
  title <- report_title_key(x$title); rt <- report_title_key(registry$title)
  vapply(seq_len(nrow(x)), function(i) {
    hits <- which(nzchar(doi[i]) & rd == doi[i])
    if (!length(hits)) hits <- which(nzchar(title[i]) & rt == title[i])
    # A historical Rayyan title ends in an explicit ellipsis. Allow only a
    # long exact prefix resolving to one registry entry, never fuzzy matching.
    if (!length(hits) && isTRUE(grepl('\\.\\.\\.$', trimws(x$title[i]))) && nchar(title[i]) >= 80)
      hits <- which(startsWith(rt, title[i]))
    if (length(hits) != 1L) stop('Screening record does not resolve uniquely: ', x$title[i])
    if (nzchar(doi[i]) && nzchar(rd[hits]) && doi[i] != rd[hits])
      stop('Conflicting DOIs for screening record: ', x$title[i])
    registry$duplicate_id[hits]
  }, character(1))
}

report_input_counts <- function(root = here::here(), details = FALSE) {
  source(file.path(root, 'R/dedup_helpers.R'), local = TRUE)
  path <- function(...) file.path(root, ...)
  csv <- function(p) as.data.frame(readr::read_csv(p, col_types = readr::cols(.default = 'c'), na = ''))
  resolve <- function(x, registry) report_registry_ids(x, registry, normalise_doi)
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
  historical <- function(folder, pattern) {
    files <- list.files(path(folder), pattern = pattern, full.names = TRUE)
    dplyr::bind_rows(lapply(files, function(p) {
      x <- as.data.frame(readxl::read_excel(p))
      x$source_file <- basename(p)
      x
    }))
  }
  registry <- csv(path('data/0.registry/records_master_full.csv'))
  if (anyDuplicated(registry$duplicate_id)) stop('Duplicate registry IDs.')
  old_ta <- historical('data/1.abstracting', '_deduplicated_processed\\.xls[xm]$')
  old_ft <- historical('data/2.full_abstracting', '_fullabstracting\\.xls[xm]$')
  modern_ta_ids <- resolve(ta, registry); modern_ft_ids <- resolve(ft, registry)
  legacy_ta_ids <- setdiff(resolve(old_ta, registry), modern_ta_ids)
  legacy_ft_ids <- setdiff(resolve(old_ft, registry), modern_ft_ids)
  ta_ids <- union(legacy_ta_ids, modern_ta_ids)
  ft_ids <- union(legacy_ft_ids, modern_ft_ids)
  if (!setequal(ta_ids, registry$duplicate_id)) stop('Registry and documented abstract screening do not reconcile.')
  if (!all(ft_ids %in% ta_ids)) stop('Full text without abstract screening.')
  legacy_ta <- length(legacy_ta_ids); legacy_ft <- length(legacy_ft_ids)
  registry_n <- nrow(registry)
  screened <- length(ta_ids)
  assessed <- length(ft_ids)
  if (registry_n < screened || screened < assessed || sum(n) < registry_n) stop('Inconsistent report counts.')
  counts <- list(scopus = sum(n[imports$database == 'Scopus']), wos = sum(n[imports$database == 'Web of Science']),
    external = sum(n[grepl('^External:', imports$database)]), identified = sum(n),
    registry = registry_n, duplicates = sum(n) - registry_n,
    screened = screened, assessed = assessed, screening_gap = registry_n - screened,
    legacy_screened = legacy_ta, legacy_assessed = legacy_ft, batches = batches)
  if (details) counts$details <- list(imports = imports, registry = registry, ta_ids = ta_ids, ft_ids = ft_ids)
  counts
}

# One row per acquisition batch. Attribution uses the first recorded source
# occurrence, not the publication date or the date screening was performed.
report_search_history <- function(root = here::here(), results = NULL) {
  x <- report_input_counts(root, details = TRUE)
  d <- x$details
  source(file.path(root, 'R/dedup_helpers.R'), local = TRUE)
  if (is.null(results)) {
    read <- function(p) as.data.frame(readr::read_csv(file.path(root, p), col_types = readr::cols(.default = 'c')))
    results <- list(metadata = read('data/3.meta_data/meta_data/metadata.csv'),
      review = read('data/3.meta_data/review_data/review_data.csv'))
  }
  ids <- function(z) unique(report_registry_ids(z, d$registry, normalise_doi))
  review <- ids(results$review); meta <- ids(results$metadata)
  included <- union(review, meta)
  if (!all(included %in% d$ft_ids)) stop('Included articles without documented full text.')
  first_batch <- vapply(strsplit(d$registry$cite_string, ',[[:space:]]*'), function(b) sort(unique(b))[1L], character(1))
  batches <- sort(unique(d$imports$batch_id))
  if (any(!first_batch %in% batches)) stop('Registry source batch not found in import manifests.')
  history <- lapply(batches, function(batch) {
    imp <- d$imports[d$imports$batch_id == batch, ]
    pub <- d$registry$duplicate_id[first_batch == batch]
    imported <- function(database) sum(as.integer(imp$imported_records[database]))
    assessed <- intersect(pub, d$ft_ids)
    inc <- intersect(pub, included)
    data.frame(batch_id = batch, scopus = imported(imp$database == 'Scopus'),
      wos = imported(imp$database == 'Web of Science'), external = imported(grepl('^External:', imp$database)),
      identified = sum(as.integer(imp$imported_records)),
      duplicates_removed = sum(as.integer(imp$imported_records)) - length(pub),
      screened = length(pub), abstract_excluded = length(setdiff(pub, assessed)),
      full_text_assessed = length(assessed), not_in_current_datasets = length(setdiff(assessed, included)),
      review_only = length(setdiff(intersect(pub, review), meta)), included = length(inc),
      meta = length(intersect(pub, meta)))
  })
  history <- do.call(rbind, history)
  if (any(as.matrix(history[-1]) < 0) || sum(history$identified) != x$identified ||
      sum(history$screened) != x$screened || sum(history$full_text_assessed) != x$assessed)
    stop('Monthly history does not reconcile with the report.')
  history
}

report_history_workbook <- function(history) {
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = 'Arial', fontSize = 10)
  openxlsx::addWorksheet(wb, 'Aggiornamenti', gridLines = FALSE)
  display <- history
  names(display) <- c('Lotto', 'Scopus', 'WoS', 'Esterne', 'Record importati',
    'Duplicati rimossi', 'Abstract valutati', 'Esclusi abstract', 'Full text valutati',
    'Non nei dataset attuali', 'Solo review', 'Inclusi', 'Meta-analisi')
  openxlsx::writeDataTable(wb, 'Aggiornamenti', display, tableName = 'Aggiornamenti', tableStyle = 'TableStyleMedium2')
  openxlsx::setColWidths(wb, 'Aggiornamenti', 1, 26)
  openxlsx::setColWidths(wb, 'Aggiornamenti', 2:ncol(display), 14)
  openxlsx::setRowHeights(wb, 'Aggiornamenti', 1, 45)
  openxlsx::addStyle(wb, 'Aggiornamenti', openxlsx::createStyle(wrapText = TRUE, valign = 'center', halign = 'center'),
    rows = 1, cols = seq_len(ncol(display)), gridExpand = TRUE, stack = TRUE)
  total <- nrow(history) + 2L
  openxlsx::writeData(wb, 'Aggiornamenti', 'Totale', startRow = total, colNames = FALSE)
  openxlsx::writeData(wb, 'Aggiornamenti', t(colSums(history[-1])), startRow = total, startCol = 2,
    colNames = FALSE, rowNames = FALSE)
  openxlsx::addStyle(wb, 'Aggiornamenti', openxlsx::createStyle(textDecoration = 'bold', border = 'top'),
    rows = total, cols = seq_len(ncol(display)), gridExpand = TRUE)
  openxlsx::freezePane(wb, 'Aggiornamenti', firstActiveRow = 2, firstActiveCol = 2)
  wb
}
