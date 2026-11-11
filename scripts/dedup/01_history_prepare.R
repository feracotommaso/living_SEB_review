# Bootstrap step 1: import and automatically deduplicate all historical searches.
# Historical input must stop at history_through and exclude the current batch.
# Scopus BibTeX preambles are normalised automatically in a separate directory.

suppressPackageStartupMessages({
  library(CiteSource)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(normalised_history_dir, recursive = TRUE, showWarnings = FALSE)

files <- list_bibliography_files(history_dir)
manifest_file <- file.path(
  bootstrap_dir,
  "history_file_manifest.csv"
)

if (!file.exists(manifest_file)) {
  manifest <- make_file_manifest(files)
  readr::write_csv(manifest, manifest_file)
  message("Created manifest: ", manifest_file)
} else {
  manifest <- readr::read_csv(
    manifest_file,
    show_col_types = FALSE
  )
}

validate_file_manifest(manifest)
validate_manifest_file_set(manifest, files)
validate_history_period(
  manifest,
  history_through = history_through,
  current_batch = batch_id
)

import_manifest <- prepare_import_manifest(
  manifest,
  normalised_dir = normalised_history_dir
)

imported <- import_citations_from_manifest(import_manifest)
raw_history <- imported$data

write_import_audit(
  imported$audit,
  file.path(bootstrap_dir, "history_import_manifest.csv")
)

saveRDS(
  raw_history,
  file.path(bootstrap_dir, "history_raw_records.rds")
)

history_result <- CiteSource::dedup_citations(
  raw_history,
  manual = TRUE
)

saveRDS(
  history_result,
  file.path(bootstrap_dir, "history_dedup_result.rds")
)

CiteSource::export_csv(
  history_result$unique,
  filename = file.path(
    bootstrap_dir,
    "history_unique_automatic.csv"
  ),
  fields = "full",
  separate = NULL,
  manual_dedup_complete = FALSE
)

candidate_file <- file.path(
  bootstrap_dir,
  "history_candidate_pairs.csv"
)
review_workbook <- file.path(
  bootstrap_dir,
  "history_candidate_pairs_review.xlsx"
)

if (nrow(history_result$manual_dedup) > 0L) {
  CiteSource::export_dedup_candidates(
    history_result$manual_dedup,
    candidate_file
  )
  write_review_workbook(
    history_result$manual_dedup,
    review_workbook,
    title = paste0(
      "Living SEB historical duplicate review through ",
      history_through
    ),
    overwrite = FALSE
  )
} else {
  unlink(c(candidate_file, review_workbook), force = TRUE)
}

readr::write_csv(
  CiteSource::dedup_log(history_result),
  file.path(bootstrap_dir, "history_auto_dedup_log.csv")
)

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  history_through = history_through,
  source_files = nrow(manifest),
  normalised_scopus_files = sum(
    imported$audit$normalisation != "none"
  ),
  removed_scopus_preamble_lines = sum(
    imported$audit$removed_preamble_lines
  ),
  raw_records = nrow(raw_history),
  unique_after_automatic_dedup = nrow(history_result$unique),
  automatic_duplicate_pairs = nrow(history_result$auto_pairs),
  manual_candidate_pairs = nrow(history_result$manual_dedup)
)

readr::write_csv(
  summary_data,
  file.path(bootstrap_dir, "history_prepare_summary.csv")
)

write_session_snapshot(
  file.path(bootstrap_dir, "history_session_info.txt")
)

print(summary_data)

if (nrow(history_result$manual_dedup) > 0L) {
  message("Next: open and review ", review_workbook)
  message("Use the dropdown in the result column: 'match' or 'no_match'.")
  message("Then run 02_history_finalize.R.")
} else {
  message("No manual candidate pairs were generated.")
  message("Run 02_history_finalize.R.")
}
