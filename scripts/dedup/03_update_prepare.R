# Current update step 1: normalise and import the current search files, then
# compare them with the historical master.

suppressPackageStartupMessages({
  library(CiteSource)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

if (!file.exists(registry_file)) {
  stop(
    "Historical registry not found: ", registry_file,
    "\nRun the two history scripts first.",
    call. = FALSE
  )
}

dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(normalised_update_dir, recursive = TRUE, showWarnings = FALSE)

files <- list_bibliography_files(update_dir)
manifest_file <- file.path(
  batch_dir,
  "search_file_manifest.csv"
)

if (!file.exists(manifest_file)) {
  manifest <- make_file_manifest(files)
  manifest$batch_id <- batch_id
  readr::write_csv(manifest, manifest_file)
  message("Created manifest: ", manifest_file)
} else {
  manifest <- readr::read_csv(
    manifest_file,
    show_col_types = FALSE
  )
}

validate_file_manifest(
  manifest,
  expected_batch = batch_id
)
validate_manifest_file_set(manifest, files)

import_manifest <- prepare_import_manifest(
  manifest,
  normalised_dir = normalised_update_dir
)

imported <- import_citations_from_manifest(import_manifest)
new_raw <- imported$data

write_import_audit(
  imported$audit,
  file.path(batch_dir, "search_import_manifest.csv")
)

master <- CiteSource::reimport_csv(registry_file)
validate_deduplicated_registry(master)

saveRDS(
  new_raw,
  file.path(batch_dir, "new_raw_records.rds")
)

update_result <- CiteSource::dedup_citations_add_sources(
  existing_citations = master,
  new_citations = new_raw,
  manual = TRUE
)

saveRDS(
  update_result,
  file.path(batch_dir, "update_dedup_result.rds")
)

CiteSource::export_csv(
  update_result$unique,
  filename = file.path(
    batch_dir,
    "combined_registry_automatic.csv"
  ),
  fields = "full",
  separate = NULL,
  manual_dedup_complete = FALSE
)

candidate_file <- file.path(
  batch_dir,
  "update_candidate_pairs.csv"
)
review_workbook <- file.path(
  batch_dir,
  "update_candidate_pairs_review.xlsx"
)

if (nrow(update_result$manual_dedup) > 0L) {
  CiteSource::export_dedup_candidates(
    update_result$manual_dedup,
    candidate_file
  )
  write_review_workbook(
    update_result$manual_dedup,
    review_workbook,
    title = paste0(
      "Living SEB duplicate review for update ",
      batch_id
    ),
    overwrite = FALSE
  )
} else {
  unlink(c(candidate_file, review_workbook), force = TRUE)
}

readr::write_csv(
  CiteSource::dedup_log(update_result),
  file.path(batch_dir, "update_auto_dedup_log.csv")
)

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  batch_id = batch_id,
  source_files = nrow(manifest),
  normalised_scopus_files = sum(
    imported$audit$normalisation != "none"
  ),
  removed_scopus_preamble_lines = sum(
    imported$audit$removed_preamble_lines
  ),
  raw_records_downloaded = nrow(new_raw),
  historical_unique_records = nrow(master),
  combined_unique_after_automatic_dedup = nrow(update_result$unique),
  automatic_duplicate_pairs = nrow(update_result$auto_pairs),
  manual_candidate_pairs = nrow(update_result$manual_dedup)
)

readr::write_csv(
  summary_data,
  file.path(batch_dir, "update_prepare_summary.csv")
)

write_session_snapshot(
  file.path(batch_dir, "update_session_info.txt")
)

print(summary_data)

if (nrow(update_result$manual_dedup) > 0L) {
  message("Next: open and review ", review_workbook)
  message("Use the dropdown in the result column: 'match' or 'no_match'.")
  message("Then run 04_update_finalize.R.")
} else {
  message("No manual candidate pairs were generated.")
  message("Run 04_update_finalize.R.")
}
