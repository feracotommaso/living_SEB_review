# Current update step 2: apply manual decisions, identify genuinely new records,
# and create a proposed updated registry. The live registry is not overwritten.

suppressPackageStartupMessages({
  library(CiteSource)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

result_file <- file.path(
  batch_dir,
  "update_dedup_result.rds"
)

if (!file.exists(result_file)) {
  stop(
    "Missing update_dedup_result.rds. Run 03_update_prepare.R first.",
    call. = FALSE
  )
}

if (!file.exists(registry_file)) {
  stop("Historical registry not found: ", registry_file, call. = FALSE)
}

master <- CiteSource::reimport_csv(registry_file)
validate_deduplicated_registry(master)

update_result <- readRDS(result_file)
expected_candidates <- nrow(update_result$manual_dedup)
reviewed <- NULL

if (expected_candidates > 0L) {
  review_workbook <- file.path(
    batch_dir,
    "update_candidate_pairs_review.xlsx"
  )
  legacy_reviewed_file <- file.path(
    batch_dir,
    "update_candidate_pairs_reviewed.csv"
  )

  reviewed <- load_reviewed_candidates(
    expected_pairs = update_result$manual_dedup,
    excel_file = review_workbook,
    legacy_csv_file = legacy_reviewed_file
  )

  combined_final <- CiteSource::dedup_citations_add_manual(
    update_result$unique,
    reviewed
  )
} else {
  combined_final <- update_result$unique
}

validate_deduplicated_registry(combined_final)

historical_ids <- split_record_ids(
  c(master$duplicate_id, master$record_ids)
)

is_genuinely_new <- vapply(
  combined_final$record_ids,
  FUN = function(record_ids) {
    cluster_ids <- split_record_ids(record_ids)
    length(intersect(cluster_ids, historical_ids)) == 0L
  },
  FUN.VALUE = logical(1)
)

new_records <- combined_final[
  is_genuinely_new,
  ,
  drop = FALSE
]

proposed_master_file <- file.path(
  batch_dir,
  paste0(
    "records_master_full_proposed_",
    batch_id,
    ".csv"
  )
)

CiteSource::export_csv(
  combined_final,
  filename = proposed_master_file,
  fields = "full",
  separate = NULL,
  manual_dedup_complete = TRUE
)

full_new_file <- file.path(
  batch_dir,
  paste0(batch_id, "_new_records_full.csv")
)

if (!getOption("living_seb.compact", file.exists("data/3.meta_data/coding/records.csv"))) CiteSource::export_csv(
  new_records,
  filename = full_new_file,
  fields = "full",
  separate = NULL,
  manual_dedup_complete = TRUE
)

dir.create(abstracting_dir, recursive = TRUE, showWarnings = FALSE)

screening_file <- file.path(
  abstracting_dir,
  paste0(batch_id, "_new_records.csv")
)

CiteSource::export_csv(
  new_records,
  filename = screening_file,
  fields = "standard"
)

screening_xlsx_file <- file.path(
  abstracting_dir,
  paste0(batch_id, "_new_records.xlsx")
)

screening_for_excel <- read_table_auto(screening_file)
if (!getOption("living_seb.compact", file.exists("data/3.meta_data/coding/records.csv"))) write_human_workbook(
  screening_for_excel,
  screening_xlsx_file,
  sheet = "new_records"
)

full_log <- CiteSource::dedup_log(
  update_result,
  confirmed_manual_pairs = reviewed
)

readr::write_csv(
  full_log,
  file.path(batch_dir, "update_dedup_log_final.csv")
)

manual_matches <- if (is.null(reviewed)) {
  0L
} else {
  sum(reviewed$result == "match")
}

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  batch_id = batch_id,
  historical_unique_records = nrow(master),
  proposed_master_records = nrow(combined_final),
  genuinely_new_records = nrow(new_records),
  automatic_duplicate_pairs = nrow(update_result$auto_pairs),
  manual_candidate_pairs = expected_candidates,
  manually_confirmed_duplicate_pairs = manual_matches,
  screening_file = screening_file,
  screening_xlsx_file = screening_xlsx_file,
  proposed_master_file = proposed_master_file
)

readr::write_csv(
  summary_data,
  file.path(batch_dir, "update_finalize_summary.csv")
)

print(summary_data)
message("Current update finalized, but the registry has not been promoted.")
if (isTRUE(require_rayyan_comparison)) {
  message("Rayyan comparison is required by config before promotion.")
  message("Run 05_compare_rayyan.R, then 06_promote_registry.R.")
} else {
  message("Rayyan comparison is optional for this batch.")
  message("Inspect the outputs, then run 06_promote_registry.R.")
}
