# Bootstrap step 2: apply reviewed manual decisions and create the historical
# master registry through history_through.

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
  bootstrap_dir,
  "history_dedup_result.rds"
)

if (!file.exists(result_file)) {
  stop(
    "Missing history_dedup_result.rds. Run 01_history_prepare.R first.",
    call. = FALSE
  )
}

history_result <- readRDS(result_file)
expected_candidates <- nrow(history_result$manual_dedup)
reviewed <- NULL

if (expected_candidates > 0L) {
  review_workbook <- file.path(
    bootstrap_dir,
    "history_candidate_pairs_review.xlsx"
  )
  legacy_reviewed_file <- file.path(
    bootstrap_dir,
    "history_candidate_pairs_reviewed.csv"
  )

  reviewed <- load_reviewed_candidates(
    expected_pairs = history_result$manual_dedup,
    excel_file = review_workbook,
    legacy_csv_file = legacy_reviewed_file
  )

  master <- CiteSource::dedup_citations_add_manual(
    history_result$unique,
    reviewed
  )
} else {
  master <- history_result$unique
}

validate_deduplicated_registry(master)
dir.create(registry_dir, recursive = TRUE, showWarnings = FALSE)

versioned_master_file <- file.path(
  registry_dir,
  paste0(
    "records_master_full_through_",
    history_through,
    ".csv"
  )
)

if (file.exists(versioned_master_file) || file.exists(registry_file)) {
  stop(
    "A historical registry already exists. Run 00_reset_full_rebuild.R ",
    "before rebuilding from scratch.",
    call. = FALSE
  )
}

CiteSource::export_csv(
  master,
  filename = versioned_master_file,
  fields = "full",
  separate = NULL,
  manual_dedup_complete = TRUE
)

copied <- file.copy(
  versioned_master_file,
  registry_file,
  overwrite = FALSE
)

if (!copied) {
  stop("Could not create the registry alias.", call. = FALSE)
}

saveRDS(
  master,
  file.path(
    registry_dir,
    paste0(
      "records_master_full_through_",
      history_through,
      ".rds"
    )
  )
)

full_log <- CiteSource::dedup_log(
  history_result,
  confirmed_manual_pairs = reviewed
)

readr::write_csv(
  full_log,
  file.path(bootstrap_dir, "history_dedup_log_final.csv")
)

manual_matches <- if (is.null(reviewed)) {
  0L
} else {
  sum(reviewed$result == "match")
}

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  history_through = history_through,
  final_unique_records = nrow(master),
  automatic_duplicate_pairs = nrow(history_result$auto_pairs),
  manual_candidate_pairs = expected_candidates,
  manually_confirmed_duplicate_pairs = manual_matches,
  registry_file = registry_file,
  versioned_registry_file = versioned_master_file
)

readr::write_csv(
  summary_data,
  file.path(bootstrap_dir, "history_finalize_summary.csv")
)

print(summary_data)
message("Historical registry created successfully.")
message("Next: run 03_update_prepare.R for batch ", batch_id, ".")
