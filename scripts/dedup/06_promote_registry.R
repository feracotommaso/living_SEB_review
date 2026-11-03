# Promote the reviewed proposed registry to the live registry alias.
# Run only after reviewing the Rayyan comparison.

if (!interactive()) {
  stop("Run this promotion script interactively in RStudio.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(CiteSource)
  library(readr)
  library(tibble)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

proposed_master_file <- file.path(
  batch_dir,
  paste0("records_master_full_proposed_", batch_id, ".csv")
)

if (!file.exists(proposed_master_file)) {
  stop(
    "Proposed registry not found: ", proposed_master_file,
    "\nRun 04_update_finalize.R first.",
    call. = FALSE
  )
}

comparison_summary <- file.path(batch_dir, "comparison_summary.csv")
if (!file.exists(comparison_summary)) {
  stop(
    "Rayyan comparison summary not found. Run 05_compare_rayyan.R first.",
    call. = FALSE
  )
}

proposed <- CiteSource::reimport_csv(proposed_master_file)
validate_deduplicated_registry(proposed)

confirmation <- readline(
  paste0(
    "Type PROMOTE ", batch_id,
    " to replace records_master_full.csv with the reviewed proposal: "
  )
)

if (!identical(confirmation, paste("PROMOTE", batch_id))) {
  stop("Promotion cancelled.", call. = FALSE)
}

versioned_master_file <- file.path(
  registry_dir,
  paste0("records_master_full_through_", batch_id, ".csv")
)

if (file.exists(versioned_master_file)) {
  stop(
    "Versioned registry already exists: ", versioned_master_file,
    call. = FALSE
  )
}

copied_versioned <- file.copy(
  proposed_master_file,
  versioned_master_file,
  overwrite = FALSE
)

if (!copied_versioned) {
  stop("Could not create versioned registry.", call. = FALSE)
}

copied_alias <- file.copy(
  proposed_master_file,
  registry_file,
  overwrite = TRUE
)

if (!copied_alias) {
  stop("Could not replace the live registry alias.", call. = FALSE)
}

saveRDS(
  proposed,
  file.path(
    registry_dir,
    paste0("records_master_full_through_", batch_id, ".rds")
  )
)

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  promoted_batch = batch_id,
  promoted_records = nrow(proposed),
  versioned_registry_file = versioned_master_file,
  live_registry_file = registry_file,
  promoted_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

readr::write_csv(
  summary_data,
  file.path(batch_dir, "registry_promotion_summary.csv")
)

print(summary_data)
message("Registry promoted successfully.")
