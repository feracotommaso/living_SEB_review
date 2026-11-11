# Promote the reviewed proposed registry to the live registry alias.
# Rayyan comparison is optional unless require_rayyan_comparison is TRUE.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) && identical(args[1L], "--reconcile")) {
  if (length(args) != 3L) stop("Use --reconcile YYYY-MM-DD 'RECONCILE YYYY-MM-DD'.")
  source(here::here("R", "dedup_helpers.R"))
  # Reconciliation cannot bypass unresolved pairs or an existing failed comparison.
  pair_files <- list.files(here::here("data/0.deduplication"), pattern = "_candidate_pairs\\.csv$", recursive = TRUE, full.names = TRUE)
  for (p in pair_files) {
    pairs <- readr::read_csv(p, show_col_types = FALSE)
    if (nrow(pairs)) load_reviewed_candidates(pairs,
      sub("\\.csv$", "_review.xlsx", p), sub("\\.csv$", "_reviewed.csv", p))
  }
  comparisons <- list.files(here::here("data/0.deduplication"), pattern = "^comparison_summary\\.csv$", recursive = TRUE, full.names = TRUE)
  for (p in comparisons) {
    x <- readr::read_csv(p, show_col_types = FALSE)
    if (nrow(x) != 1L || !isTRUE(with(x, r_records == rayyan_records && exact_matches == r_records &&
        only_in_R_before_fuzzy_review == 0 && only_in_Rayyan_before_fuzzy_review == 0 && fuzzy_title_candidates == 0)))
      stop("Unresolved Rayyan comparison: ", p)
  }
  reconcile_registry(here::here("data/0.registry/records_master_full.csv"),
    here::here("data/0.deduplication/bootstrap/registry_corrections.csv"), args[2L], args[3L])
} else {
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

comparison_summary_file <- file.path(batch_dir, "comparison_summary.csv")
comparison_status <- "not_run"

if (file.exists(comparison_summary_file)) {
  comparison <- readr::read_csv(
    comparison_summary_file,
    show_col_types = FALSE
  )

  required_comparison_columns <- c(
    "r_records",
    "rayyan_records",
    "exact_matches",
    "only_in_R_before_fuzzy_review",
    "only_in_Rayyan_before_fuzzy_review",
    "fuzzy_title_candidates"
  )
  missing_comparison_columns <- setdiff(
    required_comparison_columns,
    names(comparison)
  )

  if (length(missing_comparison_columns) > 0L || nrow(comparison) != 1L) {
    stop(
      "Invalid Rayyan comparison summary. Missing/invalid fields: ",
      paste(missing_comparison_columns, collapse = ", "),
      call. = FALSE
    )
  }

  exact_agreement <- with(
    comparison,
    r_records == rayyan_records &&
      exact_matches == r_records &&
      only_in_R_before_fuzzy_review == 0L &&
      only_in_Rayyan_before_fuzzy_review == 0L &&
      fuzzy_title_candidates == 0L
  )

  if (!isTRUE(exact_agreement[[1L]])) {
    stop(
      "Rayyan comparison contains unresolved discrepancies. ",
      "Resolve them before registry promotion.",
      call. = FALSE
    )
  }

  comparison_status <- "exact_agreement"
} else if (isTRUE(require_rayyan_comparison)) {
  stop(
    "Rayyan comparison is required by config, but comparison_summary.csv is missing. ",
    "Run 05_compare_rayyan.R first.",
    call. = FALSE
  )
} else {
  message(
    "No Rayyan comparison found. Promotion is permitted because ",
    "require_rayyan_comparison is FALSE."
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
  rayyan_comparison_status = comparison_status,
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
}
