# Living SEB: apply consolidated human adjudication to the monthly decision workbook
# Version: 2.2.0

required_packages <- c("readr", "readxl", "dplyr", "digest", "openxlsx", "jsonlite", "xml2", "zip")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Install required packages before running this script: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

source(file.path("R", "full_text_workflow.R"))

args <- commandArgs(trailingOnly = TRUE)
batch_arg <- if (length(args) >= 1L) trimws(args[[1]]) else ""
if (file.exists(file.path("config", "dedup_config.R"))) source(file.path("config", "dedup_config.R"))
if (nzchar(batch_arg)) {
  batch_id <- batch_arg
} else if (!exists("batch_id", inherits = FALSE) || !nzchar(batch_id)) {
  stop("No batch_id available. Set config/dedup_config.R or run: Rscript scripts/full_text/03_finalize_full_text_decisions.R YYYY-MM", call. = FALSE)
}
if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", batch_id)) stop("batch_id must use YYYY-MM or YYYY-MM-external-NN.", call. = FALSE)

full_text_dir <- file.path("data", "2.full_abstracting")
common_workbook <- file.path("data/1.abstracting", batch_id, paste0(batch_id, "_screening.xlsx"))
if (file.exists(common_workbook)) {
  source("workflow.R")
  importa_screening(batch_id)
} else {
workbook_path <- file.path(full_text_dir, paste0(batch_id, "_full_text_decisions.xlsx"))
wb <- ft_read_decision_workbook(workbook_path)
paths <- ft_snapshot_paths(workbook_path)
if (!file.exists(paths$decisions)) stop("Canonical decision CSV missing. Import existing workbook with Step 00 first.", call. = FALSE)
canonical <- ft_read_canonical_decisions(paths$decisions)
ft_assert_same_table(canonical, wb$decisions, "paper_id", "The decisions sheet was edited. Edit only human_review; restore decisions from the canonical CSV.")
if (!file.exists(paths$human_review)) stop("Canonical human-review audit CSV missing.", call. = FALSE)
ft_validate_queue_edits(ft_read_table(paths$human_review), wb$human_review)
res <- ft_apply_adjudications(canonical, wb$human_review)
ft_write_monthly_outputs(res$decisions, res$human_review, workbook_path, batch_id)

cat("Living SEB full-text adjudication applied.\n")
cat("Workbook: ", workbook_path, "\n", sep = "")
cat("Adjudications applied: ", res$applied, "\n", sep = "")
cat("Resolved decisions: ", sum(res$decisions$workflow_status == "RESOLVED"), "\n", sep = "")
cat("Human review still open: ", sum(res$decisions$workflow_status == "HUMAN_REVIEW_REQUIRED"), "\n", sep = "")
cat("Pending data/retrieval: ", sum(res$decisions$workflow_status == "PENDING_DATA"), "\n", sep = "")

if (any(res$decisions$workflow_status == "HUMAN_REVIEW_REQUIRED")) {
  cat("\nSome substantive decisions remain open. Discuss the remaining cases as a batch, update human_review, then rerun this script.\n")
} else {
  cat("\nNo substantive human-review cases remain open.\n")
}
}
