# Living SEB: apply consolidated human adjudication to the monthly decision workbook
# Version: 2.1.0

required_packages <- c("readr", "readxl", "dplyr", "digest", "openxlsx")
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

full_text_dir <- file.path("data", "2.full_abstracting")
workbook_path <- file.path(full_text_dir, paste0(batch_id, "_full_text_decisions.xlsx"))
wb <- ft_read_decision_workbook(workbook_path)
res <- ft_apply_adjudications(wb$decisions, wb$human_review)
ft_write_decision_workbook(res$decisions, res$human_review, workbook_path, batch_id)

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
