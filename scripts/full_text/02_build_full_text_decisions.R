# Living SEB: build the canonical monthly full-text decision workbook
# Version: 2.1.0

required_packages <- c("readr", "readxl", "dplyr", "digest", "openxlsx", "jsonlite")
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
  stop("No batch_id available. Set config/dedup_config.R or run: Rscript scripts/full_text/02_build_full_text_decisions.R YYYY-MM", call. = FALSE)
}
if (!grepl("^[0-9]{4}-[0-9]{2}$", batch_id)) stop("batch_id must use YYYY-MM format.", call. = FALSE)

full_text_dir <- file.path("data", "2.full_abstracting")
manifest_path <- file.path(full_text_dir, paste0(batch_id, "_full_text_manifest.xlsx"))
decision_dir <- file.path(full_text_dir, "ai_decisions", batch_id)
output_path <- file.path(full_text_dir, paste0(batch_id, "_full_text_decisions.xlsx"))

if (!file.exists(manifest_path)) stop("Run Step 1 first. Manifest not found: ", manifest_path, call. = FALSE)
manifest <- ft_clean_names(as.data.frame(readxl::read_excel(manifest_path, sheet = "full_text_manifest"), stringsAsFactors = FALSE))
ai <- ft_read_ai_decisions(decision_dir)
decisions <- ft_merge_manifest_decisions(manifest, ai, batch_id)
human_review <- ft_build_human_review_queue(decisions, ai)
ft_write_decision_workbook(decisions, human_review, output_path, batch_id)

cat("Living SEB full-text decision workbook created.\n")
cat("Batch: ", batch_id, "\n", sep = "")
cat("AI decision directory: ", decision_dir, "\n", sep = "")
cat("Workbook: ", output_path, "\n", sep = "")
cat("Records: ", nrow(decisions), "\n", sep = "")
cat("Resolved automatically: ", sum(decisions$workflow_status == "RESOLVED"), "\n", sep = "")
cat("Human review required: ", sum(decisions$workflow_status == "HUMAN_REVIEW_REQUIRED"), "\n", sep = "")
cat("Pending data/retrieval: ", sum(decisions$workflow_status == "PENDING_DATA"), "\n", sep = "")
if (nrow(human_review) > 0L) {
  cat("\nReview all human-review cases together; do not adjudicate them one at a time unless needed.\n")
  cat("Open sheet: human_review\n")
}
