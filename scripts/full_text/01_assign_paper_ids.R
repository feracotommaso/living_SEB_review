# Living SEB: assign persistent paper IDs before full-text eligibility screening
# Version: 2.2.0

required_packages <- c("readr", "readxl", "dplyr", "digest", "openxlsx", "xml2", "zip")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(
    "Install required packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source(file.path("R", "full_text_workflow.R"))

# Batch selection: command-line argument takes precedence; otherwise use the
# project-wide dedup configuration when available.
args <- commandArgs(trailingOnly = TRUE)
batch_arg <- if (length(args) >= 1L) trimws(args[[1]]) else ""

if (file.exists(file.path("config", "dedup_config.R"))) {
  source(file.path("config", "dedup_config.R"))
}

if (nzchar(batch_arg)) {
  batch_id <- batch_arg
} else if (!exists("batch_id", inherits = FALSE) || !nzchar(batch_id)) {
  stop(
    "No batch_id available. Either set batch_id in config/dedup_config.R or run: ",
    "Rscript scripts/full_text/01_assign_paper_ids.R YYYY-MM",
    call. = FALSE
  )
}

if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", batch_id)) {
  stop("batch_id must use YYYY-MM format. Received: ", batch_id, call. = FALSE)
}

current_abstracting_dir <- file.path("data", "1.abstracting", batch_id)

full_text_dir <- file.path("data", "2.full_abstracting")
pdf_dir <- file.path(full_text_dir, "pdfs")
registry_path <- file.path(full_text_dir, "paper_id_registry.csv")
manifest_path <- file.path(full_text_dir, paste0(batch_id, "_full_text_manifest.xlsx"))

alias_path <- file.path(full_text_dir, "paper_id_aliases.csv")

source_file <- ft_find_title_abstract_input(batch_id, current_abstracting_dir)
current <- ft_prepare_current_records(source_file, batch_id)

legacy <- ft_bootstrap_legacy_registry(full_text_dir)

if (file.exists(registry_path)) {
  registry <- ft_read_table(registry_path)
  registry <- ft_validate_registry(registry)
} else {
  registry <- data.frame()
}

registry <- ft_reconcile_legacy_registry(registry, legacy)
# Reserve persisted aliases even when historical source files are unavailable.
if (file.exists(alias_path)) {
  aliases <- ft_read_table(alias_path)
  if (any(aliases$alias_paper_id %in% registry$paper_id)) stop("Alias ID collides with canonical registry.", call. = FALSE)
  attr(registry, "reserved_paper_ids") <- unique(c(attr(registry, "reserved_paper_ids"), aliases$alias_paper_id))
}
assigned <- ft_assign_paper_ids(current, registry, batch_id, source_file)
manifest <- ft_build_manifest(assigned$current, batch_id, pdf_dir, source_file)

ft_atomic_write_csv(assigned$registry, registry_path)
ft_atomic_write_csv(manifest, sub("\\.xlsx$", ".csv", manifest_path))
ft_atomic_write_manifest_xlsx(manifest, manifest_path)

cat("Living SEB full-text paper ID assignment complete.\n")
cat("Batch: ", batch_id, "\n", sep = "")
cat("Title/abstract input: ", source_file, "\n", sep = "")
cat("Registry: ", registry_path, "\n", sep = "")
cat("Manifest: ", manifest_path, "\n", sep = "")
if (file.exists(alias_path)) cat("Legacy paper-ID aliases: ", alias_path, "\n", sep = "")
cat("Records entering full text: ", nrow(manifest), "\n", sep = "")
cat("Existing paper_id values reused: ", assigned$reused, "\n", sep = "")
cat("New paper_id values assigned: ", assigned$newly_assigned, "\n", sep = "")
cat("PDFs available: ", sum(manifest$pdf_status == "AVAILABLE"), "\n", sep = "")
cat("PDFs missing: ", sum(manifest$pdf_status == "MISSING"), "\n", sep = "")

if (any(manifest$pdf_status == "MISSING")) {
  cat("\nPDFs to add to data/2.full_abstracting/pdfs/:\n")
  cat(paste0("  ", manifest$pdf_file[manifest$pdf_status == "MISSING"]), sep = "\n")
  cat("\n")
}
