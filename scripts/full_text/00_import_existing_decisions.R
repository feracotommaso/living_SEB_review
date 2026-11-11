# Explicit, one-time recovery of existing operational workbooks, never fixtures.
source(file.path("R", "full_text_workflow.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", args[[1]])) stop("Usage: Rscript scripts/full_text/00_import_existing_decisions.R <batch_id>", call. = FALSE)
batch_id <- args[[1]]
base <- file.path("data", "2.full_abstracting")
path <- file.path(base, paste0(batch_id, "_full_text_decisions.xlsx"))
ft_import_existing_workbook(path, file.path(base, "ai_decisions", batch_id), batch_id)
cat("Existing operational decisions imported with migration provenance: ", batch_id, "\n", sep = "")
