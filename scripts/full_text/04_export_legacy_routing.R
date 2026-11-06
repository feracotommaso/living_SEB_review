# Eligibility handoff only; detailed extraction is required before basic_info.
source(file.path("R", "full_text_workflow.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", args[[1]])) stop("Usage: Rscript scripts/full_text/04_export_legacy_routing.R <batch_id>", call. = FALSE)
batch_id <- args[[1]]
base <- file.path("data", "2.full_abstracting")
decisions <- ft_read_canonical_decisions(file.path(base, paste0(batch_id, "_full_text_decisions.csv")))
routing <- ft_legacy_routing(decisions)
path <- file.path(base, paste0(batch_id, "_legacy_routing.csv"))
ft_atomic_write_csv(routing, path)
cat("Resolved eligibility handoff: ", path, "\nNo sample or matrix IDs assigned; complete extraction before updating basic_info.\n", sep = "")
