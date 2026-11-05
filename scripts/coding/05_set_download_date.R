# Confirm source provenance; derived outputs are rebuilt by steps 02 and 04.
source("R/codebook_workflow.R")
source("R/coding_provenance.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript scripts/coding/05_set_download_date.R YYYY-MM YYYY-MM-DD")
coding_set_download_date(args[[1]], args[[2]])
cat("Bibliographic download date recorded in source coding:", args[[2]], "for", args[[1]], "\n")
