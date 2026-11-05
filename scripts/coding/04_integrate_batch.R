# Validate monthly sources, register/copy ready matrices, rebuild final outputs.
source("R/full_text_workflow.R")
source("R/coding_compatibility.R")
source("R/article_coding.R")
source("R/codebook_workflow.R")
source("R/analytical_integration.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript scripts/coding/04_integrate_batch.R YYYY-MM")
result <- integration_register_batch(args[[1]])
source("data/3.2cor_to_metadata.R")
cat("Integrated", result$papers, "papers from", result$batch,
    "; quantitative samples awaiting extraction:", length(result$pending), "\n")
