# Additive codebook maintenance; run from repository root.
# Optional additions CSVs have the existing codebook columns plus:
# paper_id, change_date (YYYY-MM-DD), reason, source.
source("R/codebook_workflow.R")
args <- commandArgs(trailingOnly = TRUE)
allowed <- c("--add-variables", "--add-topics")
if (length(args) %% 2L || (length(args) &&
    (any(!args[seq(1L, length(args), by = 2L)] %in% allowed) ||
     anyDuplicated(args[seq(1L, length(args), by = 2L)]))))
  stop("Usage: Rscript scripts/coding/03_update_codebooks.R [--add-variables CSV] [--add-topics CSV]")
read_additions <- function(flag) {
  i <- match(flag, args)
  if (is.na(i)) NULL else codebook_csv(args[[i + 1L]])
}
result <- codebook_sync(add_variables = read_additions("--add-variables"),
  add_topics = read_additions("--add-topics"))
cat("Codebooks verified/synchronised:", sum(!codebook_blank(result$variables$column_name)),
    "variables;", sum(!codebook_blank(result$topics$Topic)), "review topics.\n")
