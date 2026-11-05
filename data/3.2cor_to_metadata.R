library(tidyverse)
library(readxl)
source("R/utils.R")
source("R/coding_compatibility.R")
source("R/codebook_workflow.R")
source("R/analytical_integration.R")

# Read historical coding plus explicitly integrated monthly batches.
current <- integration_current()
basic_info_data <- current$basic
matrix_paths <- list.files("data/3.meta_data/matrices", pattern = "\\.xlsx$", full.names = TRUE)
inventory <- coding_matrix_inventory(basic_info_data, matrix_paths)
matrix_paths <- inventory$matrix_path[inventory$status == "included"]
combined_data <- map_dfr(matrix_paths, ~ data_combine(.x, basic_info_data))
review_data <- current$review

# Preserve historical values except exact, versioned before/after corrections.
# The original numeric fixture remains frozen; the revision ledger pins new values.
coding_validate_historical(combined_data)
stopifnot(identical(names(combined_data)[1:31], coding_dictionary()$field))

# Stage all derived files before replacement, keeping CSV/XLSX outputs in sync.
scratch <- tempfile("analytical_outputs_"); dir.create(scratch)
stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
staged <- destinations <- hashes <- character()
save_table <- function(data, csv, xlsx = NULL) {
  for (target in c(csv, xlsx)) {
    temporary <- file.path(scratch, paste0(length(staged) + 1L, "_", basename(target)))
    if (endsWith(target, ".xlsx")) writexl::write_xlsx(data, temporary)
    else readr::write_csv(data, temporary, na = "")
    staged <<- c(staged, temporary); destinations <<- c(destinations, target)
    hashes <<- c(hashes, integration_hash(target))
  }
}
save_table(combined_data, "data/3.meta_data/meta_data/metadata.csv", "data/3.meta_data/meta_data/metadata.xlsx")
save_table(review_data, "data/3.meta_data/review_data/review_data.csv", "data/3.meta_data/review_data/review_data.xlsx")
save_table(current$legacy, "data/3.meta_data/legacy_basic_info.csv")
save_table(basic_info_data, "data/3.meta_data/current_basic_info.csv")
save_table(current$provenance, "data/3.meta_data/coding_provenance.csv")
save_table(inventory, "data/3.meta_data/matrix_inventory.csv")
save_table(attr(basic_info_data, "topic_corrections"), "data/3.meta_data/topic_corrections.csv")
save_table(coding_topic_audit(basic_info_data, coding_read_csv("data/topics_codebook.csv")), "data/3.meta_data/topic_review_queue.csv")
codebook_commit_files(staged, destinations, hashes)
unlink(scratch, recursive = TRUE)
cat("Analytical samples:", nrow(combined_data), "; review papers:", nrow(review_data),
    "; inactive matrices:", sum(inventory$status == "excluded_by_recorded_decision"),
    "; coding rows awaiting a matrix:", sum(inventory$status == "missing_matrix_file"), "\n")
