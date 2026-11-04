library(tidyverse)
library(readxl)

source("R/utils.R")
source("R/coding_compatibility.R")

# Read and merge all basic infos
basic_info_data <- coding_read_basic_info()

# Combine the basic info with the different matrices
matrix_paths <- list.files(path = "data/3.meta_data/matrices/", pattern = "\\.xlsx$", full.names = TRUE)
inventory <- coding_matrix_inventory(basic_info_data, matrix_paths)
matrix_paths <- inventory$matrix_path[inventory$status == "included"]

# Process all correlation matrices and combine them into a single dataset
combined_data <- map_dfr(matrix_paths, ~ data_combine(.x, basic_info_data))

# Write and save the excel
writexl::write_xlsx(combined_data, "data/3.meta_data/meta_data/metadata.xlsx")
# write.csv(combined_data, "data/3.meta_data/meta_data/25_01_31_metadata.csv")

# Write and save the review data excel
review_data <- basic_info_data[!duplicated(basic_info_data$paper_id),]
writexl::write_xlsx(review_data, "data/3.meta_data/review_data/review_data.xlsx")

# Canonical machine-readable compatibility views and explicit reconciliation logs.
# Historical coding files and correlation matrices are never modified here.
readr::write_csv(combined_data, "data/3.meta_data/meta_data/metadata.csv", na = "")
readr::write_csv(review_data, "data/3.meta_data/review_data/review_data.csv", na = "")
readr::write_csv(basic_info_data, "data/3.meta_data/legacy_basic_info.csv", na = "")
readr::write_csv(inventory, "data/3.meta_data/matrix_inventory.csv", na = "")
readr::write_csv(attr(basic_info_data, "topic_corrections"), "data/3.meta_data/topic_corrections.csv", na = "")
topics <- coding_read_csv("data/topics_codebook.csv")
readr::write_csv(coding_topic_audit(basic_info_data, topics), "data/3.meta_data/topic_review_queue.csv", na = "")
cat("Matrix reconciliation: ", sum(inventory$status == "included"), " included; ",
    sum(inventory$status == "excluded_by_recorded_decision"), " excluded by recorded decision; ",
    sum(inventory$status == "missing_matrix_file"), " historical rows awaiting matrix reconciliation.\n", sep = "")
