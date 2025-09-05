library(tidyverse)
library(readxl)

source("R/utils.R")

# Read and merge all basic infos
basic_info_paths <- list.files(
  path = "data/3.meta_data/basic_info/",
  pattern = "\\.xlsx$", 
  full.names = TRUE
)
all_data <- list()
for (i in seq_along(basic_info_paths)) {
  all_data[[i]] <- read_excel(basic_info_paths[i])
}
basic_info_data <- do.call(rbind, all_data)

# Combine the basic info with the different matrices
matrix_paths <- list.files(path = "data/3.meta_data/matrices/", pattern = "*.xlsx", full.names = TRUE)  # Folder with correlation matrices

# Process all correlation matrices and combine them into a single dataset
combined_data <- map_dfr(matrix_paths, ~ data_combine(.x, basic_info_data))

# Write and save the excel
writexl::write_xlsx(combined_data, "data/3.meta_data/meta_data/metadata.xlsx")
# write.csv(combined_data, "data/3.meta_data/meta_data/25_01_31_metadata.csv")

# Write and save the review data excel
review_data <- basic_info_data[!duplicated(basic_info_data$paper_id),]
writexl::write_xlsx(review_data, "data/3.meta_data/review_data/review_data.xlsx")
