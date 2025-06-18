# GLOBAL VARIABLES

load_xlsx_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = temp_file, mode = "wb")
  readxl::read_excel(temp_file)
}

# Necessary data and info
# Data
# combined_data <- read.csv("https://raw.githubusercontent.com/feracotommaso/living_SEB_review/refs/heads/main/data/3.meta_data/meta_data/metadata.csv")
# combined_data <- readxl::read_excel(here::here("data/3.meta_data/meta_data/25_01_31_metadata.xlsx"))

data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/3.meta_data/meta_data/metadata.xlsx"
combined_data <- load_xlsx_url(data_url)

# Codebook and vars
# admcol <- readxl::read_excel(here::here("data/matrix_codebook.xlsx")) # Acceptable column names
admcol_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/matrix_codebook.xlsx"
admcol <- load_xlsx_url(admcol_url)
varlist <- admcol[complete.cases(admcol[,1:4]),] # List of the variables that can be used with different grain [Topic, BROAD, specific, and label]
labels_app <- setNames(varlist$label, varlist$column_name) # Create a named vector from the lookup table

pred_vars <- sort(c("selfmanagement", "cooperation", "socialengagement", "innovation", "emotionalresilience"))

