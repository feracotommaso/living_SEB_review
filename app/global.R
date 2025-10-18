# GLOBAL VARIABLES

load_xlsx_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  tryCatch({
    download.file(url, destfile = temp_file, mode = "wb")
    readxl::read_excel(temp_file)
  }, error = function(e) {
    stop("Failed to download or read Excel file: ", e$message)
  })
}

# Necessary data and info
# Data
cat(">>> Downloading data...\n")
data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/3.meta_data/meta_data/metadata.xlsx"
combined_data <- load_xlsx_url(data_url)

# Codebook and vars
cat(">>> Downloading codebook...\n")

admcol_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/matrix_codebook.xlsx"
admcol <- load_xlsx_url(admcol_url)

cat(">>> Preparing varlist...\n")
varlist <- admcol[complete.cases(admcol[,1:4]),] # List of the variables that can be used with different grain [Topic, BROAD, specific, and label]
labels_app <- setNames(varlist$label, varlist$column_name) # Create a named vector from the lookup table

pred_vars <- sort(c("selfmanagement", "cooperation", "socialengagement", "innovation", "emotionalresilience"))

# Review data
cat(">>> Downloading review data...\n")
rev_data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/3.meta_data/review_data/review_data.xlsx"
review <- load_xlsx_url(rev_data_url)

# review <- readxl::read_excel(here::here("data/3.meta_data/review_data/review_data.xlsx"))

# Topics data
cat(">>> Downloading topic list...\n")
topic_data_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/topics_codebook.xlsx"
topics_list <- load_xlsx_url(topic_data_url)

# topics_list <- readxl::read_excel(here::here("data/topics_codebook.xlsx"))
topics_list <- topics_list[is.na(topics_list$Broad_topic)==FALSE,]

# Bib
cat(">>> Downloading bib list...\n")
bib_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/data/0.bib_download/bibAll.bib"
dest_file <- tempfile(fileext = ".bib")
download.file(bib_url, destfile = dest_file, mode = "wb")
library(RefManageR)
bibAll <- ReadBib(dest_file)

# bibAll <- RefManageR::ReadBib(here::here("data/0.bib_download/bibAll.bib"))


# Citations app
load_text_url <- function(url, fallback = NULL, keep_newlines = TRUE) {
  temp_file <- tempfile(fileext = ".txt")
  tryCatch({
    download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
    txt <- readLines(temp_file, warn = FALSE, encoding = "UTF-8")
    paste(txt, collapse = if (keep_newlines) "\n" else "")
  }, error = function(e) {
    if (!is.null(fallback) && file.exists(fallback)) {
      txt <- readLines(fallback, warn = FALSE, encoding = "UTF-8")
      return(paste(txt, collapse = if (keep_newlines) "\n" else ""))
    }
    stop("Failed to download or read text file: ", e$message)
  })
}

# ---- Download citation text ----
cat(">>> Downloading citations...\n")

# Direct links to the raw GitHub files on 'main'
apa_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/R/apa.txt"
bib_url <- "https://raw.githubusercontent.com/feracotommaso/living_SEB_review/main/R/cite.bib"

# Load once at app startup
apa_text <- load_text_url(apa_url, fallback = "apa.txt", keep_newlines = TRUE)
bib_text <- load_text_url(bib_url, fallback = "cite.bib", keep_newlines = TRUE)