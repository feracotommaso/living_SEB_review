# Install/check packages and create title/abstract screening folders.

if (!file.exists("living_SEB_review.Rproj")) {
  stop(
    "Open living_SEB_review.Rproj and run this script from the project root.",
    call. = FALSE
  )
}

required_packages <- c(
  "dplyr",
  "readr",
  "readxl",
  "openxlsx",
  "here",
  "yaml",
  "jsonlite",
  "digest",
  "stringi",
  "tibble",
  "testthat"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0L) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "title_abstract_screening.R"))

policy <- screening_load_policy()
output_dir <- file.path(abstracting_dir, "title_abstract")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Title/abstract screening setup completed.")
message("Batch: ", batch_id)
message("Policy version: ", policy$version)
message("Prompt version: ", policy$prompt_version)
message("Output directory: ", output_dir)
