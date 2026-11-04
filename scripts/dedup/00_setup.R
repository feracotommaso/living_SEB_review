# Run once from the root of the living_SEB_review RStudio project.

if (!file.exists("living_SEB_review.Rproj")) {
  stop(
    "Open living_SEB_review.Rproj and run this script from the project root.",
    call. = FALSE
  )
}

required_packages <- c(
  "CiteSource",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "purrr",
  "here",
  "readxl",
  "openxlsx",
  "stringi",
  "stringdist",
  "tidyr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0L) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

folders <- c(
  history_dir,
  update_dir,
  normalised_history_dir,
  normalised_update_dir,
  registry_dir,
  bootstrap_dir,
  batch_dir,
  abstracting_dir,
  rayyan_dir
)

for (folder in folders) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

installed_citesource <- as.character(packageVersion("CiteSource"))
if (!identical(installed_citesource, expected_citesource_version)) {
  warning(
    "This workflow was prepared for CiteSource ",
    expected_citesource_version,
    ", but version ", installed_citesource,
    " is installed. Continue only after reviewing package changes.",
    call. = FALSE
  )
}

metadata <- workflow_metadata(
  workflow_version = workflow_version,
  batch_id = batch_id,
  history_through = history_through,
  search_run_date = search_run_date
)

readr::write_csv(
  metadata,
  here::here("data", "0.deduplication", "active_workflow_config.csv")
)

message("Folder setup completed.")
message("Current publication-month batch: ", batch_id)
message("Historical files must stop at: ", history_through)
message("Historical files belong in: ", history_dir)
message("Current update files belong in: ", update_dir)
message("Rayyan comparison export belongs in: ", rayyan_dir)
message("Rayyan comparison required before promotion: ", require_rayyan_comparison)
