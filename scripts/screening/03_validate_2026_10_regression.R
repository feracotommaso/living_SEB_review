# Optional regression check for the independently validated 2026-10 pilot.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "title_abstract_screening.R"))

actual_file <- file.path(
  here::here("data", "1.abstracting", "2026-10", "title_abstract"),
  "2026-10_title_abstract_decisions.csv"
)
expected_file <- here::here(
  "tests", "fixtures", "title_abstract",
  "2026-10_expected_decisions.csv"
)

if (!file.exists(actual_file)) {
  stop("Pilot decisions not found: ", actual_file, call. = FALSE)
}
if (!file.exists(expected_file)) {
  stop("Golden fixture not found: ", expected_file, call. = FALSE)
}

actual <- readr::read_csv(actual_file, show_col_types = FALSE)
expected <- readr::read_csv(expected_file, show_col_types = FALSE)
check <- screening_compare_golden(actual, expected)


if (!all(check$matches_expected)) {
  print(check[!check$matches_expected, , drop = FALSE])
  stop(
    "The current 2026-10 decisions differ from the validated pilot.",
    call. = FALSE
  )
}

message("2026-10 regression check passed: ", nrow(check), "/", nrow(check), " records.")
