# Run title/abstract screening unit tests.

suppressPackageStartupMessages({
  library(testthat)
  library(here)
})

testthat::test_dir(
  here::here("tests", "testthat"),
  reporter = "summary"
)
