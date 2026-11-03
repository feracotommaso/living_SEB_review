# Compare the R-generated new-record list with one Rayyan CSV/XLSX export.
# Place exactly one Rayyan export in data/0.deduplication/<batch>/rayyan/.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringdist)
  library(tibble)
  library(tidyr)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "dedup_helpers.R"))

r_file <- file.path(
  abstracting_dir,
  paste0(batch_id, "_new_records.csv")
)

if (!file.exists(r_file)) {
  stop(
    "R-generated screening file not found: ", r_file,
    "\nRun 04_update_finalize.R first.",
    call. = FALSE
  )
}

rayyan_files <- list.files(
  rayyan_dir,
  pattern = "\\.(csv|xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(rayyan_files) != 1L) {
  stop(
    "Place exactly one Rayyan CSV/XLSX export in: ", rayyan_dir,
    "\nFiles currently found: ", length(rayyan_files),
    call. = FALSE
  )
}

r_data <- read_table_auto(r_file) |>
  clean_column_names()
rayyan_data <- read_table_auto(rayyan_files[[1L]]) |>
  clean_column_names()

r_title_col <- pick_first_column(
  r_data,
  c("title", "article_title", "document_title")
)
r_doi_col <- pick_first_column(
  r_data,
  c("doi", "digital_object_identifier"),
  required = FALSE
)

rayyan_title_col <- pick_first_column(
  rayyan_data,
  c("title", "article_title", "document_title")
)
rayyan_doi_col <- pick_first_column(
  rayyan_data,
  c("doi", "digital_object_identifier"),
  required = FALSE
)

prepare_for_comparison <- function(data, title_col, doi_col, origin) {
  doi_values <- if (is.na(doi_col)) {
    rep("", nrow(data))
  } else {
    data[[doi_col]]
  }

  data |>
    mutate(
      comparison_row = dplyr::row_number(),
      title_original = .data[[title_col]],
      title_normalised = normalise_title(.data[[title_col]]),
      doi_normalised = normalise_doi(doi_values),
      exact_key = dplyr::case_when(
        nzchar(doi_normalised) ~ paste0("doi:", doi_normalised),
        nzchar(title_normalised) ~ paste0("title:", title_normalised),
        TRUE ~ paste0(tolower(origin), ":row:", comparison_row)
      ),
      comparison_origin = origin
    )
}

r_cmp <- prepare_for_comparison(
  r_data,
  r_title_col,
  r_doi_col,
  "R"
)

rayyan_cmp <- prepare_for_comparison(
  rayyan_data,
  rayyan_title_col,
  rayyan_doi_col,
  "Rayyan"
)

r_duplicate_keys <- r_cmp |>
  count(exact_key, name = "n") |>
  filter(n > 1L)
rayyan_duplicate_keys <- rayyan_cmp |>
  count(exact_key, name = "n") |>
  filter(n > 1L)

readr::write_csv(
  r_duplicate_keys,
  file.path(batch_dir, "comparison_duplicate_keys_R.csv")
)
readr::write_csv(
  rayyan_duplicate_keys,
  file.path(batch_dir, "comparison_duplicate_keys_Rayyan.csv")
)

r_unique <- r_cmp |>
  distinct(exact_key, .keep_all = TRUE)
rayyan_unique <- rayyan_cmp |>
  distinct(exact_key, .keep_all = TRUE)

exact_matches <- inner_join(
  r_unique |>
    select(
      exact_key,
      r_title = title_original,
      r_doi = doi_normalised
    ),
  rayyan_unique |>
    select(
      exact_key,
      rayyan_title = title_original,
      rayyan_doi = doi_normalised
    ),
  by = "exact_key"
)

only_r <- anti_join(
  r_unique,
  rayyan_unique |>
    select(exact_key),
  by = "exact_key"
)

only_rayyan <- anti_join(
  rayyan_unique,
  r_unique |>
    select(exact_key),
  by = "exact_key"
)

fuzzy_candidates <- tibble::tibble()

if (nrow(only_r) > 0L && nrow(only_rayyan) > 0L) {
  candidate_grid <- tidyr::crossing(
    r_index = seq_len(nrow(only_r)),
    rayyan_index = seq_len(nrow(only_rayyan))
  ) |>
    mutate(
      title_distance = stringdist::stringdist(
        only_r$title_normalised[r_index],
        only_rayyan$title_normalised[rayyan_index],
        method = "jw"
      )
    ) |>
    group_by(r_index) |>
    slice_min(title_distance, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    filter(title_distance <= 0.08)

  if (nrow(candidate_grid) > 0L) {
    fuzzy_candidates <- candidate_grid |>
      transmute(
        r_title = only_r$title_original[r_index],
        r_doi = only_r$doi_normalised[r_index],
        rayyan_title = only_rayyan$title_original[rayyan_index],
        rayyan_doi = only_rayyan$doi_normalised[rayyan_index],
        title_similarity = 1 - title_distance
      ) |>
      arrange(desc(title_similarity))
  }
}

readr::write_csv(
  exact_matches,
  file.path(batch_dir, "comparison_exact_matches.csv")
)
readr::write_csv(
  only_r,
  file.path(batch_dir, "comparison_only_in_R.csv")
)
readr::write_csv(
  only_rayyan,
  file.path(batch_dir, "comparison_only_in_Rayyan.csv")
)
readr::write_csv(
  fuzzy_candidates,
  file.path(batch_dir, "comparison_fuzzy_title_candidates.csv")
)

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  batch_id = batch_id,
  r_records = nrow(r_unique),
  rayyan_records = nrow(rayyan_unique),
  exact_matches = nrow(exact_matches),
  only_in_R_before_fuzzy_review = nrow(only_r),
  only_in_Rayyan_before_fuzzy_review = nrow(only_rayyan),
  fuzzy_title_candidates = nrow(fuzzy_candidates)
)

readr::write_csv(
  summary_data,
  file.path(batch_dir, "comparison_summary.csv")
)

comparison_workbook <- file.path(
  batch_dir,
  paste0("comparison_with_rayyan_", batch_id, ".xlsx")
)

write_comparison_workbook(
  list(
    summary = summary_data,
    only_in_R = only_r,
    only_in_Rayyan = only_rayyan,
    fuzzy_candidates = fuzzy_candidates,
    exact_matches = exact_matches,
    duplicate_keys_R = r_duplicate_keys,
    duplicate_keys_Rayyan = rayyan_duplicate_keys
  ),
  comparison_workbook
)

print(summary_data)
message("Human-readable comparison workbook: ", comparison_workbook)
message("Review the only_in_R, only_in_Rayyan, and fuzzy_candidates sheets ")
message("before promoting the proposed master. Machine-readable CSVs are retained for audit.")
