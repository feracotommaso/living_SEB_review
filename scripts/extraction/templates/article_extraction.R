# Article extraction template. Copy to scripts/extraction/articles/<paper_id>.R.
# Read policies/extraction_conventions.md. Run from the repository root.
# Keep scientific steps explicit; replace all TODOs before considering export.

# 1. Article and sources ----------------------------------------------------
paper_id <- NA_character_             # Permanent registered paper ID
batch_id <- NA_character_             # YYYY-MM or YYYY-MM-external-NN
matrix_id <- NA_character_            # Assigned eligible sample/matrix ID
component_label <- NA_character_      # Publication label, e.g. Study 2
occasion_label <- NA_character_       # First eligible contemporaneous occasion
expected_n <- NA_integer_             # Source-reported reference analytic N
# TODO: DOI, source files/checksums, source pages and scoring references.

if (is.na(paper_id) || is.na(matrix_id)) {
  stop("Template only: replace IDs and complete the article-specific extraction.")
}

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
})

raw_dir <- file.path("data/3.meta_data/open_data/raw", paper_id)
individual_path <- file.path("data/3.meta_data/open_data/individual_data",
                             paste0(matrix_id, ".csv"))
matrix_dir <- "data/3.meta_data/matrices"
source("workflow.R")
source("R/extraction_helpers.R")
args <- extraction_arguments(commandArgs(trailingOnly = TRUE))
extraction_validate_scope(batch_id, paper_id, matrix_id, component_label)

# 2. Import verified originals ---------------------------------------------
sources <- extraction_originals(paper_id) # Checks coding/sources.csv and originals.
# raw_session1 <- readRDS(file.path(raw_dir, "<original_filename>"))
# raw_session2 <- readRDS(file.path(raw_dir, "<original_filename>"))
# Keep downloaded author code as a reference; inspect before adapting it.

# 3. Analytic sample --------------------------------------------------------
# TODO: Explicit linkage keys, duplicate checks, exclusions and occasion choice.
# data_analytic <- ...
# Document aggregate N before/after each exclusion. Do not filter for age alone.

# 4. Recoding and scoring ---------------------------------------------------
# data_scored <- data_analytic
# TODO: State missing-value codes, allowed missing items and any reversals.
# List the actual items for each score directly in this script.
# Use item means, preserving documented weights; turn all-missing NaN into NA.
# For questionnaire BESSI dual rating only: score_5 <- (score_9 + 1) / 2.
# TODO: Code age and sex/gender using verified source meanings.

# 5. Canonical variable names ----------------------------------------------
# TODO: Rename each scored measure explicitly to an existing/new codebook name.
# Retain original instrument/item mapping and explain broad construct mappings.
# Suffixes are attached directly: ...other, ...iat (see shared CSV registry).
# Resolve codebook additions before export; do not silently drop unknown names.

# 6. Scientific checks ------------------------------------------------------
# TODO: Confirm row/ID uniqueness, analytic N, source comparison, score ranges,
# BESSI questionnaire metric 1-5 and missingness. Explain N discrepancies.
# if (nrow(data_scored) != expected_n) stop("Reconcile analytic N with source.")
# Age missingness must not introduce a different sample at the export stage.

# 7. Correlations -----------------------------------------------------------
# TODO: List every intended matrix variable, including sex/gender if available.
# matrix_variables <- c(...)
# data_matrix <- data_scored[, matrix_variables, drop = FALSE]
# Validate names against the evolving codebook and suffix registry.
# stopifnot(!"age" %in% names(data_matrix))
# stopifnot(!anyDuplicated(names(data_matrix)))
# stopifnot(all(vapply(data_matrix, is.numeric, logical(1))))
# Stop and investigate all-missing or constant columns rather than dropping them.
# cor_matrix <- cor(data_matrix, method = "pearson", use = "pairwise.complete.obs")
# observed <- !is.na(as.matrix(data_matrix))
# pair_n <- crossprod(observed)
# Check symmetry, [-1, 1] range, diagonal and pair Ns; compare with source results.
# Diagnose any non-positive-semidefinite result without silently repairing it.

# 8. Export ----------------------------------------------------------------
# TODO: Export only after all scientific checks and codebook updates are complete.
# Preserve the same analytic participants in individual data and the matrix.
# individual_variables <- unique(c(matrix_variables, "age"))
# Retain only available, explicitly documented columns; no age-based row filter.
# checks <- as.data.frame(extraction_check_matrix(data_matrix, cor_matrix, pair_n))
# if (!"--check-only" %in% args) wf_compact_extraction(paper_id, matrix_id,
#   data_scored[, individual_variables, drop = FALSE], cor_matrix, pair_n, checks)
# This writes the common matrix CSV/XLSX, pair_n/<matrix_id>.csv and the cumulative
# extraction/summary.csv. Source provenance belongs in coding/sources.csv and
# scientific evidence in coding/evidence.csv; no extra per-article audit folders.
# Individual files remain local under Git-ignored open_data/, without personal IDs.
# Matrix creation does not itself promote coding into metadata/review_data.

stop("Template incomplete: implement and verify article-specific steps before export.")
