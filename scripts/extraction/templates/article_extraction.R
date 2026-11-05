# Article extraction template. Copy to scripts/extraction/articles/<paper_id>.R.
# Read policies/extraction_conventions.md. Run from the repository root.
# Keep scientific steps explicit; replace all TODOs before considering export.

# 1. Article and sources ----------------------------------------------------
paper_id <- NA_character_             # Permanent registered paper ID
batch_id <- NA_character_             # Monthly publication batch, YYYY-MM
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
audit_dir <- file.path("data/3.meta_data/extraction", paper_id, matrix_id)
# Sample-specific audits prevent one sample from overwriting another sample's
# checks, pair Ns or extraction manifest. Acquisition manifests remain per paper.

# 2. Import verified originals ---------------------------------------------
# TODO: Verify source_manifest.csv checksums before reading local files.
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
# readr::write_csv(data_scored[, individual_variables, drop = FALSE], individual_path)
# Save cor_matrix as CSV with explicit row labels and square legacy XLSX.
# Save pair_n and aggregate/provenance checks in audit_dir, without observations
# or participant IDs. Individual files remain local under Git-ignored open_data/.
# Matrix creation does not itself promote coding into metadata/review_data.

stop("Template incomplete: implement and verify article-specific steps before export.")
