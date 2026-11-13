# Read-only historical verification. This module never promotes scientific data.
# Scores, sample selection and variable mapping belong in each article script.
audit_verify_sources <- function(expected) {
  for (path in names(expected)) if (!file.exists(path) ||
      !identical(digest::digest(file = path, algo = 'sha256'), unname(expected[[path]])))
    stop('Historical audit source absent or changed: ', path)
  invisible(names(expected))
}

audit_compare_matrix <- function(paper_id, matrix_id, data_matrix, reference_n,
                                 note, sources, evidence, status = 'REVIEW_REQUIRED',
                                 comparison = 'source_reconstruction') {
  stopifnot(!'age' %in% names(data_matrix), !anyDuplicated(names(data_matrix)),
    all(vapply(data_matrix, is.numeric, logical(1))))
  path <- file.path('data/3.meta_data/matrices', paste0(matrix_id, '.xlsx'))
  saved <- as.matrix(readxl::read_excel(path))
  stopifnot(nrow(saved) == ncol(saved))
  rownames(saved) <- colnames(saved)
  candidate <- cor(data_matrix, method = 'pearson', use = 'pairwise.complete.obs')
  pair_n <- crossprod(!is.na(as.matrix(data_matrix)))
  added <- setdiff(colnames(candidate), colnames(saved))
  removed <- setdiff(colnames(saved), colnames(candidate))
  common <- intersect(colnames(saved), colnames(candidate))
  a <- saved[common, common, drop = FALSE]
  b <- candidate[common, common, drop = FALSE]
  pairs <- upper.tri(a)
  mismatch <- sum(xor(is.na(a[pairs]), is.na(b[pairs])))
  delta <- abs(a[pairs] - b[pairs])
  changed <- sum(delta > 1e-10, na.rm = TRUE) + mismatch
  current <- readr::read_csv('data/3.meta_data/coding/records.csv',
    col_types = readr::cols(.default = 'c'))
  recorded_n <- as.numeric(current$n[match(matrix_id, current$matrix_id)])
  ids <- paste(sources, vapply(sources, function(p) digest::digest(file = p, algo = 'sha256'), character(1)), sep = '=')
  row <- data.frame(paper_id, matrix_id, comparison, reference_n, recorded_n,
    input_rows = nrow(data_matrix), variables = ncol(candidate), compared_pairs = sum(pairs),
    changed_pairs = changed, max_abs_delta = if (all(is.na(delta))) NA_real_ else max(delta, na.rm = TRUE),
    pair_n_min = min(pair_n[upper.tri(pair_n)]), pair_n_max = max(pair_n[upper.tri(pair_n)]),
    added_variables = paste(added, collapse = ';'), removed_variables = paste(removed, collapse = ';'),
    status, note, evidence, sources_sha256 = paste(ids, collapse = ';'),
    stringsAsFactors = FALSE)
  list(summary = row, data_matrix = data_matrix, candidate = candidate, pair_n = pair_n)
}

audit_save_summary <- function(rows) {
  # One cumulative comparison table, containing no individual observations.
  path <- 'data/3.meta_data/extraction/historical_audit.csv'
  old <- if (file.exists(path)) as.data.frame(readr::read_csv(path, show_col_types = FALSE)) else rows[FALSE, ]
  key <- function(x) paste(x$matrix_id, x$comparison)
  result <- dplyr::bind_rows(old[!key(old) %in% key(rows), ], rows)
  result <- result[order(result$matrix_id, result$comparison), ]
  readr::write_csv(result, path, na = '')
  invisible(path)
}
