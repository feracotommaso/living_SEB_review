# Shared technical checks only. Scientific scoring stays in the article script.
extraction_arguments <- function(args) {
  if (any(!args %in% "--check-only")) stop("Only --check-only is accepted.", call. = FALSE)
  args
}

extraction_validate_scope <- function(batch_id, paper_id, matrix_id, component_label) {
  eligibility <- codebook_csv(file.path("data/2.full_abstracting", paste0(batch_id, "_full_text_decisions.csv")))
  registry <- codebook_csv(if (file.exists("data/3.meta_data/coding/records.csv"))
    "data/3.meta_data/coding/records.csv" else "data/3.meta_data/coding/sample_id_registry.csv")
  hit <- which(eligibility$paper_id == paper_id)
  assigned <- registry$paper_id == paper_id & registry$matrix_id == matrix_id & registry$component_label == component_label
  if (length(hit) != 1L || !eligibility$final_route[hit] %in% c("both", "meta") || !any(assigned) ||
      !component_label %in% trimws(strsplit(eligibility$eligible_quantitative_components[hit], ";", fixed = TRUE)[[1]]))
    stop("Extraction identity/component differs from resolved eligibility or sample registry.", call. = FALSE)
}

extraction_originals <- function(paper_id) {
  x <- codebook_csv("data/3.meta_data/coding/sources.csv")
  x <- x[x$paper_id == paper_id & !x$role %in% c("legacy_derived_evidence", "coding_evidence"), , drop = FALSE]
  if (!nrow(x)) stop("Registra le fonti originali nel foglio fonti: ", paper_id)
  for (i in seq_len(nrow(x))) if (!file.exists(x$path[i]) ||
      digest::digest(file = x$path[i], algo = "sha256") != x$sha256[i]) stop("Source absent or changed: ", x$path[i])
  x
}

extraction_check_item_links <- function(analysis, raw, key, items = character()) {
  if (anyDuplicated(raw$CASE)) stop("Duplicate source CASE identifiers.", call. = FALSE)
  link <- match(analysis[[key]], raw$CASE)
  if (!length(items) && anyNA(link)) stop("Missing source session links.", call. = FALSE)
  for (name in items) if (!identical(as.numeric(analysis[[name]]), as.numeric(raw[[name]][link])))
    stop("Source item differs after linkage: ", name, call. = FALSE)
  sum(!is.na(link))
}

extraction_item_mean <- function(x, min_response = 0.8, weights = rep(1, ncol(x))) {
  x <- as.matrix(x)
  stopifnot(length(weights) == ncol(x), all(weights > 0))
  available <- !is.na(x)
  denominator <- rowSums(sweep(available, 2, weights, "*"))
  value <- rowSums(sweep(x, 2, weights, "*"), na.rm = TRUE) / denominator
  value[rowSums(available) < ceiling(ncol(x) * min_response) | denominator == 0] <- NA_real_
  unname(value)
}

extraction_verify_sources <- function(path, paper_id) {
  manifest <- codebook_csv(path)
  required <- c("paper_id", "filename", "local_path", "sha256", "retrieved_at_utc")
  if (!all(required %in% names(manifest)) || anyDuplicated(manifest$filename) ||
      any(manifest$paper_id != paper_id) || any(codebook_blank(manifest$retrieved_at_utc)))
    stop("Invalid source manifest.", call. = FALSE)
  for (i in seq_len(nrow(manifest))) {
    file <- manifest$local_path[[i]]
    if (!file.exists(file) || !identical(digest::digest(file, algo = "sha256", file = TRUE), manifest$sha256[[i]]))
      stop("Source absent or changed: ", file, call. = FALSE)
  }
  invisible(manifest)
}

extraction_check_matrix <- function(data_matrix, cor_matrix, pair_n) {
  if (ncol(data_matrix) < 2L || nrow(data_matrix) < 2L || anyDuplicated(names(data_matrix)) ||
      "age" %in% names(data_matrix) || !all(vapply(data_matrix, is.numeric, logical(1))))
    stop("Invalid matrix inputs: check age, names, dimensions and numeric types.", call. = FALSE)
  codebook_resolve_variables(names(data_matrix))
  if (any(vapply(data_matrix, function(x) any(!is.na(x) & !is.finite(x)) ||
      sum(!is.na(x)) < 2L || is.na(sd(x, na.rm = TRUE)) || sd(x, na.rm = TRUE) == 0, logical(1))))
    stop("Nonfinite, constant or insufficiently observed matrix variable.", call. = FALSE)
  for (x in list(cor_matrix, pair_n)) {
    if (!identical(colnames(x), names(data_matrix)) || !identical(rownames(x), names(data_matrix)) ||
        !isTRUE(all.equal(x, t(x), check.attributes = TRUE))) stop("Matrix names or symmetry differ.", call. = FALSE)
  }
  if (any(abs(cor_matrix) > 1 + 1e-12, na.rm = TRUE) ||
      any(!is.na(cor_matrix) & !is.finite(cor_matrix)) || any(abs(diag(cor_matrix) - 1) > 1e-12))
    stop("Invalid correlation range/diagonal.", call. = FALSE)
  expected_n <- crossprod(!is.na(as.matrix(data_matrix)))
  if (!identical(pair_n, expected_n)) stop("Pair-specific Ns do not match input observations.", call. = FALSE)
  if (any(!is.na(cor_matrix) & pair_n < 2L)) stop("Correlation with fewer than two observations.", call. = FALSE)
  minimum_eigenvalue <- if (anyNA(cor_matrix)) NA_real_ else min(eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values)
  list(n = nrow(data_matrix), variables = ncol(data_matrix),
    minimum_pair_n = min(pair_n[upper.tri(pair_n)]), maximum_pair_n = max(pair_n[upper.tri(pair_n)]),
    minimum_eigenvalue = minimum_eigenvalue)
}

extraction_export <- function(paper_id, matrix_id, individual_data, cor_matrix,
    pair_n, variable_map, checks, decisions,
    audit_dir = file.path("data/3.meta_data/extraction", paper_id, matrix_id)) {
  if (file.exists("data/3.meta_data/coding/records.csv")) {
    source("R/workflow_io.R")
    source("R/workflow_results.R")
    return(wf_compact_extraction(paper_id, matrix_id, individual_data, cor_matrix, pair_n, checks))
  }
  # Matrices are staged here until the separate analytical integration step.
  matrix_dir <- file.path(audit_dir, "matrices")
  paths <- c(file.path("data/3.meta_data/open_data/individual_data", paste0(matrix_id, ".csv")),
    file.path(matrix_dir, paste0(matrix_id, c(".csv", ".xlsx"))),
    file.path(audit_dir, c("pair_correlations.csv", "variable_map.csv", "score_summary.csv", "checks.csv", "scientific_decisions.csv", "extraction_manifest.csv")))
  hashes <- vapply(paths, function(p) if (file.exists(p)) digest::digest(p, algo = "sha256", file = TRUE) else "absent", character(1))
  scratch <- tempfile("extraction_"); dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  # Individual and matrix CSV deliberately share a basename in different folders.
  # Give each staged file a distinct name so one cannot overwrite the other.
  staged <- file.path(scratch, paste0(seq_along(paths), "_", basename(paths)))
  readr::write_csv(individual_data, staged[[1]], na = "")
  restored_individual <- readr::read_csv(staged[[1]], show_col_types = FALSE)
  if (nrow(restored_individual) != nrow(individual_data) ||
      !identical(names(restored_individual), names(individual_data)) ||
      !isTRUE(all.equal(as.matrix(restored_individual), as.matrix(individual_data), check.attributes = FALSE, tolerance = 1e-14)))
    stop("Individual CSV round-trip differs.", call. = FALSE)
  labelled <- data.frame(variable = rownames(cor_matrix), cor_matrix, check.names = FALSE)
  readr::write_csv(labelled, staged[[2]], na = "")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "matrix")
  openxlsx::writeData(wb, "matrix", as.data.frame(cor_matrix))
  openxlsx::setColWidths(wb, "matrix", cols = seq_len(ncol(cor_matrix)), widths = 18)
  openxlsx::setRowHeights(wb, "matrix", rows = 1, heights = 65)
  openxlsx::addStyle(wb, "matrix", openxlsx::createStyle(wrapText = TRUE, valign = "center", textDecoration = "bold"),
    rows = 1, cols = seq_len(ncol(cor_matrix)), gridExpand = TRUE)
  openxlsx::addStyle(wb, "matrix", openxlsx::createStyle(numFmt = "0.0000"),
    rows = seq_len(nrow(cor_matrix)) + 1L, cols = seq_len(ncol(cor_matrix)), gridExpand = TRUE)
  openxlsx::freezePane(wb, "matrix", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, staged[[3]], overwrite = TRUE)
  restored <- as.matrix(readxl::read_excel(staged[[3]]))
  if (!identical(colnames(restored), colnames(cor_matrix)) ||
      !isTRUE(all.equal(unname(restored), unname(cor_matrix), tolerance = 1e-14))) stop("Matrix XLSX round-trip differs.")
  pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  correlations <- data.frame(matrix_id = matrix_id, var1 = rownames(cor_matrix)[pairs[, 1]],
    var2 = colnames(cor_matrix)[pairs[, 2]], r = cor_matrix[pairs], n = pair_n[pairs])
  readr::write_csv(correlations, staged[[4]], na = "")
  readr::write_csv(variable_map, staged[[5]], na = "")
  summary <- do.call(rbind, lapply(names(individual_data), function(name) {
    x <- individual_data[[name]]; observed <- x[!is.na(x)]
    data.frame(variable = name, n = length(observed), missing = sum(is.na(x)),
      mean = if (length(observed)) mean(observed) else NA_real_, sd = sd(observed),
      min = if (length(observed)) min(observed) else NA_real_, max = if (length(observed)) max(observed) else NA_real_)
  }))
  readr::write_csv(summary, staged[[6]], na = "")
  readr::write_csv(data.frame(matrix_id = matrix_id, check = names(checks),
    value = vapply(checks, as.character, character(1))), staged[[7]], na = "")
  readr::write_csv(decisions, staged[[8]], na = "")
  inputs <- c(file.path("scripts/extraction/articles", paste0(paper_id, ".R")),
    file.path("scripts/extraction/keys", paste0(paper_id, ".R")),
    file.path("config/extraction", paste0(paper_id, "_decisions.csv")),
    "R/extraction_helpers.R", "R/codebook_workflow.R", "policies/extraction_conventions.md",
    "data/matrix_codebook.csv", "config/coding/variable_suffixes.csv", file.path("data/3.meta_data/extraction", paper_id, "source_manifest.csv"))
  optional <- c(file.path("scripts/extraction/keys", paste0(paper_id, ".R")), file.path("config/extraction", paste0(paper_id, "_decisions.csv")))
  inputs <- inputs[!inputs %in% optional | file.exists(inputs)]
  provenance <- data.frame(paper_id = paper_id, matrix_id = matrix_id,
    role = c(rep("input", length(inputs)), rep("output", 8L)),
    path = c(inputs, paths[1:8]), sha256 = vapply(c(inputs, staged[1:8]), function(p)
      digest::digest(p, algo = "sha256", file = TRUE), character(1)), stringsAsFactors = FALSE)
  readr::write_csv(provenance, staged[[9]], na = "")
  codebook_commit_files(staged, paths, hashes)
  invisible(paths)
}
