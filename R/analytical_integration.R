# Guarded bridge from validated monthly coding/extraction to legacy consumers.
integration_hash <- function(path) if (file.exists(path)) digest::digest(path, algo = "sha256", file = TRUE) else "absent"
integration_read <- function(path, columns) {
  if (!file.exists(path)) return(as.data.frame(setNames(rep(list(character()), length(columns)), columns)))
  x <- codebook_csv(path)
  if (!identical(names(x), columns)) stop("Integration registry contract differs: ", path, call. = FALSE)
  x
}
integration_batch_columns <- function() c("batch_id", "legacy_path", "legacy_sha256", "papers_path", "papers_sha256", "samples_path", "samples_sha256", "evidence_path", "evidence_sha256")
integration_matrix_columns <- function() c("batch_id", "paper_id", "matrix_id", "csv_path", "csv_sha256", "xlsx_path", "xlsx_sha256", "manifest_path", "manifest_sha256")

integration_validate_batch <- function(batch_id, base = "data/3.meta_data/coding") {
  # Rebuild into a temporary directory and compare the actual canonical tables.
  scratch <- tempfile("integration_validation_"); dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  inputs <- file.path(base, "artifacts", batch_id)
  dir.create(file.path(scratch, "artifacts", batch_id), recursive = TRUE)
  files <- list.files(inputs, pattern = "\\.json$", full.names = TRUE)
  if (!length(files) || !all(file.copy(files, file.path(scratch, "artifacts", batch_id)))) stop("Missing article coding artifacts.")
  if (!file.copy(file.path(base, "sample_id_registry.csv"), scratch)) stop("Missing sample registry.")
  fresh <- coding_build_batch(batch_id, scratch)
  for (table in c("legacy_export", "samples", "evidence")) {
    actual <- file.path(base, batch_id, paste0(table, ".csv"))
    if (integration_hash(actual) != integration_hash(file.path(scratch, batch_id, paste0(table, ".csv"))))
      stop("Stale or manually edited coding table; rebuild batch first: ", table, call. = FALSE)
  }
  if (nrow(fresh$papers)) fresh$papers$artifact_path <- gsub("\\\\", "/", file.path(base, "artifacts", batch_id, paste0(fresh$papers$paper_id, ".json")))
  expected <- file.path(scratch, "papers_verified.csv")
  readr::write_csv(fresh$papers, expected, na = "")
  if (integration_hash(expected) != integration_hash(file.path(base, batch_id, "papers.csv")))
    stop("Stale paper coding; rebuild batch first.", call. = FALSE)
  fresh
}

integration_verify_extraction <- function(paper_id, matrix_id, expected_n) {
  paper_directory <- file.path("data/3.meta_data/extraction", paper_id)
  directory <- file.path(paper_directory, matrix_id)
  manifest_path <- file.path(directory, "extraction_manifest.csv")
  if (!file.exists(manifest_path)) {
    # Compatibility with the single-sample October pilot, without mixing samples.
    legacy_path <- file.path(paper_directory, "extraction_manifest.csv")
    if (!file.exists(legacy_path)) return(NULL)
    legacy <- codebook_csv(legacy_path)
    if (!nrow(legacy) || !all(legacy$matrix_id == matrix_id)) return(NULL)
    directory <- paper_directory; manifest_path <- legacy_path
  }
  manifest <- codebook_csv(manifest_path)
  if (!all(manifest$paper_id == paper_id & manifest$matrix_id == matrix_id)) stop("Extraction manifest identity differs.")
  # Individual data stay local; their checksum is checked just like the matrix.
  for (i in seq_len(nrow(manifest))) if (integration_hash(manifest$path[[i]]) != manifest$sha256[[i]])
    stop("Extraction input/output changed; rerun article extraction: ", manifest$path[[i]], call. = FALSE)
  stem <- file.path(directory, "matrices", matrix_id)
  csv <- codebook_csv(paste0(stem, ".csv"))
  matrix <- as.matrix(readxl::read_excel(paste0(stem, ".xlsx")))
  if (!identical(csv[[1]], colnames(matrix)) || !identical(names(csv)[-1], colnames(matrix)) ||
      !isTRUE(all.equal(unname(matrix), unname(matrix(as.numeric(as.matrix(csv[-1])), nrow = nrow(csv))), tolerance = 1e-13)))
    stop("Staged CSV/XLSX matrix mismatch.", call. = FALSE)
  codebook_resolve_variables(colnames(matrix))
  if ("age" %in% colnames(matrix) || nrow(matrix) != ncol(matrix) ||
      !isTRUE(all.equal(matrix, t(matrix), check.attributes = FALSE)) ||
      any(abs(matrix) > 1 + 1e-12, na.rm = TRUE)) stop("Invalid staged matrix.")
  checks <- codebook_csv(file.path(directory, "checks.csv"))
  if (as.numeric(checks$value[match("n", checks$check)]) != expected_n) stop("Extraction/coding analytic N differs.")
  list(csv = paste0(stem, ".csv"), xlsx = paste0(stem, ".xlsx"), manifest = manifest_path)
}

integration_register_batch <- function(batch_id, directory = "data/3.meta_data/integration",
    matrix_dir = "data/3.meta_data/matrices", base = "data/3.meta_data/coding") {
  if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", batch_id)) stop("Invalid batch ID.")
  fresh <- integration_validate_batch(batch_id, base)
  batches_path <- file.path(directory, "batches.csv")
  matrices_path <- file.path(directory, "matrices.csv")
  batches <- integration_read(batches_path, integration_batch_columns())
  matrices <- integration_read(matrices_path, integration_matrix_columns())
  historical <- coding_read_basic_info()
  if (any(fresh$papers$paper_id %in% historical$paper_id)) stop("Historical overlap requires an explicit crosswalk.")
  for (other in batches$batch_id[batches$batch_id != batch_id]) {
    row <- batches[batches$batch_id == other, ]
    if (any(fresh$papers$paper_id %in% codebook_csv(row$papers_path)$paper_id)) stop("Paper already belongs to another integrated batch.")
  }
  old <- batches[batches$batch_id == batch_id, , drop = FALSE]
  if (nrow(old) && !setequal(codebook_csv(old$papers_path)$paper_id, fresh$papers$paper_id))
    stop("Integrated paper set changed; an explicit migration is required.")
  record <- data.frame(batch_id = batch_id, stringsAsFactors = FALSE)
  for (pair in list(c("legacy", "legacy_export"), c("papers", "papers"), c("samples", "samples"), c("evidence", "evidence"))) {
    record[[paste0(pair[[1]], "_path")]] <- file.path(base, batch_id, paste0(pair[[2]], ".csv"))
    record[[paste0(pair[[1]], "_sha256")]] <- integration_hash(record[[paste0(pair[[1]], "_path")]])
  }
  batches <- rbind(batches[batches$batch_id != batch_id, , drop = FALSE], record)
  scratch <- tempfile("integrate_"); dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  staged <- destinations <- hashes <- character()
  stage <- function(source, destination) {
    staged <<- c(staged, source); destinations <<- c(destinations, destination)
    hashes <<- c(hashes, integration_hash(destination))
  }
  pending <- character()
  for (i in seq_len(nrow(fresh$samples))) {
    sample <- fresh$samples[i, ]
    extraction <- integration_verify_extraction(sample$paper_id, sample$matrix_id, as.numeric(sample$n))
    if (is.null(extraction)) { pending <- c(pending, sample$matrix_id); next }
    previous <- matrices[matrices$matrix_id == sample$matrix_id, , drop = FALSE]
    entry <- data.frame(batch_id = batch_id, paper_id = sample$paper_id, matrix_id = sample$matrix_id)
    for (format in c("csv", "xlsx")) {
      target <- file.path(matrix_dir, paste0(sample$matrix_id, ".", format))
      if (file.exists(target)) {
        known <- if (nrow(previous)) previous[[paste0(format, "_sha256")]] else integration_hash(extraction[[format]])
        if (integration_hash(target) != known) stop("Active matrix has unregistered/manual changes: ", target)
      }
      entry[[paste0(format, "_path")]] <- target
      entry[[paste0(format, "_sha256")]] <- integration_hash(extraction[[format]])
      if (integration_hash(target) != entry[[paste0(format, "_sha256")]]) stage(extraction[[format]], target)
    }
    entry$manifest_path <- extraction$manifest
    entry$manifest_sha256 <- integration_hash(extraction$manifest)
    matrices <- rbind(matrices[matrices$matrix_id != sample$matrix_id, , drop = FALSE], entry)
  }
  registered <- matrices$matrix_id[matrices$batch_id == batch_id]
  if (any(!registered %in% fresh$samples$matrix_id)) stop("Previously integrated sample disappeared; explicit migration required.")
  for (item in list(list(batches, batches_path), list(matrices, matrices_path))) {
    temporary <- file.path(scratch, basename(item[[2]]))
    readr::write_csv(item[[1]], temporary, na = "")
    if (integration_hash(item[[2]]) != integration_hash(temporary)) stage(temporary, item[[2]])
  }
  codebook_commit_files(staged, destinations, hashes)
  invisible(list(batch = batch_id, papers = nrow(fresh$papers), samples = nrow(fresh$samples), pending = pending))
}

integration_current <- function(directory = "data/3.meta_data/integration") {
  legacy <- coding_read_basic_info()
  basic <- legacy
  review <- legacy[!duplicated(legacy$paper_id), , drop = FALSE]
  batches <- integration_read(file.path(directory, "batches.csv"), integration_batch_columns())
  matrices <- integration_read(file.path(directory, "matrices.csv"), integration_matrix_columns())
  if (anyDuplicated(batches$batch_id) || anyDuplicated(matrices$matrix_id)) stop("Duplicate integration registry entries.")
  provenance <- data.frame(paper_id = legacy$paper_id, matrix_id = legacy$matrix_id,
    origin = "historical", batch_id = NA_character_, eligibility_status = "historical_not_reassessed", stringsAsFactors = FALSE)
  for (i in seq_len(nrow(batches))) {
    entry <- batches[i, ]
    for (prefix in c("legacy", "papers", "samples", "evidence"))
      if (integration_hash(entry[[paste0(prefix, "_path")]]) != entry[[paste0(prefix, "_sha256")]])
        stop("Integrated coding changed; rerun guarded batch integration: ", entry$batch_id, call. = FALSE)
    papers <- codebook_csv(entry$papers_path)
    for (j in seq_len(nrow(papers))) if (integration_hash(papers$artifact_path[[j]]) != papers$artifact_sha256[[j]])
      stop("Integrated coding artifact changed; rebuild and reintegrate batch.")
    new <- codebook_csv(entry$legacy_path)
    if (!identical(names(new), names(legacy)) || any(new$paper_id %in% basic$paper_id)) stop("Duplicate paper or changed 31-field contract.")
    if (!nrow(new)) next # Valid monthly batch with no retained papers.
    for (field in names(new)) {
      if (inherits(legacy[[field]], "POSIXct")) new[[field]] <- as.POSIXct(new[[field]], format = "%Y-%m-%d", tz = "UTC")
      else if (is.numeric(legacy[[field]])) {
        number <- suppressWarnings(as.numeric(new[[field]]))
        if (any(!is.na(new[[field]]) & is.na(number))) stop("Invalid numeric compatibility field: ", field)
        new[[field]] <- number
      }
    }
    basic <- rbind(basic, new)
    eligible <- new$final_decision %in% c("review", "both")
    review <- rbind(review, new[eligible & !duplicated(new$paper_id), , drop = FALSE])
    provenance <- rbind(provenance, data.frame(paper_id = new$paper_id, matrix_id = new$matrix_id,
      origin = "monthly_coding", batch_id = entry$batch_id, eligibility_status = "full_text_v2_resolved"))
  }
  for (i in seq_len(nrow(matrices))) for (prefix in c("csv", "xlsx", "manifest"))
    if (integration_hash(matrices[[paste0(prefix, "_path")]][[i]]) != matrices[[paste0(prefix, "_sha256")]][[i]])
      stop("Integrated matrix or provenance changed; rerun integration.")
  if (anyDuplicated(basic$matrix_id)) stop("Duplicate combined matrix IDs.")
  attr(basic, "topic_corrections") <- attr(legacy, "topic_corrections")
  list(basic = basic, review = review, legacy = legacy, provenance = provenance, matrices = matrices)
}
