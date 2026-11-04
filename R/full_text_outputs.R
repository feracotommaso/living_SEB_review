# Canonical outputs, safe rebuilds and migration of existing operational data.

ft_save_workbook <- function(wb, path) {
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  # openxlsx 4.2.x can emit relationships to nonexistent default drawing/VML/
  # printer parts even for plain tables. Remove only these absent default parts
  # and their corresponding references; reject any other broken internal link.
  scratch <- tempfile("full_text_xlsx_", tmpdir = tempdir())
  dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = scratch)
  changed <- FALSE
  rels <- list.files(scratch, pattern = "\\.rels$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  ns <- c(r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
  for (rel_path in rels) {
    doc <- xml2::read_xml(rel_path)
    source_dir <- dirname(dirname(rel_path))
    source_path <- file.path(source_dir, sub("\\.rels$", "", basename(rel_path)))
    if (basename(rel_path) == ".rels") source_dir <- scratch
    source <- if (file.exists(source_path) && !dir.exists(source_path)) xml2::read_xml(source_path) else NULL
    rel_changed <- FALSE
    for (rel in xml2::xml_find_all(doc, "//*[local-name()='Relationship']")) {
      if (identical(xml2::xml_attr(rel, "TargetMode"), "External")) next
      target <- xml2::xml_attr(rel, "Target")
      if (file.exists(file.path(source_dir, target))) next
      type <- sub(".*/", "", xml2::xml_attr(rel, "Type"))
      if (!type %in% c("drawing", "vmlDrawing", "printerSettings")) stop("Broken XLSX relationship: ", target, call. = FALSE)
      id <- xml2::xml_attr(rel, "Id")
      if (!is.null(source)) for (node in xml2::xml_find_all(source, paste0("//*[@r:id='", id, "']"), ns)) {
        if (xml2::xml_name(node) == "pageSetup") xml2::xml_attr(node, "r:id", ns) <- NULL
        else xml2::xml_remove(node)
      }
      xml2::xml_remove(rel)
      rel_changed <- TRUE
    }
    if (rel_changed) {
      xml2::write_xml(doc, rel_path)
      if (!is.null(source)) xml2::write_xml(source, source_path)
      changed <- TRUE
    }
  }
  if (changed) zip::zipr(normalizePath(path, winslash = "/"),
    files = list.files(scratch, recursive = TRUE, all.files = TRUE), root = scratch, mode = "mirror")
  invisible(path)
}

ft_fit_wrapped_rows <- function(wb, sheet, data, widths, minimum = 24) {
  if (!nrow(data)) return(invisible(NULL))
  widths[is.na(widths)] <- 26
  lines <- vapply(seq_along(data), function(j) {
    pmax(1, ceiling(nchar(ft_blank_to_empty(data[[j]])) / pmax(8, widths[[j]] - 3) * 1.15))
  }, numeric(nrow(data)))
  if (is.null(dim(lines))) lines <- matrix(lines, nrow = nrow(data))
  heights <- pmin(409, pmax(minimum, apply(lines, 1, max) * 15 + 8))
  openxlsx::setRowHeights(wb, sheet, rows = seq_len(nrow(data)) + 1L, heights = heights)
}

ft_split_values <- function(x) {
  x <- ft_first_nonblank(x)
  if (!nzchar(x)) return(list())
  as.list(trimws(strsplit(x, ";", fixed = TRUE)[[1]]))
}

ft_row_to_object <- function(row) {
  schema <- jsonlite::fromJSON(file.path("schemas", "full_text_eligibility_decision.schema.json"), simplifyVector = FALSE)
  nullable <- c("review_exclusion_reason", "meta_exclusion_reason", "meta_pending_reason", "final_route",
                "human_review_summary", "ai_recommendation")
  arrays <- c("framework_relation_tags", "eligible_quantitative_components", "human_review_reasons", "decision_options")
  booleans <- c("review_eligible", "language_ai_assessed", "requires_human_review")
  obj <- list()
  for (key in names(schema$properties)) {
    source_key <- if (key == "human_review_summary") "issue_summary" else key
    value <- if (source_key %in% names(row)) row[[source_key]][[1]] else ""
    if (key %in% c("review_evidence", "meta_evidence")) {
      pages <- ft_first_nonblank(row[[paste0(key, "_pages")]])
      # Preserve the exact existing evidence text and page list; do not infer
      # new page-level claims when importing a flattened historical assessment.
      value <- list(list(evidence = ft_first_nonblank(value), page = NULL,
                         locator = if (nzchar(pages)) paste0("Recorded PDF pages: ", pages) else NULL))
    } else if (key %in% booleans) {
      value <- ft_bool(value)[[1]]
    } else if (key %in% arrays) {
      value <- ft_split_values(value)
    } else {
      value <- ft_first_nonblank(value)
      if (key %in% nullable && !nzchar(value)) value <- NULL
    }
    obj[key] <- list(value)
  }
  if (!nzchar(obj$workflow_version)) obj$workflow_version <- ft_workflow_version
  obj
}

ft_validate_human_queue <- function(human, decisions) {
  if (!all(ft_human_review_columns() %in% names(human))) stop("Human-review columns missing.", call. = FALSE)
  if (anyDuplicated(human$paper_id)) stop("Duplicate paper_id in human review.", call. = FALSE)
  idx <- match(human$paper_id, decisions$paper_id)
  if (anyNA(idx) || any(human$screening_record_id != decisions$screening_record_id[idx])) {
    stop("Human-review identity mismatch with decisions.", call. = FALSE)
  }
  if (any(!toupper(ft_blank_to_empty(human$adjudication_status)) %in% c("OPEN", "RESOLVED"))) stop("Invalid adjudication_status.", call. = FALSE)
  open <- human$adjudication_status == "OPEN"
  if (any(open & !ft_bool(decisions$requires_human_review[idx]))) stop("Open adjudication must correspond to unresolved human review.", call. = FALSE)
  if (any(open & (!nzchar(ft_blank_to_empty(human$issue_summary)) |
                  !nzchar(ft_blank_to_empty(human$ai_recommendation)) |
                  !nzchar(ft_blank_to_empty(human$decision_options))))) stop("Human-review evidence packet is incomplete.", call. = FALSE)
  missing <- setdiff(decisions$paper_id[ft_bool(decisions$requires_human_review)], human$paper_id)
  if (length(missing)) stop("Human-review queue missing papers: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(human)
}

ft_preserve_human_review <- function(queue, previous, decisions) {
  if (is.null(previous)) return(queue)
  ft_validate_human_queue(previous, decisions)
  queue[] <- lapply(queue, ft_blank_to_empty)
  previous[] <- lapply(previous, ft_blank_to_empty)
  # Preserve open edits and resolved evidence packets exactly, matched by ID.
  dplyr::bind_rows(previous, queue[!queue$paper_id %in% previous$paper_id, , drop = FALSE])
}

ft_decision_codebook <- function() {
  base <- ft_base_decision_codebook()
  schema <- jsonlite::fromJSON(file.path("schemas", "full_text_eligibility_decision.schema.json"), simplifyVector = FALSE)
  fields <- unique(c(ft_decision_columns(), ft_human_review_columns()))
  description <- base$description[match(fields, base$field)]
  description[is.na(description)] <- vapply(fields[is.na(description)], function(key) {
    switch(key,
      workflow_version = "Deterministic full-text implementation version used to generate this output.",
      eligibility_policy_version = "Scientific eligibility policy version; must match the versioned schema.",
      decision_file = "Input decision artifact path; retain together with its SHA-256 checksum.",
      decision_sha256 = "SHA-256 checksum of the exact input JSON artifact.",
      provenance_note = "Origin and migration limitations of the decision; never represents a new PDF assessment.",
      adjudication_date = "Date recorded by the adjudicator (YYYY-MM-DD); do not invent missing historical dates.",
      screening_date = "Original assessment date (YYYY-MM-DD), preserved during migration and rebuilds.",
      human_meta_pending_reason = "Required operational reason when adjudicated meta status is pending.",
      paper_id = "Persistent report identifier, stored as text with at least four digits; never recycled.",
      screening_record_id = "Exact upstream bibliographic screening identifier; preserved verbatim.",
      "Recorded assessment/provenance field. Human-prefixed inputs are editable only in human_review; ai-prefixed fields preserve the original assessment.")
  }, character(1))
  allowed <- vapply(fields, function(key) {
    canonical <- sub("^(human_|ai_)", "", key)
    property <- schema$properties[[canonical]]
    if (!is.null(property$enum)) return(paste(vapply(property$enum, function(v) if (is.null(v)) "blank" else as.character(v), character(1)), collapse = "; "))
    if (identical(property$type, "boolean")) return("TRUE; FALSE")
    if (!is.null(property$items$enum)) return(paste(unlist(property$items$enum), collapse = "; "))
    if (!is.null(property$const)) return(as.character(property$const))
    "See policy/schema; multiple values use semicolons in CSV/XLSX."
  }, character(1))
  data.frame(field = fields, description = description, allowed_values = allowed)
}

ft_validate_adjudication <- function(row) {
  schema <- jsonlite::fromJSON(file.path("schemas", "full_text_adjudication.schema.json"), simplifyVector = FALSE)
  obj <- list()
  for (key in names(schema$properties)) {
    value <- ft_first_nonblank(row[[key]])
    if (key == "human_review_eligible") value <- ft_bool(value)[[1]]
    else if (key %in% c("human_eligible_quantitative_components", "human_framework_relation_tags")) value <- ft_split_values(value)
    else if (!nzchar(value) && key != "adjudication_note") value <- NULL
    obj[key] <- list(value)
  }
  errors <- ft_schema_errors(obj, schema)
  if (length(errors)) stop("Invalid adjudication: ", paste(errors, collapse = "; "), call. = FALSE)
  invisible(obj)
}

ft_snapshot_paths <- function(workbook_path) {
  stem <- sub("_full_text_decisions\\.xlsx$", "", workbook_path)
  list(decisions = paste0(stem, "_full_text_decisions.csv"),
       human_review = paste0(stem, "_full_text_human_review.csv"))
}

ft_read_canonical_decisions <- function(path) {
  data <- ft_read_table(path)
  data[] <- lapply(data, ft_blank_to_empty)
  for (key in c("review_eligible", "requires_human_review", "language_ai_assessed")) data[[key]] <- ft_bool(data[[key]])
  data
}

ft_assert_same_table <- function(x, y, key, message) {
  if (!setequal(names(x), names(y)) || nrow(x) != nrow(y) || anyDuplicated(y[[key]]) || !setequal(x[[key]], y[[key]])) stop(message, call. = FALSE)
  y <- y[match(x[[key]], y[[key]]), names(x), drop = FALSE]
  if (!all(vapply(names(x), function(col) identical(ft_blank_to_empty(x[[col]]), ft_blank_to_empty(y[[col]])), logical(1)))) stop(message, call. = FALSE)
  invisible(TRUE)
}

ft_validate_queue_edits <- function(previous, edited) {
  immutable <- ft_human_review_columns()[seq_len(match("ai_framework_relation_tags", ft_human_review_columns()))]
  ft_assert_same_table(previous[, immutable, drop = FALSE], edited[, immutable, drop = FALSE], "paper_id",
    "Human-review audit rows or original evidence were removed/edited. Preserve all rows and edit only human_* adjudication inputs, note, reviewer, date and status.")
  invisible(edited)
}

ft_write_monthly_outputs <- function(decisions, human_review, path, batch_id) {
  decisions <- ft_validate_ai_decisions(decisions, decisions)
  ft_validate_human_queue(human_review, decisions)
  decisions$workflow_version <- ft_workflow_version
  ft_write_decision_workbook(decisions, human_review, path, batch_id)
  paths <- ft_snapshot_paths(path)
  ft_atomic_write_csv(decisions, paths$decisions)
  ft_atomic_write_csv(human_review, paths$human_review)
  invisible(paths)
}

ft_rebuild_decisions <- function(manifest, ai, batch_id, workbook_path) {
  decisions <- ft_merge_manifest_decisions(manifest, ai, batch_id)
  queue <- ft_build_human_review_queue(decisions, ai)
  paths <- ft_snapshot_paths(workbook_path)
  if (file.exists(workbook_path)) {
    if (!file.exists(paths$human_review)) stop("Existing workbook has no canonical audit CSV. Import it explicitly with Step 00 first.", call. = FALSE)
    previous <- ft_read_table(paths$human_review)
    previous[] <- lapply(previous, ft_blank_to_empty)
    workbook <- ft_read_decision_workbook(workbook_path)
    ft_assert_same_table(previous, workbook$human_review, "paper_id",
                         "Unfinalized workbook edits detected. Run Step 03 before rebuilding.")
    queue <- ft_preserve_human_review(queue, previous, decisions)
  } else if (file.exists(paths$human_review)) {
    previous <- ft_read_table(paths$human_review)
    previous[] <- lapply(previous, ft_blank_to_empty)
    queue <- ft_preserve_human_review(queue, previous, decisions)
  }
  ft_apply_adjudications(decisions, queue)
}

ft_legacy_routing <- function(decisions) {
  decisions <- ft_validate_ai_decisions(decisions, decisions)
  if (any(!decisions$meta_status %in% c("eligible", "ineligible")) || any(decisions$requires_human_review)) {
    stop("Legacy routing export requires all decisions to be resolved; pending/uncertain cannot be final.", call. = FALSE)
  }
  decisions$final_decision <- decisions$final_route
  decisions
}

ft_import_existing_workbook <- function(path, decision_dir, batch_id) {
  paths <- ft_snapshot_paths(path)
  if (dir.exists(decision_dir) || any(file.exists(unlist(paths)))) stop("Import is only for an existing workbook without canonical inputs; refusing overwrite.", call. = FALSE)
  old <- ft_read_decision_workbook(path)
  decisions <- old$decisions
  decisions$workflow_version <- ft_workflow_version
  decisions$provenance_note <- paste0("Imported from existing operational workbook ", basename(path),
    "; original decision source/date and recorded evidence retained. PDFs were not re-screened during migration.")
  for (key in c("issue_summary", "ai_recommendation", "decision_options")) decisions[[key]] <- ""
  for (i in seq_len(nrow(decisions))) {
    j <- match(decisions$paper_id[[i]], old$human_review$paper_id)
    if (!is.na(j)) for (key in c("issue_summary", "ai_recommendation", "decision_options")) decisions[[key]][[i]] <- old$human_review[[key]][[j]]
  }
  objects <- lapply(seq_len(nrow(decisions)), function(i) ft_row_to_object(decisions[i, , drop = FALSE]))
  for (obj in objects) ft_validate_decision_object(obj)
  ft_validate_human_queue(old$human_review, decisions)
  dir.create(decision_dir, recursive = TRUE, showWarnings = FALSE)
  for (obj in objects) jsonlite::write_json(obj, file.path(decision_dir, paste0(obj$paper_id, ".json")),
                                          auto_unbox = TRUE, pretty = TRUE, null = "null")
  # Re-import from the persisted artifacts so hashes and provenance are real.
  ai <- ft_read_ai_decisions(decision_dir)
  manifest <- decisions[, c("screening_record_id", "paper_id", "doi", "title", "pdf_path")]
  fresh <- ft_merge_manifest_decisions(manifest, ai, batch_id)
  result <- ft_apply_adjudications(fresh, old$human_review)
  ft_write_monthly_outputs(result$decisions, result$human_review, path, batch_id)
}
