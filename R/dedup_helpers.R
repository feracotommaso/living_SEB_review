# Helper functions for Living SEB bibliographic import, normalisation,
# deduplication, validation, comparison, and audit logging.

normalise_path_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

absolute_path <- function(path, root = here::here()) {
  path <- as.character(path)
  is_absolute <- grepl("^([A-Za-z]:[\\\\/]|/)", path)
  path[!is_absolute] <- file.path(root, path[!is_absolute])

  normalise_path_slashes(
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

relative_to_project <- function(path, root = here::here()) {
  root_abs <- normalise_path_slashes(
    normalizePath(root, winslash = "/", mustWork = TRUE)
  )
  path_abs <- normalise_path_slashes(
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )

  prefix <- paste0(root_abs, "/")
  ifelse(
    startsWith(path_abs, prefix),
    substring(path_abs, nchar(prefix) + 1L),
    path_abs
  )
}

safe_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  x[!nzchar(x)] <- "file"
  x
}

list_bibliography_files <- function(directory) {
  if (!dir.exists(directory)) {
    stop("Directory not found: ", directory, call. = FALSE)
  }

  files <- list.files(
    directory,
    pattern = "\\.(bib|ris)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  files <- sort(unique(files))

  if (length(files) == 0L) {
    stop(
      "No .bib or .ris files found in: ", directory,
      call. = FALSE
    )
  }

  files
}

detect_database <- function(path) {
  text <- tolower(normalise_path_slashes(path))

  if (stringr::str_detect(text, "scopus")) {
    return("Scopus")
  }

  if (stringr::str_detect(
    text,
    "(^|[^a-z])(wos|web[ _-]*of[ _-]*science|webofscience|savedrecs)([^a-z]|$)"
  )) {
    return("Web of Science")
  }

  NA_character_
}

detect_batch_id <- function(path, default = "historical") {
  text <- normalise_path_slashes(path)
  filename <- basename(text)

  four_digit <- stringr::str_match(
    text,
    "(20\\d{2})[-_/](0[1-9]|1[0-2])"
  )

  if (!is.na(four_digit[1L, 1L])) {
    return(paste(four_digit[1L, 2L], four_digit[1L, 3L], sep = "-"))
  }

  compact <- stringr::str_match(
    text,
    "(20\\d{2})(0[1-9]|1[0-2])"
  )

  if (!is.na(compact[1L, 1L])) {
    return(paste(compact[1L, 2L], compact[1L, 3L], sep = "-"))
  }

  two_digit <- stringr::str_match(
    filename,
    "(?:^|[^0-9])(\\d{2})[-_](0[1-9]|1[0-2])(?:[-_]\\d{2})?(?:[^0-9]|$)"
  )

  if (!is.na(two_digit[1L, 1L])) {
    year <- paste0("20", two_digit[1L, 2L])
    return(paste(year, two_digit[1L, 3L], sep = "-"))
  }

  parent <- basename(dirname(text))
  parent_match <- stringr::str_match(
    parent,
    "^(20\\d{2})[-_](0[1-9]|1[0-2])$"
  )

  if (!is.na(parent_match[1L, 1L])) {
    return(paste(parent_match[1L, 2L], parent_match[1L, 3L], sep = "-"))
  }

  default
}

make_file_manifest <- function(files, root = here::here()) {
  tibble::tibble(
    path = relative_to_project(files, root),
    database = vapply(
      files,
      detect_database,
      FUN.VALUE = character(1)
    ),
    batch_id = vapply(
      files,
      detect_batch_id,
      FUN.VALUE = character(1)
    )
  )
}

resolve_manifest_paths <- function(manifest, root = here::here()) {
  absolute_path(manifest$path, root)
}

validate_file_manifest <- function(
  manifest,
  root = here::here(),
  expected_batch = NULL
) {
  required <- c("path", "database", "batch_id")
  missing_columns <- setdiff(required, names(manifest))

  if (length(missing_columns) > 0L) {
    stop(
      "Manifest is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(manifest) == 0L) {
    stop("Manifest has no rows.", call. = FALSE)
  }

  manifest$path <- trimws(as.character(manifest$path))
  manifest$database <- trimws(as.character(manifest$database))
  manifest$batch_id <- trimws(as.character(manifest$batch_id))

  if (anyNA(manifest$path) || any(!nzchar(manifest$path))) {
    stop("Manifest contains an empty path.", call. = FALSE)
  }

  external_types <- if (any(startsWith(manifest$database[!is.na(manifest$database)], "External:")))
    readr::read_csv(file.path(root, "config", "external_source_types.csv"), show_col_types = FALSE)$code else character()
  allowed_databases <- c("Scopus", "Web of Science", paste0("External:", external_types))
  invalid_database <- is.na(manifest$database) |
    !manifest$database %in% allowed_databases

  if (any(invalid_database)) {
    bad_rows <- which(invalid_database)
    stop(
      "Complete or correct the database column in rows: ",
      paste(bad_rows, collapse = ", "),
      ". Use Scopus, Web of Science or a registered External:<source_type>.",
      call. = FALSE
    )
  }

  if (anyNA(manifest$batch_id) || any(!nzchar(manifest$batch_id))) {
    stop("Manifest contains an empty batch_id.", call. = FALSE)
  }

  if (!is.null(expected_batch)) {
    wrong_batch <- unique(
      manifest$batch_id[manifest$batch_id != expected_batch]
    )

    if (length(wrong_batch) > 0L) {
      stop(
        "All current-update files must have batch_id = '",
        expected_batch,
        "'. Found: ",
        paste(wrong_batch, collapse = ", "),
        call. = FALSE
      )
    }
  }

  resolved_paths <- resolve_manifest_paths(manifest, root)
  missing_files <- resolved_paths[!file.exists(resolved_paths)]

  if (length(missing_files) > 0L) {
    stop(
      "Manifest references missing files:\n",
      paste(missing_files, collapse = "\n"),
      call. = FALSE
    )
  }

  if (anyDuplicated(resolved_paths)) {
    stop("Manifest contains duplicated file paths.", call. = FALSE)
  }

  resolved_paths
}


validate_manifest_file_set <- function(manifest, discovered_files, root = here::here()) {
  manifest_paths <- sort(unique(resolve_manifest_paths(manifest, root)))
  discovered_paths <- sort(unique(
    normalise_path_slashes(
      normalizePath(discovered_files, winslash = "/", mustWork = TRUE)
    )
  ))

  missing_from_manifest <- setdiff(discovered_paths, manifest_paths)
  absent_from_directory <- setdiff(manifest_paths, discovered_paths)

  if (length(missing_from_manifest) > 0L || length(absent_from_directory) > 0L) {
    message_parts <- c(
      if (length(missing_from_manifest) > 0L) {
        paste0(
          "Files present in the input directory but absent from the manifest:\n",
          paste(missing_from_manifest, collapse = "\n")
        )
      },
      if (length(absent_from_directory) > 0L) {
        paste0(
          "Files referenced by the manifest but absent from the input directory:\n",
          paste(absent_from_directory, collapse = "\n")
        )
      }
    )

    stop(
      paste(message_parts, collapse = "\n\n"),
      "\nDelete the stale manifest and rerun the prepare script.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

year_month_index <- function(x) {
  valid <- grepl("^20[0-9]{2}-(0[1-9]|1[0-2])$", x)
  result <- rep(NA_integer_, length(x))
  result[valid] <- as.integer(substr(x[valid], 1L, 4L)) * 12L +
    as.integer(substr(x[valid], 6L, 7L))
  result
}

validate_history_period <- function(
  manifest,
  history_through,
  current_batch
) {
  detected <- year_month_index(manifest$batch_id)
  cutoff <- year_month_index(history_through)
  current <- year_month_index(current_batch)

  too_recent <- !is.na(detected) & detected > cutoff
  current_in_history <- !is.na(detected) & detected == current

  if (any(too_recent | current_in_history)) {
    bad <- manifest[too_recent | current_in_history, , drop = FALSE]
    stop(
      "The historical manifest contains files after ",
      history_through,
      " or from the current batch ",
      current_batch,
      ":\n",
      paste(bad$path, collapse = "\n"),
      call. = FALSE
    )
  }

  unknown <- is.na(detected)
  if (any(unknown)) {
    warning(
      "Could not infer a YYYY-MM batch for these historical files:\n",
      paste(manifest$path[unknown], collapse = "\n"),
      "\nThey are retained, but verify that none belongs to the current update.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

bibtex_entry_start <- function(lines) {
  grepl(
    "^[[:space:]]*@[[:alpha:]][[:alnum:]_:-]*[[:space:]]*[({]",
    lines,
    perl = TRUE
  )
}

bibtex_directive <- function(lines) {
  grepl(
    "^[[:space:]]*@(comment|string|preamble)[[:space:]]*[({]",
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )
}

count_bibtex_entries <- function(lines) {
  starts <- bibtex_entry_start(lines)
  directives <- bibtex_directive(lines)
  sum(starts & !directives)
}

md5_file <- function(path) {
  unname(as.character(tools::md5sum(path)))
}

normalise_scopus_bib <- function(source_path, output_path) {
  lines <- readLines(
    source_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  if (length(lines) == 0L) {
    stop("Empty Scopus BibTeX file: ", source_path, call. = FALSE)
  }

  lines[1L] <- sub("^\ufeff", "", lines[1L])
  entry_rows <- which(bibtex_entry_start(lines))

  if (length(entry_rows) == 0L) {
    stop(
      "No BibTeX entry beginning with @ was found in Scopus file: ",
      source_path,
      call. = FALSE
    )
  }

  first_entry <- min(entry_rows)
  removed_lines <- if (first_entry > 1L) {
    lines[seq_len(first_entry - 1L)]
  } else {
    character()
  }

  cleaned <- lines[first_entry:length(lines)]
  expected_entries <- count_bibtex_entries(cleaned)

  if (expected_entries == 0L) {
    stop(
      "No bibliographic BibTeX entries remained after normalising: ",
      source_path,
      call. = FALSE
    )
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(cleaned, output_path, useBytes = TRUE)

  first_removed_nonempty <- removed_lines[nzchar(trimws(removed_lines))]
  first_removed_nonempty <- if (length(first_removed_nonempty) > 0L) {
    first_removed_nonempty[[1L]]
  } else {
    ""
  }

  tibble::tibble(
    import_path = normalise_path_slashes(
      normalizePath(output_path, winslash = "/", mustWork = TRUE)
    ),
    normalisation = if (first_entry > 1L) {
      "scopus_preamble_removed"
    } else {
      "scopus_copy_no_preamble"
    },
    removed_preamble_lines = first_entry - 1L,
    first_removed_nonempty = first_removed_nonempty,
    expected_entries = expected_entries,
    source_md5 = md5_file(source_path),
    import_md5 = md5_file(output_path)
  )
}

prepare_import_manifest <- function(
  manifest,
  normalised_dir,
  root = here::here()
) {
  source_paths <- validate_file_manifest(manifest, root)
  dir.create(normalised_dir, recursive = TRUE, showWarnings = FALSE)

  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    source_path <- source_paths[[i]]
    database <- manifest$database[[i]]
    extension <- tolower(tools::file_ext(source_path))

    if (identical(database, "Scopus") && identical(extension, "bib")) {
      output_name <- paste0(
        sprintf("%03d", i),
        "_",
        safe_slug(manifest$batch_id[[i]]),
        "_",
        basename(source_path)
      )

      normalised <- normalise_scopus_bib(
        source_path,
        file.path(normalised_dir, output_name)
      )
    } else {
      expected_entries <- if (identical(extension, "bib")) {
        lines <- readLines(source_path, warn = FALSE, encoding = "UTF-8")
        if (length(lines) > 0L) {
          lines[1L] <- sub("^\ufeff", "", lines[1L])
        }
        count_bibtex_entries(lines)
      } else {
        NA_integer_
      }

      normalised <- tibble::tibble(
        import_path = normalise_path_slashes(
          normalizePath(source_path, winslash = "/", mustWork = TRUE)
        ),
        normalisation = "none",
        removed_preamble_lines = 0L,
        first_removed_nonempty = "",
        expected_entries = expected_entries,
        source_md5 = md5_file(source_path),
        import_md5 = md5_file(source_path)
      )
    }

    dplyr::bind_cols(
      tibble::tibble(
        file_index = i,
        source_path = normalise_path_slashes(
          normalizePath(source_path, winslash = "/", mustWork = TRUE)
        ),
        database = database,
        batch_id = manifest$batch_id[[i]],
        extension = extension
      ),
      normalised
    )
  })

  dplyr::bind_rows(rows)
}

read_one_citation_file <- function(
  import_path,
  source_path,
  database,
  batch_id
) {
  tryCatch({
    if (startsWith(database, "External:") && tolower(tools::file_ext(source_path)) == "bib") {
      source(file.path("R", "external_intake.R"))
      data <- external_read_bibtex(source_path, database, batch_id)
    } else data <- CiteSource::read_citations(
      files = import_path,
      cite_sources = database,
      cite_strings = batch_id,
      cite_labels = "search",
      verbose = FALSE,
      only_key_fields = TRUE
    )
    data
    },
    error = function(e) {
      stop(
        "Import failed for source file:\n",
        source_path,
        "\nImport path:\n",
        import_path,
        "\nCause: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

import_citations_from_manifest <- function(import_manifest) {
  records <- vector("list", nrow(import_manifest))
  imported_counts <- integer(nrow(import_manifest))

  for (i in seq_len(nrow(import_manifest))) {
    current <- read_one_citation_file(
      import_path = import_manifest$import_path[[i]],
      source_path = import_manifest$source_path[[i]],
      database = import_manifest$database[[i]],
      batch_id = import_manifest$batch_id[[i]]
    )

    imported_counts[[i]] <- nrow(current)

    if (nrow(current) == 0L) {
      stop(
        "No records were imported from: ",
        import_manifest$source_path[[i]],
        call. = FALSE
      )
    }

    expected <- import_manifest$expected_entries[[i]]
    if (!is.na(expected) && nrow(current) != expected) {
      stop(
        "Record-count mismatch for: ",
        import_manifest$source_path[[i]],
        "\nExpected BibTeX entries: ", expected,
        "\nImported records: ", nrow(current),
        call. = FALSE
      )
    }

    records[[i]] <- current
  }

  data <- dplyr::bind_rows(records)

  if (nrow(data) == 0L) {
    stop("No bibliographic records were imported.", call. = FALSE)
  }

  # Stable within this import because manifest order is deterministic.
  # This also prevents CiteSource from generating row-number IDs with a warning.
  data$record_id <- seq_len(nrow(data))

  if (anyDuplicated(data$record_id)) {
    stop("Generated record_id values are not unique.", call. = FALSE)
  }

  audit <- import_manifest
  audit$imported_records <- imported_counts

  list(data = data, audit = audit)
}

write_import_audit <- function(audit, path) {
  export <- audit
  export$source_path <- relative_to_project(export$source_path)
  export$import_path <- relative_to_project(export$import_path)
  readr::write_csv(export, path)
}

excel_safe_text <- function(x, limit = 32000L) {
  x <- as.character(x)
  too_long <- !is.na(x) & nchar(x, type = "chars") > limit
  x[too_long] <- paste0(
    substr(x[too_long], 1L, limit - 24L),
    " [TRUNCATED FOR EXCEL]"
  )
  x
}

review_column_order <- function(data) {
  preferred <- c(
    "result",
    "duplicate_id.x", "duplicate_id.y",
    "title.x", "title.y",
    "doi.x", "doi.y",
    "author.x", "author.y",
    "year.x", "year.y",
    "journal.x", "journal.y",
    "abstract.x", "abstract.y"
  )
  c(intersect(preferred, names(data)), setdiff(names(data), preferred))
}

write_review_workbook <- function(
  data,
  path,
  title = "Manual duplicate review",
  overwrite = FALSE
) {
  if (file.exists(path) && !overwrite) {
    return(invisible(path))
  }

  if (!"result" %in% names(data)) {
    data$result <- NA_character_
  }

  data <- data[, review_column_order(data), drop = FALSE]
  data[] <- lapply(data, excel_safe_text)
  data$result <- ""

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Instructions", gridLines = FALSE)
  openxlsx::addWorksheet(wb, "review", gridLines = FALSE)

  instructions <- data.frame(
    item = c(
      "Purpose",
      "How to decide",
      "Allowed value 1",
      "Allowed value 2",
      "Conservative rule",
      "Do not edit",
      "Saving"
    ),
    guidance = c(
      title,
      "Classify each candidate pair in the result column.",
      "match = the two rows are the same bibliographic publication.",
      "no_match = distinct publications, including related reports, preprint vs published article, or uncertain cases.",
      "When uncertain, use no_match. A false merge can remove an eligible publication.",
      "Do not change duplicate_id.x or duplicate_id.y. Other bibliographic columns are for inspection only.",
      "Save the workbook in place with the same filename, then run the corresponding finalize script."
    ),
    stringsAsFactors = FALSE
  )

  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#1F4E78",
    textDecoration = "bold",
    halign = "left",
    valign = "center"
  )
  input_style <- openxlsx::createStyle(
    fontColour = "#0000FF",
    fgFill = "#FCE4D6",
    valign = "top"
  )
  imported_style <- openxlsx::createStyle(
    fontColour = "#008000",
    valign = "top",
    wrapText = TRUE
  )
  static_style <- openxlsx::createStyle(
    fontColour = "#666666",
    valign = "top",
    wrapText = TRUE
  )

  openxlsx::writeDataTable(
    wb, "Instructions", instructions,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )
  openxlsx::setColWidths(wb, "Instructions", cols = 1, widths = 22)
  openxlsx::setColWidths(wb, "Instructions", cols = 2, widths = 90)
  openxlsx::addStyle(
    wb, "Instructions", static_style,
    rows = 2:(nrow(instructions) + 1L), cols = 1:2,
    gridExpand = TRUE, stack = TRUE
  )

  openxlsx::writeDataTable(
    wb, "review", data,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  openxlsx::addStyle(
    wb, "review", header_style,
    rows = 1, cols = seq_len(ncol(data)),
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::freezePane(wb, "review", firstActiveRow = 2, firstActiveCol = 2)

  if (nrow(data) > 0L) {
    openxlsx::addStyle(
      wb, "review", imported_style,
      rows = 2:(nrow(data) + 1L), cols = seq_len(ncol(data)),
      gridExpand = TRUE, stack = TRUE
    )

    result_col <- match("result", names(data))
    openxlsx::addStyle(
      wb, "review", input_style,
      rows = 2:(nrow(data) + 1L), cols = result_col,
      gridExpand = TRUE, stack = TRUE
    )
    openxlsx::dataValidation(
      wb, "review",
      cols = result_col,
      rows = 2:(nrow(data) + 1L),
      type = "list",
      value = '"match,no_match"'
    )
  }

  for (j in seq_len(ncol(data))) {
    nm <- names(data)[[j]]
    width <- if (nm == "result") {
      14
    } else if (grepl("title|abstract", nm, ignore.case = TRUE)) {
      55
    } else if (grepl("author", nm, ignore.case = TRUE)) {
      32
    } else if (grepl("doi|duplicate_id|record_id", nm, ignore.case = TRUE)) {
      24
    } else {
      18
    }
    openxlsx::setColWidths(wb, "review", cols = j, widths = width)
  }

  openxlsx::setRowHeights(wb, "review", rows = 1, heights = 28)
  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  invisible(path)
}

read_review_workbook <- function(path) {
  if (!file.exists(path)) {
    stop("Missing Excel review workbook: ", path, call. = FALSE)
  }

  data <- readxl::read_excel(
    path,
    sheet = "review",
    col_types = "text",
    .name_repair = "minimal"
  )
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  required <- c("duplicate_id.x", "duplicate_id.y", "result")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "The Excel review workbook is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  data
}

pair_key <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  paste(pmin(x, y), pmax(x, y), sep = "::")
}

apply_excel_review_decisions <- function(expected_pairs, review_data) {
  expected_keys <- pair_key(
    expected_pairs$duplicate_id.x,
    expected_pairs$duplicate_id.y
  )
  review_keys <- pair_key(
    review_data$duplicate_id.x,
    review_data$duplicate_id.y
  )

  if (anyDuplicated(review_keys)) {
    stop(
      "The Excel review workbook contains duplicated candidate pairs.",
      call. = FALSE
    )
  }

  if (!setequal(expected_keys, review_keys)) {
    stop(
      "The Excel review workbook does not contain the same candidate pairs as ",
      "the current deduplication result. Delete the stale workbook, rerun the ",
      "prepare script, and review the newly generated workbook.",
      call. = FALSE
    )
  }

  idx <- match(expected_keys, review_keys)
  reviewed <- expected_pairs
  reviewed$result <- review_data$result[idx]
  validate_reviewed_candidates(reviewed, expected_pairs)
}

load_reviewed_candidates <- function(
  expected_pairs,
  excel_file,
  legacy_csv_file = NULL
) {
  if (file.exists(excel_file)) {
    review_data <- read_review_workbook(excel_file)
    return(apply_excel_review_decisions(expected_pairs, review_data))
  }

  if (!is.null(legacy_csv_file) && file.exists(legacy_csv_file)) {
    warning(
      "Using legacy CSV review file because the Excel workbook is absent: ",
      legacy_csv_file,
      call. = FALSE
    )
    reviewed <- CiteSource::reimport_dedup_candidates(legacy_csv_file)
    return(validate_reviewed_candidates(reviewed, expected_pairs))
  }

  stop(
    "Missing reviewed candidate workbook: ", excel_file,
    call. = FALSE
  )
}

write_human_workbook <- function(data, path, sheet = "records") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[] <- lapply(data, excel_safe_text)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  if (nrow(data) > 0L && ncol(data) > 0L) {
    openxlsx::writeDataTable(
      wb, sheet, data,
      tableStyle = "TableStyleMedium2",
      withFilter = TRUE
    )
  } else if (ncol(data) > 0L) {
    openxlsx::writeData(wb, sheet, data, colNames = TRUE)
  } else {
    openxlsx::writeData(wb, sheet, "No records.")
  }
  openxlsx::freezePane(wb, sheet, firstActiveRow = 2)

  if (nrow(data) > 0L && ncol(data) > 0L) {
    imported_style <- openxlsx::createStyle(
      fontColour = "#008000",
      valign = "top",
      wrapText = TRUE
    )
    openxlsx::addStyle(
      wb, sheet, imported_style,
      rows = 2:(nrow(data) + 1L), cols = seq_len(ncol(data)),
      gridExpand = TRUE, stack = TRUE
    )
  }

  for (j in seq_len(ncol(data))) {
    nm <- names(data)[[j]]
    width <- if (grepl("title|abstract", nm, ignore.case = TRUE)) {
      55
    } else if (grepl("author", nm, ignore.case = TRUE)) {
      32
    } else if (grepl("doi|id", nm, ignore.case = TRUE)) {
      24
    } else {
      18
    }
    openxlsx::setColWidths(wb, sheet, cols = j, widths = width)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

write_comparison_workbook <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()

  safe_sheet_name <- function(x) {
    x <- gsub("[\\\\/?*\\[\\]:]", "_", x)
    substr(x, 1L, 31L)
  }

  for (nm in names(sheets)) {
    sheet_name <- safe_sheet_name(nm)
    data <- as.data.frame(sheets[[nm]], stringsAsFactors = FALSE)
    data[] <- lapply(data, excel_safe_text)

    openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
    if (nrow(data) > 0L && ncol(data) > 0L) {
      openxlsx::writeDataTable(
        wb, sheet_name, data,
        tableStyle = "TableStyleMedium2",
        withFilter = TRUE
      )
    } else if (ncol(data) > 0L) {
      openxlsx::writeData(wb, sheet_name, data, colNames = TRUE)
    } else {
      openxlsx::writeData(wb, sheet_name, "No records in this category.")
    }
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2)

    for (j in seq_len(ncol(data))) {
      col_name <- names(data)[[j]]
      width <- if (grepl("title|abstract", col_name, ignore.case = TRUE)) {
        55
      } else if (grepl("doi|key", col_name, ignore.case = TRUE)) {
        28
      } else {
        18
      }
      openxlsx::setColWidths(wb, sheet_name, cols = j, widths = width)
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

split_record_ids <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]

  if (length(x) == 0L) {
    return(character())
  }

  ids <- unlist(
    strsplit(x, ",[[:space:]]*"),
    use.names = FALSE
  )

  ids <- trimws(ids)
  unique(ids[nzchar(ids)])
}

validate_deduplicated_registry <- function(data) {
  required <- c(
    "duplicate_id", "record_ids", "cite_source", "cite_label"
  )
  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Deduplicated registry is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(data) == 0L) {
    stop("Deduplicated registry has no records.", call. = FALSE)
  }

  if (anyNA(data$duplicate_id) || any(!nzchar(data$duplicate_id))) {
    stop("Some duplicate_id values are missing.", call. = FALSE)
  }

  if (anyDuplicated(data$duplicate_id)) {
    stop("duplicate_id is not unique in the registry.", call. = FALSE)
  }

  if (anyNA(data$record_ids) || any(!nzchar(data$record_ids))) {
    stop("Some record_ids values are missing.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_reviewed_candidates <- function(reviewed, expected_pairs) {
  if (!"result" %in% names(reviewed)) {
    stop(
      "The reviewed candidate file has no result column.",
      call. = FALSE
    )
  }

  expected_n <- nrow(expected_pairs)
  reviewed$result <- tolower(trimws(as.character(reviewed$result)))
  allowed <- c("match", "no_match")

  if (nrow(reviewed) != expected_n) {
    stop(
      "The reviewed candidate file contains ", nrow(reviewed),
      " rows, but ", expected_n, " were expected.",
      call. = FALSE
    )
  }

  pair_columns <- c("duplicate_id.x", "duplicate_id.y")
  if (
    all(pair_columns %in% names(reviewed)) &&
      all(pair_columns %in% names(expected_pairs))
  ) {
    reviewed_keys <- paste(
      as.character(reviewed[[pair_columns[[1L]]]]),
      as.character(reviewed[[pair_columns[[2L]]]]),
      sep = "::"
    )
    expected_keys <- paste(
      as.character(expected_pairs[[pair_columns[[1L]]]]),
      as.character(expected_pairs[[pair_columns[[2L]]]]),
      sep = "::"
    )

    if (!identical(reviewed_keys, expected_keys)) {
      stop(
        "The reviewed candidate file does not contain the same candidate pairs ",
        "in the same order as the current deduplication result. Delete the stale ",
        "reviewed file, rerun the prepare script, and review the new copy.",
        call. = FALSE
      )
    }
  }

  invalid <- is.na(reviewed$result) |
    !reviewed$result %in% allowed

  if (any(invalid)) {
    stop(
      "Every candidate pair must be classified as exactly ",
      "'match' or 'no_match'. Invalid rows: ",
      paste(which(invalid), collapse = ", "),
      call. = FALSE
    )
  }

  reviewed
}

clean_column_names <- function(data) {
  cleaned <- trimws(tolower(names(data)))
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  names(data) <- cleaned
  data
}

pick_first_column <- function(data, candidates, required = TRUE) {
  found <- intersect(candidates, names(data))

  if (length(found) > 0L) {
    return(found[[1L]])
  }

  if (required) {
    stop(
      "Could not find any of these columns: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }

  NA_character_
}

normalise_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi[[:space:]]*:[[:space:]]*", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x <- sub("[[:punct:]]+$", "", x)
  x
}

normalise_title <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("&amp;", " and ", x, fixed = TRUE)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

read_table_auto <- function(path) {
  extension <- tolower(tools::file_ext(path))

  if (extension == "csv") {
    first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(first_line) == 0L) {
      stop("CSV file is empty: ", path, call. = FALSE)
    }
    comma_count <- stringr::str_count(first_line, stringr::fixed(","))
    semicolon_count <- stringr::str_count(first_line, stringr::fixed(";"))
    delimiter <- if (semicolon_count > comma_count) ";" else ","

    return(
      readr::read_delim(
        path,
        delim = delimiter,
        show_col_types = FALSE,
        guess_max = 100000
      )
    )
  }

  if (extension %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path))
  }

  stop(
    "Unsupported table format: ", extension,
    call. = FALSE
  )
}

write_session_snapshot <- function(path) {
  lines <- capture.output(sessionInfo())
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

workflow_metadata <- function(
  workflow_version,
  batch_id,
  history_through,
  search_run_date
) {
  tibble::tibble(
    workflow_version = workflow_version,
    batch_id = batch_id,
    history_through = history_through,
    search_run_date = search_run_date,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    citesource_version = as.character(utils::packageVersion("CiteSource")),
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}
