# Living SEB full-text workflow helpers
# Version: 2.1.0

ft_clean_names <- function(data) {
  nm <- tolower(trimws(names(data)))
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("^_+|_+$", "", nm)
  names(data) <- nm
  data
}

ft_blank_to_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

ft_normalise_doi <- function(x) {
  x <- tolower(ft_blank_to_empty(x))
  x <- trimws(x)

  # Historical workbooks may contain literal missing-value placeholders.
  # These must never be treated as bibliographic identities (for example, doi:na).
  missing_tokens <- c(
    "na", "n/a", "n.a.", "none", "null", "missing",
    "not available", "not applicable", "no doi", "no_doi",
    "-", "--", "."
  )
  x[x %in% missing_tokens] <- ""

  x <- sub("^doi\\s*:\\s*", "", x)
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- trimws(x)
  x <- sub("[.,;]+$", "", x)

  # A malformed/non-DOI legacy value is not suitable as a stable key.
  # In that case, ft_make_record_identity() falls back to title/year/author.
  valid <- !nzchar(x) | grepl("^10\\.[0-9]{4,9}/\\S+$", x)
  x[!valid] <- ""
  x
}

ft_normalise_key_text <- function(x) {
  x <- tolower(ft_blank_to_empty(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ft_pick_column <- function(data, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) > 0L) return(hit[[1]])
  if (required) {
    stop(
      "None of the required columns was found: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  NA_character_
}

ft_make_record_identity <- function(data) {
  data <- ft_clean_names(data)

  title_col <- ft_pick_column(data, c("title", "article_title", "document_title"))
  doi_col <- ft_pick_column(
    data,
    c("doi", "digital_object_identifier"),
    required = FALSE
  )
  author_col <- ft_pick_column(
    data,
    c("author", "authors", "author_et_al"),
    required = FALSE
  )
  year_col <- ft_pick_column(
    data,
    c("year", "publication_year"),
    required = FALSE
  )

  doi <- if (is.na(doi_col)) rep("", nrow(data)) else data[[doi_col]]
  author <- if (is.na(author_col)) rep("", nrow(data)) else data[[author_col]]
  year <- if (is.na(year_col)) rep("", nrow(data)) else data[[year_col]]

  doi <- ft_normalise_doi(doi)
  title <- ft_normalise_key_text(data[[title_col]])
  author <- ft_normalise_key_text(author)
  year <- ft_normalise_key_text(year)

  if (any(!nzchar(title) & !nzchar(doi))) {
    stop(
      "At least one record has neither a usable title nor a DOI.",
      call. = FALSE
    )
  }

  record_key <- ifelse(
    nzchar(doi),
    paste0("doi:", doi),
    paste("title", title, "year", year, "author", author, sep = "|")
  )

  hashes <- vapply(
    record_key,
    digest::digest,
    FUN.VALUE = character(1),
    algo = "sha256",
    serialize = FALSE
  )

  data.frame(
    screening_record_id = paste0("seb_", substr(hashes, 1L, 16L)),
    record_key = record_key,
    doi_normalised = doi,
    stringsAsFactors = FALSE
  )
}

ft_format_paper_id <- function(x, min_width = 4L) {
  raw <- ft_blank_to_empty(x)
  parsed <- suppressWarnings(as.integer(raw))
  bad <- nzchar(raw) & is.na(parsed)
  if (any(bad)) {
    stop(
      "paper_id must be numeric before zero-padding. Invalid values: ",
      paste(unique(raw[bad]), collapse = ", "),
      call. = FALSE
    )
  }
  width <- max(min_width, nchar(as.character(max(parsed, na.rm = TRUE))), na.rm = TRUE)
  out <- rep("", length(parsed))
  keep <- !is.na(parsed)
  out[keep] <- sprintf(paste0("%0", width, "d"), parsed[keep])
  out
}

ft_first_nonblank <- function(x) {
  x <- ft_blank_to_empty(x)
  hit <- x[nzchar(x)]
  if (length(hit) == 0L) return("")
  hit[[1]]
}

ft_read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    out <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    out <- readxl::read_excel(path)
  } else {
    stop("Unsupported input format: ", path, call. = FALSE)
  }
  ft_clean_names(as.data.frame(out, stringsAsFactors = FALSE))
}

# Historical full-text files evolved over time and the same column may have
# different inferred types across monthly workbooks (e.g., gender numeric in
# one file and character in another). The legacy registry only needs textual
# identity fields, so coerce legacy frames to character before row-binding.
# This prevents vctrs/dplyr type conflicts without modifying source files.
ft_coerce_legacy_bind_types <- function(data) {
  data[] <- lapply(data, function(x) {
    if (inherits(x, "POSIXt")) {
      return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }
    if (inherits(x, "Date")) {
      return(format(x, "%Y-%m-%d"))
    }
    if (is.list(x)) {
      return(vapply(
        x,
        function(value) {
          if (length(value) == 0L || all(is.na(value))) return("")
          paste(as.character(value), collapse = "; ")
        },
        FUN.VALUE = character(1)
      ))
    }
    as.character(x)
  })
  data
}

ft_find_title_abstract_input <- function(batch_id, abstracting_dir) {
  preferred <- c(
    file.path(
      abstracting_dir,
      "title_abstract",
      paste0(batch_id, "_title_abstract_included.csv")
    ),
    file.path(abstracting_dir, paste0(batch_id, "_title_abstract_included.csv")),
    file.path(
      abstracting_dir,
      "title_abstract",
      paste0(batch_id, "_title_abstract_included.xlsx")
    ),
    file.path(abstracting_dir, paste0(batch_id, "_title_abstract_included.xlsx"))
  )

  hit <- preferred[file.exists(preferred)]
  if (length(hit) == 1L) return(hit[[1]])
  if (length(hit) > 1L) return(hit[[1]])

  candidates <- list.files(
    abstracting_dir,
    pattern = paste0("^", batch_id, "_title_abstract_included\\.(csv|xlsx)$"),
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(candidates) == 0L) {
    stop(
      "Could not find the retained title/abstract file for batch ",
      batch_id,
      " under ",
      abstracting_dir,
      call. = FALSE
    )
  }
  if (length(candidates) > 1L) {
    stop(
      "Multiple retained title/abstract files were found. Keep one canonical file or set the path explicitly. Files: ",
      paste(candidates, collapse = "; "),
      call. = FALSE
    )
  }
  candidates[[1]]
}

ft_prepare_current_records <- function(path, batch_id) {
  data <- ft_read_table(path)

  if ("automatic_action" %in% names(data)) {
    data <- data[toupper(ft_blank_to_empty(data$automatic_action)) != "EXCLUDE", , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("No records were retained for full-text screening.", call. = FALSE)
  }

  identity <- ft_make_record_identity(data)

  if ("screening_record_id" %in% names(data)) {
    existing_id <- ft_blank_to_empty(data$screening_record_id)
    use_existing <- nzchar(existing_id)
    if (any(use_existing & existing_id != identity$screening_record_id)) {
      bad <- which(use_existing & existing_id != identity$screening_record_id)
      stop(
        "Existing screening_record_id does not match the stable DOI/title identity for rows: ",
        paste(bad, collapse = ", "),
        call. = FALSE
      )
    }
    data$screening_record_id <- ifelse(
      use_existing,
      existing_id,
      identity$screening_record_id
    )
  } else {
    data$screening_record_id <- identity$screening_record_id
  }

  data$record_key <- identity$record_key
  data$doi_normalised <- identity$doi_normalised
  data$batch_id <- batch_id

  if (anyDuplicated(data$screening_record_id)) {
    stop("Duplicate screening_record_id values in current full-text input.", call. = FALSE)
  }
  if (anyDuplicated(data$record_key)) {
    stop("Duplicate bibliographic identities in current full-text input.", call. = FALSE)
  }

  data
}

ft_list_legacy_decision_files <- function(full_text_dir) {
  files <- list.files(
    full_text_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  base <- basename(files)
  files[
    grepl("full.*decision|decision.*progress", base, ignore.case = TRUE)
  ]
}

ft_alias_columns <- function() {
  c(
    "alias_paper_id",
    "canonical_paper_id",
    "screening_record_id",
    "record_key",
    "doi",
    "title",
    "resolution",
    "source_files"
  )
}

ft_build_legacy_aliases <- function(all) {
  groups <- all |>
    dplyr::group_by(record_key, screening_record_id) |>
    dplyr::filter(dplyr::n_distinct(paper_id) > 1L) |>
    dplyr::group_split()

  if (length(groups) == 0L) {
    out <- data.frame(matrix(ncol = length(ft_alias_columns()), nrow = 0L),
                      stringsAsFactors = FALSE)
    names(out) <- ft_alias_columns()
    return(out)
  }

  rows <- lapply(groups, function(d) {
    ids <- sort(unique(d$paper_id), method = "radix")
    numeric_ids <- suppressWarnings(as.integer(ids))
    if (anyNA(numeric_ids)) {
      stop(
        "Cannot reconcile legacy aliases because a paper_id is not numeric: ",
        paste(ids, collapse = ", "),
        call. = FALSE
      )
    }

    canonical <- ids[[which.min(numeric_ids)]]
    aliases <- setdiff(ids, canonical)

    data.frame(
      alias_paper_id = aliases,
      canonical_paper_id = canonical,
      screening_record_id = unique(d$screening_record_id)[[1]],
      record_key = unique(d$record_key)[[1]],
      doi = ft_first_nonblank(d$doi_normalised),
      title = ft_first_nonblank(d$title),
      resolution = "AUTO_EARLIEST_PAPER_ID_FOR_SAME_BIBLIOGRAPHIC_IDENTITY",
      source_files = paste(sort(unique(d$.source_file)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)[, ft_alias_columns(), drop = FALSE]
}

ft_write_legacy_alias_report <- function(aliases, full_text_dir) {
  path <- file.path(full_text_dir, "paper_id_aliases.csv")
  if (nrow(aliases) > 0L) {
    readr::write_csv(aliases, path, na = "")
  } else if (file.exists(path)) {
    unlink(path)
  }
  invisible(path)
}

ft_write_legacy_conflict_report <- function(all, full_text_dir) {
  report_path <- file.path(full_text_dir, "legacy_registry_conflicts.csv")

  # Same bibliographic identity under multiple paper IDs is handled separately
  # as a legacy alias. The genuinely unsafe case is the reverse: one paper_id
  # associated with more than one bibliographic identity.
  pid_conflicts <- all |>
    dplyr::group_by(paper_id) |>
    dplyr::filter(dplyr::n_distinct(record_key) > 1L) |>
    dplyr::summarise(
      conflict_type = "ONE_PAPER_ID_MULTIPLE_IDENTITIES",
      record_key = paste(sort(unique(record_key)), collapse = ";"),
      screening_record_id = paste(sort(unique(screening_record_id)), collapse = ";"),
      paper_ids = dplyr::first(paper_id),
      doi = paste(sort(unique(doi_normalised[nzchar(doi_normalised)])), collapse = ";"),
      title = paste(sort(unique(title[nzchar(title)])), collapse = " | "),
      source_files = paste(sort(unique(.source_file)), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::select(-paper_id)

  if (nrow(pid_conflicts) > 0L) {
    readr::write_csv(pid_conflicts, report_path, na = "")
    return(report_path)
  }

  if (file.exists(report_path)) unlink(report_path)
  invisible(NA_character_)
}

ft_bootstrap_legacy_registry <- function(full_text_dir) {
  files <- ft_list_legacy_decision_files(full_text_dir)
  if (length(files) == 0L) {
    return(data.frame())
  }

  rows <- lapply(files, function(path) {
    x <- ft_read_table(path)
    if (!"paper_id" %in% names(x)) return(NULL)
    x <- ft_coerce_legacy_bind_types(x)
    x$.source_file <- basename(path)
    x
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame())

  all <- dplyr::bind_rows(rows)
  pid_raw <- ft_blank_to_empty(all$paper_id)
  all <- all[nzchar(pid_raw), , drop = FALSE]
  if (nrow(all) == 0L) return(data.frame())

  all$paper_id <- ft_format_paper_id(all$paper_id)
  identity <- ft_make_record_identity(all)
  all$screening_record_id <- identity$screening_record_id
  all$record_key <- identity$record_key
  all$doi_normalised <- identity$doi_normalised

  title_col <- ft_pick_column(all, c("title", "article_title", "document_title"))
  all$title <- ft_blank_to_empty(all[[title_col]])

  # First protect against the unsafe reverse collision: one historical paper_id
  # referring to more than one bibliographic identity.
  conflict_report <- ft_write_legacy_conflict_report(all, full_text_dir)
  if (!is.na(conflict_report)) {
    stop(
      "Unsafe legacy paper-ID conflicts remain. Review: ",
      conflict_report,
      call. = FALSE
    )
  }

  # The same bibliographic identity may have accidentally received more than
  # one paper_id in historical manual workflows. Keep the earliest numerical
  # ID as canonical, reserve every later ID as an alias, and never reuse it.
  aliases <- ft_build_legacy_aliases(all)
  ft_write_legacy_alias_report(aliases, full_text_dir)

  canonical_map <- stats::setNames(
    aliases$canonical_paper_id,
    aliases$alias_paper_id
  )
  canonical_pid <- all$paper_id
  is_alias <- canonical_pid %in% names(canonical_map)
  canonical_pid[is_alias] <- unname(canonical_map[canonical_pid[is_alias]])
  all$canonical_paper_id <- canonical_pid

  author_col <- ft_pick_column(
    all,
    c("author", "authors", "author_et_al"),
    required = FALSE
  )
  year_col <- ft_pick_column(
    all,
    c("year", "publication_year"),
    required = FALSE
  )

  out <- lapply(sort(unique(all$canonical_paper_id)), function(pid) {
    d <- all[all$canonical_paper_id == pid, , drop = FALSE]
    keys <- unique(d$record_key[nzchar(d$record_key)])
    if (length(keys) != 1L) {
      stop(
        "Canonical legacy paper_id ", pid,
        " maps to more than one bibliographic identity. Resolve before continuing.",
        call. = FALSE
      )
    }
    data.frame(
      paper_id = pid,
      screening_record_id = unique(d$screening_record_id)[[1]],
      record_key = keys[[1]],
      doi = ft_first_nonblank(d$doi_normalised),
      title = ft_first_nonblank(d[[title_col]]),
      author = if (is.na(author_col)) "" else ft_first_nonblank(d[[author_col]]),
      year = if (is.na(year_col)) "" else ft_first_nonblank(d[[year_col]]),
      assigned_batch = "legacy",
      assigned_at_utc = "",
      assignment_source = "legacy_full_text_files",
      first_seen_source = ft_first_nonblank(d$.source_file),
      stringsAsFactors = FALSE
    )
  })
  registry <- dplyr::bind_rows(out)

  if (anyDuplicated(registry$paper_id)) {
    stop("Duplicate canonical paper_id values found while bootstrapping legacy registry.", call. = FALSE)
  }
  if (anyDuplicated(registry$record_key) || anyDuplicated(registry$screening_record_id)) {
    stop(
      "Unexpected duplicate canonical identity remained after legacy alias reconciliation.",
      call. = FALSE
    )
  }

  # Attributes are used during this run so alias IDs remain reserved when new
  # IDs are assigned. The alias CSV provides the persistent audit trail.
  attr(registry, "reserved_paper_ids") <- sort(unique(all$paper_id))
  attr(registry, "paper_id_aliases") <- aliases
  registry
}

ft_registry_columns <- function() {
  c(
    "paper_id",
    "screening_record_id",
    "record_key",
    "doi",
    "title",
    "author",
    "year",
    "assigned_batch",
    "assigned_at_utc",
    "assignment_source",
    "first_seen_source"
  )
}

ft_validate_registry <- function(registry) {
  reserved_ids <- attr(registry, "reserved_paper_ids")
  aliases <- attr(registry, "paper_id_aliases")
  required <- ft_registry_columns()
  missing <- setdiff(required, names(registry))
  if (length(missing) > 0L) {
    stop(
      "paper_id_registry.csv is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  registry$paper_id <- ft_format_paper_id(registry$paper_id)
  registry$screening_record_id <- ft_blank_to_empty(registry$screening_record_id)
  registry$record_key <- ft_blank_to_empty(registry$record_key)

  if (any(!nzchar(registry$paper_id)) || anyDuplicated(registry$paper_id)) {
    stop("paper_id_registry.csv contains blank or duplicate paper_id values.", call. = FALSE)
  }
  if (any(!nzchar(registry$screening_record_id)) || anyDuplicated(registry$screening_record_id)) {
    stop(
      "paper_id_registry.csv contains blank or duplicate screening_record_id values.",
      call. = FALSE
    )
  }
  if (any(!nzchar(registry$record_key)) || anyDuplicated(registry$record_key)) {
    stop("paper_id_registry.csv contains blank or duplicate record_key values.", call. = FALSE)
  }

  out <- registry[, required, drop = FALSE]
  attr(out, "reserved_paper_ids") <- reserved_ids
  attr(out, "paper_id_aliases") <- aliases
  out
}

ft_reconcile_legacy_registry <- function(registry, legacy) {
  reserved_ids <- attr(legacy, "reserved_paper_ids")
  aliases <- attr(legacy, "paper_id_aliases")

  if (nrow(legacy) == 0L) return(registry)
  if (nrow(registry) == 0L) {
    out <- ft_validate_registry(legacy)
    attr(out, "reserved_paper_ids") <- reserved_ids
    attr(out, "paper_id_aliases") <- aliases
    return(out)
  }

  registry <- ft_validate_registry(registry)
  legacy_valid <- ft_validate_registry(legacy)

  shared_pid <- intersect(registry$paper_id, legacy_valid$paper_id)
  if (length(shared_pid) > 0L) {
    for (pid in shared_pid) {
      rkey <- registry$record_key[registry$paper_id == pid][[1]]
      lkey <- legacy_valid$record_key[legacy_valid$paper_id == pid][[1]]
      if (!identical(rkey, lkey)) {
        stop(
          "Registry conflict: paper_id ", pid,
          " maps to different bibliographic identities in registry and legacy files.",
          call. = FALSE
        )
      }
    }
  }

  missing_pid <- setdiff(legacy_valid$paper_id, registry$paper_id)
  if (length(missing_pid) > 0L) {
    add <- legacy_valid[legacy_valid$paper_id %in% missing_pid, , drop = FALSE]
    if (any(add$record_key %in% registry$record_key)) {
      stop(
        "Registry conflict: a canonical legacy bibliographic identity already exists under another paper_id.",
        call. = FALSE
      )
    }
    registry <- dplyr::bind_rows(registry, add)
  }

  out <- ft_validate_registry(registry)
  attr(out, "reserved_paper_ids") <- unique(c(reserved_ids, out$paper_id))
  attr(out, "paper_id_aliases") <- aliases
  out
}

ft_new_registry_row <- function(data_row, paper_id, batch_id, source_file) {
  author_col <- ft_pick_column(
    data_row,
    c("author", "authors", "author_et_al"),
    required = FALSE
  )
  year_col <- ft_pick_column(
    data_row,
    c("year", "publication_year"),
    required = FALSE
  )
  title_col <- ft_pick_column(
    data_row,
    c("title", "article_title", "document_title")
  )

  data.frame(
    paper_id = paper_id,
    screening_record_id = data_row$screening_record_id[[1]],
    record_key = data_row$record_key[[1]],
    doi = data_row$doi_normalised[[1]],
    title = ft_blank_to_empty(data_row[[title_col]])[[1]],
    author = if (is.na(author_col)) "" else ft_blank_to_empty(data_row[[author_col]])[[1]],
    year = if (is.na(year_col)) "" else ft_blank_to_empty(data_row[[year_col]])[[1]],
    assigned_batch = batch_id,
    assigned_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    assignment_source = "title_abstract_retained",
    first_seen_source = basename(source_file),
    stringsAsFactors = FALSE
  )
}

ft_assign_paper_ids <- function(current, registry, batch_id, source_file) {
  registry <- if (nrow(registry) == 0L) {
    data.frame(matrix(ncol = length(ft_registry_columns()), nrow = 0L),
               stringsAsFactors = FALSE)
  } else {
    ft_validate_registry(registry)
  }
  if (nrow(registry) == 0L) names(registry) <- ft_registry_columns()

  current$paper_id <- ""
  reused <- 0L
  newly_assigned <- 0L

  reserved_ids <- attr(registry, "reserved_paper_ids")
  id_pool <- unique(c(registry$paper_id, reserved_ids))
  id_pool <- id_pool[nzchar(ft_blank_to_empty(id_pool))]
  if (length(id_pool) > 0L) {
    max_id <- max(as.integer(id_pool), na.rm = TRUE)
  } else {
    max_id <- 0L
  }

  order_new <- order(current$screening_record_id)
  for (i in order_new) {
    sid <- current$screening_record_id[[i]]
    key <- current$record_key[[i]]

    sid_hit <- which(registry$screening_record_id == sid)
    key_hit <- which(registry$record_key == key)

    if (length(sid_hit) > 1L || length(key_hit) > 1L) {
      stop("Registry is not one-to-one for current record: ", sid, call. = FALSE)
    }

    if (length(sid_hit) == 1L && length(key_hit) == 1L && sid_hit != key_hit) {
      stop(
        "Registry conflict for ", sid,
        ": screening_record_id and bibliographic key point to different paper_id values.",
        call. = FALSE
      )
    }

    hit <- unique(c(sid_hit, key_hit))
    if (length(hit) == 1L) {
      current$paper_id[[i]] <- registry$paper_id[[hit]]
      reused <- reused + 1L
    } else {
      max_id <- max_id + 1L
      pid <- sprintf("%04d", max_id)
      current$paper_id[[i]] <- pid
      registry <- dplyr::bind_rows(
        registry,
        ft_new_registry_row(current[i, , drop = FALSE], pid, batch_id, source_file)
      )
      newly_assigned <- newly_assigned + 1L
    }
  }

  registry <- registry[order(as.integer(registry$paper_id)), , drop = FALSE]
  rownames(registry) <- NULL
  registry <- ft_validate_registry(registry)

  list(
    current = current,
    registry = registry,
    reused = reused,
    newly_assigned = newly_assigned
  )
}

ft_atomic_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "living_seb_", tmpdir = dirname(path), fileext = ".csv")
  readr::write_csv(data, tmp, na = "")
  ok <- file.copy(tmp, path, overwrite = TRUE)
  unlink(tmp)
  if (!isTRUE(ok)) stop("Could not write file: ", path, call. = FALSE)
  invisible(path)
}

ft_atomic_write_manifest_xlsx <- function(data, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to write the full-text manifest.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "living_seb_", tmpdir = dirname(path), fileext = ".xlsx")

  wb <- openxlsx::createWorkbook()
  sheet <- "full_text_manifest"
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

  openxlsx::writeDataTable(
    wb, sheet, data,
    startRow = 1, startCol = 1,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )

  # Write DOI URLs as real Excel hyperlinks while displaying the complete URL.
  doi_col <- match("doi", names(data))
  if (!is.na(doi_col) && nrow(data) > 0L) {
    doi_urls <- ft_blank_to_empty(data$doi)
    for (i in seq_along(doi_urls)) {
      url <- doi_urls[[i]]
      if (nzchar(url)) {
        link <- url
        class(link) <- "hyperlink"
        openxlsx::writeData(wb, sheet, link, startCol = doi_col, startRow = i + 1L)
      }
    }
    hyperlink_style <- openxlsx::createStyle(
      fontColour = "#0563C1",
      textDecoration = "underline"
    )
    openxlsx::addStyle(
      wb, sheet, hyperlink_style,
      rows = seq_len(nrow(data)) + 1L, cols = doi_col,
      gridExpand = TRUE, stack = TRUE
    )
  }

  openxlsx::freezePane(wb, sheet, firstRow = TRUE)

  # Keep operational fields readable when the workbook is opened.
  width_map <- c(
    batch_id = 12,
    screening_record_id = 24,
    paper_id = 10,
    doi = 42,
    title = 65,
    author = 30,
    year = 10,
    journal = 35,
    pdf_file = 14,
    pdf_path = 48,
    pdf_status = 14,
    supplement_files = 30,
    supplement_status = 18,
    source_title_abstract_file = 36
  )
  present <- intersect(names(width_map), names(data))
  if (length(present) > 0L) {
    openxlsx::setColWidths(
      wb, sheet,
      cols = match(present, names(data)),
      widths = unname(width_map[present])
    )
  }

  wrap_cols <- intersect(c("title", "author", "journal", "pdf_path", "supplement_files"), names(data))
  if (length(wrap_cols) > 0L && nrow(data) > 0L) {
    wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
    openxlsx::addStyle(
      wb, sheet, wrap_style,
      rows = seq_len(nrow(data)) + 1L,
      cols = match(wrap_cols, names(data)),
      gridExpand = TRUE, stack = TRUE
    )
  }

  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  ok <- file.copy(tmp, path, overwrite = TRUE)
  unlink(tmp)
  if (!isTRUE(ok)) stop("Could not write file: ", path, call. = FALSE)
  invisible(path)
}

ft_build_manifest <- function(current, batch_id, pdf_dir, source_file) {
  title_col <- ft_pick_column(current, c("title", "article_title", "document_title"))
  author_col <- ft_pick_column(
    current,
    c("author", "authors", "author_et_al"),
    required = FALSE
  )
  year_col <- ft_pick_column(
    current,
    c("year", "publication_year"),
    required = FALSE
  )
  journal_col <- ft_pick_column(
    current,
    c("journal", "source_title", "publication_name"),
    required = FALSE
  )

  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

  pdf_file <- paste0(current$paper_id, ".pdf")
  pdf_path <- file.path(pdf_dir, pdf_file)
  pdf_status <- ifelse(file.exists(pdf_path), "AVAILABLE", "MISSING")

  supplement_files <- vapply(current$paper_id, function(pid) {
    all_pdf <- list.files(pdf_dir, pattern = "\\.pdf$", ignore.case = TRUE)
    hit <- all_pdf[
      grepl(paste0("^", pid, "_(supp|supplement|appendix|sup)"), all_pdf, ignore.case = TRUE)
    ]
    paste(hit, collapse = ";")
  }, FUN.VALUE = character(1))

  data.frame(
    batch_id = batch_id,
    screening_record_id = current$screening_record_id,
    paper_id = current$paper_id,
    doi = ifelse(
      nzchar(current$doi_normalised),
      paste0("https://doi.org/", current$doi_normalised),
      ""
    ),
    title = ft_blank_to_empty(current[[title_col]]),
    author = if (is.na(author_col)) "" else ft_blank_to_empty(current[[author_col]]),
    year = if (is.na(year_col)) "" else ft_blank_to_empty(current[[year_col]]),
    journal = if (is.na(journal_col)) "" else ft_blank_to_empty(current[[journal_col]]),
    pdf_file = pdf_file,
    pdf_path = gsub("\\\\", "/", pdf_path),
    pdf_status = pdf_status,
    supplement_files = supplement_files,
    supplement_status = ifelse(nzchar(supplement_files), "AVAILABLE", "NONE_FOUND"),
    source_title_abstract_file = basename(source_file),
    stringsAsFactors = FALSE
  )[order(as.integer(current$paper_id)), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Full-text decision workbook + adjudication workflow (v2.1.0)
# -----------------------------------------------------------------------------

ft_decision_columns <- function() {
  c(
    "batch_id", "screening_record_id", "paper_id", "doi", "title", "pdf_path",
    "review_eligible", "review_exclusion_reason", "framework_relation_tags",
    "review_rationale", "review_evidence", "review_evidence_pages",
    "meta_status", "meta_exclusion_reason", "meta_pending_reason",
    "bessi_measure_classification", "bessi_target_construct",
    "eligible_quantitative_components", "quantitative_scope_rationale",
    "meta_rationale", "meta_evidence", "meta_evidence_pages",
    "publication_language", "language_ai_assessed",
    "clinical_population_classification", "current_route", "final_route",
    "requires_human_review", "human_review_reasons", "decision_confidence",
    "decision_source", "eligibility_policy_version", "screening_date",
    "workflow_status", "adjudication_status", "adjudication_notes"
  )
}

ft_human_review_columns <- function() {
  c(
    "paper_id", "screening_record_id", "title", "issue_summary",
    "ai_recommendation", "decision_options", "human_review_reasons",
    "review_evidence", "review_evidence_pages", "meta_evidence", "meta_evidence_pages",
    "ai_review_eligible", "ai_meta_status", "ai_current_route",
    "ai_bessi_measure_classification", "ai_bessi_target_construct",
    "ai_eligible_quantitative_components", "ai_framework_relation_tags",
    "human_review_eligible", "human_review_exclusion_reason", "human_meta_status", "human_meta_exclusion_reason",
    "human_meta_pending_reason", "human_bessi_measure_classification",
    "human_bessi_target_construct", "human_clinical_population_classification", "human_eligible_quantitative_components",
    "human_framework_relation_tags", "adjudication_note", "adjudicated_by",
    "adjudication_date", "adjudication_status"
  )
}

ft_bool <- function(x, allow_blank = FALSE) {
  if (is.logical(x)) return(x)
  z <- tolower(ft_blank_to_empty(x))
  out <- rep(NA, length(z))
  out[z %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[z %in% c("false", "f", "0", "no", "n")] <- FALSE
  if (!allow_blank && anyNA(out)) {
    stop("Expected TRUE/FALSE value but found: ", paste(unique(z[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  out
}

ft_collapse_values <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (is.atomic(x)) {
    z <- ft_blank_to_empty(x)
    return(paste(z[nzchar(z)], collapse = ";"))
  }
  paste(vapply(x, function(v) paste(as.character(v), collapse = ";"), character(1)), collapse = ";")
}

ft_flatten_evidence <- function(items) {
  if (is.null(items) || length(items) == 0L) {
    return(list(text = "", pages = ""))
  }
  texts <- character(0)
  pages <- character(0)
  for (item in items) {
    if (is.null(item)) next
    ev <- ft_first_nonblank(item$evidence %||% "")
    pg <- item$page %||% ""
    loc <- ft_first_nonblank(item$locator %||% "")
    prefix <- ""
    if (length(pg) == 1L && !is.null(pg) && !is.na(pg) && nzchar(as.character(pg))) {
      prefix <- paste0("[p. ", pg, if (nzchar(loc)) paste0(", ", loc) else "", "] ")
      pages <- c(pages, as.character(pg))
    } else if (nzchar(loc)) {
      prefix <- paste0("[", loc, "] ")
    }
    if (nzchar(ev)) texts <- c(texts, paste0(prefix, ev))
  }
  list(text = paste(texts, collapse = " || "), pages = paste(unique(pages), collapse = ";"))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

ft_flatten_decision_object <- function(obj) {
  rev_ev <- ft_flatten_evidence(obj$review_evidence)
  meta_ev <- ft_flatten_evidence(obj$meta_evidence)
  data.frame(
    screening_record_id = ft_first_nonblank(obj$screening_record_id %||% ""),
    paper_id = ft_first_nonblank(obj$paper_id %||% ""),
    review_eligible = as.logical(obj$review_eligible %||% NA),
    review_exclusion_reason = ft_first_nonblank(obj$review_exclusion_reason %||% ""),
    framework_relation_tags = ft_collapse_values(obj$framework_relation_tags),
    review_rationale = ft_first_nonblank(obj$review_rationale %||% ""),
    review_evidence = rev_ev$text,
    review_evidence_pages = rev_ev$pages,
    meta_status = ft_first_nonblank(obj$meta_status %||% ""),
    meta_exclusion_reason = ft_first_nonblank(obj$meta_exclusion_reason %||% ""),
    meta_pending_reason = ft_first_nonblank(obj$meta_pending_reason %||% ""),
    meta_rationale = ft_first_nonblank(obj$meta_rationale %||% ""),
    meta_evidence = meta_ev$text,
    meta_evidence_pages = meta_ev$pages,
    bessi_measure_classification = ft_first_nonblank(obj$bessi_measure_classification %||% ""),
    bessi_target_construct = ft_first_nonblank(obj$bessi_target_construct %||% ""),
    eligible_quantitative_components = ft_collapse_values(obj$eligible_quantitative_components),
    quantitative_scope_rationale = ft_first_nonblank(obj$quantitative_scope_rationale %||% ""),
    publication_language = ft_first_nonblank(obj$publication_language %||% ""),
    language_ai_assessed = as.logical(obj$language_ai_assessed %||% NA),
    clinical_population_classification = ft_first_nonblank(obj$clinical_population_classification %||% ""),
    requires_human_review = as.logical(obj$requires_human_review %||% FALSE),
    human_review_reasons = ft_collapse_values(obj$human_review_reasons),
    decision_confidence = ft_first_nonblank(obj$decision_confidence %||% ""),
    final_route = ft_first_nonblank(obj$final_route %||% ""),
    current_route = ft_first_nonblank(obj$current_route %||% ""),
    decision_source = ft_first_nonblank(obj$decision_source %||% "ai"),
    eligibility_policy_version = ft_first_nonblank(obj$eligibility_policy_version %||% ""),
    screening_date = ft_first_nonblank(obj$screening_date %||% ""),
    issue_summary = ft_first_nonblank(obj$human_review_summary %||% ""),
    ai_recommendation = ft_first_nonblank(obj$ai_recommendation %||% ""),
    decision_options = ft_collapse_values(obj$decision_options),
    stringsAsFactors = FALSE
  )
}

ft_read_ai_decisions <- function(decision_dir) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to read AI full-text decisions.", call. = FALSE)
  }
  if (!dir.exists(decision_dir)) {
    stop("AI decision directory does not exist: ", decision_dir, call. = FALSE)
  }
  files <- sort(list.files(decision_dir, pattern = "\\.json$", full.names = TRUE, ignore.case = TRUE))
  if (length(files) == 0L) {
    stop("No JSON decision files found in: ", decision_dir, call. = FALSE)
  }
  rows <- lapply(files, function(path) {
    obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!is.list(obj) || is.null(obj$paper_id)) {
      stop("Each AI decision JSON must contain one decision object with paper_id. File: ", path, call. = FALSE)
    }
    out <- ft_flatten_decision_object(obj)
    out$.decision_file <- basename(path)
    out
  })
  out <- dplyr::bind_rows(rows)
  if (anyDuplicated(out$paper_id)) stop("Duplicate paper_id values across AI decision JSON files.", call. = FALSE)
  if (anyDuplicated(out$screening_record_id)) stop("Duplicate screening_record_id values across AI decision JSON files.", call. = FALSE)
  out
}

ft_derive_route <- function(review_eligible, meta_status) {
  review_eligible <- ft_bool(review_eligible)
  meta_status <- tolower(ft_blank_to_empty(meta_status))
  final <- rep("", length(meta_status))
  current <- rep("", length(meta_status))
  for (i in seq_along(meta_status)) {
    if (meta_status[[i]] == "eligible") {
      final[[i]] <- if (review_eligible[[i]]) "both" else "meta"
      current[[i]] <- final[[i]]
    } else if (meta_status[[i]] == "ineligible") {
      final[[i]] <- if (review_eligible[[i]]) "review" else "exclude"
      current[[i]] <- final[[i]]
    } else if (meta_status[[i]] == "pending") {
      current[[i]] <- if (review_eligible[[i]]) "review_meta_pending" else "meta_pending"
    } else if (meta_status[[i]] == "uncertain") {
      current[[i]] <- if (review_eligible[[i]]) "review_meta_uncertain" else "meta_uncertain"
    } else {
      stop("Invalid meta_status: ", meta_status[[i]], call. = FALSE)
    }
  }
  data.frame(final_route = final, current_route = current, stringsAsFactors = FALSE)
}

ft_validate_ai_decisions <- function(ai, manifest, policy_version = "2.0.0") {
  if (nrow(ai) != nrow(manifest)) {
    stop("AI decisions must cover every manifest record before the decision workbook is generated. Manifest records: ",
         nrow(manifest), "; AI decisions: ", nrow(ai), call. = FALSE)
  }
  miss_pid <- setdiff(manifest$paper_id, ai$paper_id)
  extra_pid <- setdiff(ai$paper_id, manifest$paper_id)
  if (length(miss_pid) || length(extra_pid)) {
    stop("AI decision coverage mismatch. Missing paper_id: ", paste(miss_pid, collapse = ", "),
         "; unexpected paper_id: ", paste(extra_pid, collapse = ", "), call. = FALSE)
  }

  idx <- match(manifest$paper_id, ai$paper_id)
  ai <- ai[idx, , drop = FALSE]
  if (any(ai$screening_record_id != manifest$screening_record_id)) {
    stop("AI decision screening_record_id does not match the monthly manifest.", call. = FALSE)
  }
  ai$paper_id <- ft_format_paper_id(ai$paper_id)
  ai$review_eligible <- ft_bool(ai$review_eligible)
  ai$requires_human_review <- ft_bool(ai$requires_human_review)
  ai$language_ai_assessed <- ft_bool(ai$language_ai_assessed, allow_blank = TRUE)
  ai$meta_status <- tolower(ft_blank_to_empty(ai$meta_status))

  allowed_meta <- c("eligible", "ineligible", "pending", "uncertain")
  if (any(!ai$meta_status %in% allowed_meta)) {
    stop("Invalid meta_status in AI decisions.", call. = FALSE)
  }
  route <- ft_derive_route(ai$review_eligible, ai$meta_status)
  supplied_final <- ft_blank_to_empty(ai$final_route)
  supplied_current <- ft_blank_to_empty(ai$current_route)
  if (any(nzchar(supplied_final) & supplied_final != route$final_route)) {
    stop("At least one AI final_route is inconsistent with review_eligible/meta_status.", call. = FALSE)
  }
  if (any(nzchar(supplied_current) & supplied_current != route$current_route)) {
    stop("At least one AI current_route is inconsistent with review_eligible/meta_status.", call. = FALSE)
  }
  ai$final_route <- route$final_route
  ai$current_route <- route$current_route

  resolved <- ai$meta_status %in% c("eligible", "ineligible")
  if (any(resolved & ai$requires_human_review)) {
    stop("Resolved AI decisions cannot retain requires_human_review=TRUE.", call. = FALSE)
  }
  if (any(ai$meta_status == "uncertain" & !ai$requires_human_review)) {
    stop("meta_status='uncertain' requires human review.", call. = FALSE)
  }
  if (any(ai$meta_status == "eligible" & !nzchar(ft_blank_to_empty(ai$eligible_quantitative_components)))) {
    stop("meta_status='eligible' requires eligible_quantitative_components.", call. = FALSE)
  }
  if (any(ai$meta_status == "ineligible" & !nzchar(ft_blank_to_empty(ai$meta_exclusion_reason)))) {
    stop("meta_status='ineligible' requires meta_exclusion_reason.", call. = FALSE)
  }
  if (any(ai$meta_status == "pending" & !nzchar(ft_blank_to_empty(ai$meta_pending_reason)))) {
    stop("meta_status='pending' requires meta_pending_reason.", call. = FALSE)
  }
  if (any(ai$review_eligible & !nzchar(ft_blank_to_empty(ai$framework_relation_tags)))) {
    stop("review_eligible=TRUE requires at least one framework_relation_tag.", call. = FALSE)
  }
  if (any(!ai$review_eligible & !nzchar(ft_blank_to_empty(ai$review_exclusion_reason)))) {
    stop("review_eligible=FALSE requires review_exclusion_reason.", call. = FALSE)
  }
  bad_target <- ai$bessi_target_construct %in% c(
    "belief_goal_or_judgment_about_seb_skills", "other_non_skill_construct"
  ) & (ai$meta_status != "ineligible" | ai$meta_exclusion_reason != "NO_ELIGIBLE_SEB_SKILL_CONSTRUCT")
  if (any(bad_target)) {
    stop("A clearly non-skill BESSI target must be meta-ineligible with NO_ELIGIBLE_SEB_SKILL_CONSTRUCT.", call. = FALSE)
  }
  if (any(ft_blank_to_empty(ai$eligibility_policy_version) != policy_version)) {
    stop("AI decision policy version does not match expected version ", policy_version, ".", call. = FALSE)
  }

  ai
}

ft_merge_manifest_decisions <- function(manifest, ai, batch_id) {
  ai <- ft_validate_ai_decisions(ai, manifest)
  idx <- match(manifest$paper_id, ai$paper_id)
  a <- ai[idx, , drop = FALSE]
  status <- ifelse(a$meta_status == "pending", "PENDING_DATA",
                   ifelse(a$meta_status == "uncertain", "HUMAN_REVIEW_REQUIRED", "RESOLVED"))
  out <- data.frame(
    batch_id = batch_id,
    screening_record_id = manifest$screening_record_id,
    paper_id = manifest$paper_id,
    doi = manifest$doi,
    title = manifest$title,
    pdf_path = manifest$pdf_path,
    review_eligible = a$review_eligible,
    review_exclusion_reason = a$review_exclusion_reason,
    framework_relation_tags = a$framework_relation_tags,
    review_rationale = a$review_rationale,
    review_evidence = a$review_evidence,
    review_evidence_pages = a$review_evidence_pages,
    meta_status = a$meta_status,
    meta_exclusion_reason = a$meta_exclusion_reason,
    meta_pending_reason = a$meta_pending_reason,
    bessi_measure_classification = a$bessi_measure_classification,
    bessi_target_construct = a$bessi_target_construct,
    eligible_quantitative_components = a$eligible_quantitative_components,
    quantitative_scope_rationale = a$quantitative_scope_rationale,
    meta_rationale = a$meta_rationale,
    meta_evidence = a$meta_evidence,
    meta_evidence_pages = a$meta_evidence_pages,
    publication_language = a$publication_language,
    language_ai_assessed = a$language_ai_assessed,
    clinical_population_classification = a$clinical_population_classification,
    current_route = a$current_route,
    final_route = a$final_route,
    requires_human_review = a$requires_human_review,
    human_review_reasons = a$human_review_reasons,
    decision_confidence = a$decision_confidence,
    decision_source = a$decision_source,
    eligibility_policy_version = a$eligibility_policy_version,
    screening_date = a$screening_date,
    workflow_status = status,
    adjudication_status = ifelse(a$requires_human_review, "OPEN", "NOT_REQUIRED"),
    adjudication_notes = "",
    stringsAsFactors = FALSE
  )
  out[, ft_decision_columns(), drop = FALSE]
}

ft_build_human_review_queue <- function(decisions, ai) {
  open <- decisions$requires_human_review %in% TRUE
  if (!any(open)) {
    out <- data.frame(matrix(ncol = length(ft_human_review_columns()), nrow = 0L), stringsAsFactors = FALSE)
    names(out) <- ft_human_review_columns()
    return(out)
  }
  aidx <- match(decisions$paper_id[open], ai$paper_id)
  a <- ai[aidx, , drop = FALSE]
  out <- data.frame(
    paper_id = decisions$paper_id[open],
    screening_record_id = decisions$screening_record_id[open],
    title = decisions$title[open],
    issue_summary = a$issue_summary,
    ai_recommendation = a$ai_recommendation,
    decision_options = a$decision_options,
    human_review_reasons = decisions$human_review_reasons[open],
    review_evidence = decisions$review_evidence[open],
    review_evidence_pages = decisions$review_evidence_pages[open],
    meta_evidence = decisions$meta_evidence[open],
    meta_evidence_pages = decisions$meta_evidence_pages[open],
    ai_review_eligible = decisions$review_eligible[open],
    ai_meta_status = decisions$meta_status[open],
    ai_current_route = decisions$current_route[open],
    ai_bessi_measure_classification = decisions$bessi_measure_classification[open],
    ai_bessi_target_construct = decisions$bessi_target_construct[open],
    ai_eligible_quantitative_components = decisions$eligible_quantitative_components[open],
    ai_framework_relation_tags = decisions$framework_relation_tags[open],
    human_review_eligible = "",
    human_review_exclusion_reason = "",
    human_meta_status = "",
    human_meta_exclusion_reason = "",
    human_meta_pending_reason = "",
    human_bessi_measure_classification = "",
    human_bessi_target_construct = "",
    human_clinical_population_classification = "",
    human_eligible_quantitative_components = "",
    human_framework_relation_tags = "",
    adjudication_note = "",
    adjudicated_by = "",
    adjudication_date = "",
    adjudication_status = "OPEN",
    stringsAsFactors = FALSE
  )
  out[, ft_human_review_columns(), drop = FALSE]
}

ft_decision_codebook <- function() {
  data.frame(
    field = c(
      "workflow_status", "review_eligible", "meta_status", "final_route",
      "framework_relation_tags", "bessi_measure_classification", "bessi_target_construct",
      "eligible_quantitative_components", "requires_human_review", "human_review_reasons",
      "adjudication_status", "human_review_eligible", "human_review_exclusion_reason", "human_meta_status",
      "human_meta_exclusion_reason", "human_clinical_population_classification", "human_eligible_quantitative_components",
      "human_framework_relation_tags", "adjudication_note"
    ),
    description = c(
      "Operational state: RESOLVED, PENDING_DATA, or HUMAN_REVIEW_REQUIRED.",
      "Whether the publication is eligible for the living review.",
      "Quantitative eligibility status: eligible, ineligible, pending, uncertain.",
      "Derived resolved route: exclude, review, meta, both. Blank for pending/uncertain.",
      "Controlled tags describing the publication's substantive relation to SEB/BESSI.",
      "Source-measure classification: clearly_eligible, clearly_ineligible, human_judgment, not_applicable, unknown.",
      "Construct actually measured; BESSI-derived beliefs/goals are not SEB skill-level data.",
      "Publication labels for only the quantitative components that proceed to sample/matrix coding.",
      "TRUE only when substantive human judgment is still required.",
      "Controlled trigger(s) explaining why human adjudication is needed.",
      "OPEN or RESOLVED for rows in human_review; NOT_REQUIRED in decisions when no human judgment is needed.",
      "Human adjudicated review eligibility. Fill only in human_review.",
      "Required when the human adjudication sets review_eligible=FALSE.",
      "Human adjudicated meta status. Fill only in human_review.",
      "Required when human_meta_status=ineligible.",
      "Optional human override when clinical severity/comparability was the issue.",
      "Required when human_meta_status=eligible; publication component labels only.",
      "Framework-relation tags after adjudication; preserve extension/modification information when relevant.",
      "Short rationale for the adjudication; should state the generalizable rule when one is established."
    ),
    stringsAsFactors = FALSE
  )
}

ft_workbook_summary <- function(decisions, batch_id) {
  data.frame(
    metric = c(
      "batch_id", "records", "resolved", "human_review_open", "pending_data",
      "route_exclude", "route_review", "route_meta", "route_both"
    ),
    value = c(
      batch_id,
      nrow(decisions),
      sum(decisions$workflow_status == "RESOLVED"),
      sum(decisions$workflow_status == "HUMAN_REVIEW_REQUIRED"),
      sum(decisions$workflow_status == "PENDING_DATA"),
      sum(decisions$final_route == "exclude"),
      sum(decisions$final_route == "review"),
      sum(decisions$final_route == "meta"),
      sum(decisions$final_route == "both")
    ),
    stringsAsFactors = FALSE
  )
}

ft_write_decision_workbook <- function(decisions, human_review, path, batch_id) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to write full-text decisions.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "living_seb_decisions_", tmpdir = dirname(path), fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  for (sheet in c("summary", "decisions", "human_review", "codebook")) {
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  }
  summary <- ft_workbook_summary(decisions, batch_id)
  openxlsx::writeDataTable(wb, "summary", summary, tableStyle = "TableStyleMedium2", withFilter = FALSE)
  openxlsx::writeDataTable(wb, "decisions", decisions, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  if (nrow(human_review) > 0L) {
    openxlsx::writeDataTable(wb, "human_review", human_review, tableStyle = "TableStyleMedium9", withFilter = TRUE)
  } else {
    openxlsx::writeData(wb, "human_review", as.data.frame(setNames(replicate(length(ft_human_review_columns()), character(0), simplify = FALSE), ft_human_review_columns())))
  }
  openxlsx::writeDataTable(wb, "codebook", ft_decision_codebook(), tableStyle = "TableStyleMedium4", withFilter = TRUE)

  for (sheet in c("summary", "decisions", "human_review", "codebook")) openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, "summary", cols = 1:2, widths = c(28, 20))
  openxlsx::setColWidths(wb, "decisions", cols = seq_along(names(decisions)), widths = "auto")
  openxlsx::setColWidths(wb, "human_review", cols = seq_along(ft_human_review_columns()), widths = "auto")
  openxlsx::setColWidths(wb, "codebook", cols = 1:2, widths = c(38, 90))

  # Cap narrative columns so auto-width does not make the workbook unusably wide.
  narrative_decision <- intersect(c("title", "review_rationale", "review_evidence", "meta_rationale", "meta_evidence", "quantitative_scope_rationale", "adjudication_notes"), names(decisions))
  if (length(narrative_decision)) openxlsx::setColWidths(wb, "decisions", cols = match(narrative_decision, names(decisions)), widths = 45)
  narrative_hr <- intersect(c("title", "issue_summary", "ai_recommendation", "decision_options", "review_evidence", "meta_evidence", "adjudication_note"), ft_human_review_columns())
  if (length(narrative_hr)) openxlsx::setColWidths(wb, "human_review", cols = match(narrative_hr, ft_human_review_columns()), widths = 45)

  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  if (nrow(decisions)) openxlsx::addStyle(wb, "decisions", wrap, rows = 2:(nrow(decisions)+1), cols = seq_along(names(decisions)), gridExpand = TRUE, stack = TRUE)
  if (nrow(human_review)) openxlsx::addStyle(wb, "human_review", wrap, rows = 2:(nrow(human_review)+1), cols = seq_along(ft_human_review_columns()), gridExpand = TRUE, stack = TRUE)

  # Human-editable adjudication fields are blue; open cases are highlighted.
  if (nrow(human_review)) {
    editable <- c(
      "human_review_eligible", "human_review_exclusion_reason", "human_meta_status", "human_meta_exclusion_reason",
      "human_meta_pending_reason", "human_bessi_measure_classification", "human_bessi_target_construct", "human_clinical_population_classification",
      "human_eligible_quantitative_components", "human_framework_relation_tags", "adjudication_note",
      "adjudicated_by", "adjudication_date", "adjudication_status"
    )
    blue <- openxlsx::createStyle(fontColour = "#0000FF")
    orange <- openxlsx::createStyle(fgFill = "#FCE4D6")
    openxlsx::addStyle(wb, "human_review", blue, rows = 2:(nrow(human_review)+1), cols = match(editable, ft_human_review_columns()), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, "human_review", orange, rows = 2:(nrow(human_review)+1), cols = seq_along(ft_human_review_columns()), gridExpand = TRUE, stack = TRUE)

    openxlsx::dataValidation(wb, "human_review", cols = match("human_review_eligible", ft_human_review_columns()), rows = 2:(nrow(human_review)+1), type = "list", value = '"TRUE,FALSE"')
    openxlsx::dataValidation(wb, "human_review", cols = match("human_meta_status", ft_human_review_columns()), rows = 2:(nrow(human_review)+1), type = "list", value = '"eligible,ineligible,pending,uncertain"')
    openxlsx::dataValidation(wb, "human_review", cols = match("adjudication_status", ft_human_review_columns()), rows = 2:(nrow(human_review)+1), type = "list", value = '"OPEN,RESOLVED"')
  }

  # DOI hyperlinks on the decisions sheet.
  doi_col <- match("doi", names(decisions))
  if (!is.na(doi_col) && nrow(decisions)) {
    for (i in seq_len(nrow(decisions))) {
      url <- ft_blank_to_empty(decisions$doi[[i]])
      if (nzchar(url)) {
        link <- url; class(link) <- "hyperlink"
        openxlsx::writeData(wb, "decisions", link, startCol = doi_col, startRow = i + 1L)
      }
    }
    link_style <- openxlsx::createStyle(fontColour = "#0563C1", textDecoration = "underline")
    openxlsx::addStyle(wb, "decisions", link_style, rows = 2:(nrow(decisions)+1), cols = doi_col, gridExpand = TRUE, stack = TRUE)
  }

  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  ok <- file.copy(tmp, path, overwrite = TRUE)
  unlink(tmp)
  if (!isTRUE(ok)) stop("Could not write decision workbook: ", path, call. = FALSE)
  invisible(path)
}

ft_read_decision_workbook <- function(path) {
  if (!file.exists(path)) stop("Decision workbook not found: ", path, call. = FALSE)
  decisions <- ft_clean_names(as.data.frame(readxl::read_excel(path, sheet = "decisions"), stringsAsFactors = FALSE))
  human <- ft_clean_names(as.data.frame(readxl::read_excel(path, sheet = "human_review"), stringsAsFactors = FALSE))
  list(decisions = decisions, human_review = human)
}

ft_apply_adjudications <- function(decisions, human_review) {
  if (nrow(human_review) == 0L) return(list(decisions = decisions, human_review = human_review, applied = 0L))
  human_review$adjudication_status <- toupper(ft_blank_to_empty(human_review$adjudication_status))
  resolved_rows <- which(human_review$adjudication_status == "RESOLVED")
  if (length(resolved_rows) == 0L) return(list(decisions = decisions, human_review = human_review, applied = 0L))

  for (j in resolved_rows) {
    pid <- ft_format_paper_id(human_review$paper_id[[j]])
    i <- match(pid, decisions$paper_id)
    if (is.na(i)) stop("Adjudication paper_id not found in decisions: ", pid, call. = FALSE)

    rev <- ft_bool(human_review$human_review_eligible[[j]])[[1]]
    rev_excl <- ft_first_nonblank(human_review$human_review_exclusion_reason[[j]])
    meta <- tolower(ft_first_nonblank(human_review$human_meta_status[[j]]))
    if (!meta %in% c("eligible", "ineligible", "pending")) {
      stop("A RESOLVED adjudication must set human_meta_status to eligible, ineligible, or pending for paper ", pid, call. = FALSE)
    }
    route <- ft_derive_route(rev, meta)
    meta_excl <- ft_first_nonblank(human_review$human_meta_exclusion_reason[[j]])
    meta_pending <- ft_first_nonblank(human_review$human_meta_pending_reason[[j]])
    components <- ft_first_nonblank(human_review$human_eligible_quantitative_components[[j]])
    tags <- ft_first_nonblank(human_review$human_framework_relation_tags[[j]])

    if (!rev && !nzchar(rev_excl)) stop("Resolved review-ineligible adjudication requires human_review_exclusion_reason for paper ", pid, call. = FALSE)
    if (meta == "ineligible" && !nzchar(meta_excl)) stop("Resolved ineligible adjudication requires human_meta_exclusion_reason for paper ", pid, call. = FALSE)
    if (meta == "pending" && !nzchar(meta_pending)) stop("Resolved pending adjudication requires human_meta_pending_reason for paper ", pid, call. = FALSE)
    if (meta == "eligible" && !nzchar(components)) stop("Resolved eligible adjudication requires human_eligible_quantitative_components for paper ", pid, call. = FALSE)
    if (rev && !nzchar(tags)) tags <- decisions$framework_relation_tags[[i]]
    if (rev && !nzchar(tags)) stop("Review-eligible adjudication requires framework relation tags for paper ", pid, call. = FALSE)

    decisions$review_eligible[[i]] <- rev
    decisions$review_exclusion_reason[[i]] <- if (rev) "" else rev_excl
    decisions$framework_relation_tags[[i]] <- if (rev) tags else ""
    decisions$meta_status[[i]] <- meta
    decisions$meta_exclusion_reason[[i]] <- if (meta == "ineligible") meta_excl else ""
    decisions$meta_pending_reason[[i]] <- if (meta == "pending") meta_pending else ""
    decisions$eligible_quantitative_components[[i]] <- if (meta == "eligible") components else ""

    bm <- ft_first_nonblank(human_review$human_bessi_measure_classification[[j]])
    bt <- ft_first_nonblank(human_review$human_bessi_target_construct[[j]])
    hc <- ft_first_nonblank(human_review$human_clinical_population_classification[[j]])
    if (nzchar(bt) && bt %in% c("belief_goal_or_judgment_about_seb_skills", "other_non_skill_construct") && (meta != "ineligible" || meta_excl != "NO_ELIGIBLE_SEB_SKILL_CONSTRUCT")) {
      stop("Adjudicated non-skill BESSI target requires meta_status=ineligible with NO_ELIGIBLE_SEB_SKILL_CONSTRUCT for paper ", pid, call. = FALSE)
    }
    if (nzchar(bm)) decisions$bessi_measure_classification[[i]] <- bm
    if (nzchar(bt)) decisions$bessi_target_construct[[i]] <- bt
    if (nzchar(hc)) decisions$clinical_population_classification[[i]] <- hc

    decisions$final_route[[i]] <- route$final_route[[1]]
    decisions$current_route[[i]] <- route$current_route[[1]]
    decisions$requires_human_review[[i]] <- FALSE
    decisions$human_review_reasons[[i]] <- ""
    decisions$workflow_status[[i]] <- if (meta == "pending") "PENDING_DATA" else "RESOLVED"
    decisions$adjudication_status[[i]] <- "RESOLVED"
    decisions$adjudication_notes[[i]] <- ft_first_nonblank(human_review$adjudication_note[[j]])
    decisions$decision_source[[i]] <- "human_adjudicated"
  }

  list(decisions = decisions, human_review = human_review, applied = length(resolved_rows))
}
