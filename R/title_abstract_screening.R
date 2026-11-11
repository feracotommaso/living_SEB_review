# Living SEB title/abstract screening helpers.
# AI supplies structured decisions; R validates them and applies the
# deterministic retention/exclusion policy.

screening_clean_column_names <- function(data) {
  cleaned <- trimws(names(data))
  cleaned <- tolower(cleaned)
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  names(data) <- cleaned
  data
}

screening_read_table <- function(path, sheet = NULL) {
  if (!file.exists(path)) {
    stop("Screening input not found: ", path, call. = FALSE)
  }

  extension <- tolower(tools::file_ext(path))

  if (extension == "csv") {
    return(
      readr::read_csv(
        path,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        na = c("", "NA")
      ) |>
        as.data.frame(stringsAsFactors = FALSE)
    )
  }

  if (extension %in% c("xlsx", "xls")) {
    args <- list(
      path = path,
      col_types = "text",
      .name_repair = "minimal"
    )
    if (!is.null(sheet)) {
      args$sheet <- sheet
    }
    return(
      do.call(readxl::read_excel, args) |>
        as.data.frame(stringsAsFactors = FALSE)
    )
  }

  if (extension %in% c("jsonl", "ndjson")) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0L) {
      return(data.frame())
    }
    return(
      dplyr::bind_rows(lapply(lines, jsonlite::fromJSON)) |>
        as.data.frame(stringsAsFactors = FALSE)
    )
  }

  stop("Unsupported screening table extension: ", extension, call. = FALSE)
}

screening_load_policy <- function(
  path = here::here("config", "screening", "title_abstract.yml")
) {
  if (!file.exists(path)) {
    stop("Screening policy not found: ", path, call. = FALSE)
  }

  policy <- yaml::read_yaml(path)
  required <- c(
    "version",
    "prompt_version",
    "stage",
    "exclusion_reasons",
    "allowed_values",
    "operational_policy"
  )
  missing <- setdiff(required, names(policy))

  if (length(missing) > 0L) {
    stop(
      "Screening policy is missing fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(policy$stage, "title_abstract")) {
    stop("Unexpected screening stage in policy.", call. = FALSE)
  }

  policy
}

screening_normalise_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi[[:space:]]*:[[:space:]]*", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x <- sub("[.,;]+$", "", x)
  x
}

screening_normalise_key_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

screening_pick_column <- function(data, candidates, required = TRUE) {
  available <- intersect(candidates, names(data))
  if (length(available) > 0L) {
    return(available[[1L]])
  }

  if (required) {
    stop(
      "None of the required columns was found: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }

  NA_character_
}

screening_make_record_ids <- function(data) {
  title_col <- screening_pick_column(
    data,
    c("title", "article_title", "document_title")
  )
  doi_col <- screening_pick_column(
    data,
    c("doi", "digital_object_identifier"),
    required = FALSE
  )
  author_col <- screening_pick_column(
    data,
    c("author", "authors"),
    required = FALSE
  )
  year_col <- screening_pick_column(
    data,
    c("year", "publication_year"),
    required = FALSE
  )

  doi <- if (is.na(doi_col)) rep("", nrow(data)) else data[[doi_col]]
  author <- if (is.na(author_col)) rep("", nrow(data)) else data[[author_col]]
  year <- if (is.na(year_col)) rep("", nrow(data)) else data[[year_col]]

  doi <- screening_normalise_doi(doi)
  title <- screening_normalise_key_text(data[[title_col]])
  author <- screening_normalise_key_text(author)
  year <- screening_normalise_key_text(year)

  keys <- ifelse(
    nzchar(doi),
    paste0("doi:", doi),
    paste("title", title, "year", year, "author", author, sep = "|")
  )

  if (any(!nzchar(title) & !nzchar(doi))) {
    stop(
      "At least one record has neither a usable title nor a DOI.",
      call. = FALSE
    )
  }

  hashes <- vapply(
    keys,
    digest::digest,
    FUN.VALUE = character(1),
    algo = "sha256",
    serialize = FALSE
  )
  ids <- paste0("seb_", substr(hashes, 1L, 16L))

  if (anyDuplicated(ids)) {
    duplicated_rows <- which(duplicated(ids) | duplicated(ids, fromLast = TRUE))
    stop(
      "Stable screening_record_id collision/duplicate for rows: ",
      paste(duplicated_rows, collapse = ", "),
      ". Resolve bibliographic duplicates before screening.",
      call. = FALSE
    )
  }

  ids
}

screening_prepare_input <- function(data, batch_id, policy) {
  data <- screening_clean_column_names(data)

  if (nrow(data) == 0L) {
    stop("No records available for title/abstract screening.", call. = FALSE)
  }

  if ("screening_record_id" %in% names(data)) {
    ids <- trimws(as.character(data$screening_record_id))
    if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
      stop("Existing screening_record_id values are invalid.", call. = FALSE)
    }
  } else {
    ids <- screening_make_record_ids(data)
  }

  data$screening_record_id <- ids
  data$screening_batch <- batch_id
  data$policy_version <- policy$version
  data$prompt_version <- policy$prompt_version

  preferred <- c(
    "screening_record_id",
    "screening_batch",
    "title",
    "abstract",
    "document_type",
    "publication_type",
    "journal",
    "year",
    "author",
    "doi",
    "url",
    "isbn",
    "issn",
    "cite_source",
    "cite_label",
    "cite_string",
    "policy_version",
    "prompt_version"
  )

  data[, c(intersect(preferred, names(data)), setdiff(names(data), preferred)), drop = FALSE]
}

screening_excel_safe_text <- function(x, limit = 32000L) {
  x <- as.character(x)
  too_long <- !is.na(x) & nchar(x, type = "chars") > limit
  x[too_long] <- paste0(
    substr(x[too_long], 1L, limit - 24L),
    " [TRUNCATED FOR EXCEL]"
  )
  x
}

screening_set_widths <- function(wb, sheet, data) {
  if (ncol(data) == 0L) {
    return(invisible(NULL))
  }

  for (j in seq_len(ncol(data))) {
    nm <- names(data)[[j]]
    width <- if (grepl("abstract|rationale|evidence", nm, ignore.case = TRUE)) {
      60
    } else if (grepl("title", nm, ignore.case = TRUE)) {
      52
    } else if (grepl("author", nm, ignore.case = TRUE)) {
      32
    } else if (grepl("doi|record_id|url", nm, ignore.case = TRUE)) {
      28
    } else {
      18
    }
    openxlsx::setColWidths(wb, sheet, cols = j, widths = width)
  }
}

screening_write_input_workbook <- function(data, path) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[] <- lapply(data, screening_excel_safe_text)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Instructions", gridLines = FALSE)
  openxlsx::addWorksheet(wb, "records", gridLines = FALSE)

  instructions <- data.frame(
    item = c(
      "Purpose",
      "Canonical input",
      "AI output",
      "Policy",
      "Human intervention"
    ),
    guidance = c(
      "Input records for conservative Living SEB title/abstract screening.",
      "The CSV with the same basename is the canonical machine-readable input.",
      "The AI must preserve screening_record_id and write the required decisions CSV.",
      "Exclude only when an allowed criterion is clear. Reviews, meta-analyses, editorials and uncertain records are retained.",
      "Scientific uncertainty is retained automatically; human screening is not required at this stage."
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::writeDataTable(
    wb, "Instructions", instructions,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )
  openxlsx::setColWidths(wb, "Instructions", cols = 1, widths = 24)
  openxlsx::setColWidths(wb, "Instructions", cols = 2, widths = 95)

  openxlsx::writeDataTable(
    wb, "records", data,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, "records", firstActiveRow = 2, firstActiveCol = 2)

  if (nrow(data) > 0L && ncol(data) > 0L) {
    imported_style <- openxlsx::createStyle(
      fontColour = "#008000",
      valign = "top",
      wrapText = TRUE
    )
    openxlsx::addStyle(
      wb, "records", imported_style,
      rows = 2:(nrow(data) + 1L),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  screening_set_widths(wb, "records", data)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

screening_write_jsonl <- function(data, path) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  lines <- vapply(
    seq_len(nrow(data)),
    FUN = function(i) {
      jsonlite::toJSON(
        as.list(data[i, , drop = FALSE]),
        auto_unbox = TRUE,
        na = "null",
        null = "null"
      )
    },
    FUN.VALUE = character(1)
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

screening_required_decision_columns <- function() {
  c(
    "screening_record_id",
    "ai_decision",
    "ai_exclusion_reason",
    "ai_confidence",
    "ai_rationale",
    "ai_evidence",
    "policy_version",
    "prompt_version",
    "model",
    "screened_at_utc"
  )
}

screening_read_decisions <- function(path) {
  data <- screening_read_table(path)
  screening_clean_column_names(data)
}

screening_blank_to_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

screening_validate_decisions <- function(decisions, input, policy) {
  decisions <- as.data.frame(decisions, stringsAsFactors = FALSE)
  input <- as.data.frame(input, stringsAsFactors = FALSE)

  required <- screening_required_decision_columns()
  missing <- setdiff(required, names(decisions))
  if (length(missing) > 0L) {
    stop(
      "AI decision file is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  decisions <- decisions[, required, drop = FALSE]
  decisions[] <- lapply(decisions, screening_blank_to_empty)
  decisions$ai_decision <- tolower(decisions$ai_decision)
  decisions$ai_confidence <- tolower(decisions$ai_confidence)
  decisions$ai_exclusion_reason <- toupper(decisions$ai_exclusion_reason)

  ids <- decisions$screening_record_id
  expected_ids <- as.character(input$screening_record_id)

  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("AI decisions contain blank or duplicated record IDs.", call. = FALSE)
  }

  if (!setequal(ids, expected_ids)) {
    missing_ids <- setdiff(expected_ids, ids)
    extra_ids <- setdiff(ids, expected_ids)
    stop(
      "AI decision coverage does not match the screening input. Missing IDs: ",
      paste(missing_ids, collapse = ", "),
      "; extra IDs: ",
      paste(extra_ids, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_decisions <- unlist(policy$allowed_values$ai_decision)
  allowed_confidence <- unlist(policy$allowed_values$ai_confidence)
  allowed_reasons <- names(policy$exclusion_reasons)

  bad_decisions <- !decisions$ai_decision %in% allowed_decisions
  bad_confidence <- !decisions$ai_confidence %in% allowed_confidence

  if (any(bad_decisions)) {
    stop("Invalid ai_decision value(s).", call. = FALSE)
  }
  if (any(bad_confidence)) {
    stop("Invalid ai_confidence value(s).", call. = FALSE)
  }

  exclusion_rows <- decisions$ai_decision == "exclude"
  non_exclusion_rows <- !exclusion_rows

  if (any(exclusion_rows & !decisions$ai_exclusion_reason %in% allowed_reasons)) {
    stop("Excluded records must use an allowed exclusion reason.", call. = FALSE)
  }
  if (any(non_exclusion_rows & nzchar(decisions$ai_exclusion_reason))) {
    stop(
      "ai_exclusion_reason must be blank for include/uncertain decisions.",
      call. = FALSE
    )
  }
  if (any(!nzchar(decisions$ai_rationale))) {
    stop("Every AI decision requires a rationale.", call. = FALSE)
  }
  if (any(exclusion_rows & !nzchar(decisions$ai_evidence))) {
    stop("Every exclusion requires explicit evidence.", call. = FALSE)
  }
  if (any(decisions$policy_version != policy$version)) {
    stop("AI decision policy_version does not match the active policy.", call. = FALSE)
  }
  if (any(decisions$prompt_version != policy$prompt_version)) {
    stop("AI decision prompt_version does not match the active prompt.", call. = FALSE)
  }
  if (any(!nzchar(decisions$model)) || any(!nzchar(decisions$screened_at_utc))) {
    stop("Model and screened_at_utc are required for audit.", call. = FALSE)
  }

  decisions[match(expected_ids, decisions$screening_record_id), , drop = FALSE]
}

screening_apply_operational_policy <- function(decisions, policy) {
  decisions <- as.data.frame(decisions, stringsAsFactors = FALSE)
  allowed_reasons <- names(policy$exclusion_reasons)

  valid_high_exclusion <-
    decisions$ai_decision == "exclude" &
    decisions$ai_confidence == "high" &
    decisions$ai_exclusion_reason %in% allowed_reasons &
    nzchar(decisions$ai_evidence)

  decisions$automatic_action <- dplyr::case_when(
    decisions$ai_decision == "include" ~ "KEEP",
    decisions$ai_decision == "uncertain" ~ "KEEP_UNCERTAIN",
    valid_high_exclusion ~ "EXCLUDE",
    TRUE ~ "KEEP_UNCERTAIN"
  )

  decisions$action_basis <- dplyr::case_when(
    decisions$automatic_action == "KEEP" ~ "AI include",
    decisions$ai_decision == "uncertain" ~ "Uncertain records are retained by policy",
    decisions$automatic_action == "EXCLUDE" ~ "High-confidence exclusion with valid reason and evidence",
    TRUE ~ "Non-high-confidence exclusion is retained conservatively"
  )

  decisions$final_screening_decision <- ifelse(
    decisions$automatic_action == "EXCLUDE",
    "exclude",
    "include"
  )
  decisions$human_review_required <- FALSE
  decisions
}

screening_write_records_workbook <- function(data, path, sheet = "records") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[] <- lapply(data, screening_excel_safe_text)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

  if (nrow(data) > 0L && ncol(data) > 0L) {
    openxlsx::writeDataTable(
      wb, sheet, data,
      tableStyle = "TableStyleMedium2",
      withFilter = TRUE
    )
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = 2)
    imported_style <- openxlsx::createStyle(
      fontColour = "#008000",
      valign = "top",
      wrapText = TRUE
    )
    openxlsx::addStyle(
      wb, sheet, imported_style,
      rows = 2:(nrow(data) + 1L),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE,
      stack = TRUE
    )
  } else if (ncol(data) > 0L) {
    openxlsx::writeData(wb, sheet, data, colNames = TRUE)
  } else {
    openxlsx::writeData(wb, sheet, "No records.")
  }

  screening_set_widths(wb, sheet, data)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

screening_write_decisions_workbook <- function(
  decisions,
  included,
  excluded,
  summary,
  path
) {
  wb <- openxlsx::createWorkbook()

  instructions <- data.frame(
    item = c(
      "Purpose",
      "Retention policy",
      "Automatic exclusion",
      "Human screening"
    ),
    guidance = c(
      "Audit workbook for Living SEB title/abstract screening.",
      "AI include and uncertain decisions are retained.",
      "Only high-confidence exclusions with an allowed reason and explicit evidence are excluded.",
      "No scientific human review is required at this stage; schema or coverage errors stop the workflow instead."
    ),
    stringsAsFactors = FALSE
  )

  sheets <- list(
    Instructions = instructions,
    summary = summary,
    decisions = decisions,
    included = included,
    excluded = excluded
  )

  for (nm in names(sheets)) {
    data <- as.data.frame(sheets[[nm]], stringsAsFactors = FALSE)
    data[] <- lapply(data, screening_excel_safe_text)
    openxlsx::addWorksheet(wb, nm, gridLines = FALSE)

    if (nrow(data) > 0L && ncol(data) > 0L) {
      openxlsx::writeDataTable(
        wb, nm, data,
        tableStyle = "TableStyleMedium2",
        withFilter = nm != "Instructions"
      )
      openxlsx::freezePane(wb, nm, firstActiveRow = 2)
    } else if (ncol(data) > 0L) {
      openxlsx::writeData(wb, nm, data, colNames = TRUE)
    } else {
      openxlsx::writeData(wb, nm, "No records.")
    }

    screening_set_widths(wb, nm, data)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

screening_finalize_outputs <- function(
  input,
  decisions,
  output_dir,
  batch_id,
  policy,
  workflow_version = NA_character_
) {
  validated <- screening_validate_decisions(decisions, input, policy)
  operational <- screening_apply_operational_policy(validated, policy)

  combined <- dplyr::left_join(
    input,
    operational,
    by = "screening_record_id",
    suffix = c("", ".decision")
  )

  included <- combined[combined$automatic_action != "EXCLUDE", , drop = FALSE]
  excluded <- combined[combined$automatic_action == "EXCLUDE", , drop = FALSE]

  summary <- tibble::tibble(
    workflow_version = workflow_version,
    policy_version = policy$version,
    prompt_version = policy$prompt_version,
    batch_id = batch_id,
    input_records = nrow(combined),
    keep_records = sum(combined$automatic_action == "KEEP"),
    keep_uncertain_records = sum(combined$automatic_action == "KEEP_UNCERTAIN"),
    excluded_records = sum(combined$automatic_action == "EXCLUDE"),
    included_for_next_stage = nrow(included),
    human_review_required = FALSE,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- list(
    decisions_csv = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_decisions.csv")
    ),
    decisions_xlsx = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_screening.xlsx")
    ),
    included_csv = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_included.csv")
    ),
    included_xlsx = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_included.xlsx")
    ),
    excluded_csv = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_excluded.csv")
    ),
    excluded_xlsx = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_excluded.xlsx")
    ),
    summary_csv = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_summary.csv")
    ),
    manifest_json = file.path(
      output_dir,
      paste0(batch_id, "_title_abstract_manifest.json")
    )
  )

  readr::write_csv(combined, paths$decisions_csv)
  readr::write_csv(included, paths$included_csv)
  if (!getOption("living_seb.compact", file.exists("data/3.meta_data/coding/records.csv"))) {
  readr::write_csv(excluded, paths$excluded_csv)
  readr::write_csv(summary, paths$summary_csv)

  screening_write_records_workbook(
    included,
    paths$included_xlsx,
    sheet = "included"
  )
  screening_write_records_workbook(
    excluded,
    paths$excluded_xlsx,
    sheet = "excluded"
  )
  screening_write_decisions_workbook(
    combined,
    included,
    excluded,
    summary,
    paths$decisions_xlsx
  )

  manifest <- list(
    workflow_version = workflow_version,
    policy_version = policy$version,
    prompt_version = policy$prompt_version,
    batch_id = batch_id,
    input_records = nrow(combined),
    included_for_next_stage = nrow(included),
    excluded_records = nrow(excluded),
    keep_uncertain_records = sum(combined$automatic_action == "KEEP_UNCERTAIN"),
    human_review_required = FALSE,
    files = lapply(paths, normalizePath, winslash = "/", mustWork = FALSE),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  jsonlite::write_json(
    manifest,
    paths$manifest_json,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  }

  list(
    summary = summary,
    decisions = combined,
    included = included,
    excluded = excluded,
    paths = paths
  )
}

screening_compare_golden <- function(actual, expected) {
  actual <- screening_clean_column_names(actual)
  expected <- screening_clean_column_names(expected)

  required_actual <- c(
    "screening_record_id",
    "ai_decision",
    "ai_exclusion_reason",
    "ai_confidence",
    "automatic_action"
  )
  required_expected <- c(
    "screening_record_id",
    "expected_ai_decision",
    "expected_ai_exclusion_reason",
    "expected_ai_confidence",
    "expected_automatic_action"
  )

  if (length(setdiff(required_actual, names(actual))) > 0L ||
      length(setdiff(required_expected, names(expected))) > 0L) {
    stop("Golden comparison files have invalid columns.", call. = FALSE)
  }

  check <- dplyr::inner_join(
    actual[, required_actual, drop = FALSE],
    expected[, required_expected, drop = FALSE],
    by = "screening_record_id"
  )

  if (nrow(check) != nrow(expected) || nrow(check) != nrow(actual)) {
    stop("Golden comparison record coverage differs.", call. = FALSE)
  }

  check$matches_expected <-
    check$ai_decision == check$expected_ai_decision &
    screening_blank_to_empty(check$ai_exclusion_reason) ==
      screening_blank_to_empty(check$expected_ai_exclusion_reason) &
    check$ai_confidence == check$expected_ai_confidence &
    check$automatic_action == check$expected_automatic_action

  check
}
