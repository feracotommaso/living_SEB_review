# Additive, audited codebook maintenance. CSV is canonical; XLSX is an interface.
codebook_columns <- function(kind) switch(kind,
  variables = c("Topic", "broad", "column_name", "label", "notes"),
  topics = c("Broad_topic", "Broad_topic_labs", "Topic", "Topic_labs", "Explanation", "notes"),
  stop("Unknown codebook kind: ", kind, call. = FALSE))

codebook_key <- function(kind) if (kind == "variables") "column_name" else "Topic"
codebook_blank <- function(x) is.na(x) | !nzchar(trimws(x))
codebook_hash <- function(x) digest::digest(x, algo = "sha256")
codebook_csv <- function(path) as.data.frame(readr::read_csv(path,
  col_types = readr::cols(.default = "c"), na = "", progress = FALSE))
codebook_excel <- function(path) as.data.frame(readxl::read_excel(path, col_types = "text"))

codebook_validate <- function(x, kind, additions = FALSE) {
  expected <- codebook_columns(kind)
  if (!identical(names(x), expected)) stop("Codebook column contract differs: ", kind, call. = FALSE)
  key <- x[[codebook_key(kind)]]
  present <- !codebook_blank(key)
  if (anyDuplicated(key[present])) stop("Duplicate codebook key: ", kind, call. = FALSE)
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", key[present])))
    stop("Invalid codebook name: ", kind, call. = FALSE)
  required <- if (kind == "variables") expected[1:4] else expected[1:4]
  if (additions) required <- c(required, if (kind == "variables") "notes" else "Explanation")
  for (field in required) if (any(codebook_blank(x[[field]][present])))
    stop("Missing codebook field ", field, ": ", kind, call. = FALSE)
  if (additions && any(!present)) stop("An addition requires a codebook key.", call. = FALSE)
  invisible(x)
}

codebook_suffixes <- function(path = "config/coding/variable_suffixes.csv") {
  x <- codebook_csv(path)
  if (!identical(names(x), c("suffix", "label", "definition", "example")) ||
      !nrow(x) || anyDuplicated(x$suffix) || any(codebook_blank(x$suffix)) ||
      any(!grepl("^[a-z]+$", x$suffix)) || any(codebook_blank(x$label)) ||
      any(codebook_blank(x$definition))) stop("Invalid measurement suffix registry.", call. = FALSE)
  x
}

codebook_resolve_variables <- function(requested,
    variables = codebook_csv("data/matrix_codebook.csv"), suffixes = codebook_suffixes()) {
  codebook_validate(variables, "variables")
  if (any(codebook_blank(requested)) || anyDuplicated(requested))
    stop("Requested variable names must be present and unique.", call. = FALSE)
  base <- variables[!codebook_blank(variables$column_name), , drop = FALSE]
  rows <- lapply(requested, function(name) {
    j <- match(name, base$column_name)
    suffix <- ""
    if (is.na(j)) {
      candidates <- which(vapply(suffixes$suffix, function(s) endsWith(name, s) &&
        substr(name, 1L, nchar(name) - nchar(s)) %in% base$column_name, logical(1)))
      if (length(candidates) != 1L) stop("Unregistered or ambiguous variable: ", name, call. = FALSE)
      suffix <- suffixes$suffix[[candidates]]
      j <- match(substr(name, 1L, nchar(name) - nchar(suffix)), base$column_name)
    }
    data.frame(column_name = name, base_name = base$column_name[[j]], suffix = suffix,
      Topic = base$Topic[[j]], broad = base$broad[[j]], label = if (nzchar(suffix))
        paste0(base$label[[j]], " (", suffixes$label[match(suffix, suffixes$suffix)], ")") else base$label[[j]],
      stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame(column_name = character(), base_name = character(),
    suffix = character(), Topic = character(), broad = character(), label = character()))
  do.call(rbind, rows)
}

codebook_add <- function(current, proposals, kind) {
  codebook_validate(current, kind)
  if (is.null(proposals) || !nrow(proposals)) return(list(data = current, audit = NULL))
  fields <- codebook_columns(kind)
  provenance <- c("paper_id", "change_date", "reason", "source")
  if (!setequal(names(proposals), c(fields, provenance)))
    stop("Additions need codebook columns plus paper_id, change_date, reason and source.", call. = FALSE)
  additions <- proposals[, fields, drop = FALSE]
  codebook_validate(additions, kind, additions = TRUE)
  for (field in provenance) if (any(codebook_blank(proposals[[field]])))
    stop("Missing addition provenance: ", field, call. = FALSE)
  if (any(!grepl("^[0-9]{4,}$", proposals$paper_id)) ||
      any(is.na(as.Date(proposals$change_date, format = "%Y-%m-%d"))))
    stop("Invalid addition paper_id or change_date.", call. = FALSE)
  key <- codebook_key(kind)
  audit <- list()
  for (i in seq_len(nrow(additions))) {
    entry <- additions[i, , drop = FALSE]; rownames(entry) <- NULL
    j <- match(entry[[key]], current[[key]])
    if (!is.na(j)) {
      existing <- current[j, , drop = FALSE]; rownames(existing) <- NULL
      if (!identical(existing, entry)) stop("Addition would redefine an existing code: ", entry[[key]], call. = FALSE)
    } else current <- rbind(current, entry)
    evidence <- proposals[i, provenance, drop = FALSE]; rownames(evidence) <- NULL
    audit[[i]] <- data.frame(change_id = codebook_hash(list(kind, entry, evidence)),
      kind = kind, code = entry[[key]], action = "add", paper_id = proposals$paper_id[[i]],
      change_date = proposals$change_date[[i]], reason = proposals$reason[[i]],
      source = proposals$source[[i]], entry_sha256 = codebook_hash(entry), stringsAsFactors = FALSE)
  }
  rownames(current) <- NULL
  list(data = current, audit = do.call(rbind, audit))
}

codebook_format_new_rows <- function(wb, sheet, rows, values, source_path) {
  # Read existing column widths; wrap only added cells without restyling history.
  con <- unz(source_path, "xl/worksheets/sheet1.xml", open = "rb")
  document <- tryCatch(xml2::read_xml(con), finally = close(con))
  definitions <- xml2::xml_find_all(document, "//*[local-name()='cols']/*[local-name()='col']")
  widths <- rep(8.43, ncol(values))
  for (node in definitions) {
    columns <- intersect(seq.int(as.integer(xml2::xml_attr(node, "min")), as.integer(xml2::xml_attr(node, "max"))), seq_len(ncol(values)))
    widths[columns] <- as.numeric(xml2::xml_attr(node, "width"))
  }
  definition_column <- match(if ("Explanation" %in% names(values)) "Explanation" else "notes", names(values))
  widths[[definition_column]] <- max(widths[[definition_column]], 70)
  openxlsx::setColWidths(wb, sheet, cols = definition_column, widths = widths[[definition_column]])
  heights <- vapply(seq_len(nrow(values)), function(i) {
    text <- vapply(values[i, , drop = FALSE], function(x) if (is.na(x)) "" else x, character(1))
    max(30, max(ceiling(nchar(text) / pmax(1, widths * .8))) * 15 + 6)
  }, numeric(1))
  if (any(heights > 409)) stop("New codebook description exceeds a readable Excel row height.")
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = rows, cols = seq_len(ncol(values)), gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = rows, heights = heights)
}

# Prepare everything before replacing files. Restore previous bytes on copy failure.
codebook_commit_files <- function(staged, destinations, original_hashes) {
  current <- vapply(destinations, function(p) if (file.exists(p))
    digest::digest(p, algo = "sha256", file = TRUE) else "absent", character(1))
  if (!identical(unname(current), unname(original_hashes)))
    stop("Codebook destinations changed during preparation; rebuild before saving.", call. = FALSE)
  backups <- lapply(destinations, function(p) if (file.exists(p)) readBin(p, "raw", n = file.info(p)$size) else NULL)
  for (i in seq_along(destinations)) {
    dir.create(dirname(destinations[[i]]), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(staged[[i]], destinations[[i]], overwrite = TRUE)) {
      for (j in seq_len(i)) {
        if (is.null(backups[[j]])) {
          if (file.exists(destinations[[j]])) unlink(destinations[[j]])
        } else writeBin(backups[[j]], destinations[[j]])
      }
      stop("Could not save codebook outputs; prior content restored.", call. = FALSE)
    }
  }
}

codebook_sync <- function(root = ".", add_variables = NULL, add_topics = NULL) {
  kinds <- c("variables", "topics")
  stems <- c(variables = "matrix_codebook", topics = "topics_codebook")
  ledger_path <- file.path(root, "data/3.meta_data/codebook_changes.csv")
  initial_paths <- c(file.path(root, "data", paste0(rep(stems, each = 2L), rep(c(".csv", ".xlsx"), 2L))), ledger_path)
  initial_hashes <- setNames(vapply(initial_paths, function(p) if (file.exists(p))
    digest::digest(p, algo = "sha256", file = TRUE) else "absent", character(1)), initial_paths)
  proposals <- list(variables = add_variables, topics = add_topics)
  original <- results <- list()
  audits <- list()
  for (kind in kinds) {
    csv <- file.path(root, "data", paste0(stems[[kind]], ".csv"))
    xlsx <- sub("\\.csv$", ".xlsx", csv)
    if (!file.exists(xlsx)) stop("Missing current codebook workbook: ", xlsx, call. = FALSE)
    excel <- codebook_excel(xlsx)
    canonical <- if (file.exists(csv)) codebook_csv(csv) else excel
    codebook_validate(canonical, kind)
    if (!identical(canonical, excel))
      stop("CSV/XLSX codebook conflict; reconcile explicitly: ", kind, call. = FALSE)
    original[[kind]] <- canonical
    results[[kind]] <- codebook_add(canonical, proposals[[kind]], kind)
    audits[[kind]] <- results[[kind]]$audit
  }
  ledger <- if (file.exists(ledger_path)) codebook_csv(ledger_path) else NULL
  new_audit <- Filter(Negate(is.null), audits)
  if (length(new_audit)) {
    new_audit <- do.call(rbind, new_audit); rownames(new_audit) <- NULL
    if (!is.null(ledger)) new_audit <- new_audit[!new_audit$change_id %in% ledger$change_id, , drop = FALSE]
    ledger <- if (is.null(ledger)) new_audit else rbind(ledger, new_audit)
  }
  scratch <- tempfile("codebooks_"); dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  staged <- destinations <- character()
  stage <- function(path, temporary) {
    staged <<- c(staged, temporary); destinations <<- c(destinations, path)
  }
  for (kind in kinds) {
    x <- results[[kind]]$data
    csv <- file.path(root, "data", paste0(stems[[kind]], ".csv"))
    xlsx <- sub("\\.csv$", ".xlsx", csv)
    changed <- !identical(x, original[[kind]])
    if (!file.exists(csv) || changed) {
      temp_csv <- file.path(scratch, paste0(kind, ".csv"))
      readr::write_csv(x, temp_csv, na = "")
      if (!identical(codebook_csv(temp_csv), x)) stop("CSV round-trip differs.", call. = FALSE)
      stage(csv, temp_csv)
    }
    if (changed) {
      wb <- openxlsx::loadWorkbook(xlsx)
      sheet <- names(wb)[[1]]
      appended <- x[seq.int(nrow(original[[kind]]) + 1L, nrow(x)), , drop = FALSE]
      openxlsx::writeData(wb, sheet, appended, startRow = nrow(original[[kind]]) + 2L,
        colNames = FALSE, keepNA = FALSE)
      codebook_format_new_rows(wb, sheet, seq_len(nrow(appended)) + nrow(original[[kind]]) + 1L, appended, xlsx)
      temp_xlsx <- file.path(scratch, paste0(kind, ".xlsx"))
      openxlsx::saveWorkbook(wb, temp_xlsx, overwrite = TRUE)
      if (!identical(codebook_excel(temp_xlsx), x)) stop("XLSX round-trip differs.", call. = FALSE)
      stage(xlsx, temp_xlsx)
    }
  }
  if (!is.null(ledger) && (!file.exists(ledger_path) || !identical(ledger, codebook_csv(ledger_path)))) {
    temp_ledger <- file.path(scratch, "changes.csv")
    readr::write_csv(ledger, temp_ledger, na = ""); stage(ledger_path, temp_ledger)
  }
  codebook_commit_files(staged, destinations, initial_hashes[destinations])
  invisible(list(variables = results$variables$data, topics = results$topics$data, changes = ledger))
}
