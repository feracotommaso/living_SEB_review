# Shared I/O for the human workflow. Scientific calculations live in article R files.
wf_hash <- function(path) if (file.exists(path)) digest::digest(file = path, algo = "sha256") else "absent"
wf_read <- function(path) as.data.frame(readr::read_csv(path, col_types = readr::cols(.default = "c"), na = "", progress = FALSE))
wf_text <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
wf_same <- function(a, b) identical(wf_text(a), wf_text(b))
wf_empty <- function(fields) as.data.frame(setNames(rep(list(character()), length(fields)), fields))
wf_number <- function(x, field) {
  y <- suppressWarnings(as.numeric(x))
  if (any(nzchar(wf_text(x)) & (is.na(y) | !is.finite(y)))) stop("Valore numerico non valido: ", field, call. = FALSE)
  y
}
wf_commit <- function(tables, xlsx = list()) {
  paths <- c(names(tables), names(xlsx))
  if (!length(paths)) return(invisible(NULL))
  before <- vapply(paths, wf_hash, character(1))
  scratch <- tempfile("seb_save_"); dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  files <- file.path(scratch, paste0(seq_along(paths), "_", basename(paths)))
  for (i in seq_along(tables)) readr::write_csv(tables[[i]], files[[i]], na = "")
  for (i in seq_along(xlsx)) {
    if (inherits(xlsx[[i]], 'Workbook')) openxlsx::saveWorkbook(xlsx[[i]], files[[length(tables) + i]])
    else writexl::write_xlsx(xlsx[[i]], files[[length(tables) + i]])
  }
  codebook_commit_files(files, paths, unname(before))
  invisible(paths)
}

wf_write_book <- function(tables, path, sources, editable = list(), instructions = NULL) {
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  if (!is.null(instructions)) tables <- c(list(istruzioni = instructions), tables)
  for (name in names(tables)) {
    x <- tables[[name]]
    openxlsx::addWorksheet(wb, name, gridLines = FALSE)
    # Keep machine identifiers as text, but observations and dates as native values.
    for (field in intersect(c("n", "gender", "age", "year", "page"), names(x))) x[[field]] <- wf_number(x[[field]], field)
    for (field in intersect(c("download_date", "received_date", "coding_date"), names(x)))
      x[[field]] <- as.Date(x[[field]])
    if (nrow(x)) openxlsx::writeDataTable(wb, name, x, tableStyle = "TableStyleMedium2")
    else openxlsx::writeData(wb, name, x)
    widths <- rep(22, ncol(x))
    widths[grepl("_id$|^year$|^n$|^age$|^gender$", names(x))] <- 14
    widths[names(x) %in% c("title", "notes", "evidence", "description", "definition", "istruzione", "provenance_note", "source", "path", "review_topics", "authors", "open_data", "doi", "ai_rationale", "ai_evidence", "adjudication_note", "issue")] <- 65
    widths[names(x) == "abstract"] <- 100
    openxlsx::setColWidths(wb, name, seq_len(ncol(x)), widths)
    openxlsx::addStyle(wb, name, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
      rows = seq_len(nrow(x) + 1L), cols = seq_len(ncol(x)), gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, name, 1, 35)
    ft_fit_wrapped_rows(wb, name, x, widths, minimum = 30)
    ids <- which(grepl("_id$", names(x)))
    if (length(ids) && nrow(x)) openxlsx::addStyle(wb, name,
      openxlsx::createStyle(numFmt = "@", halign = "left"), rows = seq_len(nrow(x)) + 1L,
      cols = ids, gridExpand = TRUE, stack = TRUE)
    edit <- which(names(x) %in% editable[[name]])
    if (length(edit) && nrow(x)) openxlsx::addStyle(wb, name,
      openxlsx::createStyle(fgFill = "#FFF8DD"), rows = seq_len(nrow(x)) + 1L,
      cols = edit, gridExpand = TRUE, stack = TRUE)
    choices <- list(human_decision = '"include,exclude,uncertain"', result = '"match,no_match"', adjudication_status = '"OPEN,RESOLVED"')
    for (field in intersect(names(choices), names(x)[edit])) if (nrow(x))
      openxlsx::dataValidation(wb, name, cols = match(field, names(x)), rows = seq_len(nrow(x)) + 1L,
        type = "list", value = choices[[field]])
    dates <- which(names(x) %in% c("download_date", "received_date", "coding_date"))
    if (length(dates) && nrow(x)) openxlsx::addStyle(wb, name,
      openxlsx::createStyle(numFmt = "yyyy-mm-dd"), rows = seq_len(nrow(x)) + 1L,
      cols = dates, gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, name, firstActiveRow = 2, firstActiveCol = if (ncol(x) > 5) 3 else 1)
  }
  openxlsx::addWorksheet(wb, "_versione", visible = FALSE)
  openxlsx::writeData(wb, "_versione", data.frame(path = sources, sha256 = vapply(sources, wf_hash, character(1))))
  baseline <- list()
  for (name in names(tables)) {
    if (!nrow(tables[[name]])) baseline[[length(baseline)+1L]] <- data.frame(sheet = name, value = "null")
    for (i in seq_len(nrow(tables[[name]])))
      baseline[[length(baseline)+1L]] <- data.frame(sheet = name, value = as.character(jsonlite::toJSON(
        lapply(tables[[name]][i, , drop = FALSE], function(v) wf_text(v)), auto_unbox = TRUE, null = "null")))
  }
  openxlsx::addWorksheet(wb, "_base", visible = FALSE)
  openxlsx::writeData(wb, "_base", if (length(baseline)) do.call(rbind, baseline) else data.frame(sheet = character(), value = character()))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".xlsx"); on.exit(unlink(tmp), add = TRUE)
  ft_save_workbook(wb, tmp)
  codebook_commit_files(tmp, path, wf_hash(path))
  invisible(path)
}

wf_book_has_edits <- function(path) {
  baseline <- wf_read_book(path, "_base")
  for (sheet in unique(baseline$sheet)) {
    if (sheet == "istruzioni") next
    if (identical(baseline$value[baseline$sheet == sheet], "null")) {
      if (nrow(wf_read_book(path, sheet))) return(TRUE)
      next
    }
    rows <- lapply(baseline$value[baseline$sheet == sheet], function(v) as.data.frame(jsonlite::fromJSON(v), stringsAsFactors = FALSE))
    old <- do.call(rbind, rows); now <- wf_read_book(path, sheet)
    if (!setequal(names(old), names(now)) || nrow(old) != nrow(now)) return(TRUE)
    normal <- function(x) {
      x[] <- lapply(x, wf_text)
      for (field in intersect(c("n", "gender", "age", "year", "page"), names(x))) {
        n <- suppressWarnings(as.numeric(x[[field]])); hit <- is.finite(n)
        x[[field]][hit] <- as.character(signif(n[hit], 12))
      }
      sort(vapply(seq_len(nrow(x)), function(i) as.character(jsonlite::toJSON(as.list(x[i, sort(names(x)), drop = FALSE]), auto_unbox = TRUE)), character(1)))
    }
    if (!identical(normal(old), normal(now))) return(TRUE)
  }
  FALSE
}

wf_read_book <- function(path, sheet) {
  x <- as.data.frame(readxl::read_excel(path, sheet = sheet))
  for (name in names(x)) {
    if (inherits(x[[name]], "POSIXt") || inherits(x[[name]], "Date")) x[[name]] <- as.character(as.Date(x[[name]]))
    else x[[name]] <- as.character(x[[name]])
  }
  x
}
wf_check_version <- function(path) {
  version <- wf_read_book(path, "_versione")
  if (any(vapply(version$path, wf_hash, character(1)) != version$sha256))
    stop("L'Excel si riferisce a tabelle precedenti. Conserva le modifiche e rigenera l'interfaccia prima di importarle.", call. = FALSE)
}
wf_check_keys <- function(before, after, key, allow_new = FALSE) {
  if (!setequal(names(before), names(after))) stop("Colonne modificate nel foglio ", key, call. = FALSE)
  if (anyNA(after[[key]]) || anyDuplicated(after[[key]])) stop("Identificativi mancanti o duplicati: ", key, call. = FALSE)
  if (length(setdiff(before[[key]], after[[key]])) || (!allow_new && length(setdiff(after[[key]], before[[key]]))))
    stop("Righe aggiunte/rimosse o identificativi cambiati: ", key, ". Usa la preparazione del lotto per aggiungere articoli.", call. = FALSE)
  after[match(before[[key]], after[[key]]), names(before), drop = FALSE]
}

wf_diff <- function(before, after, key, dataset) {
  changes <- list()
  added <- setdiff(after[[key]], before[[key]]); removed <- setdiff(before[[key]], after[[key]])
  if (length(added)) changes[[length(changes)+1L]] <- data.frame(dataset, id = added, field = ".record", before = "", after = "present")
  if (length(removed)) changes[[length(changes)+1L]] <- data.frame(dataset, id = removed, field = ".record", before = "present", after = "")
  ids <- intersect(before[[key]], after[[key]])
  ia <- match(ids, before[[key]]); ib <- match(ids, after[[key]])
  for (field in union(names(before), names(after))) {
    a <- if (field %in% names(before)) wf_text(before[[field]][ia]) else rep("", length(ids))
    b <- if (field %in% names(after)) wf_text(after[[field]][ib]) else rep("", length(ids))
    na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
    same <- a == b | (nzchar(a) & nzchar(b) & is.finite(na) & is.finite(nb) & abs(na-nb) <= 1e-12 * pmax(1, abs(na), abs(nb)))
    if (endsWith(field, "_date")) same <- same | substr(a, 1, 10) == substr(b, 1, 10)
    hit <- which(!same)
    if (length(hit)) changes[[length(changes)+1L]] <- data.frame(dataset, id = ids[hit], field, before = a[hit], after = b[hit])
  }
  if (length(changes)) do.call(rbind, changes) else wf_empty(c("dataset", "id", "field", "before", "after"))
}
