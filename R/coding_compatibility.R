# Coding v1: the explicit legacy field contract and auditable compatibility fixes.

coding_read_csv <- function(path) {
  as.data.frame(readr::read_csv(path, col_types = readr::cols(.default = "c"), na = "", progress = FALSE))
}

coding_dictionary <- function(path = "config/coding/legacy_31_fields.csv") {
  x <- coding_read_csv(path)
  if (nrow(x) != 31L || anyDuplicated(x$field) || !identical(as.integer(x$order), seq_len(31L))) {
    stop("Coding dictionary must contain exactly 31 unique fields in legacy order.", call. = FALSE)
  }
  x
}

coding_normalise_topics <- function(x, aliases = coding_read_csv("config/coding/topic_aliases.csv")) {
  if (anyDuplicated(aliases$alias) || any(aliases$canonical_topic %in% aliases$alias)) {
    stop("Topic aliases must be unique, direct mappings without chains or cycles.", call. = FALSE)
  }
  vapply(as.character(x), function(value) {
    if (is.na(value)) return(NA_character_)
    tokens <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
    idx <- match(tokens, aliases$alias)
    hit <- !is.na(idx)
    tokens[hit] <- aliases$canonical_topic[idx[hit]]
    paste(unique(tokens[nzchar(tokens)]), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

coding_read_basic_info <- function(directory = "data/3.meta_data/basic_info") {
  fields <- coding_dictionary()$field
  paths <- sort(list.files(directory, pattern = "\\.xlsx$", full.names = TRUE))
  if (!length(paths)) stop("No historical basic_info files.", call. = FALSE)
  parts <- lapply(paths, function(path) {
    x <- as.data.frame(readxl::read_excel(path))
    if (!identical(names(x), fields)) stop("Legacy column contract differs: ", path, call. = FALSE)
    x
  })
  # Preserve the same base-R type coercions as the established analytical builder.
  data <- do.call(rbind, parts)
  source_files <- rep(paths, vapply(parts, nrow, integer(1)))
  keep <- !is.na(data$paper_id) & nzchar(as.character(data$paper_id))
  data <- data[keep, , drop = FALSE]
  source_files <- source_files[keep]
  if (anyDuplicated(data$matrix_id)) stop("Duplicate legacy matrix_id values.", call. = FALSE)
  old_topics <- data$review_topics
  data$review_topics <- coding_normalise_topics(old_topics)
  changed <- !is.na(old_topics) & old_topics != data$review_topics
  attr(data, "topic_corrections") <- data.frame(source_file = source_files[changed],
    paper_id = data$paper_id[changed], matrix_id = data$matrix_id[changed],
    old_value = old_topics[changed], new_value = data$review_topics[changed])
  rownames(data) <- NULL
  coding_apply_historical_revisions(data)
}

coding_matrix_inventory <- function(basic, paths,
    exclusions = coding_read_csv("config/coding/matrix_exclusions.csv")) {
  if (anyDuplicated(exclusions$matrix_id)) stop("Duplicate matrix exclusions.", call. = FALSE)
  if (anyDuplicated(basic$matrix_id)) stop("Duplicate basic_info matrix IDs.", call. = FALSE)
  ids <- tools::file_path_sans_ext(basename(paths))
  if (anyDuplicated(ids)) stop("Duplicate matrix filenames.", call. = FALSE)
  rows <- lapply(seq_along(paths), function(i) {
    id <- ids[[i]]
    j <- match(id, basic$matrix_id)
    e <- match(id, exclusions$matrix_id)
    if (!is.na(e)) {
      source <- basic[basic$paper_id == exclusions$paper_id[[e]], , drop = FALSE]
      if (!nrow(source) || any(source$final_decision != "review") ||
          any(source$exclusion_meta != exclusions$legacy_meta_exclusion[[e]])) {
        stop("Excluded matrix conflicts with recorded report eligibility: ", id, call. = FALSE)
      }
      status <- "excluded_by_recorded_decision"
      pid <- exclusions$paper_id[[e]]
      reason <- exclusions$reason[[e]]
    } else {
      if (is.na(j)) stop("Unmatched matrix requires explicit reconciliation: ", id, call. = FALSE)
      if (!basic$final_decision[[j]] %in% c("both", "meta")) {
        stop("Matrix belongs to a non-quantitative route: ", id, call. = FALSE)
      }
      status <- "included"
      pid <- basic$paper_id[[j]]
      reason <- "Matched quantitative basic_info row"
    }
    data.frame(paper_id = pid, matrix_id = id, matrix_path = gsub("\\\\", "/", paths[[i]]),
               status = status, reason = reason, stringsAsFactors = FALSE)
  })
  planned <- basic[basic$final_decision %in% c("both", "meta") & !basic$matrix_id %in% ids, , drop = FALSE]
  for (i in seq_len(nrow(planned))) rows[[length(rows) + 1L]] <- data.frame(
    paper_id = planned$paper_id[[i]], matrix_id = planned$matrix_id[[i]], matrix_path = "",
    status = "missing_matrix_file", reason = "Historical quantitative row has no matching file; no eligibility change inferred",
    stringsAsFactors = FALSE)
  if (!length(rows)) return(data.frame(paper_id = character(), matrix_id = character(), matrix_path = character(), status = character(), reason = character()))
  do.call(rbind, rows)
}

coding_topic_audit <- function(basic, codebook) {
  rows <- lapply(seq_len(nrow(basic)), function(i) {
    if (is.na(basic$review_topics[[i]])) return(NULL)
    topics <- trimws(strsplit(basic$review_topics[[i]], ";", fixed = TRUE)[[1]])
    unknown <- setdiff(topics[nzchar(topics)], na.omit(codebook$Topic))
    if (!length(unknown)) return(NULL)
    data.frame(paper_id = basic$paper_id[[i]], matrix_id = basic$matrix_id[[i]],
      topic = unknown, issue = "Unrecognised topic; source-level adjudication needed before recoding", stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(paper_id = character(), matrix_id = character(), topic = character(), issue = character()))
  unique(do.call(rbind, rows))
}

coding_patch_topic_codebook <- function(path = "data/topics_codebook.xlsx") {
  aliases <- coding_read_csv("config/coding/topic_aliases.csv")
  before <- as.data.frame(readxl::read_excel(path))
  after <- before
  after$Topic <- coding_normalise_topics(after$Topic, aliases)
  if (identical(before, after)) return(invisible(FALSE))
  scratch <- tempfile("coding_topic_", tmpdir = tempdir())
  dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = scratch)
  strings <- file.path(scratch, "xl", "sharedStrings.xml")
  doc <- xml2::read_xml(strings)
  for (node in xml2::xml_find_all(doc, "//*[local-name()='si']")) {
    text <- xml2::xml_text(node)
    j <- match(text, aliases$alias)
    if (is.na(j)) next
    parts <- xml2::xml_find_all(node, ".//*[local-name()='t']")
    if (length(parts) != 1L) stop("Rich-text topic needs an explicit preservation-aware patch.", call. = FALSE)
    xml2::xml_text(parts[[1]]) <- aliases$canonical_topic[[j]]
  }
  xml2::write_xml(doc, strings)
  tmp <- tempfile("coding_topic_", tmpdir = dirname(path), fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  zip::zipr(normalizePath(tmp, winslash = "/", mustWork = FALSE),
    list.files(scratch, recursive = TRUE, all.files = TRUE), root = scratch, mode = "mirror")
  check <- as.data.frame(readxl::read_excel(tmp))
  if (!identical(check, after)) stop("Topic patch changed unexpected workbook content.", call. = FALSE)
  if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not replace topic codebook.", call. = FALSE)
  invisible(TRUE)
}

coding_write_dictionary <- function(path = "data/coding_dictionary.xlsx") {
  d <- coding_dictionary()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "fields", gridLines = FALSE)
  openxlsx::writeDataTable(wb, "fields", d, tableStyle = "TableStyleMedium2")
  widths <- c(15, 8, 25, 26, 16, 55, 48, 28, 55, 45, 70, 28)
  openxlsx::setColWidths(wb, "fields", cols = seq_along(d), widths = widths)
  openxlsx::freezePane(wb, "fields", firstActiveRow = 2, firstActiveCol = 4)
  openxlsx::addStyle(wb, "fields", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = 1:(nrow(d) + 1L), cols = seq_along(d), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, "fields", openxlsx::createStyle(fontColour = "#FFFFFF", textDecoration = "bold"),
    rows = 1, cols = seq_along(d), gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, "fields", rows = 1, heights = 32)
  ft_fit_wrapped_rows(wb, "fields", d, widths, minimum = 48)
  ft_save_workbook(wb, path)
  invisible(path)
}

coding_numeric_contract <- function(data) {
  effects <- setdiff(names(data), coding_dictionary()$field)
  rows <- lapply(seq_len(nrow(data)), function(i) {
    values <- as.numeric(data[i, effects, drop = TRUE])
    names(values) <- effects
    values <- values[!is.na(values)]
    values <- values[order(names(values))]
    payload <- paste(names(values), format(signif(values, 12), scientific = TRUE, trim = TRUE), sep = "=", collapse = "\n")
    data.frame(paper_id = data$paper_id[[i]], matrix_id = data$matrix_id[[i]], n = as.character(data$n[[i]]),
      n_effects = length(values), effects_sha256 = digest::digest(payload, algo = "sha256", serialize = FALSE))
  })
  do.call(rbind, rows)
}

# Keep the original preservation fixture frozen. Scientific corrections are
# explicit before/after transitions, never blanket exceptions for a matrix ID.
coding_read_historical_revisions <- function(path = "config/coding/historical_revisions.csv") {
  fields <- c("revision_id", "revision_date", "paper_id", "matrix_id",
    "previous_n", "previous_n_effects", "previous_effects_sha256",
    "n", "n_effects", "effects_sha256", "reason", "notes")
  if (!file.exists(path)) return(as.data.frame(setNames(rep(list(character()), length(fields)), fields)))
  x <- coding_read_csv(path)
  if (!identical(names(x), fields) || anyNA(x) || any(!nzchar(as.matrix(x))) ||
      anyDuplicated(x$revision_id)) stop("Invalid historical revision ledger.", call. = FALSE)
  for (field in c("previous_n", "n", "previous_n_effects", "n_effects"))
    if (any(!grepl("^[0-9]+$", x[[field]]))) stop("Invalid historical revision counts.", call. = FALSE)
  for (field in c("previous_effects_sha256", "effects_sha256"))
    if (any(!grepl("^[0-9a-f]{64}$", x[[field]]))) stop("Invalid historical revision signature.", call. = FALSE)
  x
}

coding_historical_contract <- function(
    path = "tests/fixtures/coding/pre_maintenance_numeric_contract.csv",
    revisions = coding_read_historical_revisions()) {
  expected <- coding_read_csv(path)
  expected$n_effects <- as.integer(expected$n_effects)
  if (anyDuplicated(expected$matrix_id)) stop("Duplicate historical contract IDs.")
  for (i in seq_len(nrow(revisions))) {
    revision <- revisions[i, ]
    j <- match(revision$matrix_id, expected$matrix_id)
    if (is.na(j) || expected$paper_id[[j]] != revision$paper_id ||
        expected$n[[j]] != revision$previous_n ||
        expected$n_effects[[j]] != as.integer(revision$previous_n_effects) ||
        expected$effects_sha256[[j]] != revision$previous_effects_sha256)
      stop("Historical revision does not match its previous contract: ", revision$revision_id, call. = FALSE)
    expected$n[[j]] <- revision$n
    expected$n_effects[[j]] <- as.integer(revision$n_effects)
    expected$effects_sha256[[j]] <- revision$effects_sha256
  }
  expected
}

coding_apply_historical_revisions <- function(data, revisions = coding_read_historical_revisions()) {
  for (i in seq_len(nrow(revisions))) {
    revision <- revisions[i, ]
    j <- match(revision$matrix_id, data$matrix_id)
    if (is.na(j) || data$paper_id[[j]] != revision$paper_id ||
        is.na(data$n[[j]]) || as.character(data$n[[j]]) != revision$previous_n)
      stop("Historical revision conflicts with basic_info: ", revision$revision_id, call. = FALSE)
    data$n[[j]] <- as.numeric(revision$n)
    notes <- c(data$notes[[j]], revision$notes)
    data$notes[[j]] <- paste(unique(notes[!is.na(notes) & nzchar(notes)]), collapse = " ")
  }
  data
}

coding_validate_historical <- function(data, expected = coding_historical_contract()) {
  if (anyDuplicated(data$matrix_id)) stop("Duplicate analytical matrix IDs.", call. = FALSE)
  index <- match(expected$matrix_id, data$matrix_id)
  if (anyNA(index)) stop("Missing historical matrices: ", paste(expected$matrix_id[is.na(index)], collapse = ", "), call. = FALSE)
  actual <- coding_numeric_contract(data[index, , drop = FALSE])
  changed <- !vapply(seq_len(nrow(expected)), function(i)
    isTRUE(all.equal(expected[i, , drop = FALSE], actual[i, , drop = FALSE], check.attributes = FALSE)), logical(1))
  if (any(changed)) stop("Historical correlations or sample Ns changed; explicit reconciliation required: ",
    paste(expected$matrix_id[changed], collapse = ", "), call. = FALSE)
  invisible(actual)
}
