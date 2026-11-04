# Descriptive extraction v1. Scientific values are supplied as evidence-based JSON;
# deterministic R validates, assigns stable quantitative sample IDs, and exports.

coding_article_fields <- function() c("download_date", "doi", "title", "abstract", "journal", "authors",
  "author_et_al", "year", "review_topics", "email", "open_data", "coder", "notes", "additional")
coding_sample_fields <- function() c("study_design", "country", "gender", "age", "n", "age_class",
  "clinical", "clinical_spec", "seb_measure", "seb_measure_type", "trait_frame", "notes")
coding_flat <- function(x) if (is.null(x)) NA else if (is.list(x)) paste(unlist(x), collapse = "; ") else x

coding_validate_artifact <- function(obj, eligibility, topics) {
  required <- c("coding_version", "screening_record_id", "paper_id", "coding_date", "article", "samples", "evidence")
  if (!is.list(obj) || !setequal(names(obj), required) || obj$coding_version != "1.0.0") stop("Invalid article coding envelope.", call. = FALSE)
  i <- match(obj$paper_id, eligibility$paper_id)
  if (is.na(i) || obj$screening_record_id != eligibility$screening_record_id[[i]]) stop("Coding/eligibility identity mismatch.", call. = FALSE)
  if (!eligibility$final_route[[i]] %in% c("review", "meta", "both")) stop("Only resolved retained papers enter coding.", call. = FALSE)
  if (!setequal(names(obj$article), coding_article_fields())) stop("Article field contract mismatch.", call. = FALSE)
  if (!identical(ft_normalise_doi(obj$article$doi), ft_normalise_doi(eligibility$doi[[i]]))) stop("Coding DOI differs from eligibility.", call. = FALSE)
  check_array <- function(value, field) {
    if (is.null(value)) return(invisible(NULL))
    if (!is.list(value) || !is.null(names(value)) || anyDuplicated(value) ||
        any(!vapply(value, function(v) is.character(v) && length(v) == 1L && nzchar(v), logical(1)))) stop("Expected string array: ", field, call. = FALSE)
  }
  for (f in c("review_topics", "email", "open_data")) check_array(obj$article[[f]], f)
  if (!is.list(obj$samples) || !is.null(names(obj$samples))) stop("samples must be a JSON array.", call. = FALSE)
  if (eligibility$final_route[[i]] == "review" && length(obj$samples)) stop("Review-only papers must not receive sample coding.", call. = FALSE)
  if (eligibility$final_route[[i]] %in% c("meta", "both") && !length(obj$samples)) stop("Quantitative paper lacks sample coding.", call. = FALSE)
  if (any(!unlist(obj$article$review_topics) %in% topics$Topic)) stop("Unknown review topic in new coding.", call. = FALSE)
  if (!is.null(obj$article$year) && (!is.numeric(obj$article$year) || obj$article$year != floor(obj$article$year))) stop("year must be an integer.", call. = FALSE)
  if (!is.character(obj$article$title) || length(obj$article$title) != 1L || !nzchar(obj$article$title) ||
      !is.character(obj$article$coder) || !nzchar(obj$article$coder)) stop("Title and coder are required.", call. = FALSE)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", obj$coding_date) || is.na(as.Date(obj$coding_date))) stop("Invalid coding date.", call. = FALSE)
  allowed_components <- trimws(strsplit(eligibility$eligible_quantitative_components[[i]], ";", fixed = TRUE)[[1]])
  keys <- character()
  for (s in obj$samples) {
    if (!setequal(names(s), c("component_label", "sample_label", "occasion_label", "occasion_role", coding_sample_fields()))) stop("Sample field contract mismatch.", call. = FALSE)
    if (!identical(s$occasion_role, "first_eligible")) stop("Only the first eligible occasion is enabled in coding v1.", call. = FALSE)
    if (!s$component_label %in% allowed_components) stop("Ineligible publication component entered coding.", call. = FALSE)
    if (!is.character(s$sample_label) || !nzchar(s$sample_label) || !is.character(s$occasion_label) || !nzchar(s$occasion_label)) stop("Sample/occasion labels required.", call. = FALSE)
    keys <- c(keys, paste(s$component_label, s$sample_label, sep = "|"))
    for (f in c("country", "seb_measure", "trait_frame")) check_array(s[[f]], f)
    for (field in c("gender", "age", "n")) if (!is.null(s[[field]]) &&
        (length(s[[field]]) != 1L || !is.numeric(s[[field]]) || !is.finite(s[[field]]))) stop("Sample numbers must be numeric: ", field, call. = FALSE)
    if (is.null(s$n) || s$n <= 0 || s$n != floor(s$n)) stop("Positive analytic n required.", call. = FALSE)
    if (!is.null(s$gender) && (s$gender < 0 || s$gender > 100)) stop("gender must be a percentage from 0 to 100.", call. = FALSE)
    if (!is.null(s$age) && s$age < 0) stop("age must be nonnegative.", call. = FALSE)
    enums <- list(study_design = c("cross", "long", "exp"), age_class = c("children", "school_age", "young_adult", "adult", "older_adult"),
      clinical = c("clinical", "not_clinical"), seb_measure_type = c("short", "long"))
    for (field in names(enums)) if (!is.null(s[[field]]) && !s[[field]] %in% enums[[field]]) stop("Invalid sample category: ", field, call. = FALSE)
  }
  if (anyDuplicated(keys)) stop("Duplicate sample identity.", call. = FALSE)
  if (!is.list(obj$evidence) || !length(obj$evidence)) stop("Coding evidence required.", call. = FALSE)
  hashes <- new.env(parent = emptyenv())
  for (ev in obj$evidence) {
    if (!setequal(names(ev), c("unit", "field", "status", "source", "source_sha256", "page", "evidence")) ||
        !ev$status %in% c("extracted", "derived", "not_reported", "not_verified") ||
        !is.character(ev$source) || !nzchar(ev$source) || !file.exists(ev$source) ||
        !is.character(ev$evidence) || !nzchar(ev$evidence)) stop("Invalid coding evidence.", call. = FALSE)
    if (!is.null(ev$page) && (!is.numeric(ev$page) || ev$page < 1 || ev$page != floor(ev$page))) stop("Invalid PDF evidence page.", call. = FALSE)
    if (tolower(tools::file_ext(ev$source)) == "pdf" && is.null(ev$page)) stop("PDF coding evidence requires a page.", call. = FALSE)
    if (!ev$unit %in% c("paper", keys)) stop("Unknown evidence unit.", call. = FALSE)
    if (!ev$field %in% if (ev$unit == "paper") coding_article_fields() else c("component_label", "sample_label", "occasion_label", "occasion_role", coding_sample_fields())) stop("Unknown evidence field.", call. = FALSE)
    if (!exists(ev$source, hashes, inherits = FALSE)) assign(ev$source, digest::digest(file = ev$source, algo = "sha256"), hashes)
    if (!identical(ev$source_sha256, get(ev$source, hashes, inherits = FALSE))) stop("Coding source changed; verify evidence before rebuilding: ", ev$source, call. = FALSE)
  }
  require_evidence <- function(unit, fields) for (field in fields) {
    if (!any(vapply(obj$evidence, function(ev) identical(ev$unit, unit) && identical(ev$field, field), logical(1)))) stop("Evidence missing for ", unit, ":", field, call. = FALSE)
  }
  require_evidence("paper", coding_article_fields()[!vapply(obj$article, is.null, logical(1))[match(coding_article_fields(), names(obj$article))]])
  for (j in seq_along(obj$samples)) require_evidence(keys[[j]], coding_sample_fields()[!vapply(obj$samples[[j]][coding_sample_fields()], is.null, logical(1))])
  invisible(TRUE)
}

coding_build_batch <- function(batch_id, base = "data/3.meta_data/coding") {
  eligibility <- ft_read_canonical_decisions(file.path("data/2.full_abstracting", paste0(batch_id, "_full_text_decisions.csv")))
  expected <- eligibility$paper_id[eligibility$final_route %in% c("review", "meta", "both")]
  input_dir <- file.path(base, "artifacts", batch_id)
  paths <- sort(list.files(input_dir, pattern = "\\.json$", full.names = TRUE))
  objects <- lapply(paths, function(p) jsonlite::fromJSON(p, simplifyVector = FALSE))
  ids <- vapply(objects, function(o) o$paper_id, character(1))
  if (anyDuplicated(ids) || !setequal(ids, expected) || any(basename(paths) != paste0(ids, ".json"))) stop("Coding artifacts must cover exactly all retained papers.", call. = FALSE)
  topics <- coding_read_csv("data/topics_codebook.csv")
  for (obj in objects) coding_validate_artifact(obj, eligibility, topics)
  registry_path <- file.path(base, "sample_id_registry.csv")
  registry <- if (file.exists(registry_path)) coding_read_csv(registry_path) else data.frame(paper_id = character(), component_label = character(), sample_label = character(), sample_id = character(), matrix_id = character())
  previously_coded_papers <- unique(registry$paper_id)
  if (anyDuplicated(registry$matrix_id) || anyDuplicated(paste(registry$paper_id, registry$component_label, registry$sample_label))) stop("Invalid sample registry.", call. = FALSE)
  legacy <- coding_read_basic_info()
  papers <- samples <- evidence <- exports <- list()
  for (k in seq_along(objects)) {
    obj <- objects[[k]]
    i <- match(obj$paper_id, eligibility$paper_id)
    paper <- as.data.frame(lapply(obj$article, coding_flat), stringsAsFactors = FALSE)
    paper$paper_id <- obj$paper_id; paper$screening_record_id <- obj$screening_record_id
    for (f in c("review_eligible", "meta_status", "meta_exclusion_reason", "framework_relation_tags", "final_route")) paper[[f]] <- eligibility[[f]][[i]]
    paper$coding_version <- obj$coding_version; paper$coding_date <- obj$coding_date
    paper$artifact_path <- gsub("\\\\", "/", paths[[k]])
    paper$artifact_sha256 <- digest::digest(file = paths[[k]], algo = "sha256")
    papers[[k]] <- paper
    for (ev in obj$evidence) {
      ev$page <- ev$page %||% NA_integer_
      row <- as.data.frame(ev, stringsAsFactors = FALSE); row$paper_id <- obj$paper_id
      row$source_sha256 <- digest::digest(file = ev$source, algo = "sha256")
      evidence[[length(evidence) + 1L]] <- row
    }
    units <- if (length(obj$samples)) obj$samples else list(NULL)
    for (s in units) {
      out <- as.data.frame(setNames(rep(list(NA_character_), 31L), coding_dictionary()$field), stringsAsFactors = FALSE)
      for (f in c("gender", "age", "n", "year")) out[[f]] <- NA_real_
      for (f in coding_article_fields()) out[[f]] <- coding_flat(obj$article[[f]])
      out$paper_id <- obj$paper_id
      out$exclusion_general <- "include"
      out$exclusion_meta <- if (paper$meta_status == "eligible") "include" else paper$meta_exclusion_reason
      out$final_decision <- paper$final_route
      out$matrix_id <- obj$paper_id # Legacy report-row placeholder, not a quantitative matrix assignment.
      if (!is.null(s)) {
        hit <- which(registry$paper_id == obj$paper_id & registry$component_label == s$component_label & registry$sample_label == s$sample_label)
        if (!length(hit)) {
          if (obj$paper_id %in% previously_coded_papers) stop("Changed/new sample label in an existing coded paper requires explicit registry reconciliation.", call. = FALSE)
          if (obj$paper_id %in% legacy$paper_id) stop("Historical paper requires an explicit sample crosswalk before new assignment.", call. = FALSE)
          used <- registry$sample_id[registry$paper_id == obj$paper_id]
          letter <- setdiff(letters, used)
          if (!length(letter)) stop("More than 26 samples requires an explicit ID extension.", call. = FALSE)
          registry <- rbind(registry, data.frame(paper_id = obj$paper_id, component_label = s$component_label,
            sample_label = s$sample_label, sample_id = letter[[1]], matrix_id = paste0(obj$paper_id, letter[[1]])))
          hit <- nrow(registry)
        }
        sample <- as.data.frame(lapply(s, coding_flat), stringsAsFactors = FALSE)
        sample$paper_id <- obj$paper_id; sample$sample_id <- registry$sample_id[[hit]]; sample$matrix_id <- registry$matrix_id[[hit]]
        samples[[length(samples) + 1L]] <- sample
        for (f in coding_sample_fields()) out[[f]] <- coding_flat(s[[f]])
        out$notes <- paste(na.omit(c(coding_flat(obj$article$notes), coding_flat(s$notes))), collapse = " ")
        if (length(s$country) > 1L) out$country <- "mixed"
        out$sample_id <- sample$sample_id; out$matrix_id <- sample$matrix_id
      }
      exports[[length(exports) + 1L]] <- out
    }
  }
  p <- dplyr::bind_rows(papers); s <- dplyr::bind_rows(samples); e <- dplyr::bind_rows(evidence)
  p <- p[, c("paper_id", "screening_record_id", setdiff(names(p), c("paper_id", "screening_record_id"))), drop = FALSE]
  if (nrow(s)) s <- s[, c("paper_id", "sample_id", "matrix_id", setdiff(names(s), c("paper_id", "sample_id", "matrix_id"))), drop = FALSE]
  e <- e[, c("paper_id", setdiff(names(e), "paper_id")), drop = FALSE]
  legacy_export <- dplyr::bind_rows(exports)
  destination <- file.path(base, batch_id)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  for (name in c("papers", "samples", "evidence", "legacy_export")) {
    value <- switch(name, papers = p, samples = s, evidence = e, legacy_export = legacy_export)
    ft_atomic_write_csv(value, file.path(destination, paste0(name, ".csv")))
  }
  ft_atomic_write_csv(registry, registry_path)
  wb <- openxlsx::createWorkbook()
  tables <- list(papers = p, samples = s, evidence = e, legacy_export = legacy_export)
  for (name in names(tables)) {
    value <- tables[[name]]
    openxlsx::addWorksheet(wb, name, gridLines = FALSE)
    if (nrow(value)) openxlsx::writeDataTable(wb, name, value, tableStyle = "TableStyleMedium2") else openxlsx::writeData(wb, name, value)
    openxlsx::setColWidths(wb, name, cols = seq_along(value), widths = 28)
    narrative <- which(names(value) %in% c("abstract", "title", "notes", "evidence", "source", "artifact_path"))
    if (length(narrative)) openxlsx::setColWidths(wb, name, cols = narrative, widths = 65)
    if ("abstract" %in% names(value)) openxlsx::setColWidths(wb, name, cols = match("abstract", names(value)), widths = 110)
    openxlsx::addStyle(wb, name, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
      rows = 1:(nrow(value) + 1L), cols = seq_along(value), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, name, openxlsx::createStyle(fontColour = "#FFFFFF", textDecoration = "bold"),
      rows = 1, cols = seq_along(value), gridExpand = TRUE, stack = TRUE)
    widths <- rep(28, ncol(value)); widths[narrative] <- 65
    widths[names(value) == "abstract"] <- 110
    ft_fit_wrapped_rows(wb, name, value, widths, minimum = 48)
    openxlsx::setRowHeights(wb, name, rows = 1, heights = 36)
    openxlsx::freezePane(wb, name, firstActiveRow = 2, firstActiveCol = if (name == "legacy_export") 5 else if (name == "samples") 4 else 3)
  }
  ft_save_workbook(wb, file.path(destination, paste0(batch_id, "_article_coding.xlsx")))
  invisible(list(papers = p, samples = s, evidence = e, legacy_export = legacy_export, registry = registry))
}
