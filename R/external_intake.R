# External bibliography provenance. Acquisition month is independent of publication.
external_manifest <- function(batch, root = here::here()) {
  if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])-external-(0[1-9]|[1-9][0-9])$", batch))
    stop("Invalid external batch ID.", call. = FALSE)
  path <- file.path(root, "config", "external", paste0(batch, ".csv"))
  x <- as.data.frame(readr::read_csv(path, col_types = readr::cols(.default = "c"), na = "", show_col_types = FALSE))
  fields <- c("path", "source_type", "received_date", "download_date", "source_detail", "source_url", "original_filename", "sha256")
  if (!identical(names(x), fields) || !nrow(x) || anyDuplicated(x$path)) stop("Invalid external intake manifest.")
  types <- readr::read_csv(file.path(root, "config", "external_source_types.csv"), show_col_types = FALSE)$code
  if (anyNA(x[, c("path", "source_type", "received_date", "source_detail", "original_filename", "sha256")]) ||
      any(!x$source_type %in% types) || any(!nzchar(trimws(x$source_detail)))) stop("Missing or invalid external provenance.")
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x$received_date)) ||
      anyNA(as.Date(x$received_date)) || any(substr(x$received_date, 1, 7) != substr(batch, 1, 7)))
    stop("External batch month must match receipt month, not publication month.")
  dated <- !is.na(x$download_date)
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x$download_date[dated])) ||
      anyNA(as.Date(x$download_date[dated])) || any(x$download_date[dated] > x$received_date[dated]))
    stop("Invalid external bibliography download date.")
  prefix <- paste0("data/0.bib_download/external/", batch, "/")
  if (any(!startsWith(x$path, prefix)) || any(grepl("(^|/)\\.\\.(/|$)", x$path))) stop("External input is outside its batch.")
  for (i in seq_len(nrow(x))) {
    expected <- paste0(x$received_date[i], "_", x$source_type[i], "_[0-9]{2}\\.(bib|ris)$")
    if (!grepl(paste0("^", expected), basename(x$path[i]))) stop("Invalid external bibliography filename.")
    file <- file.path(root, x$path[i])
    if (!file.exists(file) || digest::digest(file, algo = "sha256", file = TRUE) != x$sha256[i])
      stop("External original missing or changed: ", x$path[i], call. = FALSE)
  }
  x$database <- paste0("External:", x$source_type)
  x$batch_id <- batch
  x
}

# Use a BibTeX parser for external publisher files: the generic citation importer
# can drop their abstracts. No source rewriting.
external_read_bibtex <- function(path, database, batch) {
  entries <- unclass(bibtex::read.bib(path))
  if (!length(entries)) stop("No external BibTeX records parsed.")
  field <- function(name) vapply(entries, function(entry)
    if (is.null(entry[[name]])) NA_character_ else paste(as.character(entry[[name]]), collapse = " and "), character(1))
  data <- as.data.frame(setNames(lapply(c("author", "title", "year", "journal", "doi", "volume",
    "number", "pages", "url", "issn", "isbn", "abstract"), field),
    c("author", "title", "year", "journal", "doi", "volume", "issue", "pages", "url", "issn", "isbn", "abstract")))
  data$doi <- tolower(sub("^https?://(dx\\.)?doi\\.org/", "", trimws(data$doi)))
  data$type <- vapply(entries, function(entry) as.character(attr(entry, "bibtype")), character(1))
  data$cite_source <- database; data$cite_string <- batch; data$cite_label <- "search"
  data
}
