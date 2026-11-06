# Confirm a batch's bibliographic export date at the coding source, with history.
coding_set_download_date <- function(batch_id, download_date, base = "data/3.meta_data/coding") {
  if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", batch_id) ||
      length(download_date) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", download_date) ||
      is.na(as.Date(download_date, format = "%Y-%m-%d"))) stop("Supply a valid batch and YYYY-MM-DD export date.")
  directory <- file.path(base, "artifacts", batch_id)
  paths <- list.files(directory, pattern = "\\.json$", full.names = TRUE)
  if (!length(paths)) stop("No article coding artifacts for batch.")
  objects <- lapply(paths, function(p) jsonlite::fromJSON(p, simplifyVector = FALSE))
  for (object in objects) if (!is.null(object$article$download_date) && object$article$download_date != download_date)
    stop("Existing download date conflicts; reconcile explicitly before changing it: ", object$paper_id)
  provenance_path <- file.path(base, "provenance", paste0(batch_id, "_download_date.csv"))
  if (file.exists(provenance_path)) {
    provenance <- codebook_csv(provenance_path)
    if (nrow(provenance) != 1L || provenance$download_date != download_date) stop("Conflicting batch date provenance.")
  } else provenance <- data.frame(batch_id = batch_id, download_date = download_date,
    source = "Human-confirmed bibliographic export date for the batch",
    confirmed_on = format(Sys.time(), "%Y-%m-%d", tz = "Europe/Rome"), stringsAsFactors = FALSE)
  scratch <- tempfile("coding_date_"); dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  temp_provenance <- file.path(scratch, "provenance.csv")
  readr::write_csv(provenance, temp_provenance, na = "")
  provenance_sha <- digest::digest(temp_provenance, algo = "sha256", file = TRUE)
  staged <- destinations <- hashes <- character()
  stage <- function(from, to) {
    if (file.exists(to) && identical(digest::digest(from, algo = "sha256", file = TRUE), digest::digest(to, algo = "sha256", file = TRUE))) return()
    staged <<- c(staged, from); destinations <<- c(destinations, to)
    hashes <<- c(hashes, if (file.exists(to)) digest::digest(to, algo = "sha256", file = TRUE) else "absent")
  }
  stage(temp_provenance, provenance_path)
  for (i in seq_along(objects)) {
    object <- objects[[i]]
    old_sha <- digest::digest(paths[[i]], algo = "sha256", file = TRUE)
    object$article$download_date <- download_date
    existing <- which(vapply(object$evidence, function(ev) ev$unit == "paper" && ev$field == "download_date", logical(1)))
    if (length(existing) > 1L) stop("Duplicate download-date evidence requires reconciliation.")
    position <- if (length(existing)) existing[[1]] else length(object$evidence) + 1L
    object$evidence[[position]] <- list(unit = "paper", field = "download_date",
      status = "extracted", source = gsub("\\\\", "/", provenance_path), source_sha256 = provenance_sha,
      page = NULL, evidence = paste("Bibliographic export date", download_date,
        "confirmed by the user for this batch; separate from publication month, coding date, PDF acquisition and individual-data retrieval."))
    temporary <- file.path(scratch, paste0(object$paper_id, ".json"))
    jsonlite::write_json(object, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
    if (digest::digest(temporary, algo = "sha256", file = TRUE) != old_sha) {
      history <- file.path(directory, "history", paste0(object$paper_id, ".before_download_date_", substr(old_sha, 1, 12), ".json"))
      stage(paths[[i]], history)
      stage(temporary, paths[[i]])
    }
  }
  codebook_commit_files(staged, destinations, hashes)
  invisible(provenance)
}
