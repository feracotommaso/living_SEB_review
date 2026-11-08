# One-time migration from checkpoint 0e1daaa. Original data and historical scripts
# are not rewritten. Outputs must compare equal before obsolete files are removed.
source("workflow.R")
source("R/analytical_integration.R")
if (file.exists(wf_records_path)) stop("La codifica generale esiste gia; migrazione non ripetibile.")
old <- list(basic = wf_read("data/3.meta_data/current_basic_info.csv"),
  provenance = wf_read("data/3.meta_data/coding_provenance.csv"))
records <- old$basic
records$download_date <- as.character(as.Date(records$download_date))
records[] <- lapply(records, as.character)
for (field in wf_context) records[[field]] <- NA_character_
records$batch_id <- old$provenance$batch_id[match(records$matrix_id, old$provenance$matrix_id)]
evidence <- list(); sources <- list()
for (batch in na.omit(unique(records$batch_id))) {
  p <- wf_read(file.path(wf_coding_dir, batch, "papers.csv"))
  s <- wf_read(file.path(wf_coding_dir, batch, "samples.csv"))
  records$screening_record_id[records$batch_id %in% batch] <- p$screening_record_id[match(records$paper_id[records$batch_id %in% batch], p$paper_id)]
  for (field in c("component_label", "sample_label", "occasion_label", "occasion_role"))
    records[[field]][records$matrix_id %in% s$matrix_id] <- s[[field]][match(records$matrix_id[records$matrix_id %in% s$matrix_id], s$matrix_id)]
  evidence[[batch]] <- wf_read(file.path(wf_coding_dir, batch, "evidence.csv"))
}
ev <- dplyr::bind_rows(evidence)
for (pid in c("0145", "0150")) {
  x <- wf_read(file.path("data/3.meta_data/extraction", pid, "source_manifest.csv"))
  if (pid == "0145") {
    x$path <- x$local_path; x$role <- "downloaded_original"; x$source_reference <- x$download_url
    x$download_date <- substr(x$retrieved_at_utc, 1, 10); x$received_date <- x$download_date
    x$download_date_status <- "recorded_at_retrieval"
    x$provenance_note <- paste("Project", x$project_url, "component", x$component_label,
      "retrieved UTC", x$retrieved_at_utc, "expected SHA256", x$expected_sha256)
  }
  sources[[pid]] <- x[, c("paper_id", "role", "path", "sha256", "source_reference", "received_date", "download_date", "download_date_status", "provenance_note")]
}
src <- dplyr::bind_rows(sources)
# Evidence files not already in acquisition provenance are retained with unknown dates.
for (i in seq_len(nrow(ev))) if (!any(src$paper_id == ev$paper_id[i] & src$path == ev$source[i])) {
  src <- rbind(src, data.frame(paper_id = ev$paper_id[i], role = "coding_evidence", path = ev$source[i], sha256 = ev$source_sha256[i],
    source_reference = NA_character_, received_date = NA_character_, download_date = NA_character_, download_date_status = "unknown",
    provenance_note = "Imported existing coding evidence; acquisition date not reconstructed."))
}
# Technical evidence derived from soon-retired reports is retained as its full text;
# it is not relabelled as a downloaded original.
derived <- !grepl("/(pdfs|open_data)/", src$path)
src$role[derived] <- "legacy_derived_evidence"
wf_commit(setNames(list(records, ev, src), file.path(wf_coding_dir, c("records.csv", "evidence.csv", "sources.csv"))))
summary <- list()
for (pid in c("0145", "0150")) {
  directory <- if (pid == "0145") "data/3.meta_data/extraction/0145" else "data/3.meta_data/extraction/0150/0150a"
  checks <- wf_read(file.path(directory, "checks.csv"))
  values <- as.list(setNames(checks$value, checks$check))
  summary[[pid]] <- as.data.frame(c(list(paper_id = pid, matrix_id = paste0(pid, "a")), values), stringsAsFactors = FALSE)
  pairs <- wf_read(file.path(directory, "pair_correlations.csv"))
  wf_commit(setNames(list(pairs[, c("var1", "var2", "n")]), file.path("data/3.meta_data/pair_n", paste0(pid, "a.csv"))))
}
wf_commit(setNames(list(dplyr::bind_rows(summary)), "data/3.meta_data/extraction/summary.csv"))
wf_export_coding(force = TRUE)
result <- wf_update_results()
stopifnot(nrow(result$changes) == 0L)
message("Migrazione verificata: codifica e risultati preservati.")
