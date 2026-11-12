# Readable comparisons replace frozen signatures on derived files.
wf_build_results <- function(records = wf_read(wf_records_path)) {
  current <- wf_current(records)
  # A completed individual-data extraction owns its analytic N.
  summary_path <- "data/3.meta_data/extraction/summary.csv"
  extraction <- if (file.exists(summary_path)) wf_read(summary_path) else data.frame(matrix_id = character(), n = character())
  for (i in seq_len(nrow(extraction))) {
    j <- match(extraction$matrix_id[i], current$basic$matrix_id)
    if (is.na(j)) stop("Matrice estratta senza codifica: ", extraction$matrix_id[i])
    current$basic$n[j] <- as.numeric(extraction$n[i])
    records$n[j] <- extraction$n[i]
  }
  current$review <- current$basic[match(current$review$matrix_id, current$basic$matrix_id), , drop = FALSE]
  paths <- list.files("data/3.meta_data/matrices", pattern = "\\.xlsx$", full.names = TRUE)
  ids <- tools::file_path_sans_ext(basename(paths))
  matched <- match(ids, current$basic$matrix_id)
  inactive <- !is.na(matched) & !current$basic$final_decision[matched] %in% c("both", "meta")
  inventory <- coding_matrix_inventory(current$basic, paths[!inactive])
  if (any(inactive)) inventory <- rbind(inventory, data.frame(paper_id = current$basic$paper_id[matched[inactive]],
    matrix_id = ids[inactive], matrix_path = paths[inactive], status = "excluded_by_recorded_decision", reason = "Resolved current full-text route is not quantitative"))
  active <- inventory$matrix_path[inventory$status == "included"]
  issues <- list()
  for (path in active) {
    id <- tools::file_path_sans_ext(basename(path))
    m <- as.matrix(readxl::read_excel(path))
    if (!is.numeric(m) || nrow(m) != ncol(m) || anyDuplicated(colnames(m)) ||
        "age" %in% colnames(m) || any(!is.na(m) & !is.finite(m)) ||
        any(abs(m) > 1 + 1e-10, na.rm = TRUE) || anyNA(diag(m)) ||
        any(abs(diag(m)-1) > 1e-10) || !isTRUE(all.equal(m, t(m), check.attributes = FALSE, tolerance = 1e-10)))
      stop("Matrice non valida: ", id, call. = FALSE)
    csv <- sub("\\.xlsx$", ".csv", path)
    if (file.exists(csv)) {
      c <- wf_read(csv)
      if (!identical(c[[1]], colnames(m)) || !identical(names(c)[-1], colnames(m)) ||
          !isTRUE(all.equal(unname(m), matrix(as.numeric(as.matrix(c[-1])), nrow = nrow(c)), tolerance = 1e-12))) stop("CSV/XLSX diversi: ", id)
    }
    n <- current$basic$n[match(id, current$basic$matrix_id)]
    if (is.na(n) || n < 2 || n != floor(n)) stop("N analitico assente/non valido: ", id)
    # Historical vocabulary is audited without silently renaming old matrices.
    unknown <- tryCatch({ codebook_resolve_variables(colnames(m)); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(unknown)) issues[[length(issues)+1L]] <- data.frame(matrix_id = id, issue = unknown)
    eig <- if (anyNA(m)) NA_real_ else min(eigen(m, symmetric = TRUE, only.values = TRUE)$values)
    if (is.na(eig) || eig < -1e-10) issues[[length(issues)+1L]] <- data.frame(matrix_id = id,
      issue = if (is.na(eig)) "Correlazioni mancanti; controllare copertura" else paste("Autovalore minimo", signif(eig, 5), ": matrice conservata senza correzioni automatiche"))
    pairs_path <- file.path("data/3.meta_data/pair_n", paste0(id, ".csv"))
    if (file.exists(pairs_path)) {
      pairs <- wf_read(pairs_path); keys <- paste(pairs$var1, pairs$var2, sep = "|")
      indices <- which(upper.tri(m), arr.ind = TRUE)
      expected <- paste(colnames(m)[indices[,1]], colnames(m)[indices[,2]], sep = "|")
      if (anyDuplicated(keys) || !setequal(keys, expected)) stop("Copertura N per coppia non valida: ", id)
      pn <- wf_number(pairs$n[match(expected, keys)], "pair n")
      if (anyNA(pn) || any(pn < 0 | pn > n | pn != floor(pn)) || any(!is.na(m[indices]) & pn < 2)) stop("N per coppia incompatibile: ", id)
      if (min(pn) != max(pn)) issues[[length(issues)+1L]] <- data.frame(matrix_id = id,
        issue = paste0("N per correlazione: ", min(pn), "-", max(pn), "; N di riferimento: ", n))
    }
  }
  metadata <- dplyr::bind_rows(lapply(active, function(path) data_combine(path, current$basic)))
  if (!length(active)) metadata <- current$basic[FALSE, ]
  for (id in inventory$matrix_id[inventory$status == "missing_matrix_file"])
    issues[[length(issues)+1L]] <- data.frame(matrix_id = id, issue = "Codifica presente; matrice non disponibile")
  issues <- if (length(issues)) do.call(rbind, issues) else wf_empty(c("matrix_id", "issue"))
  list(metadata = metadata, review = current$review, records = records, issues = issues, inventory = inventory)
}

wf_update_results <- function(save = FALSE, reason = NULL) {
  wf_require_saved_coding()
  result <- wf_build_results()
  paths <- c(metadata = "data/3.meta_data/meta_data/metadata.csv", review = "data/3.meta_data/review_data/review_data.csv")
  changes <- dplyr::bind_rows(lapply(names(paths), function(name) {
    old <- if (file.exists(paths[[name]])) wf_read(paths[[name]]) else result[[name]][FALSE, ]
    wf_diff(old, result[[name]], if (name == "metadata") "matrix_id" else "paper_id", name)
  }))
  numeric_changes <- changes[changes$dataset == "metadata" & (changes$field == "n" | !changes$field %in% coding_dictionary()$field), , drop = FALSE]
  existing_numeric <- numeric_changes[numeric_changes$field != ".record", , drop = FALSE]
  summary <- data.frame(voce = c("Campioni meta-analisi", "Articoli review", "Valori modificati", "Variazioni numeriche", "Stato", "Motivazione"),
    valore = c(nrow(result$metadata), nrow(result$review), nrow(changes), nrow(existing_numeric), if (save) "Aggiornamento richiesto" else "Anteprima", if (is.null(reason)) "" else reason))
  report <- "data/3.meta_data/last_update.xlsx"
  wf_write_book(list(riepilogo = summary, variazioni = changes, attenzione = result$issues), report, character())
  if (save) {
    if (nrow(existing_numeric) && (is.null(reason) || !nzchar(trimws(reason))))
      stop("Le variazioni numeriche sono in last_update.xlsx. Dopo la verifica, usa aggiorna_risultati(motivo = '...').", call. = FALSE)
    source(here::here('R/report_inputs.R'), local = TRUE)
    history <- report_search_history(results = result)
    wf_commit(setNames(list(result$metadata, result$review, result$records, history),
        c(unname(paths), wf_records_path, 'data/search_results_history.csv')),
      setNames(list(result$metadata, result$review, report_history_workbook(history)),
        c(sub("\\.csv$", ".xlsx", unname(paths)), 'data/search_results_history.xlsx')))
    wf_export_coding(force = TRUE)
    summary$valore[summary$voce == "Stato"] <- "Salvato"
    wf_write_book(list(riepilogo = summary, variazioni = changes, attenzione = result$issues), report, character())
  }
  message(if (save) "Risultati salvati" else "Anteprima pronta", ": ", nrow(result$metadata), " campioni, ", nrow(result$review),
    " articoli; ", nrow(changes), " variazioni. Riepilogo: ", report)
  invisible(c(result, list(changes = changes)))
}

wf_compact_extraction <- function(paper_id, matrix_id, individual_data, cor_matrix, pair_n, checks) {
  verified <- extraction_check_matrix(individual_data[, colnames(cor_matrix), drop = FALSE], cor_matrix, pair_n)
  if (as.numeric(checks$n) != verified$n) stop("N dei controlli diverso dai dati individuali.")
  stem <- file.path("data/3.meta_data/matrices", matrix_id)
  pair_index <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  pairs <- data.frame(var1 = rownames(cor_matrix)[pair_index[,1]], var2 = colnames(cor_matrix)[pair_index[,2]], n = pair_n[pair_index])
  summary_path <- "data/3.meta_data/extraction/summary.csv"
  summary <- if (file.exists(summary_path)) wf_read(summary_path) else data.frame(matrix_id = character())
  entry <- as.data.frame(c(list(paper_id = paper_id, matrix_id = matrix_id), lapply(checks, as.character)), stringsAsFactors = FALSE)
  summary <- dplyr::bind_rows(summary[summary$matrix_id != matrix_id, , drop = FALSE], entry)
  labelled <- data.frame(variable = rownames(cor_matrix), cor_matrix, check.names = FALSE)
  wf_commit(setNames(list(individual_data, labelled, pairs, summary), c(
    file.path("data/3.meta_data/open_data/individual_data", paste0(matrix_id, ".csv")),
    paste0(stem, ".csv"), file.path("data/3.meta_data/pair_n", paste0(matrix_id, ".csv")), summary_path)),
    setNames(list(as.data.frame(cor_matrix)), paste0(stem, ".xlsx")))
  invisible(stem)
}
