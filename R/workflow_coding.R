# One cumulative coding table. Excel is an editable interface, CSV is canonical.
wf_coding_dir <- "data/3.meta_data/coding"
wf_records_path <- file.path(wf_coding_dir, "records.csv")
wf_coding_path <- file.path(wf_coding_dir, "coding.xlsx")
wf_context <- c("batch_id", "screening_record_id", "component_label", "sample_label", "occasion_label", "occasion_role", "change_note")
wf_paper_fields <- setdiff(coding_article_fields(), "notes")
wf_sample_fields <- c("component_label", "sample_label", "occasion_label", "occasion_role", coding_sample_fields(), "review_topics", "exclusion_meta", "change_note")

wf_coding_views <- function(records) {
  first <- c("title", "author_et_al", "year", "review_topics", "download_date")
  papers <- records[!duplicated(records$paper_id), c("paper_id", first, setdiff(wf_paper_fields, first),
    "final_decision", "exclusion_general", "exclusion_meta", "batch_id", "screening_record_id", "change_note"), drop = FALSE]
  # Review-only notes belong to the article; quantitative notes stay by sample.
  papers$notes <- records$notes[match(papers$paper_id, records$paper_id)]
  papers$notes[papers$final_decision %in% c("both", "meta")] <- NA_character_
  samples <- records[records$final_decision %in% c("both", "meta"), c("paper_id", "sample_id", "matrix_id", wf_sample_fields), drop = FALSE]
  list(articoli = papers, campioni = samples)
}
wf_coding_tables <- function() {
  list(records = wf_read(wf_records_path),
    evidence = wf_read(file.path(wf_coding_dir, "evidence.csv")),
    sources = wf_read(file.path(wf_coding_dir, "sources.csv")))
}
wf_export_coding <- function(force = FALSE) {
  if (file.exists(wf_coding_path) && !force) {
    if (wf_book_has_edits(wf_coding_path))
      stop("Ci sono modifiche in coding.xlsx: importa_codifica() prima di rigenerarlo.", call. = FALSE)
  }
  data <- wf_coding_tables(); views <- wf_coding_views(data$records)
  tables <- c(views, list(fonti = data$sources, evidenze = data$evidence, dizionario = coding_dictionary()))
  instructions <- data.frame(azione = c("Articoli", "Campioni", "Note", "Correzioni", "Nuovi campioni", "Fonti", "Importare"),
    istruzione = c("Celle gialle modificabili. Gli identificativi e la decisione di screening si aggiornano tramite il lotto.",
      "Una riga per campione quantitativo. Restano visibili le righe storiche incomplete, senza inventare dati.",
      "Articoli: note dei review-only. Campioni: note scientifiche della singola matrice. I dati storici review-only sono conservati nel CSV per compatibilita, senza nuove estrazioni.",
      "Inserisci una breve motivazione in change_note per le correzioni. Una modifica di un campo articolo vale per tutti i suoi campioni; le modifiche esplicite nel foglio campioni hanno precedenza.",
      "Usa aggiungi_campione(): gli ID vengono assegnati da R. Non aggiungere o cancellare righe direttamente nell'Excel.",
      "Registra URL, percorso locale, data nota o sconosciuta e provenienza in fonti. Evidenze riporta pagine e motivazioni della codifica.",
      "Salva e chiudi Excel, poi importa_codifica(). Controlla anteprima_risultati() e infine aggiorna_risultati()."))
  wf_write_book(tables, wf_coding_path,
    file.path(wf_coding_dir, c("records.csv", "evidence.csv", "sources.csv")),
    editable = list(articoli = c(wf_paper_fields, "notes", "change_note"), campioni = wf_sample_fields,
      fonti = setdiff(names(data$sources), "sha256"), evidenze = names(data$evidence)), instructions = instructions)
}

wf_validate_records <- function(records, previous = NULL) {
  fields <- c(coding_dictionary()$field, wf_context)
  if (!identical(names(records), fields)) stop("Contratto della codifica non valido.")
  if (anyNA(records$paper_id) || anyNA(records$matrix_id) || anyDuplicated(records$matrix_id) ||
      any(!grepl("^[0-9]{4,}$", records$paper_id))) stop("Identificativi di codifica mancanti o duplicati.")
  if (any(!records$final_decision %in% c("review", "meta", "both", "exclude"))) stop("Decisione di screening non valida.")
  for (field in c("n", "gender", "age", "year")) {
    n <- wf_number(records[[field]], field)
    if (field == "n" && any(!is.na(n) & (n <= 0 | n != floor(n)))) stop("n deve essere un intero positivo.")
    if (field == "gender" && any(!is.na(n) & (n < 0 | n > 100))) stop("gender deve essere una percentuale 0-100.")
    if (field == "age" && any(!is.na(n) & n < 0)) stop("Eta negativa.")
  }
  date <- wf_text(records$download_date)
  if (any(nzchar(date) & (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date) | is.na(as.Date(date, format = "%Y-%m-%d"))))) stop("Data di download non valida.")
  enums <- list(study_design = c("cross", "long", "exp"), age_class = c("children", "school_age", "young_adult", "adult", "older_adult"),
    clinical = c("clinical", "not_clinical"), seb_measure_type = c("short", "long"))
  for (field in names(enums)) {
    changed <- if (is.null(previous)) rep(TRUE, nrow(records)) else
      wf_text(records[[field]]) != wf_text(previous[[field]][match(records$matrix_id, previous$matrix_id)])
    bad <- changed & nzchar(wf_text(records[[field]])) & !records[[field]] %in% enums[[field]]
    if (any(bad)) stop("Categoria non valida: ", field)
  }
  known <- wf_read("data/topics_codebook.csv")$Topic
  for (i in seq_len(nrow(records))) {
    old <- if (!is.null(previous)) previous$review_topics[match(records$matrix_id[i], previous$matrix_id)] else NA_character_
    if (!wf_same(records$review_topics[i], old)) {
      topics <- trimws(strsplit(wf_text(records$review_topics[i]), ";", fixed = TRUE)[[1]])
      unknown <- setdiff(topics[nzchar(topics)], known)
      if (length(unknown)) stop("Topic da aggiungere al codebook: ", paste(unknown, collapse = ", "))
    }
    if (!is.na(records$batch_id[i]) && records$final_decision[i] %in% c("both", "meta") && !is.na(records$sample_id[i])) {
      if (!identical(records$occasion_role[i], "first_eligible")) stop("Solo la prima occasione eleggibile e attiva.")
      path <- wf_screen_paths(records$batch_id[i])$ft
      if (file.exists(path)) {
        decision <- wf_read(path)
        components <- trimws(strsplit(wf_text(decision$eligible_quantitative_components[match(records$paper_id[i], decision$paper_id)]), ";", fixed = TRUE)[[1]])
        if (!records$component_label[i] %in% components) stop("Componente non eleggibile: ", records$matrix_id[i])
      }
    }
  }
  invisible(records)
}

wf_import_coding <- function(save = TRUE) {
  wf_check_version(wf_coding_path)
  data <- wf_coding_tables(); before <- data$records; after <- before
  views <- wf_coding_views(before)
  papers <- wf_check_keys(views$articoli, wf_read_book(wf_coding_path, "articoli"), "paper_id")
  samples <- wf_check_keys(views$campioni, wf_read_book(wf_coding_path, "campioni"), "matrix_id")
  for (field in setdiff(names(papers), c(wf_paper_fields, "notes", "change_note")))
    if (!wf_same(papers[[field]], views$articoli[[field]])) stop("Campo assegnato da R: ", field)
  for (field in c("paper_id", "sample_id", "matrix_id"))
    if (!wf_same(samples[[field]], views$campioni[[field]])) stop("Identificativo modificato: ", field)
  for (i in seq_len(nrow(papers))) for (field in c(wf_paper_fields, "notes", "change_note")) {
    if (wf_same(papers[[field]][i], views$articoli[[field]][i])) next
    if (field == "notes" && papers$final_decision[i] %in% c("both", "meta")) stop("Inserisci le note quantitative nel foglio campioni: ", papers$paper_id[i])
    after[[field]][after$paper_id == papers$paper_id[i]] <- papers[[field]][i]
  }
  for (i in seq_len(nrow(samples))) for (field in wf_sample_fields) {
    if (wf_same(samples[[field]][i], views$campioni[[field]][i])) next
    after[[field]][match(samples$matrix_id[i], after$matrix_id)] <- samples[[field]][i]
  }
  wf_validate_records(after, before)
  changes <- wf_diff(before, after, "matrix_id", "codifica")
  substantive <- changes[changes$field != "change_note", , drop = FALSE]
  if (nrow(substantive) && any(!nzchar(wf_text(after$change_note[match(substantive$id, after$matrix_id)]))))
    stop("Aggiungi una breve motivazione in change_note per le righe modificate.", call. = FALSE)
  sources <- wf_read_book(wf_coding_path, "fonti")
  evidence <- wf_read_book(wf_coding_path, "evidenze")
  if (!identical(names(sources), names(data$sources)) || !identical(names(evidence), names(data$evidence))) stop("Colonne di fonti/evidenze cambiate.")
  # Original bytes remain independently verifiable. A changed download is a new source row.
  for (i in seq_len(nrow(sources))) {
    p <- sources$path[i]
    if (!is.na(p) && nzchar(p) && sources$role[i] != "legacy_derived_evidence") {
      actual <- wf_hash(p)
      if (actual == "absent") stop("Fonte locale assente: ", p)
      if (nzchar(wf_text(sources$sha256[i])) && actual != sources$sha256[i]) stop("Originale modificato: ", p)
      sources$sha256[i] <- actual
    }
  }
  if (save) {
    wf_commit(setNames(list(after, evidence, sources), file.path(wf_coding_dir, c("records.csv", "evidence.csv", "sources.csv"))))
    wf_export_coding(force = TRUE)
    message("Codifica importata: ", nrow(substantive), " valori modificati. Esegui anteprima_risultati().")
  }
  invisible(list(records = after, changes = changes, sources = sources, evidence = evidence))
}

wf_current <- function(records = wf_read(wf_records_path)) {
  basic <- records[, coding_dictionary()$field, drop = FALSE]
  for (field in c("n", "age", "gender", "year")) basic[[field]] <- wf_number(basic[[field]], field)
  basic$download_date <- as.POSIXct(basic$download_date, format = "%Y-%m-%d", tz = "UTC")
  attr(basic, "topic_corrections") <- data.frame(source_file = character(), paper_id = character(), matrix_id = character(), old_value = character(), new_value = character())
  review <- basic[!duplicated(basic$paper_id) & basic$final_decision != "exclude" &
    (is.na(records$batch_id) | basic$final_decision %in% c("review", "both")), , drop = FALSE]
  provenance <- data.frame(paper_id = records$paper_id, matrix_id = records$matrix_id,
    origin = ifelse(is.na(records$batch_id), "historical", ifelse(grepl("-external-", records$batch_id, fixed = TRUE), "external_coding", "monthly_coding")),
    batch_id = records$batch_id, eligibility_status = ifelse(is.na(records$batch_id), "historical_not_reassessed", "full_text_v2_resolved"))
  list(basic = basic, review = review, legacy = coding_read_basic_info(), provenance = provenance, matrices = data.frame())
}

wf_require_saved_coding <- function() {
  if (file.exists(wf_coding_path) && wf_book_has_edits(wf_coding_path)) stop("Importa le modifiche di coding.xlsx prima di proseguire.")
}
wf_prepare_coding <- function(batch, download_date = NULL) {
  wf_require_saved_coding()
  p <- wf_screen_paths(batch)
  if (!file.exists(p$ft) && file.exists(p$included) && !nrow(wf_read(p$included))) {
    message("Nessun articolo da aggiungere alla codifica.")
    return(invisible(wf_read(wf_records_path)))
  }
  eligibility <- ft_read_canonical_decisions(p$ft)
  records <- wf_read(wf_records_path)
  input <- wf_read(p$input)
  for (i in which(eligibility$final_route %in% c("review", "meta", "both"))) {
    decision <- eligibility[i, ]; id <- decision$paper_id
    if (id %in% records$paper_id) {
      # Resolved reassessment updates routing without erasing descriptive coding.
      rows <- records$paper_id == id
      records$batch_id[rows] <- batch
      records$screening_record_id[rows] <- decision$screening_record_id
      records$final_decision[rows] <- decision$final_route
      records$exclusion_general[rows] <- "include"
      records$exclusion_meta[rows] <- if (decision$meta_status == "eligible") "include" else decision$meta_exclusion_reason
      next
    }
    row <- as.data.frame(setNames(rep(list(NA_character_), ncol(records)), names(records)))
    j <- match(decision$screening_record_id, input$screening_record_id)
    if (is.na(j)) stop("Record bibliografico mancante: ", id)
    for (field in intersect(names(input), names(row))) row[[field]] <- input[[field]][j]
    row$authors <- input$author[j]
    row$paper_id <- id; row$matrix_id <- id; row$screening_record_id <- decision$screening_record_id
    row$title <- decision$title; row$doi <- decision$doi; row$batch_id <- batch
    row$download_date <- if (is.null(download_date)) NA_character_ else as.character(as.Date(download_date))
    row$exclusion_general <- "include"; row$exclusion_meta <- if (decision$meta_status == "eligible") "include" else decision$meta_exclusion_reason
    row$final_decision <- decision$final_route
    records <- rbind(records, row)
  }
  # A resolved exclusion is retained in coding history, and omitted from new results.
  for (id in eligibility$paper_id[eligibility$final_route == "exclude"]) if (id %in% records$paper_id) {
    records$final_decision[records$paper_id == id] <- "exclude"
    records$batch_id[records$paper_id == id] <- batch
    records$screening_record_id[records$paper_id == id] <- eligibility$screening_record_id[match(id, eligibility$paper_id)]
  }
  wf_commit(setNames(list(records), wf_records_path)); wf_export_coding(force = TRUE)
  invisible(records)
}
wf_add_sample <- function(paper_id, component, label, occasion = "First eligible occasion") {
  wf_require_saved_coding(); records <- wf_read(wf_records_path)
  existing <- records[records$paper_id == paper_id, , drop = FALSE]
  if (!nrow(existing) || !all(existing$final_decision %in% c("both", "meta"))) stop("Articolo quantitativo non presente nella codifica.")
  if (any(wf_text(existing$component_label) == component & wf_text(existing$sample_label) == label)) stop("Campione gia presente.")
  if (any(!nzchar(c(component, label, occasion)))) stop("Specifica componente, campione e occasione.")
  batch <- existing$batch_id[1]
  if (is.na(batch)) stop("Per lo storico conserva gli ID esistenti; una nuova suddivisione richiede riconciliazione scientifica.")
  eligibility <- ft_read_canonical_decisions(wf_screen_paths(batch)$ft)
  eligible <- trimws(strsplit(eligibility$eligible_quantitative_components[match(paper_id, eligibility$paper_id)], ";", fixed = TRUE)[[1]])
  if (!component %in% eligible) stop("Componente non eleggibile: ", component)
  used <- na.omit(existing$sample_id); letter <- setdiff(letters, used)[1]
  if (is.na(letter)) stop("Piu di 26 campioni: concordare l'estensione degli ID.")
  row <- existing[1, , drop = FALSE]
  for (field in coding_sample_fields()) row[[field]] <- NA_character_
  row$sample_id <- letter; row$matrix_id <- paste0(paper_id, letter)
  row$component_label <- component; row$sample_label <- label; row$occasion_label <- occasion; row$occasion_role <- "first_eligible"
  records <- records[!(records$paper_id == paper_id & is.na(records$sample_id)), , drop = FALSE]
  records <- rbind(records, row)
  wf_commit(setNames(list(records), wf_records_path)); wf_export_coding(force = TRUE)
  message("Campione assegnato: ", row$matrix_id)
  invisible(row$matrix_id)
}

wf_import_article <- function(path, batch) {
  # AI JSON is an interchange format. The cumulative CSV owns accepted coding.
  wf_require_saved_coding()
  obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  eligibility <- ft_read_canonical_decisions(wf_screen_paths(batch)$ft)
  coding_validate_artifact(obj, eligibility, coding_read_csv("data/topics_codebook.csv"))
  data <- wf_coding_tables(); records <- data$records
  old <- records[records$paper_id == obj$paper_id, , drop = FALSE]
  if (nrow(old) && any(nzchar(wf_text(old$coder)))) stop("Articolo gia codificato: correggi coding.xlsx, senza sovrascrivere la verifica umana.")
  decision <- eligibility[match(obj$paper_id, eligibility$paper_id), ]
  row <- as.data.frame(setNames(rep(list(NA_character_), ncol(records)), names(records)))
  row$paper_id <- obj$paper_id; row$screening_record_id <- obj$screening_record_id; row$batch_id <- batch
  for (field in coding_article_fields()) row[[field]] <- as.character(coding_flat(obj$article[[field]]))
  row$exclusion_general <- "include"; row$exclusion_meta <- if (decision$meta_status == "eligible") "include" else decision$meta_exclusion_reason
  row$final_decision <- decision$final_route; row$matrix_id <- obj$paper_id
  row$change_note <- paste("Initial coding", obj$coding_version, "on", obj$coding_date, "from", basename(path))
  rows <- list()
  if (!length(obj$samples)) rows[[1]] <- row
  for (i in seq_along(obj$samples)) {
    s <- obj$samples[[i]]; r <- row
    # Reuse IDs already assigned manually to the same sample label.
    found <- which(wf_text(old$component_label) == s$component_label & wf_text(old$sample_label) == s$sample_label)
    if (length(found)) letter <- old$sample_id[found[1]] else {
      used <- c(na.omit(old$sample_id), vapply(rows, function(r) wf_text(r$sample_id), character(1)))
      letter <- setdiff(letters, used)[1]
    }
    if (is.na(letter)) stop("Piu di 26 campioni.")
    for (field in c(coding_sample_fields(), "component_label", "sample_label", "occasion_label", "occasion_role")) r[[field]] <- as.character(coding_flat(s[[field]]))
    r$notes <- paste(na.omit(c(as.character(coding_flat(obj$article$notes)), as.character(coding_flat(s$notes)))), collapse = " ")
    if (length(s$country) > 1) r$country <- "mixed"
    r$sample_id <- letter; r$matrix_id <- paste0(obj$paper_id, letter); rows[[i]] <- r
  }
  after <- dplyr::bind_rows(records[records$paper_id != obj$paper_id, ], dplyr::bind_rows(rows))
  wf_validate_records(after, records)
  ev <- dplyr::bind_rows(lapply(obj$evidence, function(e) {
    e$page <- e$page %||% NA_integer_; x <- as.data.frame(e); x$paper_id <- obj$paper_id; x
  }))
  ev[] <- lapply(ev, as.character)
  evidence <- dplyr::bind_rows(data$evidence, ev[, names(data$evidence)])
  wf_commit(setNames(list(after, evidence), c(wf_records_path, file.path(wf_coding_dir, "evidence.csv"))))
  wf_export_coding(force = TRUE)
  invisible(after[after$paper_id == obj$paper_id, ])
}
