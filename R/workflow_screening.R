# A single workbook per batch; scientific rules remain in the existing validators.
wf_batch <- function(batch) {
  if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", batch)) stop("Lotto non valido.")
  batch
}
wf_screen_paths <- function(batch) {
  wf_batch(batch)
  ta <- file.path("data/1.abstracting", batch, "title_abstract", paste0(batch, "_title_abstract_"))
  ft <- file.path("data/2.full_abstracting", paste0(batch, "_full_text_"))
  list(book = file.path("data/1.abstracting", batch, paste0(batch, "_screening.xlsx")),
    input = paste0(ta, "input.csv"), ai = paste0(ta, "ai_decisions.csv"), ta = paste0(ta, "decisions.csv"),
    included = paste0(ta, "included.csv"), manifest = paste0(ft, "manifest.csv"),
    ft = paste0(ft, "decisions.csv"), human = paste0(ft, "human_review.csv"),
    pairs = file.path("data/0.deduplication", batch, "update_candidate_pairs.csv"),
    pair_decisions = file.path("data/0.deduplication", batch, "human_duplicates.csv"))
}
wf_human_ta <- c("human_decision", "human_exclusion_reason", "human_rationale", "human_evidence")
wf_human_ft <- function() ft_human_review_columns()[seq.int(match("human_review_eligible", ft_human_review_columns()), length(ft_human_review_columns()))]
wf_screen_tables <- function(batch) {
  p <- wf_screen_paths(batch); out <- list()
  if (file.exists(p$pairs)) {
    pairs <- wf_read(p$pairs)
    if (!"result" %in% names(pairs)) pairs$result <- NA_character_
    if (file.exists(p$pair_decisions)) {
      previous <- wf_read(p$pair_decisions)
      key <- function(x) paste(x$duplicate_id.x, x$duplicate_id.y)
      pairs$result <- previous$result[match(key(pairs), key(previous))]
    }
    out$duplicati <- pairs
  }
  if (file.exists(p$input)) {
    input <- wf_read(p$input)
    if (file.exists(p$ta)) ta <- wf_read(p$ta)
    else if (file.exists(p$ai)) ta <- dplyr::left_join(input, wf_read(p$ai), by = "screening_record_id", suffix = c("", ".decision"))
    else ta <- input
    for (field in c(screening_required_decision_columns(), wf_human_ta))
      if (!field %in% names(ta)) ta[[field]] <- rep(NA_character_, nrow(ta))
    out$abstract <- ta
  }
  if (file.exists(p$manifest)) out$pdf <- wf_read(p$manifest)
  if (file.exists(p$ft)) out$full_text <- wf_read(p$ft)
  if (file.exists(p$human)) out$decisioni_full_text <- wf_read(p$human)
  if (file.exists(p$ft)) out$criteri_full_text <- ft_decision_codebook()
  if (!length(out)) out$stato <- data.frame(istruzione = "Prepara il lotto per importare i nuovi riferimenti.")
  out
}
wf_export_screening <- function(batch, force = FALSE) {
  p <- wf_screen_paths(batch)
  if (file.exists(p$book) && !force && wf_book_has_edits(p$book)) stop("Modifiche Excel non importate: importa_screening(lotto) prima di rigenerare.")
  tables <- wf_screen_tables(batch)
  input_paths <- unlist(p[setdiff(names(p), "book")], use.names = FALSE)
  human_edit <- wf_human_ft()
  instructions <- data.frame(fase = c("Duplicati", "Abstract", "PDF", "Full text", "Correzioni", "Salvare"),
    istruzione = c("Compila result con match/no_match. Poi conferma_duplicati(lotto). La promozione del registro resta un passaggio esplicito.",
      "L'IA propone; puoi compilare human_decision, motivo ed evidenza. Le esclusioni devono rispettare la policy. uncertain viene mantenuto.",
      "Salva il PDF con il paper_id indicato. prepara_full_text(lotto) aggiorna la disponibilita.",
      "full_text conserva decisioni ed evidenze. Compila solo le celle gialle in decisioni_full_text per le questioni da risolvere.",
      "Per riaprire una decisione full text gia risolta usa rivedi_full_text(lotto, paper_id, motivo). Le decisioni precedenti restano tracciate in Git e nell'evidenza originale.",
      "Salva e chiudi Excel. importa_screening(lotto) importa le modifiche. La rigenerazione si ferma se ci sono modifiche non importate."))
  wf_write_book(tables, p$book, input_paths,
    editable = list(duplicati = "result", abstract = wf_human_ta, decisioni_full_text = human_edit), instructions = instructions)
  invisible(p$book)
}
wf_import_screening <- function(batch, save = TRUE, require_complete = TRUE, assert_unedited = FALSE) {
  p <- wf_screen_paths(batch); wf_check_version(p$book)
  tables <- wf_screen_tables(batch); writes <- list(); modified <- FALSE
  for (name in intersect(names(tables), readxl::excel_sheets(p$book))) {
    old <- tables[[name]]; edit <- wf_read_book(p$book, name)
    if (name == "duplicati") {
      key <- function(x) paste(x$duplicate_id.x, x$duplicate_id.y)
      if (!setequal(key(old), key(edit)) || anyDuplicated(key(edit))) stop("Coppie di duplicati cambiate.")
      edit <- edit[match(key(old), key(edit)), names(old), drop = FALSE]
      allowed <- "result"
    } else {
      key <- if ("paper_id" %in% names(old)) "paper_id" else if ("screening_record_id" %in% names(old)) "screening_record_id" else NULL
      if (is.null(key)) next
      edit <- wf_check_keys(old, edit, key)
      allowed <- switch(name, abstract = wf_human_ta, decisioni_full_text = wf_human_ft(), character())
    }
    for (field in setdiff(names(old), allowed)) if (!wf_same(old[[field]], edit[[field]])) stop("Campo non modificabile in ", name, ": ", field)
    changed <- any(vapply(allowed, function(field) !wf_same(old[[field]], edit[[field]]), logical(1)))
    modified <- modified || changed
    if (assert_unedited && changed) stop("Modifiche Excel non importate: importa_screening(lotto) prima di rigenerare.")
    if (!save) next
    if (name == "duplicati" && changed) {
      if (any(!wf_text(edit$result) %in% c("", "match", "no_match"))) stop("Duplicati: usa match/no_match.")
      writes[[p$pair_decisions]] <- edit
    }
    if (name == "abstract" && (changed || !file.exists(p$ta))) {
      input <- wf_read(p$input); policy <- screening_load_policy()
      effective <- edit[, screening_required_decision_columns(), drop = FALSE]
      manual <- nzchar(wf_text(edit$human_decision))
      for (field in c("decision", "exclusion_reason", "rationale", "evidence"))
        effective[[paste0("ai_", field)]][manual] <- edit[[paste0("human_", field)]][manual]
      effective$ai_confidence[manual] <- "high"
      effective$model[manual] <- "human"
      effective$screened_at_utc[manual] <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      effective$policy_version[manual] <- policy$version; effective$prompt_version[manual] <- policy$prompt_version
      ready <- all(nzchar(wf_text(effective$ai_decision)))
      if (!ready && require_complete) stop("Completa le decisioni abstract mancanti prima dell'importazione.")
      if (ready) {
        result <- screening_apply_operational_policy(screening_validate_decisions(effective, input, policy), policy)
        downstream <- c("automatic_action", "action_basis", "final_screening_decision", "human_review_required")
        for (field in downstream) edit[[field]] <- result[[field]][match(edit$screening_record_id, result$screening_record_id)]
        # AI columns remain original; only the final action uses the human override.
        writes[[p$ta]] <- edit
        writes[[p$included]] <- edit[edit$automatic_action != "EXCLUDE", , drop = FALSE]
      }
    }
    if (name == "decisioni_full_text" && changed) {
      canonical <- ft_read_canonical_decisions(p$ft)
      ft_validate_queue_edits(old, edit)
      res <- ft_apply_adjudications(canonical, edit)
      writes[[p$ft]] <- res$decisions; writes[[p$human]] <- res$human_review
    }
  }
  if (save) { wf_commit(writes); wf_export_screening(batch, force = TRUE) }
  invisible(modified)
}

wf_script <- function(path, batch) {
  wf_batch(batch)
  previous <- Sys.getenv("LIVING_SEB_BATCH_ID", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("LIVING_SEB_BATCH_ID") else Sys.setenv(LIVING_SEB_BATCH_ID = previous), add = TRUE)
  Sys.setenv(LIVING_SEB_BATCH_ID = batch)
  old <- options(living_seb.compact = TRUE); on.exit(options(old), add = TRUE)
  e <- new.env(parent = globalenv()); e$commandArgs <- function(...) batch
  sys.source(path, envir = e)
  invisible(NULL)
}

wf_reopen_full_text <- function(batch, paper_id, reason) {
  p <- wf_screen_paths(batch)
  if (!nzchar(trimws(reason))) stop("Specifica il motivo della rivalutazione.")
  if (file.exists(p$book) && wf_book_has_edits(p$book)) stop("Importa prima le modifiche dello screening.")
  decisions <- ft_read_canonical_decisions(p$ft)
  i <- match(paper_id, decisions$paper_id)
  if (is.na(i)) stop("Paper assente nel lotto.")
  if (decisions$requires_human_review[i]) stop("Il paper e gia in revisione.")
  previous <- decisions[i, , drop = FALSE]
  decisions$requires_human_review[i] <- TRUE
  decisions$human_review_reasons[i] <- "OTHER_SUBSTANTIVE_UNCERTAINTY"
  decisions$issue_summary[i] <- reason
  decisions$ai_recommendation[i] <- paste("Decisione precedente:", previous$final_route, "; verificare la correzione richiesta.")
  decisions$decision_options[i] <- "Confermare la decisione precedente; correggere review o meta eligibility con evidenza"
  route <- ft_derive_route(decisions$review_eligible, decisions$meta_status, decisions$requires_human_review)
  decisions$current_route <- route$current_route; decisions$final_route <- route$final_route
  decisions$workflow_status[i] <- "HUMAN_REVIEW_REQUIRED"; decisions$adjudication_status[i] <- "OPEN"
  decisions <- ft_validate_ai_decisions(decisions, decisions)
  new <- ft_build_human_review_queue(decisions, decisions)
  new <- new[new$paper_id == paper_id, , drop = FALSE]
  # Prefill prior values so a correction only needs the fields that change.
  for (field in grep("^human_", names(new), value = TRUE)) {
    from <- sub("^human_", "", field)
    if (from %in% names(previous)) new[[field]] <- wf_text(previous[[from]])
  }
  human <- wf_read(p$human)
  if (paper_id %in% human$paper_id) {
    previous_note <- human$adjudication_note[match(paper_id, human$paper_id)]
    new$issue_summary <- paste(reason, "Previous adjudication:", previous_note)
  }
  new[] <- lapply(new, wf_text)
  human <- dplyr::bind_rows(human[human$paper_id != paper_id, ], new)
  wf_commit(setNames(list(decisions, human), c(p$ft, p$human)))
  wf_export_screening(batch, force = TRUE)
  invisible(NULL)
}
