# Living SEB: run selected lines from RStudio, starting at the repository root.
# Detailed instructions: MONTHLY_WORKFLOW.md. Loading this file never writes data.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tibble); library(purrr); library(readxl) })
source("R/utils.R")
source("R/full_text_workflow.R")
source("R/coding_compatibility.R")
source("R/article_coding.R")
source("R/codebook_workflow.R")
source("R/workflow_io.R")
source("R/workflow_coding.R")
source("R/workflow_results.R")
source("R/title_abstract_screening.R")
source("R/workflow_screening.R")

mostra_screening <- function(lotto) wf_export_screening(lotto)
importa_screening <- function(lotto) wf_import_screening(lotto)
prepara_lotto <- function(lotto) {
  if (file.exists(file.path("data/0.registry", paste0("records_master_full_through_", lotto, ".csv"))))
    stop("Lotto gia promosso: riprendi dallo screening, senza ripetere la deduplicazione.")
  wf_script("scripts/dedup/03_update_prepare.R", lotto)
  wf_export_screening(lotto)
}
conferma_duplicati <- function(lotto) {
  importa_screening(lotto)
  wf_script("scripts/dedup/04_update_finalize.R", lotto)
  wf_script("scripts/screening/01_prepare_title_abstract.R", lotto)
  wf_export_screening(lotto, force = TRUE)
}
promuovi_registro <- function(lotto) wf_script("scripts/dedup/06_promote_registry.R", lotto)
raccogli_abstract <- function(lotto) {
  p <- wf_screen_paths(lotto)
  if (file.exists(p$book) && wf_book_has_edits(p$book)) stop("Importa prima le modifiche dello screening.")
  wf_script("scripts/screening/02_finalize_title_abstract.R", lotto)
}
prepara_full_text <- function(lotto) {
  if (file.exists(wf_screen_paths(lotto)$book) && wf_book_has_edits(wf_screen_paths(lotto)$book)) stop("Importa prima le modifiche dello screening.")
  p <- wf_screen_paths(lotto)
  if (file.exists(p$included) && !nrow(wf_read(p$included))) {
    message("Nessun articolo mantenuto: il lotto si conclude allo screening abstract.")
    return(invisible(NULL))
  }
  wf_script("scripts/full_text/01_assign_paper_ids.R", lotto)
  wf_export_screening(lotto, force = TRUE)
}
raccogli_full_text <- function(lotto) {
  wf_export_screening(lotto) # Check for unimported human edits first.
  wf_script("scripts/full_text/02_build_full_text_decisions.R", lotto)
  wf_export_screening(lotto, force = TRUE)
}
rivedi_full_text <- function(lotto, paper_id, motivo) wf_reopen_full_text(lotto, paper_id, motivo)

# Human coding and final outputs
mostra_codifica <- function() wf_export_coding()
importa_codifica <- function() wf_import_coding()
anteprima_risultati <- function() wf_update_results()
aggiorna_risultati <- function(motivo = NULL) wf_update_results(save = TRUE, reason = motivo)
prepara_codifica <- function(lotto, data_download = NULL) wf_prepare_coding(lotto, data_download)
aggiungi_campione <- function(paper_id, componente, campione, occasione = "First eligible occasion")
  wf_add_sample(paper_id, componente, campione, occasione)
importa_articolo <- function(file_json, lotto) wf_import_article(file_json, lotto)
importa_codebook <- function() {
  tables <- list()
  for (kind in c("variables", "topics")) {
    stem <- if (kind == "variables") "data/matrix_codebook" else "data/topics_codebook"
    edited <- codebook_excel(paste0(stem, ".xlsx")); current <- codebook_csv(paste0(stem, ".csv"))
    codebook_validate(edited, kind)
    key <- codebook_key(kind)
    if (length(setdiff(na.omit(current[[key]]), na.omit(edited[[key]])))) stop("Non rinominare/eliminare codici gia usati: ", kind)
    added <- edited[!codebook_blank(edited[[key]]) & !edited[[key]] %in% current[[key]], , drop = FALSE]
    if (nrow(added)) codebook_validate(added, kind, additions = TRUE)
    tables[[paste0(stem, ".csv")]] <- edited
  }
  wf_commit(tables)
  message("Codebook importati; codici esistenti preservati.")
}

# Each article still has its own explicit, readable scientific script.
estrai_articolo <- function(paper_id, verifica_soltanto = FALSE) {
  if (!grepl("^[0-9]{4,}$", paper_id)) stop("paper_id non valido.")
  path <- file.path("scripts/extraction/articles", paste0(paper_id, ".R"))
  if (!file.exists(path)) stop("Script articolo assente: ", path)
  env <- new.env(parent = globalenv())
  env$commandArgs <- function(...) if (verifica_soltanto) "--check-only" else character()
  sys.source(path, envir = env)
  invisible(NULL)
}
