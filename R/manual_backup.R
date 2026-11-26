# Refresh the existing, Git-ignored manual project after accepted monthly results.
# Scientific scoring stays in its explicit local article scripts, never inferred here.
wf_manual_backup_ospath <- function(path) {
  if (.Platform$OS.type=='windows' && grepl('^[A-Za-z]:',path))
    paste0('\\\\?\\',chartr('/','\\',path)) else path
}

wf_manual_backup_path <- function(root = getwd()) {
  archive <- file.path(root, 'archive_local')
  projects <- list.files(archive, pattern = '^manual_workflow[.]Rproj$',
    recursive = TRUE, full.names = TRUE)
  projects <- projects[!grepl('/storico_originale/', gsub('\\', '/', projects, fixed = TRUE), fixed = TRUE)]
  if (!length(projects)) return(NULL)
  if (length(projects) != 1L) stop('Piu copie manuali locali: specificare il percorso con aggiorna_backup_manuale(percorso=...).')
  dirname(projects)
}

wf_manual_backup_fields <- function() c(coding_dictionary()$field, review_coding_fields())

wf_manual_backup_read <- function(path) {
  if (grepl('[.]xlsx$', path)) {
    x <- as.data.frame(readxl::read_excel(path))
    x[] <- lapply(x, function(v) if (inherits(v, 'POSIXt') || inherits(v, 'Date'))
      format(v, '%Y-%m-%d') else as.character(v))
    x
  } else wf_read(path)
}

wf_manual_backup_signature <- function(path) {
  x <- wf_manual_backup_read(path)
  x[] <- lapply(x, wf_text)
  for (f in intersect(c('n','age','gender','year'), names(x))) {
    v <- suppressWarnings(as.numeric(x[[f]])); ok <- is.finite(v)
    x[[f]][ok] <- as.character(signif(v[ok], 12))
  }
  # Formatting and harmless row/column sorting do not count as scientific edits.
  rows <- vapply(seq_len(nrow(x)), function(i)
    as.character(jsonlite::toJSON(as.list(x[i, sort(names(x)), drop = FALSE]), auto_unbox = TRUE)), character(1))
  digest::digest(list(fields = sort(names(x)), rows = sort(rows)), algo = 'sha256')
}

wf_manual_backup_inputs <- function(path) c(
  list.files(file.path(path, 'data/2.full_abstracting'), pattern = '_full_decisions[.](csv|xlsx)$', full.names = TRUE),
  file.path(path, 'data', c('matrix_codebook.csv','matrix_codebook.xlsx','topics_codebook.csv','topics_codebook.xlsx')))

wf_manual_backup_preflight <- function(result, path = wf_manual_backup_path()) {
  if (is.null(path)) return(invisible(NULL))
  path <- normalizePath(path, winslash = '/', mustWork = TRUE)
  archive <- normalizePath(file.path(getwd(),'archive_local'), winslash = '/', mustWork = TRUE)
  if (!startsWith(paste0(path, '/'), paste0(archive, '/')))
    stop('Il backup manuale deve restare in archive_local/, escluso da Git.')
  for (f in c('manual_workflow.Rproj','R/funzioni.R','data/2.full_exclude.R','data/3.2cor_to_metadata.R'))
    if (!file.exists(file.path(path, f))) stop('Copia manuale incompleta: ', f)
  papers <- unique(result$metadata$paper_id)
  if(anyNA(papers)||any(!grepl('^[0-9]{4,}$',papers)))stop('paper_id quantitativo non valido per lo scoring manuale.')
  for (id in papers) {
    script <- file.path(path, 'data/codici_articoli', paste0(id, '.R'))
    if (!file.exists(script)) {
      # A scaffold is a pending task, never a claim of reconstructed scoring.
      dir.create(dirname(script), recursive = TRUE, showWarnings = FALSE)
      j<-match(id,result$records$paper_id)
      label<-if(!is.na(j))paste(result$records$author_et_al[j],result$records$title[j])else id
      writeLines(c(paste('#', id, '-', label),
        '# Scoring manuale da completare durante estrazione.',
        'source("R/funzioni.R")',
        'stop("TODO_SCORING: inserire item, inversioni, medie, pesi e salva_dati/salva_matrice.")'), script)
    }
    parse(script)
    if (any(grepl('TODO_SCORING', readLines(script, warn = FALSE), fixed = TRUE)))
      stop('Completa e verifica lo scoring locale prima di chiudere il mese: ', script)
  }
  inputs <- wf_manual_backup_inputs(path)
  state_path <- file.path(path, 'provenienza/manual_sync.csv')
  if (file.exists(state_path)) {
    state <- wf_read(state_path)
    for (i in seq_len(nrow(state))) {
      f <- file.path(path, state$path[i])
      if (!file.exists(f) || wf_manual_backup_signature(f) != state$sha256[i])
        stop('Modifiche manuali locali da riconciliare; nessuna sovrascrittura: ', f)
    }
    extra <- setdiff(inputs, file.path(path, state$path))
    if (length(extra)) stop('Nuovi input nella copia manuale da riconciliare: ', paste(extra, collapse = ', '))
  } else {
    # Adopt the already-verified local copy, without discarding subsequent edits.
    books <- inputs[grepl('_full_decisions[.]xlsx$', inputs)]
    for (book in books) {
      csv <- sub('[.]xlsx$', '.csv', book)
      if (!file.exists(csv) || wf_manual_backup_signature(book) != wf_manual_backup_signature(csv))
        stop('Modifiche manuali locali da riconciliare: ', book)
    }
    fields <- wf_manual_backup_fields()
    old <- wf_read(file.path(path, 'provenienza/records.csv'))
    local <- if(length(books))dplyr::bind_rows(lapply(books, wf_manual_backup_read)) else old[FALSE,fields]
    local <- local[local$matrix_id %in% old$matrix_id, fields, drop = FALSE]
    changes <- wf_diff(old[, fields], local, 'matrix_id', 'manual_backup')
    if (nrow(changes)) stop('La copia manuale contiene codifiche autonome da riconciliare prima del sync.')
    for (name in c('matrix_codebook','topics_codebook'))
      if (wf_manual_backup_signature(file.path(path,'data',paste0(name,'.xlsx'))) !=
          wf_manual_backup_signature(file.path(path,'data',paste0(name,'.csv'))))
        stop('Codebook locale modificato senza importazione: ', name)
  }
  invisible(path)
}

wf_manual_backup_compare <- function(expected, actual, key, name) {
  if (nrow(expected) != nrow(actual) || !setequal(names(expected), names(actual)) ||
      anyDuplicated(actual[[key]]) || !setequal(expected[[key]], actual[[key]]))
    stop('Backup manuale non equivalente: struttura ', name)
  actual <- actual[match(expected[[key]], actual[[key]]), names(expected), drop = FALSE]
  for (f in names(expected)) {
    a <- expected[[f]]; b <- actual[[f]]
    if (inherits(a,'POSIXt') || inherits(a,'Date')) a <- format(a,'%Y-%m-%d')
    if (inherits(b,'POSIXt') || inherits(b,'Date')) b <- format(b,'%Y-%m-%d')
    if (is.numeric(a)) {
      b <- suppressWarnings(as.numeric(b))
      ok <- identical(is.na(a),is.na(b)) && all(abs(a-b)<1e-10, na.rm=TRUE)
    } else ok <- identical(wf_text(a),wf_text(b))
    if (!ok) stop('Backup manuale non equivalente: ', name, ' / ', f)
  }
  invisible(TRUE)
}

wf_manual_backup_update <- function(result = wf_build_results(), path = wf_manual_backup_path()) {
  if (is.null(path)) {
    message('Backup manuale locale non presente su questo computer; nessun contenuto privato creato nel repository pubblico.')
    return(invisible(NULL))
  }
  path <- wf_manual_backup_preflight(result, path)
  root <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
  status_path <- file.path(path,'verifica/ultimo_sync.md')
  writeLines(c('# Aggiornamento automatico della copia manuale',
    paste('Data:',as.character(Sys.Date())),
    'INCOMPLETE: sincronizzazione in corso; equivalenza non ancora verificata.'),status_path)
  fields <- wf_manual_backup_fields()
  records <- result$records
  # Keep each historical record in its established month; new rows use batch_id.
  books <- list.files(file.path(path,'data/2.full_abstracting'), pattern='_full_decisions[.]xlsx$',full.names=TRUE)
  previous <- setNames(lapply(books,wf_manual_backup_read),basename(books))
  assigned <- rep(NA_character_,nrow(records))
  for (name in names(previous)) {
    x <- previous[[name]]
    assigned[records$matrix_id %in% x$matrix_id] <- name
  }
  for (i in which(is.na(assigned))) {
    batch <- records$batch_id[i]
    siblings <- which(records$paper_id==records$paper_id[i] & !is.na(assigned))
    assigned[i] <- if (!is.na(batch) && nzchar(batch))
      paste0(gsub('-','_',sub('^20','',batch)),'_full_decisions.xlsx') else
      if (length(siblings)) assigned[siblings[1]] else 'historical_unassigned_full_decisions.xlsx'
  }
  local <- new.env(parent=globalenv())
  withr::with_dir(path, sys.source('R/funzioni.R', local))
  for (name in unique(c(names(previous),assigned))) {
    old <- previous[[name]]
    if (is.null(old)) old <- records[FALSE,fields]
    retained_exclusions <- old[!is.na(old$final_decision) & old$final_decision=='exclude' &
      !old$matrix_id %in% records$matrix_id,fields,drop=FALSE]
    out <- dplyr::bind_rows(records[assigned==name,c(fields,'batch_id'),drop=FALSE],retained_exclusions)
    for (f in c('n','age','gender','year')) out[[f]] <- suppressWarnings(as.numeric(out[[f]]))
    out$download_date <- as.Date(out$download_date)
    local$salva_tabella(out,file.path(path,'data/2.full_abstracting',tools::file_path_sans_ext(name)),'full_decisions')
  }
  copy <- function(from,to,immutable=FALSE) {
    source<-wf_manual_backup_ospath(from);target<-wf_manual_backup_ospath(to)
    if (!file.exists(source)) return(invisible(NULL))
    dir.create(wf_manual_backup_ospath(dirname(to)),recursive=TRUE,showWarnings=FALSE)
    if (immutable && file.exists(target) && wf_hash(source)!=wf_hash(target))
      stop('Originale locale diverso: conserva la nuova versione con un percorso distinto prima del sync: ',to)
    if (!file.exists(target) || wf_hash(source)!=wf_hash(target))
      if (!file.copy(source,target,overwrite=TRUE,copy.date=TRUE)) stop('Copia locale non riuscita: ',to)
  }
  for (name in c('matrix_codebook.csv','matrix_codebook.xlsx','topics_codebook.csv','topics_codebook.xlsx'))
    copy(file.path(root,'data',name),file.path(path,'data',name))
  copy(file.path(root,'config/coding/legacy_31_fields.csv'),file.path(path,'data/legacy_31_fields.csv'))
  copy(file.path(root,'config/coding/variable_suffixes.csv'),file.path(path,'data/variable_suffixes.csv'))
  dictionary <- review_coding_dictionary(coding_dictionary())
  local$salva_tabella(dictionary,file.path(path,'data/coding_dictionary'),'dictionary')
  save_input_state <- function() {
    inputs<-wf_manual_backup_inputs(path)
    state<-data.frame(path=substring(gsub('\\','/',inputs,fixed=TRUE),nchar(path)+2),
      sha256=vapply(inputs,wf_manual_backup_signature,character(1)),stringsAsFactors=FALSE)
    readr::write_csv(state,file.path(path,'provenienza/manual_sync.csv'))
  }
  # Ownership of generated inputs is separate from successful scientific validation.
  # If scoring fails later, fixing it can resume without misclassifying our writes
  # as autonomous human edits. ultimo_sync remains INCOMPLETE until comparisons pass.
  save_input_state()
  # Preserve downloaded bytes; coded individuals are rebuilt by local scoring.
  for (dir in c('data/0.bib_download','data/1.abstracting','data/2.full_abstracting/pdfs','data/3.meta_data/open_data')) {
    originals <- list.files(file.path(root,dir),recursive=TRUE,full.names=TRUE)
    originals <- originals[!grepl('/individual_data/',gsub('\\','/',originals,fixed=TRUE),fixed=TRUE)]
    originals <- originals[!grepl('^~\\$',basename(originals))]
    for (from in originals) copy(from,file.path(path,substring(gsub('\\','/',from,fixed=TRUE),nchar(root)+2)),
      immutable=dir!='data/1.abstracting' && basename(from)!='bibAll.bib')
  }
  # The index grows deterministically; article formulas themselves remain explicit.
  papers <- unique(result$metadata$paper_id)
  writeLines(c('# Indice degli scoring manuali; generato dopo aggiornamento accettato.',
    'source("R/funzioni.R")',sprintf('source("data/codici_articoli/%s.R", local=new.env())',papers)),
    file.path(path,'data/3.1opendata_to_cor.R'))
  withr::with_dir(path, {
    for (script in c('data/2.full_exclude.R','data/3.1opendata_to_cor.R','data/3.2cor_to_metadata.R'))
      sys.source(script,envir=new.env(parent=globalenv()))
  })
  actual_meta <- as.data.frame(readxl::read_excel(file.path(path,'data/3.meta_data/meta_data/metadata.xlsx')))
  actual_review <- as.data.frame(readxl::read_excel(file.path(path,'data/3.meta_data/review_data/review_data.xlsx')))
  wf_manual_backup_compare(result$metadata,actual_meta,'matrix_id','metadata')
  wf_manual_backup_compare(result$review,actual_review,'paper_id','review_data')
  for (id in result$metadata$matrix_id) {
    from <- as.matrix(readxl::read_excel(file.path(root,'data/3.meta_data/matrices',paste0(id,'.xlsx'))))
    to <- as.matrix(readxl::read_excel(file.path(path,'data/3.meta_data/matrices',paste0(id,'.xlsx'))))
    if (!setequal(colnames(from),colnames(to))) stop('Variabili diverse nella copia manuale: ',id)
    rownames(to)<-colnames(to);to<-to[colnames(from),colnames(from),drop=FALSE]
    if (!identical(unname(is.na(from)),unname(is.na(to))) || !isTRUE(all.equal(unname(from),unname(to),tolerance=1e-10)))
      stop('Correlazioni diverse nella copia manuale: ',id)
    pair_file <- file.path(root,'data/3.meta_data/pair_n',paste0(id,'.csv'))
    if (file.exists(pair_file)) {
      pairs <- function(p) {
        x<-wf_read(p)
        data.frame(pair=vapply(seq_len(nrow(x)),function(i)paste(sort(c(x$var1[i],x$var2[i])),collapse='|'),character(1)),n=as.numeric(x$n))
      }
      wf_manual_backup_compare(pairs(pair_file),pairs(file.path(path,'data/3.meta_data/pair_n',paste0(id,'.csv'))),'pair',paste('pair N',id))
    }
  }
  # Update the local provenance snapshot only after successful reconstruction.
  for (name in c('records.csv','sources.csv','evidence.csv'))
    copy(file.path(root,'data/3.meta_data/coding',name),file.path(path,'provenienza',name))
  save_input_state()
  writeLines(c('# Ultimo aggiornamento automatico della copia manuale',
    paste('Data:',as.character(Sys.Date())),
    paste('PASS:',nrow(result$metadata),'matrici e',nrow(result$review),'articoli riprodotti.'),
    'Codifiche mensili, codebook, originali e scoring locali integrati; storico_originale preservato.',
    'I contenuti di questa copia restano esclusi da Git.'),status_path)
  message('Backup manuale locale aggiornato e verificato: ',path)
  invisible(path)
}
