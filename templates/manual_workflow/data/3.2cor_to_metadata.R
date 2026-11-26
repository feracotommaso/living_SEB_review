# Unione semplice: basic_info mensili + matrici -> metadata; una riga per paper -> review.
source('R/funzioni.R')
fields <- readr::read_csv('data/legacy_31_fields.csv', show_col_types = FALSE)$field
extra <- c('main_objective','sample_context','seb_measurement','study_contribution','constructs_analyzed')
files <- list.files('data/3.meta_data/basic_info', pattern = '_basic_info[.]csv$', full.names = TRUE)
basic <- dplyr::bind_rows(lapply(files, function(p)
  readr::read_csv(p, col_types = readr::cols(.default = 'c'))))
if (anyDuplicated(basic$matrix_id)) stop('matrix_id ripetuto nei full_decisions: correggere alla fonte.')
for (field in c('n','age','gender','year')) basic[[field]] <- as.numeric(basic[[field]])
basic$download_date <- as.POSIXct(basic$download_date, format = '%Y-%m-%d', tz = 'UTC')

# Si uniscono solo matrici con una riga quantitativa nei basic_info.
# Le righe generali del paper e i campioni non indipendenti senza matrice restano descrittivi.
matrix_files <- list.files('data/3.meta_data/matrices', pattern = '[.]csv$', full.names = TRUE)
metadata_rows <- list()
for (file in matrix_files) {
  id <- tools::file_path_sans_ext(basename(file))
  row <- basic[basic$matrix_id == id & basic$final_decision %in% c('both','meta'), fields, drop = FALSE]
  if (!nrow(row)) next
  labelled <- readr::read_csv(file, show_col_types = FALSE)
  m <- as.matrix(labelled[,-1]); rownames(m) <- labelled$variable
  # Come nello script storico: prima occorrenza per coppia, con nome alfabetico.
  grid <- expand.grid(var1 = rownames(m), var2 = colnames(m), stringsAsFactors = FALSE)
  grid$correlation <- as.vector(m)
  grid <- grid[grid$var1 != grid$var2,]
  grid$pair <- vapply(seq_len(nrow(grid)), function(i)
    paste(sort(c(grid$var1[i],grid$var2[i])),collapse='_'), character(1))
  grid <- grid[!duplicated(grid$pair),]
  effects <- as.data.frame(as.list(setNames(grid$correlation,grid$pair)),check.names=FALSE)
  metadata_rows[[id]] <- cbind(row,effects)
}
metadata <- dplyr::bind_rows(metadata_rows)
# Conservare anche le celle/colonne non riportate, senza trasformarle in zeri.

# La review storica include anche il paper meta-only: conserva questo comportamento.
batch <- if('batch_id'%in%names(basic))basic$batch_id else rep(NA_character_,nrow(basic))
review_keep <- basic$final_decision %in% c('review','both') |
  (basic$final_decision=='meta' & (is.na(batch)|!nzchar(batch)))
review <- basic[!duplicated(basic$paper_id) & review_keep,c(fields,extra),drop=FALSE]
review$study_design <- vapply(review$paper_id,function(id) {
  values <- unlist(strsplit(na.omit(basic$study_design[basic$paper_id==id]),';',fixed=TRUE))
  values <- unique(trimws(values)); values <- values[nzchar(values)]
  if(length(values))paste(values,collapse='; ')else NA_character_
},character(1))
salva_tabella(metadata,'data/3.meta_data/meta_data/metadata','metadata')
salva_tabella(review,'data/3.meta_data/review_data/review_data','review_data')
message(nrow(metadata),' matrici; ',nrow(review),' articoli nella review.')
