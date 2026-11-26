# Legge gli Excel mensili compilati a mano dopo lo screening full text.
# basic_info resta un OUTPUT: non modificarlo direttamente.
source('R/funzioni.R')
fields <- readr::read_csv('data/legacy_31_fields.csv', show_col_types = FALSE)$field
extra <- c('main_objective','sample_context','seb_measurement','study_contribution','constructs_analyzed')
for (name in c('matrix_codebook','topics_codebook')) {
  book <- leggi_excel(file.path('data',paste0(name,'.xlsx')))
  groups <- if(name=='matrix_codebook')c('Topic','broad','label')else c('Broad_topic','Topic_labs')
  # I gruppi seguono l'ordine gia presente; una nuova variabile raggiunge il suo gruppo.
  group_order <- lapply(groups,function(f)match(book[[f]],unique(book[[f]])))
  book <- book[do.call(order,group_order),,drop=FALSE]
  sheet <- if(name=='matrix_codebook')'codebook'else 'topics'
  salva_tabella(book,file.path('data',name),sheet)
}
files <- list.files('data/2.full_abstracting', pattern = '_full_decisions[.]xlsx$', full.names = TRUE)
for (file in files) {
  full <- leggi_excel(file)
  if (!all(c(fields, extra) %in% names(full))) stop('Colonne mancanti: ', file)
  # L'Excel e la sua copia CSV contengono anche gli esclusi.
  readr::write_csv(full, sub('[.]xlsx$', '.csv', file), na = '')
  keep <- !is.na(full$paper_id) & nzchar(full$paper_id) &
    !is.na(full$final_decision) & full$final_decision %in% c('review','meta','both')
  basic_info <- full[keep, c(fields, extra, intersect('batch_id',names(full))), drop = FALSE]
  name <- sub('_full_decisions[.]xlsx$', '_basic_info', basename(file))
  salva_tabella(basic_info, file.path('data/3.meta_data/basic_info', name), 'basic_info')
}
message('basic_info mensili aggiornati dai full_decisions.')
