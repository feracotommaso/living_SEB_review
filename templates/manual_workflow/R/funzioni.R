# Funzioni tecniche comuni. Eseguire gli script dalla radice di questo progetto.
# I punteggi, i campioni e le scelte scientifiche restano negli script dei paper.
if (!file.exists('manual_workflow.Rproj') || !file.exists('README_backup.md'))
  stop('Apri manual_workflow.Rproj: questi script devono lavorare nella copia locale, non nel progetto principale.')

leggi_excel <- function(path) {
  x <- as.data.frame(readxl::read_excel(path, .name_repair = 'minimal'))
  for (name in names(x)) {
    if (inherits(x[[name]], 'POSIXt') || inherits(x[[name]], 'Date'))
      x[[name]] <- format(x[[name]], '%Y-%m-%d')
    else x[[name]] <- as.character(x[[name]])
  }
  x
}

salva_tabella <- function(x, path, sheet = 'data') {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, paste0(path, '.csv'), na = '')
  book <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(book, sheet)
  openxlsx::writeData(book, sheet, x, withFilter = nrow(x) > 0)
  openxlsx::freezePane(book, sheet, firstRow = TRUE)
  openxlsx::setColWidths(book, sheet, cols = seq_along(x), widths = 22)
  openxlsx::addStyle(book, sheet, openxlsx::createStyle(wrapText = FALSE),
    rows = seq_len(nrow(x) + 1), cols = seq_along(x), gridExpand = TRUE, stack = TRUE)
  openxlsx::saveWorkbook(book, paste0(path, '.xlsx'), overwrite = TRUE)
  invisible(x)
}

salva_matrice <- function(matrix_id, matrice, pair_n = NULL) {
  stopifnot(is.matrix(matrice), nrow(matrice) == ncol(matrice),
    !is.null(colnames(matrice)), !anyDuplicated(colnames(matrice)),
    !'age' %in% colnames(matrice),
    isTRUE(all.equal(matrice, t(matrice), check.attributes = FALSE)),
    all(abs(matrice) <= 1 + 1e-12, na.rm = TRUE))
  codebook <- readr::read_csv('data/matrix_codebook.csv', show_col_types = FALSE)
  suffixes <- readr::read_csv('data/variable_suffixes.csv', show_col_types = FALSE)$suffix
  allowed <- c(codebook$column_name, unlist(lapply(suffixes, function(s) paste0(codebook$column_name, s))))
  if (any(!colnames(matrice) %in% allowed)) stop('Variabile non registrata: ',
    paste(setdiff(colnames(matrice), allowed), collapse = ', '))
  path <- file.path('data/3.meta_data/matrices', matrix_id)
  salva_tabella(as.data.frame(matrice), path)
  # Nel CSV la prima colonna identifica esplicitamente le righe.
  readr::write_csv(data.frame(variable = rownames(matrice), matrice, check.names = FALSE),
    paste0(path, '.csv'), na = '')
  if (!is.null(pair_n)) {
    dimnames(pair_n) <- dimnames(matrice)
    positions <- which(upper.tri(pair_n), arr.ind = TRUE)
    pairs <- data.frame(var1 = rownames(pair_n)[positions[,1]],
      var2 = colnames(pair_n)[positions[,2]], n = as.numeric(pair_n[positions]))
    readr::write_csv(pairs,
      file.path('data/3.meta_data/pair_n', paste0(matrix_id, '.csv')), na = '')
  }
  message(matrix_id, ': ', ncol(matrice), ' variabili salvate')
  invisible(matrice)
}

salva_dati <- function(matrix_id, data_matrix, individual_data = data_matrix) {
  stopifnot(all(vapply(data_matrix, is.numeric, logical(1))),
    nrow(data_matrix) == nrow(individual_data), !'age' %in% names(data_matrix))
  data_matrix[] <- lapply(data_matrix, function(x) {x[is.nan(x)] <- NA_real_; x})
  individual_data[] <- lapply(individual_data, function(x) {
    if (is.numeric(x)) x[is.nan(x)] <- NA_real_
    x
  })
  matrice <- cor(data_matrix, method = 'pearson', use = 'pairwise.complete.obs')
  pair_n <- crossprod(!is.na(as.matrix(data_matrix)))
  salva_matrice(matrix_id, matrice, pair_n)
  readr::write_csv(individual_data,
    file.path('data/3.meta_data/open_data/individual_data', paste0(matrix_id, '.csv')), na = '')
  invisible(matrice)
}

# Media ponderata con soglia di risposta: usata solo dove lo scoring la richiede.
media_item <- function(x, min_response = 0, weights = rep(1, ncol(x))) {
  x <- as.matrix(x)
  value <- rowSums(sweep(x, 2, weights, '*'), na.rm = TRUE) /
    as.numeric((!is.na(x)) %*% weights)
  value[rowMeans(!is.na(x)) < min_response | is.nan(value)] <- NA_real_
  value
}
