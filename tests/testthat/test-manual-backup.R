withr::local_dir(here::here())
source('workflow.R')

manual_backup_fixture <- function() {
  project <- normalizePath('.',winslash='/')
  root <- tempfile('manual_backup_');dir.create(root)
  path <- file.path(root,'archive_local/manual_workflow_test')
  for (p in c('R','data','data/2.full_abstracting','data/3.meta_data/basic_info',
    'data/3.meta_data/matrices','data/3.meta_data/pair_n','data/3.meta_data/open_data/individual_data',
    'data/codici_articoli','provenienza','verifica'))dir.create(file.path(path,p),recursive=TRUE,showWarnings=FALSE)
  for (p in list.files('templates/manual_workflow',recursive=TRUE,full.names=TRUE)) {
    dest<-file.path(path,substring(p,nchar('templates/manual_workflow/')+1))
    dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(p,dest))
  }
  writeLines('Synthetic manual workflow fixture',file.path(path,'README_backup.md'))
  sources <- c('config/coding/legacy_31_fields.csv','config/coding/variable_suffixes.csv',
    'data/matrix_codebook.csv','data/matrix_codebook.xlsx','data/topics_codebook.csv','data/topics_codebook.xlsx')
  for (p in sources) {
    dest<-file.path(root,p);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);file.copy(p,dest)
    file.copy(p,file.path(path,'data',basename(p)))
  }
  records<-wf_read(wf_records_path)[rep(1,2),]
  records$paper_id<-c('9998','9999');records$matrix_id<-c('9998a','9999')
  records$sample_id<-c('a',NA_character_);records$batch_id<-'2026-12'
  records$final_decision<-c('both','review');records$title<-c('Synthetic quantitative article','Synthetic review article')
  records$n<-c('5',NA_character_);records$download_date<-'2026-12-01'
  rows<-wf_current(records)
  raw<-data.frame(selfmanagement=c(2,3,1,5,4),cooperation=c(3,4,1,2,5),age=20:24)
  m<-cor(raw[,1:2]);rownames(m)<-colnames(m)
  p<-file.path(root,'data/3.meta_data/matrices/9998a.xlsx')
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writexl::write_xlsx(as.data.frame(m),p)
  p<-file.path(root,'data/3.meta_data/pair_n/9998a.csv')
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  readr::write_csv(data.frame(var1='selfmanagement',var2='cooperation',n=5),p)
  p<-file.path(root,'data/3.meta_data/open_data/raw/9998/scores.csv')
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);readr::write_csv(raw,p)
  p<-file.path(root,wf_records_path);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);readr::write_csv(records,p,na='')
  readr::write_csv(records[FALSE,],file.path(path,'provenienza/records.csv'),na='')
  writeLines(c('source("R/funzioni.R")',
    'raw <- read.csv("data/3.meta_data/open_data/raw/9998/scores.csv")',
    'data_matrix <- raw[,c("selfmanagement","cooperation")]',
    'salva_dati("9998a",data_matrix,raw)'),file.path(path,'data/codici_articoli/9998.R'))
  metadata<-cbind(rows$basic[1,],cooperation_selfmanagement=m[1,2])
  list(root=root,path=path,records=records,result=list(metadata=metadata,review=rows$review,records=records))
}

test_that('monthly completion refreshes an independent manual project without publishing private data', {
  f<-manual_backup_fixture();withr::local_dir(f$root)
  expect_equal(normalizePath(wf_manual_backup_path(),winslash='/'),normalizePath(f$path,winslash='/'))
  expect_message(wf_manual_backup_update(f$result),'aggiornato e verificato')
  expect_true(file.exists(file.path(f$path,'data/2.full_abstracting/26_12_full_decisions.xlsx')))
  expect_true(file.exists(file.path(f$path,'data/3.meta_data/open_data/individual_data/9998a.csv')))
  expect_true(file.exists(file.path(f$path,'provenienza/manual_sync.csv')))
  expect_false(file.exists('data/codici_articoli/9998.R'))
  expect_message(wf_manual_backup_update(f$result),'aggiornato e verificato')
  # A modern meta-only report must not enter the review by legacy convention.
  records<-f$records;records$final_decision[1]<-'meta'
  result<-f$result;result$records<-records
  result$metadata$final_decision<-'meta'
  result$review<-f$result$review[f$result$review$paper_id!='9998',,drop=FALSE]
  readr::write_csv(records,wf_records_path,na='')
  expect_message(wf_manual_backup_update(result),'aggiornato e verificato')
  review<-wf_read(file.path(f$path,'data/3.meta_data/review_data/review_data.csv'))
  expect_equal(review$paper_id,'9999')
})

test_that('manual changes and unfinished new scientific scoring cannot be overwritten', {
  f<-manual_backup_fixture();withr::local_dir(f$root)
  wf_manual_backup_update(f$result)
  book<-file.path(f$path,'data/2.full_abstracting/26_12_full_decisions.xlsx')
  wb<-openxlsx::loadWorkbook(book)
  openxlsx::writeData(wb,'full_decisions','My autonomous correction',startRow=2,startCol=6,colNames=FALSE)
  openxlsx::saveWorkbook(wb,book,overwrite=TRUE)
  hash<-wf_hash(book)
  expect_error(wf_manual_backup_update(f$result),'Modifiche manuali locali')
  expect_identical(wf_hash(book),hash)
  # Preparing a future article creates a clearly blocked local scaffold only.
  future<-f$result;future$metadata$paper_id<-'9997'
  expect_error(wf_manual_backup_preflight(future,f$path),'Completa e verifica lo scoring locale')
  script<-file.path(f$path,'data/codici_articoli/9997.R')
  expect_true(file.exists(script));expect_true(any(grepl('TODO_SCORING',readLines(script),fixed=TRUE)))
})

test_that('a failed source sync preserves originals and can resume after correction', {
  f<-manual_backup_fixture();withr::local_dir(f$root)
  wf_manual_backup_update(f$result)
  source<-'data/3.meta_data/open_data/raw/9998/scores.csv'
  target<-file.path(f$path,source);hash<-wf_hash(target)
  changed<-wf_read(source);changed$selfmanagement[1]<-'999';readr::write_csv(changed,source)
  result<-f$result
  result$records$title[1]<-result$metadata$title<-result$review$title[1]<-'Accepted automated coding change'
  readr::write_csv(result$records,wf_records_path,na='')
  expect_error(wf_manual_backup_update(result),'Originale locale diverso')
  expect_identical(wf_hash(target),hash)
  expect_true(any(grepl('INCOMPLETE',readLines(file.path(f$path,'verifica/ultimo_sync.md')),fixed=TRUE)))
  file.copy(target,source,overwrite=TRUE)
  expect_message(wf_manual_backup_update(result),'aggiornato e verificato')
})
