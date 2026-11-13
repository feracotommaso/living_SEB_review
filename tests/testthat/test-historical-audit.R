withr::local_dir(here::here())
source('R/historical_audit.R')

test_that('audit aligns variable names, detects differences and preserves saved data', {
  root <- tempfile(); dir.create(root); withr::local_dir(root)
  dir.create('data/3.meta_data/matrices',recursive=TRUE)
  dir.create('data/3.meta_data/coding',recursive=TRUE)
  x <- data.frame(a=c(1,2,3,4),b=c(2,1,4,NA_real_))
  saved <- cor(x,use='pairwise.complete.obs')[c('b','a'),c('b','a')]
  path <- 'data/3.meta_data/matrices/test.xlsx'
  writexl::write_xlsx(as.data.frame(saved),path)
  readr::write_csv(data.frame(matrix_id='test',n=4),'data/3.meta_data/coding/records.csv')
  before <- digest::digest(file=path)
  result <- audit_compare_matrix('test','test',x,4,'test',path,'synthetic')
  expect_equal(result$summary$changed_pairs,0)
  expect_equal(result$summary$pair_n_min,3)
  x$a[1] <- 9
  changed <- audit_compare_matrix('test','test',x,4,'test',path,'synthetic')
  expect_equal(changed$summary$changed_pairs,1)
  expect_identical(digest::digest(file=path),before)
})

test_that('historical article checks do not overwrite scientific files or the audit table', {
  needed <- c('d0013.csv','d0018.csv','d0120a.sav','d0120b.sav','d0120a_informant.sav',
    'raw/shared/bessi_scoring_v1.pdf')
  skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',needed))),
    'Original individual-level files are local and Git-ignored.')
  protected <- c(file.path('data/3.meta_data/matrices',paste0(c('0013a','0018a','0018b','0120a','0120b'),'.xlsx')),
    'data/3.meta_data/coding/records.csv','data/3.meta_data/meta_data/metadata.xlsx',
    'data/3.meta_data/review_data/review_data.xlsx','data/3.1opendata_to_cor.R')
  report <- 'data/3.meta_data/extraction/historical_audit.csv'
  if(file.exists(report)) protected <- c(protected,report)
  hashes <- function() vapply(protected,function(p)digest::digest(file=p),character(1))
  before <- hashes()
  for(id in c('0013','0018','0120')) {
    e <- new.env(parent=globalenv()); e$commandArgs <- function(...) '--check-only'
    invisible(capture.output(sys.source(file.path('scripts/extraction/articles',paste0(id,'.R')),envir=e)))
  }
  expect_identical(hashes(),before)
  expect_equal(e$audit_0120$b_source$summary$changed_pairs,0)
  expect_gt(e$audit_0120$b_invalid_as_missing$summary$max_abs_delta,.1)
})
