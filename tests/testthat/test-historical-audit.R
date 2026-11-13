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

test_that('second historical batch checks are read-only and reproduce approved outputs', {
  needed <- c('d0026.csv','opencode0026.R','d0030a.csv','d0030b.csv',
    'opencode0030a.R','opencode0030b.R','d0034.sav','d0034t.sav',
    'raw/shared/bessi_scoring_v1.pdf')
  skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',needed))),
    'Original individual-level files are local and Git-ignored.')
  protected <- c(list.files('data/3.meta_data/matrices',full.names=TRUE),
    list.files('data/3.meta_data/coding',full.names=TRUE),
    'data/3.meta_data/meta_data/metadata.csv','data/3.meta_data/meta_data/metadata.xlsx',
    'data/3.meta_data/review_data/review_data.csv','data/3.meta_data/review_data/review_data.xlsx',
    'data/3.meta_data/extraction/historical_audit.csv','data/3.1opendata_to_cor.R',
    file.path('data/3.meta_data/open_data',needed))
  outputs <- c('data/3.meta_data/extraction/summary.csv',
    'data/3.meta_data/pair_n/0034a.csv','data/3.meta_data/open_data/individual_data/0034a.csv')
  protected <- c(protected,outputs[file.exists(outputs)])
  hashes <- function()vapply(protected,function(p)digest::digest(file=p),character(1))
  before <- hashes(); runs <- list()
  for(id in c('0026','0030','0034')) {
    e<-new.env(parent=globalenv());e$commandArgs<-function(...)'--check-only'
    invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),envir=e)))
    runs[[id]]<-e
  }
  expect_identical(hashes(),before)
  rows <- dplyr::bind_rows(runs[['0026']]$audit_0026$summary,
    runs[['0030']]$summary_0030,runs[['0034']]$audit_0034$combined$summary)
  expect_equal(sum(rows$compared_pairs),4304)
  expect_true(all(rows$changed_pairs==0))
  expect_equal(sum(runs[['0030']]$matched),68)
  expect_equal(runs[['0034']]$audit_0034$source$summary$changed_pairs,461)
  expect_equal(as.numeric(runs[['0034']]$combined_data$socialnetworknegativesocialcomparison),
    as.numeric(runs[['0034']]$raw[['ComparaciónSocial_Redes']]))
  expect_equal(as.numeric(runs[['0034']]$combined_data$socialnetworkaddictiveconsequences),
    as.numeric(runs[['0034']]$raw[['ConsecuenciasAdictivas_Redes']]))
  expect_equal(sum(runs[['0034']]$raw$edad<18),8)
  expect_equal(nrow(runs[['0034']]$combined_data),303)
})

test_that('third historical batch reproduces approved outputs without writing in check-only mode', {
  needed <- c('d0035.csv','opencode0035.R','d0043.sav','opencode0043.sps',
    'd0045.xlsx','raw/shared/bessi_scoring_v1.pdf')
  skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',needed))),
    'Original individual-level files are local and Git-ignored.')
  protected <- c(list.files('data/3.meta_data/matrices',full.names=TRUE),
    list.files('data/3.meta_data/coding',full.names=TRUE),
    'data/3.meta_data/meta_data/metadata.csv','data/3.meta_data/meta_data/metadata.xlsx',
    'data/3.meta_data/review_data/review_data.csv','data/3.meta_data/review_data/review_data.xlsx',
    'data/3.meta_data/extraction/historical_audit.csv','data/3.1opendata_to_cor.R',
    file.path('data/3.meta_data/open_data',needed))
  hashes <- function()vapply(protected,function(p)digest::digest(file=p),character(1))
  before <- hashes();runs <- list()
  for(id in c('0035','0043','0045')) {
    e<-new.env(parent=globalenv());e$commandArgs<-function(...)'--check-only'
    invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),envir=e)))
    runs[[id]]<-e
  }
  expect_identical(hashes(),before)
  rows<-dplyr::bind_rows(runs[['0035']]$audit_0035$source$summary,
    runs[['0043']]$audit_0043$source$summary,runs[['0045']]$audit_0045$source$summary)
  expect_equal(sum(rows$compared_pairs),281)
  expect_equal(rows$changed_pairs,c(0,0,8))
  approved<-lapply(runs,function(x)x$approved)
  expect_true(all(vapply(approved,function(x)x$summary$changed_pairs==0,logical(1))))
  expect_equal(vapply(approved,function(x)ncol(x$candidate),integer(1)),c('0035'=12L,'0043'=21L,'0045'=10L))
  expect_equal(sum(vapply(approved,function(x)x$summary$compared_pairs,numeric(1))),321)
  expect_equal(sum(runs[['0035']]$data_matrix$sex==1,na.rm=TRUE),468)
  expect_equal(sum(is.na(runs[['0043']]$data_matrix$parentEducation)),140)
  expect_identical(runs[['0045']]$data_matrix$volunteering,runs[['0045']]$raw$hours.total)
  expect_equal(runs[['0045']]$data_matrix$capacityforconsistency,
    rowMeans(runs[['0045']]$raw[,paste0('bessi_',seq(9,169,32))],na.rm=TRUE))
})

test_that('fourth batch detects sample duplication and scoring issues without changing science', {
  needed<-c('d0048.sav','d0049.xlsx','opencode0049.R','d0050.Rdata','d0013.csv')
  skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',needed))),
    'Original individual-level files are local and Git-ignored.')
  protected<-c(list.files('data/3.meta_data/matrices',full.names=TRUE),
    list.files('data/3.meta_data/coding',full.names=TRUE),
    list.files('data/3.meta_data/pair_n',full.names=TRUE),
    'data/3.meta_data/meta_data/metadata.csv','data/3.meta_data/meta_data/metadata.xlsx',
    'data/3.meta_data/review_data/review_data.csv','data/3.meta_data/review_data/review_data.xlsx',
    'data/3.meta_data/extraction/historical_audit.csv','data/3.meta_data/extraction/summary.csv',
    'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
    file.path('data/3.meta_data/open_data',needed))
  hashes<-function()vapply(protected,function(p)digest::digest(file=p),character(1))
  before<-hashes();runs<-list()
  for(id in c('0048','0049','0050')) {
    e<-new.env(parent=globalenv());e$commandArgs<-function(...)'--check-only'
    invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),envir=e)))
    runs[[id]]<-e
  }
  expect_identical(hashes(),before)
  rows<-dplyr::bind_rows(runs[['0048']]$audit_0048$summary,
    runs[['0049']]$audit_0049$source$summary,
    runs[['0050']]$audit_0050$a_source$summary,runs[['0050']]$audit_0050$b_source$summary)
  expect_equal(sum(rows$compared_pairs),3629)
  expect_equal(rows$changed_pairs,c(0,0,175,702))
  expect_true(all(vapply(runs[['0050']]$audit_0050[c('a_corrected','b_corrected')],
    function(x)x$summary$changed_pairs==0,logical(1))))
  expect_equal(runs[['0048']]$audit_0048$summary$overlap_linked_records,897)
  expect_equal(runs[['0048']]$audit_0048$summary$overlap_identical_correlations,210)
  expect_equal(runs[['0049']]$audit_0049$volatility$summary$added_variables,'')
  expect_equal(ncol(runs[['0049']]$volatility_candidate),25)
  expect_equal(runs[['0049']]$audit_0049$volatility$summary$published_pairs_outside_rounding,0)
  expect_equal(sum(runs[['0050']]$score_checks$a$changed_values),0)
  expect_setequal(runs[['0050']]$score_checks$b$facet[runs[['0050']]$score_checks$b$changed_values>0],
    c('persuasiveskill','expressiveskill','organizationalskill','detailmanagement',
      'responsibilitymanagement','abstractthinkingskill','creativeskill','selfreflectionskill'))
  expect_equal(runs[['0050']]$audit_0050$a_weights$summary$pair_n_min,482)
  expect_equal(runs[['0050']]$audit_0050$a_corrected$summary$pair_n_min,631)
  expect_equal(runs[['0050']]$overlap_records,238)
  expect_identical(runs[['0050']]$audit_0050$a_corrected$pair_n,runs[['0050']]$audit_0050$a_source$pair_n)
  expect_identical(runs[['0050']]$audit_0050$b_corrected$pair_n,runs[['0050']]$audit_0050$b_source$pair_n)
  expect_equal(runs[['0050']]$audit_0050$b_source$summary$input_rows,768)
  expect_equal(runs[['0050']]$audit_0050$b_source$summary$reference_n,767)
})

test_that('historical article checks do not overwrite scientific files or the audit table', {
  needed <- c('d0013.csv','d0018.csv','d0120a.sav','d0120b.sav','d0120a_informant.sav',
    'raw/shared/bessi_scoring_v1.pdf',
    'raw/0120/osf_7efmp/7efmp-osfstorage-archive.zip',
    'raw/0120/osf_7efmp/files/Study 1 Self Report (N = 1103).sav',
    'raw/0120/osf_7efmp/files/Study 2 (N = 123).sav',
    'raw/0120/osf_7efmp/files/Study 1 Informant report (N= 184).sav')
  skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',needed))),
    'Original individual-level files are local and Git-ignored.')
  protected <- c(file.path('data/3.meta_data/matrices',paste0(c('0013a','0018a','0018b','0120a','0120b'),'.xlsx')),
    'data/3.meta_data/coding/records.csv','data/3.meta_data/meta_data/metadata.xlsx',
    'data/3.meta_data/review_data/review_data.xlsx','data/3.1opendata_to_cor.R')
  approved_outputs <- c(file.path('data/3.meta_data/matrices',paste0(c('0120a','0120b'),'.csv')),
    file.path('data/3.meta_data/pair_n',paste0(c('0120a','0120b'),'.csv')),
    file.path('data/3.meta_data/open_data/individual_data',paste0(c('0120a','0120b'),'.csv')),
    'data/3.meta_data/extraction/summary.csv','data/3.meta_data/coding/coding.xlsx')
  protected <- c(protected,approved_outputs[file.exists(approved_outputs)])
  report <- 'data/3.meta_data/extraction/historical_audit.csv'
  if(file.exists(report)) protected <- c(protected,report)
  hashes <- function() vapply(protected,function(p)digest::digest(file=p),character(1))
  before <- hashes()
  for(id in c('0013','0018','0120')) {
    e <- new.env(parent=globalenv()); e$commandArgs <- function(...) '--check-only'
    invisible(capture.output(sys.source(file.path('scripts/extraction/articles',paste0(id,'.R')),envir=e)))
  }
  expect_identical(hashes(),before)
  expect_equal(e$audit_0120$b_approved$summary$changed_pairs,0)
  expect_gt(e$audit_0120$b_source$summary$max_abs_delta,.1)
  # Independent source reconstruction must not silently become a correction.
  facets <- e$bfi_score_comparison[grepl('^bf_',e$bfi_score_comparison$scale),]
  expect_equal(sum(facets$changed_values),9)
  expect_equal(sum(facets$changed_values>0),6)
  bfi_row <- e$summary_0120[e$summary_0120$comparison=='bfi2_item_reconstruction',]
  expect_equal(bfi_row$table9_pairs_compared,480)
  expect_equal(bfi_row$table9_pairs_outside_rounding,440)
  approved <- e$audit_0120$b_approved$data_matrix
  expect_equal(approved$capacityforoptimism[e$raw_b$BESSI171==32],2.6)
  unchanged <- setdiff(names(approved),c(names(e$bessi192_domain_weights),'capacityforoptimism'))
  expect_identical(approved[,unchanged],e$audit_0120$b_source$data_matrix[,unchanged])
  expect_equal(e$audit_0120$b_approved$summary$reference_n,123)
})
