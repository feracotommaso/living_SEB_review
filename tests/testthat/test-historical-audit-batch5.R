withr::local_dir(here::here())

test_that('0053/0068/0070 checks reproduce scoring without writing analytical files', {
 originals<-c(paste0('d0053',letters[3:7],'.xlsx'),'d0068.xlsx','d0070.xlsx','opencode0068.R','opencode0070.R')
 skip_if_not(all(file.exists(file.path('data/3.meta_data/open_data',originals))),
  'Private historical originals are local and Git-ignored.')
 protected<-c(list.files('data/3.meta_data/matrices',full.names=TRUE),
  list.files('data/3.meta_data/coding',full.names=TRUE),
  list.files('data/3.meta_data/meta_data',full.names=TRUE),
  list.files('data/3.meta_data/review_data',full.names=TRUE),
  list.files('data/3.meta_data/pair_n',full.names=TRUE),
  'data/3.meta_data/extraction/historical_audit.csv','data/3.meta_data/extraction/summary.csv',
  'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
  file.path('data/3.meta_data/open_data',originals))
 hashes<-function()vapply(protected,function(p)digest::digest(file=p,algo='sha256'),character(1))
 before<-hashes();runs<-list()
 for(id in c('0053','0068','0070')) {
  e<-new.env(parent=globalenv());e$commandArgs<-function(...)'--check-only'
  invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),e)))
  runs[[id]]<-e
 }
 expect_identical(hashes(),before)
 a<-runs[['0053']]
 expect_equal(sum(a$summary_0053$compared_pairs),6861)
 expect_true(all(a$summary_0053$changed_pairs==0))
 expect_true(all(a$summary_0053$score_mismatch_values==0))
 expect_equal(a$summary_0053$pair_n_min,c(400,600,247,62,488))
 expect_equal(a$summary_0053$pair_n_max,c(400,600,322,499,488))
 expect_equal(a$summary_0053$duplicate_source_case_ids,c(0,0,165,0,0))
 expect_match(a$audit_0053$g$summary$note,'target sex')
  coding<-readr::read_csv('data/3.meta_data/coding/records.csv',
   col_types=readr::cols(.default='c'))
  target<-coding[coding$matrix_id=='0053g',]
  expect_equal(as.numeric(target$age),47.52)
  expect_equal(as.numeric(target$gender),46.72)
  expect_match(target$notes,'Informant mean age 49.26')
 expect_match(a$audit_0053$f$summary$note,'prospective')
 b<-runs[['0068']]
 expect_equal(nrow(b$data_matrix),356)
 expect_equal(sum(is.na(b$data_matrix$sex)),6)
 expect_equal(b$grade_mismatches,2)
 expect_equal(b$publication$published_pairs_checked,55)
 expect_equal(b$publication$published_pairs_outside_rounding,0)
 expect_equal(b$mean_grade,rowMeans(b$raw[,c('ita','mate','engl','scie','hist')]))
 for(year in c(4,5)) {
  values<-b$data_matrix$academicachievement[b$raw$Grade==year]
  expect_equal(mean(values),0,tolerance=1e-12)
  expect_equal(sd(values),1,tolerance=1e-12)
 }
 expect_equal(b$audit_0068$approved$summary$changed_pairs,0)
 expect_equal(b$audit_0068$approved$summary$pair_n_min,350)
 expect_equal(b$audit_0068$approved$summary$pair_n_max,356)
  unchanged<-setdiff(names(b$data_matrix),'academicachievement')
  expect_equal(b$audit_0068$approved$candidate[unchanged,unchanged],
   b$audit_0068$source$candidate[unchanged,unchanged])
 c<-runs[['0070']]
 expect_equal(c$audit_0070$source$summary$changed_pairs,0)
 expect_equal(c$audit_0070$source$summary$published_pairs_checked,13)
 expect_equal(c$audit_0070$source$summary$published_pairs_outside_rounding,0)
 expect_equal(c$with_peb$proenvironmentalbehavior,c$raw$PebSingle)
 expect_equal(ncol(c$with_peb),9)
 expect_equal(c$with_peb$natureconnectedness,
  rowMeans(c$raw[,paste0('CNS_',c(1:11,13))]))
 expect_equal(c$with_peb$sustainabilitycompetence,
  rowMeans(c$raw[,paste0('SPACS_',1:12)]))
 expect_true(all(c$audit_0070$peb$pair_n==702))
 expect_identical(c$audit_0070$peb$summary$status,'APPROVED_EXTRACTION')
 expect_equal(c$audit_0070$peb$summary$published_pairs_checked,15)
 expect_equal(c$audit_0070$peb$summary$published_pairs_outside_rounding,0)
 expect_equal(c$audit_0070$peb$summary$changed_pairs,0)
 expect_equal(c$audit_0070$peb$candidate[names(c$data_matrix),names(c$data_matrix)],
  c$audit_0070$source$candidate)
})
