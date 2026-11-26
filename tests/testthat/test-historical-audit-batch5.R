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
 expect_equal(sum(a$summary_0053$compared_pairs),7281)
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
 # The five published SECA facets were omitted, not absent from the source.
 source_e<-as.data.frame(readxl::read_excel('data/3.meta_data/open_data/d0053e.xlsx'))
 map<-c(SECA_SA_StrengthsWeaknesses='secaSelfconcept',SECA_SA_EmotionKnowledge='secaEmotionknowledge',
  SECA_SM_EmotionRegulation='secaEmotionregulation',SECA_SM_GoalManagement='secaGoalmanagement',SECA_SM_SchoolWork='secaSchoolwork')
 expect_equal(ncol(a$college_cor),87)
 expect_equal(nrow(a$college_data),322)
 for(k in names(map)) {
  expect_equal(a$college_data[[map[[k]]]],source_e[[k]])
  expect_equal(sum(!is.na(a$college_data[[map[[k]]]])),249)
 }
 expect_equal(as.numeric(diag(a$college_pair_n)[unname(map)]),rep(249,5))
 expect_equal(a$audit_0053$e$summary$compared_pairs,3741)
 expect_true('age'%in%names(a$college_data))
 expect_false('age'%in%colnames(a$college_cor))
 pn<-readr::read_csv('data/3.meta_data/pair_n/0053e.csv',show_col_types=FALSE)
 new<-pn$var1%in%unname(map)|pn$var2%in%unname(map)
 expect_equal(sum(new),420)
 expect_equal(range(pn$n[new]),c(247,249))
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
  unchanged<-setdiff(intersect(names(b$data_matrix),names(b$audit_0068$source$data_matrix)),'academicachievement')
  expect_equal(b$audit_0068$approved$candidate[unchanged,unchanged],
   b$audit_0068$source$candidate[unchanged,unchanged])
 expect_equal(ncol(b$data_matrix),13)
 for(i in seq_along(b$career_facets))expect_equal(b$data_matrix[[names(b$career_facets)[i]]],
  rowMeans(b$raw[,paste0('career_',seq((i-1)*6+1,i*6))]))
 expect_equal(b$audit_0068$approved$summary$compared_pairs,78)
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
 expect_equal(ncol(c$with_subscales),12)
 expect_equal(c$audit_0070$subscales$summary$published_pairs_checked,30)
 expect_equal(c$audit_0070$subscales$summary$published_pairs_outside_rounding,0)
 expect_equal(c$audit_0070$subscales$summary$changed_pairs,0)
 expect_true(all(c$audit_0070$subscales$pair_n==702))
 expect_equal(c$audit_0070$subscales$candidate[names(c$with_peb),names(c$with_peb)],c$audit_0070$peb$candidate)
})
