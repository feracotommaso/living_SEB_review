withr::local_dir(here::here())

test_that('0108/0118/0126 reproduce the reviewed corrections without writing on check-only', {
 originals <- c('data/3.meta_data/open_data/d0108.sav','data/3.meta_data/open_data/d0118.xlsx',
  'data/3.meta_data/open_data/d0126.xlsx',paste0('data/2.full_abstracting/pdfs/',c('0108','0118','0126'),'.pdf'),
  'data/3.meta_data/open_data/raw/0108/Supplemental tables.xlsx',
  'data/3.meta_data/open_data/raw/0118/Supplementary Materials - Correlation Tables.xlsx')
 skip_if_not(all(file.exists(originals)),'Private originals and newly acquired primary references remain local.')
 protected <- c(originals,'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
  'data/3.meta_data/extraction/historical_audit.csv',
  list.files('data/3.meta_data/matrices',full.names=TRUE),
  list.files('data/3.meta_data/coding',full.names=TRUE),
  list.files('data/3.meta_data/meta_data',full.names=TRUE),
  list.files('data/3.meta_data/review_data',full.names=TRUE),
  list.files('data/3.meta_data/pair_n',full.names=TRUE))
 hashes <- function()vapply(protected,function(p)digest::digest(file=p,algo='sha256'),character(1))
 before <- hashes();runs <- list()
 for(id in c('0108','0118','0126')) {
  e <- new.env(parent=globalenv());e$commandArgs <- function(...)'--check-only'
  invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),e)))
  runs[[id]] <- e
 }
 expect_identical(hashes(),before)
 a <- runs[['0108']]
 expect_equal(a$audit_0108$historical$summary$changed_pairs,22)
 expect_equal(a$audit_0108$binary$summary$changed_pairs,0)
 expect_equal(a$audit_0108$binary$summary$compared_pairs,253)
 expect_equal(nrow(a$data_matrix),470)
 expect_equal(sum(is.na(a$data_matrix$sex)),13)
 expect_equal(a$audit_0108$binary$summary$pair_n_min,401)
 expect_equal(a$audit_0108$binary$summary$pair_n_max,470)
 expect_equal(a$audit_0108$binary$summary$published_pairs_checked,106)
 expect_equal(a$audit_0108$binary$summary$numerically_compatible_pairs,76)
 expect_equal(sum(is.na(a$published[upper.tri(a$published)])),30)
 expect_equal(max(abs(a$same_wave-a$published),na.rm=TRUE),0,tolerance=1e-10)
 expect_false(any(grepl('_t2$',names(a$data_matrix))))
 expect_equal(a$data_matrix$selfmanagement,a$data_analytic$BESSI_Self_Management)
 expect_equal(a$data_matrix$academicengagement,a$wave2$Academic_Engagement[a$link])
 b <- runs[['0118']]
 expect_equal(b$audit_0118$historical$summary$changed_pairs,29)
 expect_equal(b$audit_0118$combined$summary$changed_pairs,0)
 expect_equal(b$audit_0118$combined$summary$compared_pairs,120)
 expect_equal(b$audit_0118$historical$summary$published_pairs_checked,272)
 expect_equal(b$audit_0118$historical$summary$published_pairs_outside_rounding,0)
 expect_equal(b$cpm_mismatches,4)
 expect_equal(sum(b$cpm_sum==b$raw$CPM_sum_TOT),146)
 expect_equal(b$combined$intelligencefluid,rowSums(b$cpm_items))
 expect_equal(b$combined$sex,b$raw$gender_0F1M)
 expect_equal(sum(b$combined$sex),73)
 expect_equal(sum(b$combined$sex[b$group=='IG']),46)
 expect_equal(sum(b$combined$sex[b$group=='CG']),27)
 expect_true('innovation' %in% names(b$combined))
 c <- runs[['0126']]
 expect_equal(nrow(c$raw),3030)
 expect_equal(nrow(c$data_matrix),1917)
 expect_equal(c$audit_0126$part2$summary$changed_pairs,0)
 expect_equal(c$audit_0126$part2$summary$reference_n,1917)
 expect_equal(c$audit_0126$part2$summary$pair_n_min,1773)
 expect_equal(c$audit_0126$part2$summary$pair_n_max,1906)
 expect_identical(c$audit_0126$historical$candidate,c$audit_0126$part2$candidate)
 expect_equal(c$age,2024-c$data_analytic$DEMO2_wp2)
 expect_equal(sum(is.na(c$age)),3)
 private <- readr::read_csv('data/3.meta_data/open_data/individual_data/0126a.csv',show_col_types=FALSE)
 expect_equal(nrow(private),1917)
 expect_equal(sum(is.na(private$age)),3)
 coding <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
 expect_identical(coding$n[match(c('0108a','0118a','0126a'),coding$matrix_id)],c('470','150','1917'))
 expect_identical(coding$gender[match('0118a',coding$matrix_id)],'48.7')
 expect_equal(as.numeric(coding$age[match('0126a',coding$matrix_id)]),52.72)
 expect_equal(as.numeric(coding$gender[match('0126a',coding$matrix_id)]),50.60)
})
