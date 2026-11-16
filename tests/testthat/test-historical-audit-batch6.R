withr::local_dir(here::here())

test_that('0071/0080 item scores and 0087 transcription are checked without promotion', {
 originals <- c('data/3.meta_data/open_data/d0071.xlsx',
  'data/3.meta_data/open_data/opencode0071.R','data/3.meta_data/open_data/d0080.xlsx',
  paste0('data/2.full_abstracting/pdfs/',c('0071','0080','0087'),'.pdf'))
 skip_if_not(all(file.exists(originals)),'Private historical originals are local and Git-ignored.')
 protected <- c(originals,'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
  'data/3.meta_data/extraction/historical_audit.csv',
  list.files('data/3.meta_data/matrices',full.names=TRUE),
  list.files('data/3.meta_data/coding',full.names=TRUE),
  list.files('data/3.meta_data/meta_data',full.names=TRUE),
  list.files('data/3.meta_data/review_data',full.names=TRUE),
  list.files('data/3.meta_data/pair_n',full.names=TRUE))
 hashes <- function()vapply(protected,function(p)digest::digest(file=p,algo='sha256'),character(1))
 before <- hashes(); runs <- list()
 for(id in c('0071','0080','0087')) {
  e <- new.env(parent=globalenv());e$commandArgs <- function(...)'--check-only'
  invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),e)))
  runs[[id]] <- e
 }
 expect_identical(hashes(),before)
 a <- runs[['0071']]
 expect_equal(a$audit_0071$summary$compared_pairs,105)
 expect_equal(a$audit_0071$summary$changed_pairs,0)
 expect_equal(range(a$pair_n[upper.tri(a$pair_n)]),c(4564,5075))
 expect_equal(nrow(a$data_matrix),5075)
 expect_false('age' %in% names(a$data_matrix))
 expect_equal(a$data_scored$age,a$raw$Age)
 expect_true(all(!is.na(a$data_scored$satisfactionwithlife[a$raw$carSl=='TRUE'])))
 expect_true(all(is.na(a$data_scored$peeracceptance[a$raw$carSl=='TRUE'])))
 b <- runs[['0080']]
 expect_equal(b$audit_0080$summary$compared_pairs,45)
 expect_equal(b$audit_0080$summary$changed_pairs,0)
 expect_true(all(b$pair_n==319))
 expect_equal(b$audit_0080$summary$published_pairs_checked,55)
 expect_equal(b$audit_0080$summary$published_pairs_outside_rounding,0)
 expect_equal(b$data_scored$srlstrategies,b$raw$SRL_TOT)
 expect_equal(b$data_scored$academicachievement,b$raw$Standard_Score)
 expect_equal(sum(b$data_scored$academicachievement==0),2)
 expect_false('age' %in% names(b$data_matrix))
 c <- runs[['0087']]
 expect_equal(c$summary_0087$compared_pairs,63)
 expect_equal(c$summary_0087$changed_pairs,0)
 expect_equal(sum(is.na(c$reference[upper.tri(c$reference)])),15)
 expect_true(is.na(c$summary_0087$input_rows))
 expect_true(is.na(c$summary_0087$pair_n_min))
 expect_true(is.na(c$summary_0087$pair_n_max))
 expect_match(c$note,'not ordinary observed-score Pearson')
 expect_match(c$note,'retain quantitative inclusion')
 expect_equal(c$reference,c$saved)
})
