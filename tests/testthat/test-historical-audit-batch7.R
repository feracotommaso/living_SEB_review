withr::local_dir(here::here())

test_that('0095/0100/0107 reproduce the reviewed scoring and preserve declined alternatives', {
 originals <- c('d0095.xlsx','d0095.csv','opencode0095.Rmd',
  'd0100.xlsx','opencode0100.R','d0107a.csv','d0107b.csv','opencode0107.R')
 originals <- file.path('data/3.meta_data/open_data',originals)
 skip_if_not(all(file.exists(originals)),'Private historical originals remain Git-ignored.')
 protected <- c(originals,'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
  'data/3.meta_data/extraction/historical_audit.csv',
  list.files('data/3.meta_data/matrices',full.names=TRUE),
  list.files('data/3.meta_data/coding',full.names=TRUE),
  list.files('data/3.meta_data/meta_data',full.names=TRUE),
  list.files('data/3.meta_data/review_data',full.names=TRUE),
  list.files('data/3.meta_data/pair_n',full.names=TRUE))
 hashes <- function()vapply(protected,function(p)digest::digest(file=p,algo='sha256'),character(1))
 before <- hashes();runs <- list()
 for(id in c('0095','0100','0107')) {
  e <- new.env(parent=globalenv());e$commandArgs <- function(...)'--check-only'
  invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),e)))
  runs[[id]] <- e
 }
 expect_identical(hashes(),before)
 a <- runs[['0095']]
 expect_equal(a$audit_0095$historical$summary$changed_pairs,0)
 expect_equal(a$audit_0095$historical$summary$compared_pairs,21)
 expect_equal(a$audit_0095$author$summary$changed_pairs,20)
 expect_equal(a$audit_0095$author$summary$pair_n_min,403)
 expect_equal(a$audit_0095$author$summary$pair_n_max,443)
 expect_equal(a$audit_0095$author$summary$published_pairs_outside_rounding,0)
 expect_equal(nrow(a$author_scores),455)
 b <- runs[['0100']]
 expect_equal(b$audit_0100$historical$summary$changed_pairs,12)
 expect_equal(b$audit_0100$historical$summary$compared_pairs,78)
 expect_equal(b$audit_0100$degree$summary$changed_pairs,0)
 expect_equal(b$audit_0100$recovery$summary$changed_pairs,0)
 expect_equal(b$audit_0100$degree$summary$published_pairs_checked,91)
 expect_equal(b$audit_0100$degree$summary$published_pairs_outside_rounding,0)
 expect_equal(b$with_intelligence$intelligencefluid,b$raw$Cattell_TOT)
 expect_equal(sum(!is.na(b$author_scores$academicachievement)),331)
 expect_equal(nrow(b$author_scores),752)
 for(degree in unique(b$raw$Degree)) {
  value <- b$author_scores$academicachievement[b$raw$Degree==degree]
  expect_equal(mean(value,na.rm=TRUE),0,tolerance=1e-12)
  expect_equal(sd(value,na.rm=TRUE),1,tolerance=1e-12)
 }
 c <- runs[['0107']]
 expect_equal(c$audit_0107$historical$summary$changed_pairs,190)
 expect_equal(c$audit_0107$historical$summary$compared_pairs,190)
 expect_equal(nrow(c$legacy_join),261)
 expect_equal(nrow(c$data_analytic),259)
 expect_equal(length(unique(c$link)),259)
 expect_equal(c$data_analytic$questionnaire.1.player.item_29,
  c$raw_b$questionnaire.1.player.item_29[c$link])
 expect_equal(c$audit_0107$scoring$summary$changed_pairs,0)
 expect_equal(sum(is.na(c$matrices$gender$sex)),4)
 expect_equal(c$audit_0107$scoring$summary$pair_n_min,255)
 expect_equal(c$audit_0107$scoring$summary$pair_n_max,259)
 expect_true(all(c$matrices$scoring$ethicalcompetence>=1 & c$matrices$scoring$ethicalcompetence<=5))
 expect_equal(c$matrices$recovery$conditionalcooperationtask,c$data_analytic$CClab)
 expect_equal(sum(c$matrices$recovery$conditionalcooperationtask),202)
 unchanged <- setdiff(names(c$matrices$scoring),c('cooperation','ethicalcompetence'))
 expect_equal(c$matrices$scoring[,unchanged],c$matrices$gender[,unchanged])
 expect_identical(c$audit_0107$recovery$summary$status,'NOT_ADOPTED_BY_USER')
 expect_identical(a$audit_0095$author$summary$status,'NOT_ADOPTED_BY_USER')
 expect_match(c$note,'Q35 wording confirms teamwork')
 saved <- readxl::read_excel('data/3.meta_data/matrices/0107a.xlsx')
 expect_false(any(c('conditionalcooperation','conditionalcooperationtask') %in% names(saved)))
 coding <- readr::read_csv('data/3.meta_data/coding/records.csv',col_types=readr::cols(.default='c'))
 expect_equal(coding$n[match(c('0095a','0100a','0107a'),coding$matrix_id)],c('455','752','259'))
})
