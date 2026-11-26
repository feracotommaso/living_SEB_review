withr::local_dir(here::here())

test_that('last historical batch preserves source meaning, dependence and read-only checking', {
 originals <- c('data/3.meta_data/open_data/d0134.xlsx','data/3.meta_data/open_data/d0068.xlsx',
  'data/3.meta_data/open_data/opencode0134.Rmd','data/3.meta_data/open_data/d0141a_Matrices.xlsx',
  'data/3.meta_data/open_data/raw/0128/NoRegOSF.Rmd',
  'data/3.meta_data/open_data/raw/0128/noreg_osf_vars.csv',
  'data/3.meta_data/open_data/raw/0138/supplement_feasibility.pdf',
  'data/3.meta_data/open_data/raw/0138/02_analyses.html',
  paste0('data/2.full_abstracting/pdfs/',c('0128','0134','0138','0141'),'.pdf'))
 skip_if_not(all(file.exists(originals)),'Originals and downloaded primary references remain private.')
 protected <- c(originals,'data/3.1opendata_to_cor.R','paper/livingSEBresults.qmd',
  'data/3.meta_data/extraction/historical_audit.csv',
  list.files('data/3.meta_data/matrices',full.names=TRUE),
  list.files('data/3.meta_data/coding',full.names=TRUE),
  list.files('data/3.meta_data/meta_data',full.names=TRUE),
  list.files('data/3.meta_data/review_data',full.names=TRUE),
  list.files('data/3.meta_data/pair_n',full.names=TRUE))
 hashes <- function()vapply(protected,function(p)digest::digest(file=p,algo='sha256'),character(1))
 before <- hashes();runs <- list()
 for(id in c('0128','0134','0138','0141')) {
  e <- new.env(parent=globalenv());e$commandArgs <- function(...)'--check-only'
  invisible(capture.output(sys.source(paste0('scripts/extraction/articles/',id,'.R'),e)))
  runs[[id]] <- e
 }
 expect_identical(hashes(),before)
 a <- runs[['0128']]
 expect_equal(a$summary_0128$compared_pairs,105)
 expect_equal(a$summary_0128$changed_pairs,0)
 expect_equal(a$reference['positiveaffect','negativeaffect'],.03)
 expect_equal(a$reference['sex','emotionalresilience'],.19)
 expect_equal(a$historical_reference['sex','emotionalresilience'],-.19)
 expect_true(is.na(a$summary_0128$pair_n_min))
 expect_identical(a$summary_0128$status,'SEX_CONTRAST_RETAINED_BY_USER_WITH_NOTE')
 b <- runs[['0134']]
 expect_equal(nrow(b$data_matrix),1330)
 expect_equal(b$summary_0134$compared_pairs,45)
 expect_equal(b$summary_0134$changed_pairs,0)
 expect_equal(b$summary_0134$pair_n_min,1330)
 expect_equal(b$summary_0134$pair_n_max,1330)
 expect_equal(sum(b$data_matrix$sex),755)
 expect_false('age' %in% names(b$data_matrix))
 expect_equal(length(b$unique_common),273)
 expect_equal(sum(b$at_t1[b$new_index]),238)
 expect_true(all(b$previous$Gender[b$old_index]==b$raw$Gender[b$new_index]))
 c <- runs[['0138']]
 expect_equal(c$reference['selfefficacy','growthmindset'],.16)
 expect_equal(c$summary_0138$changed_pairs,c(1,0))
 expect_equal(c$summary_0138$max_abs_delta,c(.06,0))
 expect_equal(c$summary_0138$reference_n,c(446,446))
 expect_true(all(is.na(c$summary_0138$pair_n_min)))
 expect_false(file.exists('data/3.meta_data/open_data/individual_data/0138a.csv'))
 expect_false(file.exists('data/3.meta_data/pair_n/0138a.csv'))
 d <- runs[['0141']]
 expect_equal(d$summary_0141$compared_pairs,45)
 expect_equal(d$summary_0141$changed_pairs,0)
 expect_equal(d$summary_0141$pair_n_min,525)
 expect_true(all(d$source_pair_n==525))
 expect_match(d$note,'CONFIRMED BY USER')
 expect_match(d$note,'475 female/176 male')
 expect_match(d$note,'not independently validated for the reduced sample')
 expect_false('bessiglobal' %in% colnames(d$reference))
 metadata <- readr::read_csv('data/3.meta_data/meta_data/metadata.csv',show_col_types=FALSE)
 expect_equal(metadata$growthmindset_selfefficacy[metadata$matrix_id=='0138a'],.16)
 private <- readr::read_csv('data/3.meta_data/open_data/individual_data/0134a.csv',show_col_types=FALSE)
 expect_equal(nrow(private),1330)
 expect_true('age' %in% names(private))
 expect_false(any(grepl('_2$|_3$',names(private))))
})
