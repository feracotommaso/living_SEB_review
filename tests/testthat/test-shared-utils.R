withr::local_dir(here::here())

test_that('local and standalone app entry points load the same functions', {
  local <- new.env(); sys.source('R/utils.R', envir = local)
  # Simulate a deployment containing only app/, outside the repository.
  bundle <- tempfile('app_bundle_'); dir.create(file.path(bundle, 'R'), recursive = TRUE)
  file.copy('app/R/utils.R', file.path(bundle, 'R/utils.R'))
  app <- new.env()
  withr::with_dir(bundle, sys.source('R/utils.R', envir = app))
  expected <- c('load_xlsx_url', 'data_combine', 'sem_results_df', 'augment_lavaan_model',
    'make_osmasem_summary', 'makeReviewTable', 'generateBib', 'correlation_pair_key', 'getSEMdata', 'getMETAdata')
  expect_setequal(ls(local), expected)
  expect_setequal(ls(app), expected)
  for (name in expected) {
    expect_identical(formals(local[[name]]), formals(app[[name]]))
    expect_identical(body(local[[name]]), body(app[[name]]))
  }
})

test_that('matrix and review preparation work even when another select is in scope', {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(readxl))
  helpers <- new.env()
  helpers$select <- function(...) stop('A masked select was called.')
  sys.source('app/R/utils.R', envir = helpers)
  folder <- tempfile(); dir.create(folder)
  matrix <- data.frame(cooperation = c(1, .4), selfmanagement = c(.4, 1))
  file <- file.path(folder, 'test.xlsx'); writexl::write_xlsx(matrix, file)
  combined <- helpers$data_combine(file, data.frame(matrix_id = c('other','test'), n = c(50,100)))
  expect_equal(nrow(combined), 1L)
  expect_equal(combined$cooperation_selfmanagement, .4)
  expect_equal(combined$n, 100)
  review <- data.frame(paper_id = c('0001','0002'), author_et_al = c('A','B'), year = 2026,
    title = c('First','Second'), review_topics = c('skillassessment','nomologicalnet'), doi = c('10.1/a','10.1/b'))
  topics <- data.frame(Topic = c('skillassessment','nomologicalnet'), Broad_topic = c('assessment','validation'))
  selected <- helpers$makeReviewTable(topics = 'assessment', data = review, topics_df = topics)
  expect_identical(selected$filteredData$paper_id, '0001')
  expect_identical(selected$revTab$paper_id, '0001')
})
