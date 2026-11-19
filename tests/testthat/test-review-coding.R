withr::local_dir(here::here())
source('workflow.R')

test_that('article coding is consistent, registered and distinct from matrix variants', {
  records <- wf_read(wf_records_path)
  vars <- codebook_csv('data/matrix_codebook.csv')
  expect_silent(review_validate_article(records, vars))
  x <- records
  x$constructs_analyzed[x$paper_id == '0013'] <- 'academicachievement; academicachievement'
  expect_error(review_validate_article(x, vars), 'Duplicate')
  x$constructs_analyzed[x$paper_id == '0013'] <- 'selfmanagementother'
  expect_error(review_validate_article(x, vars), 'Unregistered')
  x <- records
  x$sample_context[x$paper_id == '0013'] <- 'Secondary school; Not applicable'
  expect_error(review_validate_article(x, vars), 'mixed missing')
  x <- records
  x$sample_context[which(x$paper_id == '0053')[1]] <- 'Other'
  expect_error(review_validate_article(x, vars), 'differs across samples')
  expect_silent(review_validate_design('exp; long'))
  expect_error(review_validate_design('long; long'), 'duplicate')
  expect_error(review_validate_design('long; typo'), 'Invalid')
})

test_that('review design combines components without changing the analytical contract', {
  x <- wf_current()
  expect_equal(names(x$basic), coding_dictionary()$field)
  expect_equal(ncol(x$basic), 31L)
  expect_equal(ncol(x$review), 36L)
  expect_equal(x$review$study_design[x$review$paper_id == '0053'], 'cross; long')
  expect_equal(x$basic$study_design[x$basic$matrix_id == '0053f'], 'long')
  views <- wf_coding_views(wf_read(wf_records_path))
  expect_false(anyDuplicated(views$costrutti[c('paper_id','construct_code')]) > 0)
  expect_true('academicachievement' %in% views$costrutti$construct_code[views$costrutti$paper_id == '0013'])
  expect_false('academicachievement' %in% views$costrutti$construct_code[views$costrutti$paper_id == '0134'])
})

test_that('new article fields import manually and the construct view is read-only', {
  root <- tempfile('review_coding_'); dir.create(root)
  files <- c(list.files(wf_coding_dir,pattern='^(records|sources|evidence)\\.csv$',full.names=TRUE),
    'config/coding/legacy_31_fields.csv','data/topics_codebook.csv','data/matrix_codebook.csv')
  for(path in files) {
    target <- file.path(root,path);dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
    stopifnot(file.copy(path,target))
  }
  sources <- wf_read(file.path(root,wf_coding_dir,'sources.csv'))
  readr::write_csv(sources[FALSE,],file.path(root,wf_coding_dir,'sources.csv'))
  withr::local_dir(root)
  wf_export_coding(force = TRUE)
  papers <- wf_read_book(wf_coding_path, 'articoli')
  row <- match('0013', papers$paper_id) + 1L
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  openxlsx::writeData(wb, 'articoli', 'Synthetic objective edit', startRow = row,
    startCol = match('main_objective', names(papers)), colNames = FALSE)
  openxlsx::writeData(wb, 'articoli', 'Synthetic edit rationale', startRow = row,
    startCol = match('change_note', names(papers)), colNames = FALSE)
  openxlsx::saveWorkbook(wb, wf_coding_path, overwrite = TRUE)
  imported <- wf_import_coding()
  expect_equal(imported$records$main_objective[imported$records$paper_id == '0013'], 'Synthetic objective edit')
  # A later label revision must refresh the view without blocking coding import.
  catalog <- wf_read('data/matrix_codebook.csv')
  catalog$label[1] <- paste(catalog$label[1], 'synthetic label update')
  readr::write_csv(catalog, 'data/matrix_codebook.csv', na = '')
  expect_equal(nrow(wf_import_coding(save = FALSE)$changes), 0L)
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  openxlsx::writeData(wb, 'articoli', 'cross', startRow = match('0053',papers$paper_id)+1L,
    startCol = match('study_design',names(papers)), colNames = FALSE)
  openxlsx::saveWorkbook(wb,wf_coding_path,overwrite=TRUE)
  expect_error(wf_import_coding(), 'disegno delle componenti')
  wf_export_coding(force=TRUE)
  wb <- openxlsx::loadWorkbook(wf_coding_path)
  openxlsx::writeData(wb, 'costrutti', 'typo', startRow = 2, startCol = 3, colNames = FALSE)
  openxlsx::saveWorkbook(wb, wf_coding_path, overwrite = TRUE)
  expect_error(wf_import_coding(), 'vista di controllo')
})

test_that('retired topics can be retained but not assigned to a new article', {
  records <- wf_read(wf_records_path)
  expect_silent(wf_validate_records(records, records))
  x <- records
  x$review_topics[x$paper_id == '0013'] <- 'positiveschooloutcomes'
  expect_error(wf_validate_records(x, records), 'Topic storico')
})

test_that('construct discovery includes review-only articles and combines independent filters', {
  review <- wf_current()$review
  topics <- wf_read('data/topics_codebook.csv')
  select <- function(...) makeReviewTable(data = review, topics_df = topics, ...)$filteredData
  expect_equal(nrow(select()), nrow(review))
  found <- select(constructs = 'academicachievement')
  expect_true('0013' %in% found$paper_id)
  expect_false('0134' %in% found$paper_id)
  expect_true('0129' %in% found$paper_id)
  found <- select(constructs = 'academicachievement', context = 'Secondary school')
  expect_true('0013' %in% found$paper_id)
  expect_false('0129' %in% found$paper_id)
  expect_true('0053' %in% select(design = 'long')$paper_id)
  expect_true('0138' %in% select(design = 'exp', measurement = 'Self-report — ability')$paper_id)
  expect_equal(nrow(select(constructs = 'unregistered_test_code')), 0L)
})

test_that('unassessed context remains distinct from explicit missing-value categories', {
  # Use a fixture: the current corpus can legitimately be fully coded.
  review <- data.frame(
    paper_id = c('missing', 'empty', 'pending', 'inapplicable', 'unreported', 'coded'),
    author_et_al = 'Fixture', year = '2026', title = 'Filter fixture',
    doi = '10.0000/fixture', review_topics = 'fixture',
    sample_context = c(NA_character_, '', 'Not yet assessed', 'Not applicable',
      'Not reported', 'Secondary school'))
  topics <- data.frame(Topic = 'fixture', Broad_topic = 'fixture')
  select <- function(value) makeReviewTable(data = review, topics_df = topics,
    context = value)$filteredData$paper_id
  expect_setequal(select('Not yet assessed'), c('missing', 'empty', 'pending'))
  expect_equal(select('Not applicable'), 'inapplicable')
  expect_equal(select('Not reported'), 'unreported')
})
