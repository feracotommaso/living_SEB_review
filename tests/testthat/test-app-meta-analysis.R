withr::local_dir(here::here())
source('app/R/utils.R')
source('app/R/meta_analysis.R')

meta_fixture <- function() {
  data <- readxl::read_excel('data/3.meta_data/meta_data/metadata.xlsx')
  skills <- sort(c('selfmanagement','cooperation','socialengagement','innovation','emotionalresilience'))
  fields <- c('download_date','doi','year','age_class','paper_id','matrix_id','author_et_al','n','title')
  effects <- getMETAdata(skills, 'academicachievement', data, fields)
  list(data=data, skills=skills, effects=meta_prepare_effects(effects))
}

test_that('current estimates and reference Ns reproduce the previous model', {
  fixture <- meta_fixture()
  run <- meta_fit_models(fixture$effects, fixture$skills)
  for (skill in fixture$skills) {
    di <- fixture$effects[fixture$effects$pred==skill & fixture$effects$analysis_status=='Ready', ]
    old <- metafor::rma.mv(yi=yi,V=vi,random=~1|paper_id/matrix_id,test='t',data=di)
    row <- run$metaRes[run$metaRes$skill==skill, ]
    expect_equal(as.numeric(coef(run$outList[[skill]])), as.numeric(coef(old)), tolerance=1e-12)
    expect_equal(row$se, as.numeric(old$se), tolerance=1e-12)
    expect_equal(row$r, as.numeric(predict(old,transf=metafor::transf.ztor)$pred), tolerance=1e-12)
    expect_equal(row$N, sum(di$n))
  }
})

test_that('sparse skills, bad values and one failed model do not stop other domains', {
  fixture <- meta_fixture()
  data <- fixture$effects
  raw <- data
  # Preserve bad source values and flag them instead of substituting an N or r.
  raw$value[1:3] <- c(1, Inf, .2)
  raw$n[3] <- 3
  checked <- meta_prepare_effects(raw)
  expect_identical(checked$value, raw$value)
  expect_identical(checked$n, raw$n)
  expect_true(all(checked$analysis_status[1:3]!='Ready'))
  expect_true(all(is.na(checked$yi[1:3])))
  data$analysis_status[data$pred=='innovation'] <- 'Missing correlation'
  first <- which(data$pred=='innovation' & !is.na(data$value))[1]
  data$analysis_status[first] <- 'Ready'
  data$paper_id[data$pred=='cooperation'] <- NA_character_
  run <- meta_fit_models(data,fixture$skills)
  expect_equal(run$metaRes$status[run$metaRes$skill=='innovation'],'Not estimable')
  expect_match(run$metaRes$note[run$metaRes$skill=='cooperation'],'Model failed')
  expect_true(is.finite(run$metaRes$r[run$metaRes$skill=='selfmanagement']))
  expect_setequal(unique(run$used_data$pred), c('selfmanagement','socialengagement','emotionalresilience'))
  empty <- meta_fit_models(data[FALSE, ],fixture$skills)
  expect_true(all(empty$metaRes$k==0))
  expect_equal(nrow(empty$used_data),0L)
  expect_null(meta_result_plot(empty$metaRes,setNames(fixture$skills,fixture$skills),'Outcome'))
})

test_that('a saved run and its report cannot mix filters with earlier estimates', {
  suppressPackageStartupMessages({library(shiny);library(shinyjs);library(tidyverse)})
  fixture <- meta_fixture()
  app <- new.env()
  app$combined_data <- fixture$data
  app$pred_vars <- fixture$skills
  app$admcol <- read.csv('data/matrix_codebook.csv',colClasses='character')
  app$varlist <- app$admcol
  app$labels_app <- setNames(app$admcol$label, app$admcol$column_name)
  app$review <- read.csv('data/3.meta_data/review_data/review_data.csv',colClasses='character')
  app$topics_list <- read.csv('data/topics_codebook.csv',colClasses='character')
  sys.source('app/R/utils.R',app)
  sys.source('app/R/review_coding.R',app)
  sys.source('app/R/review_browser.R',app)
  sys.source('app/server.R',app)
  testServer(app$server, {
    expect_false(meta_ready())
    expect_error(output$metaResults, class='shiny.silent.error')
    expect_error(output$metaPlot, class='shiny.silent.error')
    session$setInputs(metaOutcomes='academicachievement',outcomeTopics='Education',meta_analysis=1)
    saved <- meta_current()
    expect_equal(saved$selection$outcome,'academicachievement')
    expect_equal(nrow(saved$metaRes),5L)
    session$setInputs(year_filter='2024')
    expect_false(meta_ready())
    expect_error(output$metaResults, class='shiny.silent.error')
    expect_error(meta_current(),'Selection changed')
    expect_error(output$report,'Selection changed')
    expect_identical(metaAnalysis()$data,saved$data)
    session$setInputs(meta_analysis=2)
    expect_identical(meta_current()$selection$years,'2024')
    expect_true(all(meta_current()$data$year==2024))
    session$setInputs(age_filter='no matching age')
    expect_equal(nrow(dm()),0L)
    expect_true(all(meta_coverage(dm(),app$pred_vars)$k==0))
    expect_error(meta_current(),'Selection changed')
  })
})
