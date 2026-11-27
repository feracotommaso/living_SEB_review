withr::local_dir(here::here())
suppressPackageStartupMessages(library(metaSEM))
source('app/R/utils.R')
source('app/R/masem_workflow.R')

test_that('selected variable comments update without changing the user model', {
  labels <- c(x='Predictor',y='Outcome',z='Other predictor')
  model <- '# My own note\ny ~ b*x'
  first <- masem_model_variable_notes(model,c('x','y'),labels)
  expect_match(first,'# x = Predictor',fixed=TRUE)
  expect_match(first,model,fixed=TRUE)
  expect_identical(masem_model_variable_notes(first,c('x','y'),labels),first)
  changed <- masem_model_variable_notes(first,c('y','z'),labels)
  expect_false(grepl('# x = Predictor',changed,fixed=TRUE))
  expect_match(changed,'# z = Other predictor',fixed=TRUE)
  expect_match(changed,model,fixed=TRUE)
  expect_equal(lavaan::lavParseModelString(first,as.data.frame=TRUE),
    lavaan::lavParseModelString(model,as.data.frame=TRUE))
})

masem_fixture <- function() {
  raw <- readxl::read_excel('data/3.meta_data/meta_data/metadata.xlsx')
  variables <- sort(c('academicachievement','conscientiousness','selfmanagement'))
  fields <- c('download_date','doi','year','age_class','paper_id','matrix_id','author_et_al','n','title')
  variables_book <- read.csv('data/matrix_codebook.csv',colClasses='character')
  labels <- setNames(variables_book$label,variables_book$column_name)
  data <- masem_prepare(variables,raw,fields)
  data$selection <- list(variables=variables,ages=character(),years=character())
  data$labels <- labels
  list(raw=raw,data=data,labels=labels,variables_book=variables_book,
    syntax='academicachievement ~ selfmanagement + conscientiousness')
}

test_that('model augmentation preserves constraints and detects unsupported syntax', {
  syntax <- augment_lavaan_model('y ~ 0.5*x + b*z\nz ~~ x\nx ~~ 2*x',c('x','y','z'))
  expect_match(syntax,'0.5*x',fixed=TRUE)
  expect_match(syntax,'b*z',fixed=TRUE)
  parsed <- lavaan::lavParseModelString(syntax,as.data.frame=TRUE)
  expect_equal(sum(parsed$op=='~~' & parsed$lhs=='x' & parsed$rhs=='x'),1)
  expect_equal(sum(parsed$op=='~~' & ((parsed$lhs=='x' & parsed$rhs=='z') | (parsed$lhs=='z' & parsed$rhs=='x'))),1)
  expect_error(augment_lavaan_model('y ~ missing',c('x','y')),'not in the current selection')
  expect_error(augment_lavaan_model('y ~ b*x\nb == 1',c('x','y')),'not supported')
  expect_error(augment_lavaan_model('F =~ x + y + z',c('x','y','z')),'turn off automatic')
  expect_match(augment_lavaan_model('F =~ x + y + z',c('x','y','z'),
    correlate_exogenous=FALSE,saturate_endogenous=FALSE),'F =~ x + y + z',fixed=TRUE)
  cyclic <- metaSEM::lavaan2RAM('x ~ y\ny ~ x',obs.variables=c('x','y'))
  expect_error(masem_validate_ram(cyclic),'Feedback')
  isolated <- metaSEM::lavaan2RAM('y ~ x\nz ~~ 1*z',obs.variables=c('x','y','z'))
  expect_invisible(masem_validate_ram(isolated))
  fixed_cycle <- metaSEM::lavaan2RAM('x ~ 0.5*y\ny ~ x',obs.variables=c('x','y'))
  expect_error(masem_validate_ram(fixed_cycle),'Feedback')
})

test_that('reference data, missingness and model estimates remain unchanged', {
  fixture <- masem_fixture()
  before <- getSEMdata(fixture$data$selection$variables,fixture$raw,names(fixture$data$basic_info))
  expect_identical(fixture$data$cor_matrices,before$cor_matrices)
  expect_identical(fixture$data$basic_info$n,before$basic_info$n)
  expect_equal(nrow(fixture$data$pairs),3L)
  expect_identical(fixture$data$K,metaSEM::pattern.na(before$cor_matrices,show.na=FALSE))
  expect_equal(fixture$data$N,metaSEM::pattern.n(before$cor_matrices,before$basic_info$n),ignore_attr=TRUE)
  # Independent reference: the original model, without the augmentation helper.
  syntax <- paste(fixture$syntax,'conscientiousness ~~ selfmanagement',
    'conscientiousness ~~ 1*conscientiousness','selfmanagement ~~ 1*selfmanagement',sep='\n')
  RAM <- metaSEM::lavaan2RAM(syntax,obs.variables=fixture$data$selection$variables)
  T0 <- metaSEM::create.Tau2(RAM=RAM,RE.type='Zero',Transform='expLog',RE.startvalues=.05)
  frame <- metaSEM::Cor2DataFrame(before$cor_matrices,before$basic_info$n,acov='weighted')
  M0 <- metaSEM::create.vechsR(A0=RAM$A,S0=RAM$S,F0=RAM$F)
  old <- metaSEM::osmasem(model.name='Reference',Mmatrix=M0,Tmatrix=T0,data=frame,intervals.type='z')
  old_parameters <- summary(old,fitIndices=TRUE)$parameters
  run <- masem_fit(fixture$data,fixture$syntax,TRUE,fixture$labels)
  expect_true(run$ok)
  expect_equal(run$summary$parameters$Estimate,old_parameters$Estimate,tolerance=1e-7)
  expect_equal(run$summary$parameters$`Std.Error`,old_parameters$`Std.Error`,tolerance=1e-7)
  # Numeric reference saved with metaSEM 1.5.0 and lavaan 0.6-19.
  reference_names <- c('academicachievementONconscientiousness',
    'academicachievementONselfmanagement','selfmanagementWITHconscientiousness')
  reference <- run$summary$parameters[match(reference_names,run$summary$parameters$name),]
  expect_equal(reference$Estimate,c(.146814552279,.160907878114,.696770203137),tolerance=1e-7)
  expect_equal(reference$`Std.Error`,c(.02709846383589,.02064852256565,.00566931071056),tolerance=1e-7)
  expect_identical(unname(run$software['metaSEM']),as.character(packageVersion('metaSEM')))
  expect_false('Minimum eigenvalue' %in% names(run$data$audit))
  expect_equal(masem_r2(run)$R2,round(1-run$model$mx.fit$Smatrix$result[1,1],3))
  labelled <- masem_fit(fixture$data,'academicachievement ~ b*selfmanagement + conscientiousness',TRUE,fixture$labels)
  expect_true('b' %in% labelled$summary$parameters$name)
  pool <- masem_pool(fixture$data)
  expect_true(all(is.finite(pool$matrix)))
  # Out-of-range explained variance must remain visible instead of being clipped.
  altered <- run
  altered$model <- list(mx.fit=list(Amatrix=list(result=run$model$mx.fit$Amatrix$result),
    Smatrix=list(result=run$model$mx.fit$Smatrix$result)))
  altered$model$mx.fit$Smatrix$result[1,1] <- -.01
  expect_true(masem_r2(altered)$R2 > 1)
  expect_match(masem_r2(altered)$Note,'value retained')
})

test_that('fixed paths, variables without directed paths and covariance models work', {
  fixture <- masem_fixture()
  fixed <- masem_fit(fixture$data,'academicachievement ~ 0.1*selfmanagement + conscientiousness',TRUE,fixture$labels)
  expect_true(fixed$ok)
  order <- rownames(fixed$RAM$A)
  expect_equal(fixed$model$mx.fit$Amatrix$result[match('academicachievement',order),match('selfmanagement',order)],.1)
  expect_false('academicachievementONselfmanagement' %in% fixed$summary$parameters$name)
  isolated <- masem_fit(fixture$data,'academicachievement ~ selfmanagement',TRUE,fixture$labels)
  expect_true(isolated$ok)
  covariances <- masem_fit(fixture$data,'academicachievement ~~ selfmanagement + conscientiousness',TRUE,fixture$labels)
  expect_true(covariances$ok)
  expect_true(all(covariances$summary$parameters$matrix=='S0'))
  expect_equal(nrow(masem_r2(covariances)),0L)
})

test_that('variable ordering preserves the association of estimates with variable names', {
  fixture <- masem_fixture()
  before <- masem_fit(fixture$data,fixture$syntax,TRUE,fixture$labels)$summary$parameters
  changed <- fixture$data
  changed$cor_matrices <- lapply(seq_along(changed$cor_matrices),function(i) {
    matrix <- changed$cor_matrices[[i]]
    order <- if(i %% 2) rev(seq_len(nrow(matrix))) else seq_len(nrow(matrix))
    matrix[order,order,drop=FALSE]
  })
  names(changed$cor_matrices) <- names(fixture$data$cor_matrices)
  after <- masem_fit(changed,fixture$syntax,TRUE,fixture$labels)$summary$parameters
  key <- function(table) ifelse(table$matrix=='A0',paste(table$matrix,table$row,table$col),
    paste(table$matrix,pmin(table$row,table$col),pmax(table$row,table$col)))
  expect_setequal(key(before),key(after))
  after <- after[match(key(before),key(after)),]
  expect_equal(after$Estimate,before$Estimate,tolerance=1e-7)
  expect_equal(after$`Std.Error`,before$`Std.Error`,tolerance=1e-7)
})

test_that('ZIP export preserves matrix identifiers, row names, missing cells and Ns', {
  fixture <- masem_fixture();data <- fixture$data
  data$user_model <- fixture$syntax;data$syntax <- augment_lavaan_model(fixture$syntax,data$selection$variables)
  data$auto_covariances <- TRUE
  data$software <- masem_require_versions()
  folder <- withr::local_tempdir();file <- file.path(folder,'data.zip')
  masem_export_zip(data,file)
  paths <- utils::unzip(file,exdir=file.path(folder,'unpacked'))
  expect_length(paths,length(data$cor_matrices)+4)
  id <- names(data$cor_matrices)[1]
  table <- read.csv(file.path(folder,'unpacked',paste0(id,'.csv')),check.names=FALSE)
  matrix <- as.matrix(table[,-1]);rownames(matrix) <- table$variable
  expect_equal(matrix,data$cor_matrices[[id]],tolerance=1e-14)
  info <- read.csv(file.path(folder,'unpacked','basic_info.csv'),colClasses='character')
  expect_identical(info$matrix_id,as.character(data$basic_info$matrix_id))
  expect_equal(as.numeric(info$n),data$basic_info$n)
  settings <- paste(readLines(file.path(folder,'unpacked','selection_and_model.txt')),collapse='\n')
  expect_match(settings,paste('metaSEM',data$software[['metaSEM']]),fixed=TRUE)
  sparse <- data;sparse$pairs$k[1] <- 0
  expect_error(masem_require_coverage(sparse),'no reported correlation')
  bad <- fixture$raw;bad$n[1] <- 0
  expect_error(masem_prepare(data$selection$variables,bad,names(data$basic_info)),'Reference Ns')
})

test_that('data changes invalidate both stages while syntax changes only invalidate SEM', {
  suppressPackageStartupMessages({library(shiny);library(shinyjs);library(tidyverse)})
  fixture <- masem_fixture();app <- new.env()
  app$combined_data <- fixture$raw;app$labels_app <- fixture$labels
  app$admcol <- app$varlist <- fixture$variables_book
  app$review <- read.csv('data/3.meta_data/review_data/review_data.csv',colClasses='character')
  app$topics_list <- read.csv('data/topics_codebook.csv',colClasses='character')
  for(file in c('utils.R','masem_workflow.R','review_browser.R','review_coding.R','meta_analysis.R'))
    sys.source(file.path('app/R',file),app)
  sys.source('app/server.R',app)
  testServer(app$server,{
    expect_false(masem_data_ready())
    expect_false(sem_ready())
    expect_error(output$K_table,class='shiny.silent.error')
    expect_error(output$N_table,class='shiny.silent.error')
    expect_error(output$SEMresults,class='shiny.silent.error')
    session$setInputs(SEBdomains='selfmanagement',PersonalityTraits='conscientiousness',Outcomes='academicachievement',
      auto_saturate=TRUE,lavmodel=fixture$syntax,k_info=1,s1_matrix=1,fitSEM=1)
    expect_true(sem_current()$ok)
    saved <- pooled_current()$matrix
    session$setInputs(lavmodel='academicachievement ~ b*selfmanagement + conscientiousness')
    expect_false(sem_ready())
    expect_true(pooled_ready())
    expect_error(output$SEMresults,class='shiny.silent.error')
    expect_error(sem_current(),'Model syntax')
    expect_identical(pooled_current()$matrix,saved)
    session$setInputs(fitSEM=2)
    expect_true('b' %in% sem_current()$summary$parameters$name)
    session$setInputs(year_filter_masem='2024')
    expect_error(metaD(),'Get info')
    expect_error(pooled_current(),'Get info')
    expect_error(sem_current(),'Get info')
    session$setInputs(k_info=2)
    expect_error(sem_current(),'Data selection changed')
    expect_error(pooled_current(),'Data selection changed')
  })
})
