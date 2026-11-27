withr::local_dir(here::here())
source('app/R/utils.R')

test_that('report references include later acquisitions absent from the historical bib', {
  review <- read.csv('data/3.meta_data/review_data/review_data.csv', colClasses='character',fileEncoding='UTF-8')
  bib <- suppressWarnings(RefManageR::ReadBib('data/0.bib_download/bibAll.bib',check=FALSE))
  papers <- review[match(c('0050','0100','0118','0145'),review$paper_id), ]
  # A DOI URL and an altered title must still identify the same article.
  papers$doi[1] <- paste0('https://doi.org/',papers$doi[1])
  papers$title[1] <- 'Title abbreviated in an analysis export'
  selected <- papers[,c('paper_id','title','doi')]
  refs <- generateBib(bib,rbind(selected,selected),review)
  expect_length(refs,4L)
  expect_setequal(doi_code(as.data.frame(refs)$doi),doi_code(papers$doi))
  expect_match(paste(as.character(refs[[3]]$author),collapse='; '),'Cravet')
  incomplete <- review
  incomplete$authors[incomplete$paper_id=='0118'] <- ''
  expect_error(generateBib(bib,selected,incomplete),'Incomplete reference metadata for paper 0118')
  # Every current article can be cited without modifying the original bib exports.
  expect_length(generateBib(bib,review,review),nrow(review))
})

test_that('DOI links escape HTML and report counts retain whole units', {
  links <- doi_links(c('https://doi.org/10.3390/jintelligence10030063',NA,'<script>alert(1)</script>'))
  expect_match(links[1],'href="https://doi.org/10.3390/jintelligence10030063"',fixed=TRUE)
  expect_identical(links[2],'')
  expect_false(grepl('<script>',links[3],fixed=TRUE))
  table <- data.frame(Title='<b>Source title</b>',`Reference N`=607,
    r=.1234,DOI='10.3390/jintelligence10030063',check.names=FALSE)
  html <- paste(capture.output(report_table(table)),collapse='\n')
  expect_match(html,'607')
  expect_false(grepl('607.000',html,fixed=TRUE))
  expect_match(html,'0.123',fixed=TRUE)
  expect_match(html,'&lt;b&gt;Source title&lt;/b&gt;',fixed=TRUE)
  expect_match(html,'<a href=',fixed=TRUE)
})
