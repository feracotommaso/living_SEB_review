withr::local_dir(here::here())
source("R/codebook_workflow.R")

codebook_fixture <- function() {
  root <- tempfile("codebook_fixture_")
  dir.create(file.path(root, "data"), recursive = TRUE)
  file.copy(c("data/matrix_codebook.xlsx", "data/topics_codebook.xlsx", "data/topics_codebook.csv"),
    file.path(root, "data"))
  root
}

test_that("bootstrap preserves every historical cell and repeat sync changes no bytes", {
  root <- codebook_fixture()
  before <- tools::md5sum(file.path(root, "data", c("matrix_codebook.xlsx", "topics_codebook.xlsx")))
  result <- codebook_sync(root)
  expect_identical(result$variables, codebook_excel(file.path(root, "data/matrix_codebook.xlsx")))
  expect_identical(unname(tools::md5sum(names(before))), unname(before))
  files <- list.files(file.path(root, "data"), full.names = TRUE)
  hashes <- tools::md5sum(files)
  codebook_sync(root)
  expect_identical(tools::md5sum(files), hashes)
})

test_that("new variables/topics require definitions and provenance and additions are idempotent", {
  root <- codebook_fixture()
  provenance <- data.frame(paper_id = "9999", change_date = "2026-11-04", reason = "Distinct useful construct/topic",
    source = "Test fixture, not a scientific coding decision")
  variables <- cbind(data.frame(Topic = "SOCIAL", broad = "test", column_name = "newconstruct",
    label = "New construct", notes = "Defined distinctly from existing constructs"), provenance)
  topics <- cbind(data.frame(Broad_topic = "application", Broad_topic_labs = "Application",
    Topic = "newtopic", Topic_labs = "New topic", Explanation = "A substantive topic definition", notes = NA_character_), provenance)
  old <- codebook_sync(root)
  result <- codebook_sync(root, variables, topics)
  expect_identical(head(result$variables, nrow(old$variables)), old$variables)
  expect_true("newconstruct" %in% codebook_excel(file.path(root, "data/matrix_codebook.xlsx"))$column_name)
  expect_true("newtopic" %in% codebook_excel(file.path(root, "data/topics_codebook.xlsx"))$Topic)
  expect_equal(nrow(result$changes), 2L)
  hashes <- tools::md5sum(list.files(file.path(root, "data"), full.names = TRUE, recursive = TRUE))
  codebook_sync(root, variables, topics)
  expect_identical(tools::md5sum(names(hashes)), hashes)
  invalid <- topics; invalid$Explanation <- NA_character_
  expect_error(codebook_sync(root, add_topics = invalid), "Missing codebook field")
  invalid <- variables; invalid$reason <- NA_character_
  expect_error(codebook_sync(root, add_variables = invalid), "provenance")
  invalid <- variables; invalid$label <- "Silent redefinition"
  expect_error(codebook_sync(root, add_variables = invalid), "redefine")
  expect_identical(tools::md5sum(names(hashes)), hashes)
})

test_that("CSV/XLSX divergence blocks maintenance without overwriting either", {
  root <- codebook_fixture()
  codebook_sync(root)
  path <- file.path(root, "data/topics_codebook.csv")
  x <- codebook_csv(path); x$Topic_labs[[1]] <- "Unsynchronised edit"
  readr::write_csv(x, path, na = "")
  files <- list.files(file.path(root, "data"), full.names = TRUE)
  before <- tools::md5sum(files)
  expect_error(codebook_sync(root), "conflict")
  expect_identical(tools::md5sum(files), before)
})

test_that("registered attached suffixes preserve complete base names and reject arbitrary variants", {
  base <- codebook_excel("data/matrix_codebook.xlsx")
  resolved <- codebook_resolve_variables(c("selfmanagement", "selfmanagementother",
    "selfmanagementiat", "hexaco_opennessother"), base)
  expect_identical(resolved$base_name, c(rep("selfmanagement", 3), "hexaco_openness"))
  expect_identical(resolved$suffix, c("", "other", "iat", "other"))
  expect_match(resolved$label[[2]], "Other-report", fixed = TRUE)
  expect_error(codebook_resolve_variables("selfmanagement_iat", base), "Unregistered")
  expect_error(codebook_resolve_variables("unknownother", base), "Unregistered")
  expect_error(codebook_resolve_variables(c("sex", "sex"), base), "unique")
})
