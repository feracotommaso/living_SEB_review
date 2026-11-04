withr::local_dir(here::here())
source("R/full_text_workflow.R")
source("R/coding_compatibility.R")

test_that("dictionary fixes the exact 31-field legacy contract and agreed rules", {
  d <- coding_dictionary()
  legacy <- names(readxl::read_excel("data/3.meta_data/basic_info/26_09_basic_info.xlsx", n_max = 0))
  expect_identical(d$field, legacy)
  expect_equal(nrow(d), 31L)
  expect_identical(d$rule_status[d$field == "n"], "agreed")
  expect_true(all(nzchar(d$definition) & nzchar(d$source) & nzchar(d$coding_rule)))
})

test_that("topic alias normalization is exact, stable and preserves unknown codes", {
  input <- c("nomoligicalnet", "skillassessment; nomoligicalnet; nomologicalnet;", "assessment", "unclassified_topic", NA)
  expected <- c("nomologicalnet", "skillassessment; nomologicalnet", "skillassessment", "unclassified_topic", NA)
  expect_identical(coding_normalise_topics(input), expected)
  expect_identical(coding_normalise_topics(expected), expected)
  expect_identical(coding_normalise_topics("prefixnomoligicalnet"), "prefixnomoligicalnet")
  expect_error(coding_normalise_topics("x", data.frame(alias = c("x", "y"), canonical_topic = c("y", "x"))), "cycles")
})

test_that("topic audit retains ambiguous classification for human review", {
  d <- data.frame(paper_id = "9999", matrix_id = "9999", review_topics = "unclassified_topic; theorydevelopment")
  codebook <- data.frame(Topic = "theorydevelopment")
  audit <- coding_topic_audit(d, codebook)
  expect_identical(audit$topic, "unclassified_topic")
  expect_identical(d$review_topics, "unclassified_topic; theorydevelopment")
})

test_that("confirmed 0066 correction retains theory and joins the measurement taxonomy", {
  basic <- coding_read_basic_info()
  row <- basic[basic$paper_id == "0066", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_identical(row$review_topics, "skillassessment; theorydevelopment")
  corrections <- attr(basic, "topic_corrections")
  audit <- corrections[corrections$paper_id == "0066", , drop = FALSE]
  expect_identical(audit$old_value, "assessment; theorydevelopment")
  expect_identical(audit$new_value, "skillassessment; theorydevelopment")
  codebook <- coding_read_csv("data/topics_codebook.csv")
  expect_identical(codebook$Broad_topic[match("skillassessment", codebook$Topic)], "measurement")
  expect_equal(nrow(coding_topic_audit(row, codebook)), 0L)
})

test_that("0104a is an explicit inactive artifact rather than a quantitative ID", {
  basic <- coding_read_basic_info()
  paths <- list.files("data/3.meta_data/matrices", pattern = "\\.xlsx$", full.names = TRUE)
  inventory <- coding_matrix_inventory(basic, paths)
  row <- inventory[inventory$matrix_id == "0104a", ]
  expect_identical(row$status, "excluded_by_recorded_decision")
  expect_identical(row$paper_id, "0104")
  expect_identical(basic$final_decision[basic$paper_id == "0104"], "review")
  expect_equal(sum(inventory$status == "included"), 37L)
  expect_setequal(inventory$matrix_id[inventory$status == "missing_matrix_file"], c("0050", "0053a", "0053b"))
  invalid <- basic; invalid$final_decision[invalid$paper_id == "0104"] <- "both"
  expect_error(coding_matrix_inventory(invalid, paths), "conflicts")
  expect_error(coding_matrix_inventory(basic, c(paths, "new/9999a.xlsx")), "Unmatched")
})

test_that("shared-string patch preserves unrelated cells and every other XLSX part", {
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "variables")
  openxlsx::addWorksheet(wb, "unrelated")
  openxlsx::writeData(wb, "variables", data.frame(Topic = "nomoligicalnet", label = "Nomological network"))
  openxlsx::writeData(wb, "unrelated", data.frame(keep = "0012", value = 2.5))
  openxlsx::freezePane(wb, "variables", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, path)
  original <- tempfile(fileext = ".xlsx"); file.copy(path, original)
  expect_true(coding_patch_topic_codebook(path))
  expect_identical(readxl::read_excel(path)$Topic, "nomologicalnet")
  expect_identical(readxl::read_excel(path, sheet = "unrelated")$keep, "0012")
  members <- utils::unzip(original, list = TRUE)$Name
  expect_setequal(utils::unzip(path, list = TRUE)$Name, members)
  read_member <- function(file, member) {
    con <- unz(file, member, open = "rb"); on.exit(close(con))
    readBin(con, "raw", n = 10000000L)
  }
  for (member in setdiff(members, "xl/sharedStrings.xml")) {
    expect_identical(read_member(path, member), read_member(original, member), info = member)
  }
  before <- tools::md5sum(path)
  expect_false(coding_patch_topic_codebook(path))
  expect_identical(tools::md5sum(path), before)
})
