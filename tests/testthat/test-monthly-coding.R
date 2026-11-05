withr::local_dir(here::here())
source("R/full_text_workflow.R")
source("R/coding_compatibility.R")
source("R/article_coding.R")
source("R/codebook_workflow.R")
source("R/coding_provenance.R")

test_that("confirmed download dates update sources with history and are idempotent", {
  base <- tempfile("coding_date_test_")
  directory <- file.path(base, "artifacts/2026-11")
  dir.create(directory, recursive = TRUE)
  path <- file.path(directory, "9999.json")
  object <- list(paper_id = "9999", article = list(download_date = NULL, title = "Synthetic date fixture"), evidence = list())
  jsonlite::write_json(object, path, auto_unbox = TRUE, null = "null")
  coding_set_download_date("2026-11", "2026-12-02", base)
  updated <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(updated$article$download_date, "2026-12-02")
  expect_length(list.files(file.path(directory, "history")), 1L)
  expect_identical(updated$evidence[[1]]$source_sha256,
    digest::digest(updated$evidence[[1]]$source, algo = "sha256", file = TRUE))
  paths <- list.files(base, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(paths)
  coding_set_download_date("2026-11", "2026-12-02", base)
  expect_identical(tools::md5sum(paths), before)
  expect_error(coding_set_download_date("2026-11", "2026-12-03", base), "conflicts")
  expect_identical(tools::md5sum(paths), before)
})

test_that("a new monthly batch supports review-only and zero retained papers", {
  base <- tempfile("review_month_")
  directory <- file.path(base, "artifacts/2026-11")
  dir.create(directory, recursive = TRUE)
  evidence_file <- file.path(base, "synthetic_source.txt")
  writeLines("Synthetic test fixture, not an operational publication record.", evidence_file)
  article <- setNames(rep(list(NULL), length(coding_article_fields())), coding_article_fields())
  article$title <- "Synthetic review-only paper"; article$coder <- "test"
  article$doi <- "10.0000/test"; article$year <- 2026
  article$review_topics <- list("theorydevelopment")
  object <- list(coding_version = "1.0.0", screening_record_id = "synthetic_review", paper_id = "9999",
    coding_date = "2026-12-02", article = article, samples = list(), evidence = list())
  for (field in c("title", "coder", "doi", "year", "review_topics")) object$evidence[[length(object$evidence) + 1L]] <-
    list(unit = "paper", field = field, status = "extracted", source = evidence_file, page = NULL,
      source_sha256 = digest::digest(evidence_file, algo = "sha256", file = TRUE), evidence = "Synthetic fixture evidence")
  path <- file.path(directory, "9999.json")
  jsonlite::write_json(object, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  eligibility <- data.frame(paper_id = "9999", screening_record_id = "synthetic_review", doi = "10.0000/test",
    final_route = "review", review_eligible = TRUE, meta_status = "ineligible", meta_exclusion_reason = "NO_ELIGIBLE_SEB_SKILL_CONSTRUCT",
    framework_relation_tags = "TESTS_SEB_FRAMEWORK", eligible_quantitative_components = "")
  build <- coding_build_batch
  isolated <- new.env(parent = environment(build))
  isolated$ft_read_canonical_decisions <- function(...) eligibility
  environment(build) <- isolated
  result <- build("2026-11", base)
  expect_equal(nrow(result$papers), 1L)
  expect_equal(nrow(result$samples), 0L)
  expect_true("matrix_id" %in% names(result$samples))
  expect_identical(names(result$legacy_export), coding_dictionary()$field)
  # No retention next month: preserve header contracts rather than inventing rows.
  eligibility <- eligibility[FALSE, ]
  unlink(path)
  empty <- build("2026-11", base)
  expect_equal(nrow(empty$papers), 0L)
  expect_equal(nrow(empty$samples), 0L)
  expect_equal(nrow(empty$legacy_export), 0L)
  expect_identical(names(empty$legacy_export), coding_dictionary()$field)
  # A new retained paper can supply multiple eligible samples, assigned once.
  eligibility <- data.frame(paper_id = "9999", screening_record_id = "synthetic_review", doi = "10.0000/test",
    final_route = "both", review_eligible = TRUE, meta_status = "eligible", meta_exclusion_reason = "",
    framework_relation_tags = "TESTS_SEB_FRAMEWORK", eligible_quantitative_components = "Study 1; Study 2")
  object$article$title <- "Synthetic multi-sample paper"
  for (i in 1:2) {
    sample <- setNames(rep(list(NULL), length(coding_sample_fields())), coding_sample_fields())
    sample$n <- 100L + 20L * (i - 1L); sample$study_design <- "cross"
    sample$component_label <- paste("Study", i); sample$sample_label <- "Synthetic sample"
    sample$occasion_label <- "Baseline"; sample$occasion_role <- "first_eligible"
    object$samples[[i]] <- sample
    for (field in c("n", "study_design")) object$evidence[[length(object$evidence) + 1L]] <-
      list(unit = paste(sample$component_label, sample$sample_label, sep = "|"), field = field,
        status = "extracted", source = evidence_file, page = NULL,
        source_sha256 = digest::digest(evidence_file, algo = "sha256", file = TRUE), evidence = "Synthetic sample fixture")
  }
  jsonlite::write_json(object, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  multiple <- build("2026-11", base)
  expect_identical(multiple$samples$matrix_id, c("9999a", "9999b"))
  expect_identical(build("2026-11", base)$registry, multiple$registry)
})
