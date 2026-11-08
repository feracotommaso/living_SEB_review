withr::local_dir(here::here())
source("R/full_text_workflow.R")
source("R/coding_compatibility.R")
source("R/article_coding.R")

eligibility <- ft_read_canonical_decisions("data/2.full_abstracting/2026-10_full_text_decisions.csv")
topics <- coding_read_csv("data/topics_codebook.csv")
# Schema fixtures are isolated from operational provenance and frozen file hashes.
fixture_source <- tempfile(fileext = ".txt")
writeLines("Synthetic evidence for schema tests only; not an operational assessment.", fixture_source)
records <- coding_read_csv("data/3.meta_data/coding/records.csv")
artifacts <- lapply(c("0144", "0145", "0148"), function(pid) {
  r <- records[records$paper_id == pid, ][1, ]
  article <- lapply(r[, coding_article_fields(), drop = FALSE], function(v) if (is.na(v)) NULL else v)
  for (field in c("review_topics", "email", "open_data")) if (!is.null(article[[field]])) article[[field]] <- as.list(trimws(strsplit(article[[field]], ";", fixed = TRUE)[[1]]))
  article$year <- as.numeric(article$year)
  obj <- list(coding_version = "1.0.0", screening_record_id = r$screening_record_id, paper_id = pid,
    coding_date = "2026-11-06", article = article, samples = list(), evidence = list())
  if (pid == "0145") {
    sample <- lapply(r[, c("component_label", "sample_label", "occasion_label", "occasion_role", coding_sample_fields()), drop = FALSE], function(v) if (is.na(v)) NULL else v)
    for (field in c("country", "seb_measure", "trait_frame")) if (!is.null(sample[[field]])) sample[[field]] <- as.list(trimws(strsplit(sample[[field]], ";", fixed = TRUE)[[1]]))
    for (field in c("n", "age", "gender")) if (!is.null(sample[[field]])) sample[[field]] <- as.numeric(sample[[field]])
    obj$samples <- list(sample)
  }
  units <- c(list(paper = article), if (length(obj$samples)) setNames(obj$samples, paste(r$component_label, r$sample_label, sep = "|")))
  for (unit in names(units)) for (field in names(units[[unit]])) if (!is.null(units[[unit]][[field]])) {
    obj$evidence[[length(obj$evidence)+1L]] <- list(unit = unit, field = field, status = "extracted", source = fixture_source,
      source_sha256 = digest::digest(file = fixture_source, algo = "sha256"), page = NULL, evidence = "Synthetic schema fixture")
  }
  obj
})
artifact_dir <- tempfile(); dir.create(artifact_dir)
artifact_paths <- vapply(artifacts, function(obj) {
  path <- file.path(artifact_dir, paste0(obj$paper_id, ".json"))
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}, character(1))

test_that("pilot extracts only the eligible component and analytic sample", {
  for (obj in artifacts) expect_silent(coding_validate_artifact(obj, eligibility, topics))
  quant <- artifacts[[which(vapply(artifacts, function(o) o$paper_id == "0145", logical(1)))]]
  expect_equal(length(quant$samples), 1L)
  expect_identical(quant$samples[[1]]$component_label, "Study 2")
  expect_equal(quant$samples[[1]]$n, 973L)
  for (obj in artifacts[vapply(artifacts, function(o) o$paper_id != "0145", logical(1))]) expect_length(obj$samples, 0L)
  invalid <- quant; invalid$samples[[1]]$component_label <- "Study 1"
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "Ineligible")
  invalid <- quant; invalid$samples[[1]]$n <- 0
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "analytic n")
  invalid <- quant; invalid$samples[[1]]$gender <- 101
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "percentage")
  invalid <- quant; invalid$samples[[1]]$occasion_role <- "follow_up"
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "first eligible")
  invalid <- artifacts[[1]]; invalid$samples <- quant$samples
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "Review-only")
  invalid <- quant; invalid$article$doi <- "10.1000/wrong"
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "DOI")
  invalid <- quant; invalid$evidence <- list()
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "evidence")
  invalid <- quant; invalid$evidence[[1]]$source_sha256 <- paste(rep("0", 64), collapse = "")
  expect_error(coding_validate_artifact(invalid, eligibility, topics), "source changed")
})

test_that("sample IDs survive rebuilds and incomplete artifacts cannot overwrite results", {
  base <- tempfile(); dir.create(file.path(base, "artifacts", "2026-10"), recursive = TRUE)
  file.copy(artifact_paths, file.path(base, "artifacts", "2026-10"))
  result <- coding_build_batch("2026-10", base)
  expect_identical(result$samples$sample_id, "a")
  expect_identical(result$samples$matrix_id, "0145a")
  expect_true(all(is.na(result$legacy_export$n[result$legacy_export$paper_id != "0145"])))
  expect_identical(names(result$legacy_export), coding_dictionary()$field)
  again <- coding_build_batch("2026-10", base)
  expect_identical(again$registry, result$registry)
  changed <- jsonlite::fromJSON(file.path(base, "artifacts", "2026-10", "0145.json"), simplifyVector = FALSE)
  changed$samples[[1]]$sample_label <- "Renamed analytic sample"
  for (i in seq_along(changed$evidence)) if (changed$evidence[[i]]$unit == "Study 2|Analytic sample") changed$evidence[[i]]$unit <- "Study 2|Renamed analytic sample"
  jsonlite::write_json(changed, file.path(base, "artifacts", "2026-10", "0145.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  expect_error(coding_build_batch("2026-10", base), "registry reconciliation")
  file.copy(artifact_paths[basename(artifact_paths) == "0145.json"], file.path(base, "artifacts", "2026-10"), overwrite = TRUE)
  csv <- file.path(base, "2026-10", "legacy_export.csv")
  before <- tools::md5sum(csv)
  unlink(file.path(base, "artifacts", "2026-10", "0148.json"))
  expect_error(coding_build_batch("2026-10", base), "exactly all")
  expect_identical(tools::md5sum(csv), before)
})

test_that("maintenance preserves scientific values except registered exact revisions", {
  before <- coding_historical_contract()
  metadata <- as.data.frame(readxl::read_excel("data/3.meta_data/meta_data/metadata.xlsx"))
  historical <- metadata[match(before$matrix_id, metadata$matrix_id), , drop = FALSE]
  after <- coding_numeric_contract(historical)
  expect_equal(as.data.frame(before), after, ignore_attr = TRUE)
  expect_silent(coding_validate_historical(metadata))
  expect_equal(nrow(historical), 37L)
  expect_equal(sum(metadata$matrix_id == "0145a"), 1L)
  expect_false(any(grepl("nomoligicalnet", metadata$review_topics, fixed = TRUE)))
  codebook <- coding_read_csv("data/topics_codebook.csv")
  expect_true("nomologicalnet" %in% codebook$Topic)
  expect_false("nomoligicalnet" %in% codebook$Topic)
})
