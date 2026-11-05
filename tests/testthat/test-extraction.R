withr::local_dir(here::here())
source("R/codebook_workflow.R")
source("R/extraction_helpers.R")

test_that("matrix checks enforce the age boundary, registered names and observed pair Ns", {
  d <- data.frame(selfmanagement = c(1,2,4,5), sex = c(0,1,NA,1))
  r <- cor(d, use = "pairwise.complete.obs")
  n <- crossprod(!is.na(as.matrix(d)))
  result <- extraction_check_matrix(d, r, n)
  expect_equal(result$minimum_pair_n, 3L)
  expect_equal(result$n, 4L)
  wrong <- n; wrong[1,2] <- wrong[2,1] <- 4
  expect_error(extraction_check_matrix(d, r, wrong), "Ns do not match")
  bad <- d; names(bad)[[2]] <- "age"
  expect_error(extraction_check_matrix(bad, r, n), "age")
  bad <- d; names(bad)[[2]] <- "unregisteredmeasure"
  expect_error(extraction_check_matrix(bad, r, n), "Unregistered")
  bad <- r; bad[1,2] <- bad[2,1] <- 1.1
  expect_error(extraction_check_matrix(d, bad, n), "range")
  bad <- d; bad$sex <- 1
  expect_error(extraction_check_matrix(bad, r, n), "constant")
})

test_that("source verification fails on changed or absent individual data", {
  file <- tempfile(); writeLines("source", file)
  manifest <- tempfile(fileext = ".csv")
  x <- data.frame(paper_id = "9999", filename = basename(file), local_path = file,
    sha256 = digest::digest(file, algo = "sha256", file = TRUE), retrieved_at_utc = "2026-11-04T00:00:00Z")
  readr::write_csv(x, manifest)
  expect_silent(extraction_verify_sources(manifest, "9999"))
  writeLines("changed", file)
  expect_error(extraction_verify_sources(manifest, "9999"), "changed")
})

test_that("individual and matrix files with the same basename retain different data", {
  root <- tempfile("export_fixture_"); dir.create(root)
  withr::local_dir(root)
  inputs <- c("scripts/extraction/articles/9999.R", "scripts/extraction/keys/9999.R",
    "config/extraction/9999_decisions.csv", "R/extraction_helpers.R", "R/codebook_workflow.R",
    "policies/extraction_conventions.md", "data/matrix_codebook.csv",
    "config/coding/variable_suffixes.csv", "data/3.meta_data/extraction/9999/source_manifest.csv")
  for (path in inputs) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("Synthetic provenance fixture", path)
  }
  d <- data.frame(selfmanagement = c(1,2,4,5), sex = c(0,1,NA,1), age = c(18,30,NA,40))
  r <- cor(d[1:2], use = "pairwise.complete.obs")
  n <- crossprod(!is.na(as.matrix(d[1:2])))
  extraction_export("9999", "9999a", d, r, n, data.frame(variable = names(d)[1:2]),
    list(n = 4), data.frame(decision = "Synthetic test"))
  individual <- readr::read_csv("data/3.meta_data/open_data/individual_data/9999a.csv", show_col_types = FALSE)
  matrix <- readr::read_csv("data/3.meta_data/extraction/9999/9999a/matrices/9999a.csv", show_col_types = FALSE)
  expect_equal(as.data.frame(individual), d)
  expect_equal(nrow(matrix), 2L)
  expect_identical(matrix$variable, names(d)[1:2])
  manifest <- codebook_csv("data/3.meta_data/extraction/9999/9999a/extraction_manifest.csv")
  rows <- manifest[manifest$role == "output", ]
  expect_true(all(vapply(seq_len(nrow(rows)), function(i)
    identical(digest::digest(rows$path[[i]], algo = "sha256", file = TRUE), rows$sha256[[i]]), logical(1))))
  first_paths <- list.files("data/3.meta_data/extraction/9999/9999a", recursive = TRUE, full.names = TRUE)
  first_hashes <- tools::md5sum(first_paths)
  second <- d; second$selfmanagement <- 6 - second$selfmanagement
  second_r <- cor(second[1:2], use = "pairwise.complete.obs")
  extraction_export("9999", "9999b", second, second_r, n, data.frame(variable = names(d)[1:2]),
    list(n = 4), data.frame(decision = "Second synthetic sample"))
  expect_identical(tools::md5sum(first_paths), first_hashes)
  second_manifest <- codebook_csv("data/3.meta_data/extraction/9999/9999b/extraction_manifest.csv")
  expect_true(all(second_manifest$matrix_id == "9999b"))
})

test_that("0145 reconstructs distinct facets and retains participants with partial observations", {
  skip_if_not(file.exists("data/3.meta_data/open_data/raw/0145/data_study2_analysis.rds"),
    "Individual data are local and Git-ignored; acquire 0145 sources to run this scientific regression.")
  e <- new.env(parent = globalenv())
  # Test-only CLI substitution: source exactly the operational script without exports.
  e$commandArgs <- function(...) "--check-only"
  paths <- list.files("data/3.meta_data/extraction/0145", recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(paths)
  invisible(capture.output(sys.source("scripts/extraction/articles/0145.R", envir = e)))
  expect_identical(tools::md5sum(paths), before)
  expect_equal(nrow(e$individual_data), 973L)
  expect_equal(ncol(e$data_matrix), 61L)
  expect_true("age" %in% names(e$individual_data))
  expect_false("age" %in% names(e$data_matrix))
  expect_true("sex" %in% names(e$data_matrix))
  expect_equal(sum(is.na(e$data_matrix$sex)), 30L)
  expected_persuasion <- rowMeans(e$data_scored[, c("PE02_13", "PE02_45", "PE03_29")])
  expected_perspective <- rowMeans(e$data_scored[, c("PE02_02", "PE02_34", "PE03_18")])
  expect_equal(unname(e$data_scored$persuasiveskill), unname(expected_persuasion))
  expect_equal(unname(e$data_scored$perspectivetakingskill), unname(expected_perspective))
  parts <- e$data_scored[, c("leadershipskill", "persuasiveskill", "expressiveskill", "conversationalskill", "energyregulation")]
  complete <- complete.cases(parts)
  expected_social <- (2*parts$leadershipskill + 2*parts$persuasiveskill + 2*parts$expressiveskill +
    2*parts$conversationalskill + parts$energyregulation) / 9
  expect_equal(e$data_scored$socialengagement[complete], expected_social[complete])
  alternate <- (2*parts$leadershipskill + 2*e$data_scored$perspectivetakingskill + 2*parts$expressiveskill +
    2*parts$conversationalskill + parts$energyregulation) / 9
  expect_lt(cor(e$data_scored$socialengagement, alternate, use = "pairwise.complete.obs"), .999)
  expect_equal(e$checks$hexaco_missing_block, 23L)
  expect_equal(range(e$pair_n[upper.tri(e$pair_n)]), c(790,973))
  expect_true(all(vapply(e$data_scored[e$bessi_variables], function(x)
    all(is.na(x) | (x >= 1 & x <= 5)), logical(1))))
})
