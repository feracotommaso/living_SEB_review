withr::local_dir(here::here())
source("app/R/utils.R")

test_that("pair readers preserve complete names and do not absorb measurement variants", {
  d <- data.frame(matrix_id = "test", n = 100,
    hexaco_openness_selfmanagement = .4,
    hexaco_openness_selfmanagementiat = .1,
    selfmanagement_selfmanagementiat = .2)
  sem <- getSEMdata(c("selfmanagement", "hexaco_openness"), d, c("matrix_id", "n"))
  expect_equal(ncol(sem$matrices), 1L)
  expect_equal(sem$cor_matrices[[1]]["selfmanagement", "hexaco_openness"], .4)
  meta <- getMETAdata("selfmanagement", "hexaco_openness", d, c("matrix_id", "n"))
  expect_equal(nrow(meta), 1L)
  expect_equal(meta$value, .4)
  expect_identical(meta$pred, "selfmanagement")
  expect_identical(meta$out, "hexaco_openness")
  implicit <- getMETAdata("selfmanagementiat", "hexaco_openness", d, c("matrix_id", "n"))
  expect_equal(implicit$value, .1)
})

test_that("empty selections return no samples rather than a fabricated row", {
  d <- data.frame(matrix_id = "test", n = 100)
  sem <- getSEMdata(c("selfmanagement", "cooperation"), d, c("matrix_id", "n"))
  expect_equal(nrow(sem$basic_info), 0L)
  expect_length(sem$cor_matrices, 0L)
  meta <- getMETAdata("selfmanagement", "cooperation", d, c("matrix_id", "n"))
  expect_equal(nrow(meta), 0L)
})
