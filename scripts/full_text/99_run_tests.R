# Run full-text tests from the repository root.
testthat::test_dir(file.path("tests", "testthat"), filter = "full-text", reporter = "summary")
