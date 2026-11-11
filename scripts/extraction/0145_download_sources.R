# Acquire the public Study 2 source files; do not execute downloaded author code.
# Run from the repository root. Individual records remain under Git-ignored open_data/.
suppressPackageStartupMessages({ library(readr); library(digest) })
source("workflow.R")
wf_require_saved_coding()

sources <- data.frame(
  paper_id = "0145", component_label = "Study 2", project_url = "https://osf.io/an64w/",
  filename = c("thin_data_day2_sosci.rds", "thin_data_day1_sosci.rds",
    "thin_data_day2_tailor.rds", "data_study2_analysis.rds", "readme.txt",
    "Supplement_S2.1.qmd", "Supplement_S2.2.qmd", "01_Options.R"),
  download_url = paste0("https://osf.io/download/", c("68fa1c06cc194cd2e9f98056",
    "68fa1c078ec90c7caaf98305", "68fa1c09e86816677bf98133", "q9rb6", "r3xcm",
    "68fa1e0f5cc211e9e01a0262", "68fb47d7b96e8ed4b11a092d", "eyuwm"), "/"),
  expected_sha256 = c("56032ade9bd2cb8ceae158be052a02c16fa56ed40a39f85cbfeb00de0b198afb",
    "282160fc764652708dbda4eec9bd63a4736b7523fb44cb3f3b397d668d7ce017",
    "29ec2b958a6b75d0b9eee0234d08949d0098d76f8992982594458eb6998bdbf1",
    "1bdb5bcb663d5f4ef913d642c641ac2a034aafca6fb513c968596a114822ed2e",
    "793165013ec260b708403d572267945774811bb32311c8cdb128912dddf4247e",
    "44c4001cc8e61b068f29722016d2f9437108d1c0f03b71ff50898685241e4b41",
    "610032dcb0d097efa7e65c067c69bf0bebbc9e1df189f624a7901a57acf3c5da",
    "4ad786ee87676f6614748d283677392b3717de0f03ea94a8bf9be43582a69a64"),
  stringsAsFactors = FALSE)

raw_dir <- "data/3.meta_data/open_data/raw/0145"
provenance_path <- "data/3.meta_data/coding/sources.csv"
originals <- wf_read(provenance_path)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
if (anyNA(sources$expected_sha256)) stop("Every source must have a pinned OSF checksum.")

for (i in seq_len(nrow(sources))) {
  target <- file.path(raw_dir, sources$filename[i])
  previous <- which(originals$paper_id == "0145" & originals$path == target)
  if (length(previous) > 1L) stop("Ambiguous original-source provenance: ", target)
  if (!file.exists(target)) {
    temporary <- tempfile("download_", tmpdir = raw_dir)
    tryCatch({
      utils::download.file(sources$download_url[i], temporary, mode = "wb", method = "libcurl")
      if (!identical(digest(temporary, algo = "sha256", file = TRUE), sources$expected_sha256[i]))
        stop("Downloaded source checksum mismatch: ", sources$filename[i])
      if (!file.rename(temporary, target)) stop("Could not preserve source: ", target)
    }, finally = if (file.exists(temporary)) unlink(temporary))
    # If restoring a recorded original, keep its original dates and provenance.
    if (!length(previous)) {
      retrieved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      entry <- data.frame(paper_id = "0145", role = "downloaded_original", path = target,
        sha256 = sources$expected_sha256[i], source_reference = sources$download_url[i],
        received_date = substr(retrieved, 1, 10), download_date = substr(retrieved, 1, 10),
        download_date_status = "recorded_at_retrieval",
        provenance_note = paste("Project", sources$project_url[i], "component Study 2 retrieved UTC", retrieved))
      originals <- rbind(originals, entry[, names(originals)])
      wf_commit(setNames(list(originals), provenance_path))
      wf_export_coding(force = TRUE)
      previous <- nrow(originals)
    }
  } else if (!length(previous)) {
    stop("Existing source has no recorded provenance; register it in coding.xlsx: ", target)
  }
  actual <- digest(target, algo = "sha256", file = TRUE)
  if (!identical(actual, sources$expected_sha256[i]) || actual != originals$sha256[previous])
    stop("Source differs from the pinned version or recorded provenance: ", target)
}
cat("Verified", nrow(sources), "Study 2 sources. Individual data remain local.\n")
