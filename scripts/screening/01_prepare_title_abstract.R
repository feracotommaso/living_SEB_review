# Prepare the canonical AI input for title/abstract screening.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "title_abstract_screening.R"))

policy <- screening_load_policy()
output_dir <- file.path(abstracting_dir, "title_abstract")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source_candidates <- c(
  file.path(abstracting_dir, paste0(batch_id, "_new_records.csv")),
  file.path(abstracting_dir, paste0(batch_id, "_new_records.xlsx"))
)
source_file <- source_candidates[file.exists(source_candidates)][1L]

if (is.na(source_file)) {
  stop(
    "No deduplicated new-record file found. Expected one of:\n",
    paste(source_candidates, collapse = "\n"),
    "\nRun scripts/dedup/04_update_finalize.R first.",
    call. = FALSE
  )
}

raw <- screening_read_table(source_file)
prepared <- screening_prepare_input(raw, batch_id, policy)

input_csv <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_input.csv")
)
input_xlsx <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_input.xlsx")
)
input_jsonl <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_ai_request.jsonl")
)
decisions_file <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_ai_decisions.csv")
)
task_file <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_codex_task.md")
)

if (file.exists(decisions_file)) {
  existing <- screening_read_decisions(decisions_file)
  if ("screening_record_id" %in% names(existing) &&
      !setequal(existing$screening_record_id, prepared$screening_record_id)) {
    stop(
      "An AI decision file exists but belongs to a different input set. ",
      "Archive or remove it before preparing this batch.",
      call. = FALSE
    )
  }
}

readr::write_csv(prepared, input_csv)
if (!getOption("living_seb.compact", file.exists("data/3.meta_data/coding/records.csv"))) {
screening_write_input_workbook(prepared, input_xlsx)
screening_write_jsonl(prepared, input_jsonl)

prompt_path <- here::here("prompts", "title_abstract_screening.md")
task_lines <- c(
  paste0("# Codex task: Living SEB title/abstract screening for ", batch_id),
  "",
  "Use the repository skill `$living-seb-title-abstract`.",
  "",
  paste0("Input: `", normalizePath(input_csv, winslash = "/", mustWork = FALSE), "`"),
  paste0("Policy: `", normalizePath(here::here("config", "screening", "title_abstract.yml"), winslash = "/", mustWork = FALSE), "`"),
  paste0("Prompt: `", normalizePath(prompt_path, winslash = "/", mustWork = FALSE), "`"),
  paste0("Required output: `", normalizePath(decisions_file, winslash = "/", mustWork = FALSE), "`"),
  "",
  "After writing and validating the decisions file, run:",
  "",
  "```r",
  "source(\"scripts/screening/02_finalize_title_abstract.R\")",
  "```",
  "",
  "Do not ask for scientific human review when the model is uncertain: use `uncertain`; the deterministic policy retains it."
)
writeLines(task_lines, task_file, useBytes = TRUE)

summary_data <- tibble::tibble(
  workflow_version = workflow_version,
  batch_id = batch_id,
  policy_version = policy$version,
  prompt_version = policy$prompt_version,
  source_file = source_file,
  input_records = nrow(prepared),
  input_csv = input_csv,
  input_xlsx = input_xlsx,
  input_jsonl = input_jsonl,
  expected_ai_decisions_file = decisions_file
)

readr::write_csv(
  summary_data,
  file.path(output_dir, paste0(batch_id, "_title_abstract_prepare_summary.csv"))
)

print(summary_data)
message("AI input prepared.")
message("Next: run the $living-seb-title-abstract skill using: ", task_file)
message("Then run scripts/screening/02_finalize_title_abstract.R.")

} else {
  message("Input screening: ", input_csv, "\nOutput IA atteso: ", decisions_file)
}
