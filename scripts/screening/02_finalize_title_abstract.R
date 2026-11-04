# Validate AI decisions and produce the final title/abstract screening outputs.
# Scientific uncertainty is retained automatically; no human screening is
# required unless the AI output fails technical validation.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

source(here::here("config", "dedup_config.R"))
source(here::here("R", "title_abstract_screening.R"))

policy <- screening_load_policy()
output_dir <- file.path(abstracting_dir, "title_abstract")

input_file <- file.path(
  output_dir,
  paste0(batch_id, "_title_abstract_input.csv")
)

if (!file.exists(input_file)) {
  stop(
    "Prepared title/abstract input not found: ", input_file,
    "\nRun 01_prepare_title_abstract.R first.",
    call. = FALSE
  )
}

decision_candidates <- c(
  file.path(output_dir, paste0(batch_id, "_title_abstract_ai_decisions.csv")),
  file.path(output_dir, paste0(batch_id, "_title_abstract_ai_decisions.jsonl")),
  file.path(output_dir, paste0(batch_id, "_title_abstract_ai_decisions.xlsx"))
)
decisions_file <- decision_candidates[file.exists(decision_candidates)][1L]

if (is.na(decisions_file)) {
  stop(
    "AI decisions not found. Expected one of:\n",
    paste(decision_candidates, collapse = "\n"),
    "\nUse the $living-seb-title-abstract skill to create the CSV file.",
    call. = FALSE
  )
}

input <- screening_read_table(input_file)
decisions <- screening_read_decisions(decisions_file)

result <- screening_finalize_outputs(
  input = input,
  decisions = decisions,
  output_dir = output_dir,
  batch_id = batch_id,
  policy = policy,
  workflow_version = workflow_version
)

print(result$summary)
message("Title/abstract screening finalized.")
message("Input for the next stage: ", result$paths$included_xlsx)
message("Canonical included CSV: ", result$paths$included_csv)
if (result$summary$keep_uncertain_records[[1L]] > 0L) {
  message(
    result$summary$keep_uncertain_records[[1L]],
    " uncertain/non-high-confidence record(s) were retained automatically."
  )
}
