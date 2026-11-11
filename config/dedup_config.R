# Living SEB bibliographic deduplication workflow configuration.
# The batch identifier refers to the publication month represented by the update.

workflow_version <- "2026-11.1"
expected_citesource_version <- "0.2.1"

batch_id <- "2026-10"
history_through <- "2026-09"
search_run_date <- "2026-11-02"

# An explicit process-local selector isolates external intake from monthly runs.
# In RStudio: Sys.setenv(LIVING_SEB_BATCH_ID = "YYYY-MM-external-01").
# Clear with Sys.unsetenv("LIVING_SEB_BATCH_ID") before the next database update.
selected_batch <- Sys.getenv("LIVING_SEB_BATCH_ID", "")
if (nzchar(selected_batch)) {
  if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])(-external-[0-9]{2})?$", selected_batch)) stop("Invalid selected batch.")
  batch_id <- selected_batch
}
external_batch <- if (grepl("-external-", selected_batch, fixed = TRUE)) selected_batch else ""
if (nzchar(external_batch)) {
  source(here::here("R", "external_intake.R"))
  intake <- external_manifest(external_batch)
  batch_id <- external_batch
  search_run_date <- max(intake$received_date) # Intake date, not a database search/export date.
}

history_dir <- here::here(
  "data", "0.bib_download", "history"
)

update_dir <- here::here(
  "data", "0.bib_download", "updates", batch_id
)
if (nzchar(external_batch)) update_dir <- here::here("data", "0.bib_download", "external", batch_id)

normalised_root_dir <- here::here(
  "data", "0.bib_normalized"
)

normalised_history_dir <- file.path(
  normalised_root_dir,
  "history"
)

normalised_update_dir <- file.path(
  normalised_root_dir,
  "updates",
  batch_id
)

registry_dir <- here::here(
  "data", "0.registry"
)

registry_file <- file.path(
  registry_dir,
  "records_master_full.csv"
)

bootstrap_dir <- here::here(
  "data", "0.deduplication", "bootstrap"
)

batch_dir <- here::here(
  "data", "0.deduplication", batch_id
)

abstracting_dir <- here::here(
  "data", "1.abstracting", batch_id
)

rayyan_dir <- file.path(
  batch_dir,
  "rayyan"
)

# Rayyan was used to validate the pilot batch. Set TRUE only when a comparison
# is required before registry promotion for a future batch.
require_rayyan_comparison <- FALSE
