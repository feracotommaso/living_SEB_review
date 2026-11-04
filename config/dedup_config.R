# Living SEB bibliographic deduplication workflow configuration.
# The batch identifier refers to the publication month represented by the update.

workflow_version <- "2026-10.3"
expected_citesource_version <- "0.2.1"

batch_id <- "2026-10"
history_through <- "2026-09"
search_run_date <- "2026-11-02"

history_dir <- here::here(
  "data", "0.bib_download", "history"
)

update_dir <- here::here(
  "data", "0.bib_download", "updates", batch_id
)

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
