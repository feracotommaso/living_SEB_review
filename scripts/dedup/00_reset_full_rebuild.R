# DANGER: full rebuild reset.
# Use only when intentionally rebuilding the historical registry from raw files.
# Raw bibliography files and the current Rayyan folder are preserved.

if (!interactive()) {
  stop("Run this reset script interactively in RStudio.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(here)
})

source(here::here("config", "dedup_config.R"))

confirmation <- readline(
  paste0(
    "Type FULL RESET ", batch_id,
    " to delete ALL generated deduplication outputs and registries: "
  )
)

if (!identical(confirmation, paste("FULL RESET", batch_id))) {
  stop("Full reset cancelled.", call. = FALSE)
}

unlink(bootstrap_dir, recursive = TRUE, force = TRUE)
unlink(normalised_history_dir, recursive = TRUE, force = TRUE)
unlink(normalised_update_dir, recursive = TRUE, force = TRUE)
unlink(abstracting_dir, recursive = TRUE, force = TRUE)

if (dir.exists(batch_dir)) {
  generated_entries <- list.files(
    batch_dir,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  generated_entries <- generated_entries[
    basename(generated_entries) != "rayyan"
  ]
  unlink(generated_entries, recursive = TRUE, force = TRUE)
}

registry_outputs <- list.files(
  registry_dir,
  pattern = "^records_master_full",
  full.names = TRUE
)
unlink(registry_outputs, recursive = FALSE, force = TRUE)

folders <- c(
  normalised_history_dir,
  normalised_update_dir,
  registry_dir,
  bootstrap_dir,
  batch_dir,
  abstracting_dir,
  rayyan_dir
)

for (folder in folders) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

message("Full generated state removed.")
message("The historical registry must now be rebuilt with 01 and 02.")
message("Raw files under data/0.bib_download were preserved.")
message("Files already placed in the Rayyan directory were preserved.")
