# Optional clean restart. Raw bibliography files are never deleted.
# The Rayyan comparison directory is preserved.

if (!interactive()) {
  stop("Run this reset script interactively in RStudio.", call. = FALSE)
}

source(here::here("config", "dedup_config.R"))

confirmation <- readline(
  paste0(
    "Type RESET ", batch_id,
    " to remove generated deduplication outputs and registries: "
  )
)

if (!identical(confirmation, paste("RESET", batch_id))) {
  stop("Reset cancelled.", call. = FALSE)
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

message("Generated outputs removed.")
message("Raw files under data/0.bib_download were preserved.")
message("Files already placed in the Rayyan directory were preserved.")
