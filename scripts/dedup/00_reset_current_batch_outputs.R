# Optional reset of the CURRENT update only.
# Preserves the historical registry, historical bootstrap, and all raw exports.

if (!interactive()) {
  stop("Run this reset script interactively in RStudio.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(here)
})

source(here::here("config", "dedup_config.R"))

confirmation <- readline(
  paste0(
    "Type RESET CURRENT ", batch_id,
    " to remove generated outputs for the current batch only: "
  )
)

if (!identical(confirmation, paste("RESET CURRENT", batch_id))) {
  stop("Current-batch reset cancelled.", call. = FALSE)
}

# Current-batch normalised copies are derived and safe to rebuild.
unlink(normalised_update_dir, recursive = TRUE, force = TRUE)

# Preserve the Rayyan folder, if present, while deleting other batch outputs.
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

# The current abstracting folder is derived from the update and may include
# title/abstract screening outputs. Delete it only after explicit confirmation.
unlink(abstracting_dir, recursive = TRUE, force = TRUE)

folders <- c(
  normalised_update_dir,
  batch_dir,
  abstracting_dir,
  rayyan_dir
)

for (folder in folders) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

message("Current-batch generated outputs removed.")
message("Historical registry and bootstrap were preserved.")
message("Raw files under data/0.bib_download were preserved.")
message("Files already placed in the Rayyan directory were preserved.")
