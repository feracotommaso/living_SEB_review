# Compatibility entry points for the retained historical analytical builder.
# Current operations and human imports are defined in workflow.R.
integration_hash <- function(path) if (file.exists(path)) digest::digest(file = path, algo = "sha256") else "absent"
integration_current <- function(directory = "data/3.meta_data/integration") {
  if (!file.exists("data/3.meta_data/coding/records.csv"))
    stop("Codifica generale assente. Ripristina il checkpoint o completa la migrazione prima dell'aggiornamento.")
  source("R/article_coding.R")
  source("R/workflow_io.R")
  source("R/workflow_coding.R")
  wf_current()
}
