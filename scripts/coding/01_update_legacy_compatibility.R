# Explicit maintenance of the current topic codebook and the 31-field dictionary.
source(file.path("R", "full_text_workflow.R"))
source(file.path("R", "coding_compatibility.R"))
source(file.path("R", "codebook_workflow.R"))
# CSV-led maintenance refuses divergence rather than overwriting a manual edit.
codebook_sync()
if (file.exists("data/3.meta_data/coding/records.csv")) {
  source("workflow.R")
  mostra_codifica() # The dictionary is a sheet in the cumulative workbook.
} else coding_write_dictionary()
cat("Codebook CSV/XLSX consistency verified; coding dictionary refreshed.\n")
