# Explicit maintenance of the current topic codebook and the 31-field dictionary.
source(file.path("R", "full_text_workflow.R"))
source(file.path("R", "coding_compatibility.R"))
source(file.path("R", "codebook_workflow.R"))
# CSV-led maintenance refuses divergence rather than overwriting a manual edit.
codebook_sync()
coding_write_dictionary()
cat("Codebook CSV/XLSX consistency verified; 31-field dictionary written.\n")
