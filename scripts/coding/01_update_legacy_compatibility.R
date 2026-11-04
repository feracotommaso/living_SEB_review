# Explicit maintenance of the current topic codebook and the 31-field dictionary.
source(file.path("R", "full_text_workflow.R"))
source(file.path("R", "coding_compatibility.R"))
coding_patch_topic_codebook()
topics <- as.data.frame(readxl::read_excel("data/topics_codebook.xlsx"))
ft_atomic_write_csv(topics, "data/topics_codebook.csv")
coding_write_dictionary()
cat("Topic codebook normalised; canonical CSV and derived 31-field dictionary written.\n")
