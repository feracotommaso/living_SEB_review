# Presentation only: the original coding and downloaded data stay unchanged.
review_column_labels <- c(
  paper_id = "Code", author_et_al = "Authors", authors = "Full authors", year = "Year", title = "Title",
  main_objective = "Main objective", review_topics = "Topics",
  constructs_analyzed = "Analyzed constructs", study_design = "Study design",
  sample_context = "Sample context", seb_measurement = "SEB measurement",
  study_contribution = "Scientific contribution", seb_measure = "SEB instruments",
  journal = "Journal", country = "Country", open_data = "Open data",
  final_decision = "Inclusion", notes = "Coding notes", abstract = "Abstract")

review_display_text <- function(value, field, variables, topics) {
  if (!length(value) || is.na(value) || !nzchar(trimws(value))) return("Not reported")
  value <- as.character(value)
  if (field == "authors") return(gsub(" and ", "; ", value, fixed = TRUE))
  if (field == "final_decision") {
    labels <- c(review = "Review only", both = "Review and quantitative synthesis",
                meta = "Quantitative synthesis only")
    if (value %in% names(labels)) return(unname(labels[value]))
  }
  labels <- switch(field,
    constructs_analyzed = stats::setNames(variables$label, variables$column_name),
    review_topics = stats::setNames(topics$Topic_labs, topics$Topic),
    study_design = review_coding_options()$study_design, NULL)
  if (is.null(labels)) return(value)
  codes <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  text <- unname(labels[codes])
  text[is.na(text) | !nzchar(text)] <- codes[is.na(text) | !nzchar(text)]
  paste(unique(text), collapse = "; ")
}

review_browser_data <- function(data, columns, variables, topics, samples = NULL) {
  # Keep code first and preserve leading zeroes. DT can sort by any column.
  columns <- unique(c("paper_id", intersect(columns, names(review_column_labels))))
  columns <- intersect(columns, names(data))
  data <- as.data.frame(data[order(as.character(data$paper_id)), columns, drop = FALSE])
  # These legacy review fields may describe only one component of a multi-study paper.
  if (!is.null(samples)) for (field in intersect(c("seb_measure", "country"), columns)) {
    for (i in seq_len(nrow(data))) {
      values <- unique(as.character(samples[[field]][as.character(samples$paper_id) == data$paper_id[i]]))
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values)) data[[field]][i] <- paste(values, collapse = "; ")
    }
  }
  for (field in columns) {
    data[[field]] <- vapply(as.character(data[[field]]), review_display_text, character(1),
                          field = field, variables = variables, topics = topics)
  }
  names(data) <- unname(review_column_labels[columns])
  data
}

review_search_data <- function(data, query, variables, topics, samples = NULL) {
  if (is.null(query) || !nzchar(trimws(query))) return(data)
  # Search all authors, readable labels and abstracts, including hidden columns.
  display <- review_browser_data(data, names(review_column_labels), variables, topics, samples)
  text <- apply(display, 1, paste, collapse = " ")
  keep <- grepl(tolower(trimws(query)), tolower(text), fixed = TRUE)
  data[as.character(data$paper_id) %in% display$Code[keep], , drop = FALSE]
}
