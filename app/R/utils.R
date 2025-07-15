data_combine <- function(matrix_path, basic_info_data) {
  # Extract matrix_id from the file name (assuming the file name is "matrix_id.xlsx")
  matrix_id <- tools::file_path_sans_ext(basename(matrix_path))
  
  # Read correlation matrix data
  data <- read_excel(matrix_path)
  
  # Extract correlation matrix (assuming the first column is row names)
  correlation_matrix <- data %>%
    as.data.frame()
  rownames(correlation_matrix) <- colnames(correlation_matrix) 
  
  # Convert correlation matrix to long format
  long_data <- correlation_matrix %>%
    rownames_to_column(var = "var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    filter(var1 != var2) %>%  # Remove self-correlations
    mutate(
      # Ensure consistent variable pair naming order
      pair = pmap_chr(list(var1, var2), ~ paste(sort(c(..1, ..2)), collapse = "_"))
    ) %>%
    distinct(pair, .keep_all = TRUE) %>%  # Remove duplicate pairs
    select(pair, correlation)
  
  # Convert long format to wide format
  wide_data <- long_data %>%
    pivot_wider(names_from = pair, values_from = correlation)
  
  # Extract basic info corresponding to this matrix_id
  basic_info <- basic_info_data %>%
    filter(matrix_id == !!matrix_id)
  
  # Merge basic info with correlation data
  final_data <- basic_info %>%
    bind_cols(wide_data)
  
  return(final_data)
}


getSEMdata <- function(target_vars, combined_data, study_info){
  target_vars <- sort(target_vars)
  # Create a regex pattern to match correlations containing at least one of the target correlations
  pattern <- paste(combn(target_vars, 2, FUN = function(x) paste(x, collapse = "_")), collapse = "|")
  
  # Select only the study info + relevant correlations
  basic_info <- combined_data[,study_info]
  matrices <- combined_data %>% select(matches(pattern))
  
  # Remove completely empty rows
  basic_info <- basic_info[rowSums(is.na(matrices))!=ncol(matrices),]
  matrices <- matrices[rowSums(is.na(matrices))!=ncol(matrices),]
  
  # # Function to convert a row into a correlation matrix
  # create_correlation_matrix <- function(row) {
  #   # Convert row to a named vector
  #   cor_values <- as.numeric(row)
  #   names(cor_values) <- colnames(matrices)
  #   
  #   # Create an empty matrix
  #   cor_matrix <- matrix(NA, nrow = length(target_vars), ncol = length(target_vars), dimnames = list(target_vars, target_vars))
  #   
  #   # Fill the matrix using the extracted correlations
  #   for (pair in names(cor_values)) {
  #     vars <- strsplit(pair, "_")[[1]]  # Extract variable names from column names
  #     cor_matrix[vars[1], vars[2]] <- cor_values[pair]
  #     cor_matrix[vars[2], vars[1]] <- cor_values[pair]  # Fill both symmetric positions
  #   }
  #   
  #   diag(cor_matrix) <- 1  # Set diagonal to 1 (self-correlations)
  #   
  #   return(cor_matrix)
  # }
  
  create_correlation_matrix <- function(row) {
    # Convert row to a named vector
    cor_values <- as.numeric(row)
    names(cor_values) <- colnames(matrices)
    
    # Create an empty matrix with NA values
    cor_matrix <- matrix(NA, nrow = length(target_vars), ncol = length(target_vars), dimnames = list(target_vars, target_vars))
    
    # Fill the matrix using the extracted correlations
    for (pair in names(cor_values)) {
      vars <- strsplit(pair, "_")[[1]]  # Extract variable names from column names
      
      # Only assign values if both variables are present
      if (all(vars %in% target_vars)) {
        cor_matrix[vars[1], vars[2]] <- cor_values[pair]
        cor_matrix[vars[2], vars[1]] <- cor_values[pair]  # Fill both symmetric positions
      }
    }
    
    # Set diagonal to 1 only for variables that have at least one correlation
    for (var in target_vars) {
      if (any(!is.na(cor_matrix[var, ]))) {  
        cor_matrix[var, var] <- 1
      }
    }
    
    return(cor_matrix)
  }
  # Apply function to each row, storing results in a named list (THIS FUNCTION IS IN UTILS)
  correlation_matrices <- map(1:nrow(matrices), ~ create_correlation_matrix(matrices[.x, ]))
  names(correlation_matrices) <- basic_info$matrix_id  # Name the list using matrix_id
  
  return(list(basic_info = basic_info,
              cor_matrices = correlation_matrices,
              matrices = matrices))
}


getMETAdata <- function(pred_vars, out_vars, combined_data, study_info){
  pred_vars <- sort(pred_vars)
  out_vars <- sort(out_vars)
  # Create all possible pairwise combinations (pred_var with out_var)
  pairs <- expand.grid(pred_vars, out_vars, stringsAsFactors = FALSE)
  # Convert each pair to "var1_var2" format
  pair_strings <- apply(pairs, 1, function(x) c(paste(x, collapse = "_"), paste(rev(x), collapse = "_")))
  # Flatten and create regex pattern
  pattern <- paste(pair_strings, collapse = "|")
  # Select only the study info + relevant correlations
  basic_info <- combined_data[,study_info]
  meta_effects <- combined_data %>% select(matches(pattern))
  # Remove completely empty rows
  basic_info <- basic_info[rowSums(is.na(meta_effects))!=ncol(meta_effects),]
  meta_effects <- meta_effects[rowSums(is.na(meta_effects))!=ncol(meta_effects),]
  # Merge again
  data <- cbind.data.frame(basic_info,meta_effects)
  dlong <- reshape2::melt(data, id.vars = study_info)
  dlong$pred <- sapply(dlong$variable, function(x) {
    pred_vars[pred_vars %in% unlist(strsplit(as.character(x), "_"))]
  })
  # Extract outcomes
  dlong$out <- sapply(dlong$variable, function(x) {
    out_vars[out_vars %in% unlist(strsplit(as.character(x), "_"))]
  })
  return(dlong)
}

makeReviewTable <- function(topics = NULL, subtopics = NULL, data = review, topics_df = topics_list) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Step 1: Expand the review_topics into separate rows
  review_expanded <- data %>%
    mutate(id = row_number()) %>%
    separate_rows(review_topics, sep = ";\\s*") %>%
    mutate(review_topics = str_trim(review_topics))
  
  # Step 2: Join with topics_list to get Broad_topic
  review_with_broad <- review_expanded %>%
    left_join(topics_df, by = c("review_topics" = "Topic"))
  
  # Step 3: Apply correct filtering logic
  filtered <- review_with_broad %>%
    filter(
      (is.null(topics) & is.null(subtopics)) |
      (!is.null(topics) & is.null(subtopics) & Broad_topic %in% topics) |
      (is.null(topics) & !is.null(subtopics) & review_topics %in% subtopics) |
      (!is.null(topics) & !is.null(subtopics) & Broad_topic %in% topics & review_topics %in% subtopics)
    )
  
  # Step 4: Return original dataframe rows based on matched IDs
  result <- data %>%
    mutate(id = row_number()) %>%
    filter(id %in% filtered$id) %>%
    select(-id)
  
  # Step 4: Review Table
  revInfo <- c("paper_id","author_et_al","year","title","review_topics","doi")
  revTab <- result[,revInfo]
  
  return(list(filteredData = result,
              revTab = revTab))
}

generateBib <- function(bibAll = bibAll, data = NULL) {
  df_unique <- data[!duplicated(data$paper_id), ]
  
  # Clean titles from your dataset
  titles_df <- trimws(tolower(df_unique$title))
  
  # Clean titles from BibEntry object
  titles_bib <- tolower(sapply(bibAll, function(x) x$title))
  titles_bib <- as.character(titles_bib)
  
  # Titles that do not match exactly
  unmatched_titles <- titles_df[titles_df %in% titles_bib == FALSE]
  
  # Fuzzy matching: only unmatched vs all BibTeX titles
  dist_matrix <- stringdistmatrix(unmatched_titles, titles_bib, method = "osa")
  
  # Get best match (lowest distance) for each unmatched title
  closest_match_index <- apply(dist_matrix, 1, which.min)
  closest_distances <- apply(dist_matrix, 1, min)
  
  # Set a distance threshold for fuzzy match acceptance
  threshold <- 20
  
  # Build a data frame of fuzzy matches
  fuzzy_matches <- data.frame(
    original_title = unmatched_titles,
    matched_bib_title = sapply(closest_match_index, function(i) bibAll[[i]]$title), #titles_bib[closest_match_index],
    distance = closest_distances,
    stringsAsFactors = FALSE
  )
  
  # Filter to only strong fuzzy matches
  fuzzy_matches <- fuzzy_matches[fuzzy_matches$distance <= threshold, ]
  
  # Exact matches
  titles_exact <- titles_df[titles_df %in% titles_bib]
  
  # Fuzzy matches — use the matched bib title column
  titles_fuzzy <- tolower(fuzzy_matches$matched_bib_title)
  
  # Combine all matched titles
  all_matched_titles <- unique(c(titles_exact, titles_fuzzy))
  
  # Filter the bib
  matched <- titles_bib %in% all_matched_titles
  filtered_bib <- bibAll[matched]
  
  return(filtered_bib)
}

generateBib(data = dm, bibAll = bibAll)
