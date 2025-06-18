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