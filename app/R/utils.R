# Shared analysis functions for the local workflow, papers, tutorials and Shiny app.
# This file is self-contained so publishing app/ includes every helper.

load_xlsx_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  tryCatch({
    download.file(url, destfile = temp_file, mode = "wb")
    readxl::read_excel(temp_file)
  }, error = function(e) {
    stop("Failed to download or read Excel file: ", e$message)
  })
}


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
    dplyr::select(pair, correlation)

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


# Exact full-name matching shared by the paper workflow and the deployed app.
# A construct may itself contain underscores; never split a correlation key on _.
correlation_pair_key <- function(a, b) paste(sort(c(a, b)), collapse = "_")

getSEMdata <- function(target_vars, combined_data, study_info) {
  target_vars <- sort(unique(target_vars))
  if (length(target_vars) < 2L) stop("Select at least two variables.")
  pairs <- t(combn(target_vars, 2))
  keys <- apply(pairs, 1, function(x) correlation_pair_key(x[[1]], x[[2]]))
  if (anyDuplicated(keys)) stop("Ambiguous correlation names; reconcile the variable codebook.")
  found <- keys %in% names(combined_data)
  pairs <- pairs[found, , drop = FALSE]; keys <- keys[found]
  matrices <- as.data.frame(combined_data[, keys, drop = FALSE])
  keep <- rowSums(!is.na(matrices)) > 0L
  matrices <- matrices[keep, , drop = FALSE]
  basic_info <- as.data.frame(combined_data[keep, study_info, drop = FALSE])
  correlation_matrices <- lapply(seq_len(nrow(matrices)), function(i) {
    result <- matrix(NA_real_, length(target_vars), length(target_vars), dimnames = list(target_vars, target_vars))
    for (j in seq_along(keys)) {
      result[pairs[j, 1], pairs[j, 2]] <- matrices[[j]][[i]]
      result[pairs[j, 2], pairs[j, 1]] <- matrices[[j]][[i]]
    }
    connected <- rowSums(!is.na(result)) > 0L
    diag(result)[connected] <- 1
    result
  })
  names(correlation_matrices) <- basic_info$matrix_id
  list(basic_info = basic_info, cor_matrices = correlation_matrices, matrices = matrices)
}

getMETAdata <- function(pred_vars, out_vars, combined_data, study_info) {
  lookup <- expand.grid(pred = sort(unique(pred_vars)), out = sort(unique(out_vars)), stringsAsFactors = FALSE)
  lookup <- lookup[lookup$pred != lookup$out, , drop = FALSE]
  lookup$key <- vapply(seq_len(nrow(lookup)), function(i) correlation_pair_key(lookup$pred[[i]], lookup$out[[i]]), character(1))
  if (anyDuplicated(lookup$key)) stop("Overlapping predictor/outcome sets give ambiguous pair roles.")
  lookup <- lookup[lookup$key %in% names(combined_data), , drop = FALSE]
  matrices <- as.data.frame(combined_data[, lookup$key, drop = FALSE])
  keep <- rowSums(!is.na(matrices)) > 0L
  basic <- as.data.frame(combined_data[keep, study_info, drop = FALSE])
  matrices <- matrices[keep, , drop = FALSE]
  parts <- lapply(seq_len(nrow(lookup)), function(i) data.frame(basic,
    variable = rep(lookup$key[[i]], nrow(basic)), value = matrices[[i]],
    pred = rep(lookup$pred[[i]], nrow(basic)), out = rep(lookup$out[[i]], nrow(basic)), check.names = FALSE))
  if (!length(parts)) return(data.frame(basic, variable = character(), value = numeric(), pred = character(), out = character()))
  do.call(rbind, parts)
}

#' Build a formatted parameter table from metaSEM results
sem_results_df <- function(sumfit, sem, metaD, labels_app = NULL, acov = "weighted") {
  # checks
  if (!requireNamespace("metaSEM", quietly = TRUE)) {
    stop("Package 'metaSEM' is required.")
  }
  req_fields <- c("parameters")
  miss <- setdiff(req_fields, names(sumfit))
  if (length(miss)) stop("sumfit is missing: ", paste(miss, collapse = ", "))
  if (is.null(metaD$cor_matrices) || is.null(metaD$basic_info$n)) {
    stop("metaD must have $cor_matrices and $basic_info$n.")
  }

  # data needed for tau id labels
  my.df <- metaSEM::Cor2DataFrame(metaD$cor_matrices, metaD$basic_info$n, acov = acov)

  # 1) fixed-effects rows (+ param_id matching metaSEM naming)
  keep_cols <- c("matrix","row","col","Estimate","Std.Error","z value","Pr(>|z|)")
  fixedRaw <- subset(sumfit$parameters, matrix %in% c("A0", "S0"), select = keep_cols)

  fixedRaw$param_id <- ifelse(
    fixedRaw$matrix == "A0",
    paste0(fixedRaw$col, "_",  fixedRaw$row),  # paths: predictor_outcome
    paste0(fixedRaw$row, "_", fixedRaw$col)    # variances/covariances: row_col
  )

  # 2) random-effects VarCorr (tau variances)
  tauMat  <- metaSEM::VarCorr(sem)              # VCV of random effects
  tau_diag <- diag(tauMat)

  # prefer ylabels from Cor2DataFrame; fallback to colnames if needed
  tau_ids <- my.df$ylabels
  if (length(tau_ids) != length(tau_diag) || anyNA(tau_ids)) {
    tau_ids <- colnames(tauMat)
  }
  if (length(tau_ids) != length(tau_diag)) {
    warning("Couldn't confidently name tau parameters; matching may be incomplete.")
    tau_ids <- paste0("param", seq_along(tau_diag))
  }

  tau_var <- stats::setNames(as.numeric(tau_diag), tau_ids)
  tau_sd  <- sqrt(tau_var)

  # 3) map tau back to fixed effects by name
  match_idx <- match(fixedRaw$param_id, names(tau_var))
  fixedRaw$tau_var <- tau_var[match_idx]
  fixedRaw$tau_sd  <- tau_sd[match_idx]

  # 4) format "tau (sd)" column
  fmt3 <- function(x) ifelse(is.na(x), NA, sprintf("%.3f", x))
  fixedRaw$Tau <- ifelse(
    is.na(fixedRaw$tau_var), "NaN",
    paste0(fmt3(fixedRaw$tau_var), " (", fmt3(fixedRaw$tau_sd), ")")
  )

  # 5) presentation table
  parTab <- subset(sumfit$parameters, matrix %in% c("A0","S0"), select = keep_cols)
  parTab$op       <- ifelse(parTab$matrix == "A0", "~", "~~")
  parTab$outcome  <- parTab$row
  parTab$predictor<- parTab$col

  zcrit <- 1.96
  parTab$CI_lower <- parTab$Estimate - zcrit * parTab$`Std.Error`
  parTab$CI_upper <- parTab$Estimate + zcrit * parTab$`Std.Error`
  parTab$CI95     <- paste0("[", round(parTab$CI_lower, 2), "; ",
                            round(parTab$CI_upper, 2), "]")

  is_resid_var <- parTab$matrix == "S0" & parTab$row == parTab$col
  parTab$op[is_resid_var]        <- "~~ (resid var)"
  parTab$predictor[is_resid_var] <- parTab$outcome[is_resid_var]

  parTab <- parTab[, c("matrix","outcome","op","predictor","Estimate","Std.Error","z value","Pr(>|z|)","CI95")]
  names(parTab) <- c("Matrix","Outcome","op","Predictor","Estimate","SE","z","p","CI_95")

  # merge Tau by unambiguous key (Matrix,row,col)
  parTab <- merge(
    parTab,
    fixedRaw[, c("matrix","row","col","Tau")],
    by.x = c("Matrix","Outcome","Predictor"),
    by.y = c("matrix","row","col"),
    all.x = TRUE,
    sort  = FALSE
  )

  # pretty formatting
  parTab$Estimate <- round(parTab$Estimate, 3)
  parTab$SE       <- round(parTab$SE, 3)
  parTab$z        <- round(parTab$z, 3)
  parTab$p <- ifelse(
    is.na(parTab$p), NA,
    ifelse(parTab$p < .001, "<.001", sprintf("%.3f", parTab$p))
  )

  # optional pretty labels
  if (!is.null(labels_app)) {
    # allow either named vector or list
    lab <- unlist(labels_app, use.names = TRUE)
    repl <- function(x) ifelse(x %in% names(lab), unname(lab[x]), x)
    parTab$Outcome   <- repl(parTab$Outcome)
    parTab$Predictor <- repl(parTab$Predictor)
  }

  names(parTab)[names(parTab) == "Tau"] <- "tau (sd)"

  # order for display
  op_order <- c("~","~~","~~ (resid var)")
  parTab$op <- factor(parTab$op, levels = op_order)
  parTab <- parTab[order(parTab$op, parTab$Outcome, parTab$Predictor), ]

  rownames(parTab) <- NULL
  parTab
}

augment_lavaan_model <- function(user_model, varnames,
                                 fix_exo_var = TRUE,
                                 correlate_exogenous = TRUE,
                                 saturate_endogenous = TRUE,
                                 include_headings = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  squish <- function(x) gsub("\\s+", " ", trimws(x))

  # 1) Split, strip comments
  lines <- unlist(strsplit(user_model %||% "", "\n"))
  lines <- trimws(gsub("#.*$", "", lines))
  lines <- lines[nchar(lines) > 0]

  # 2) Separate parts
  meas_raw <- lines[grepl("=~", lines)]
  meas_clean <- unique(squish(meas_raw))

  is_reg  <- grepl("(^|[^~])~([^~]|$)", lines) & !grepl("~~", lines) & !grepl("=~", lines)
  reg_raw <- lines[is_reg]

  cov_existing <- unique(squish(lines[grepl("~~", lines)]))

  # 3) Sanitize regressions
  sanitize_reg_line <- function(line) {
    parts <- strsplit(line, "~", fixed = TRUE)[[1]]
    if (length(parts) < 2) return("")
    lhs <- squish(parts[1])
    rhs <- paste(parts[-1], collapse = "~")
    terms <- strsplit(rhs, "\\+")[[1]]

    clean_terms <- vapply(terms, function(t) {
      t <- trimws(gsub("[()]", "", t))
      if (grepl("\\*", t)) t <- sub(".*\\*", "", t)
      t <- trimws(gsub("^[-+]", "", t))
      if (t %in% c("", "1")) return("")
      t
    }, character(1))

    clean_terms <- unique(clean_terms[clean_terms != ""])
    if (length(clean_terms) == 0) return("")
    squish(paste(lhs, "~", paste(clean_terms, collapse = " + ")))
  }

  reg_clean <- Filter(nzchar, unique(vapply(reg_raw, sanitize_reg_line, character(1))))

  # 4) Identify endogenous/exogenous
  lhs <- if (length(reg_clean)) trimws(sub("\\~.*$", "", reg_clean)) else character(0)
  endo <- unique(lhs)
  exo  <- setdiff(varnames, endo)

  # 4b) Parse indicators from measurement model
  parse_indicators <- function(line) {
    parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(character(0))
    rhs <- squish(parts[2])
    terms <- strsplit(rhs, "\\+")[[1]]
    inds <- vapply(terms, function(t) {
      t <- trimws(gsub("[()]", "", t))
      if (grepl("\\*", t)) t <- sub(".*\\*", "", t)
      t <- trimws(gsub("^[-+]", "", t))
      if (t %in% c("", "1")) return("")
      t
    }, character(1))
    unique(inds[nzchar(inds)])
  }
  indicator_items <- unique(unlist(lapply(meas_clean, parse_indicators)))

  # 5) Build auto-additions
  add_cov <- character(0)
  add_var <- character(0)

  # Exogenous variances fixed to 1 (not for indicators if measurement present)
  if (fix_exo_var && length(exo) > 0) {
    exo_to_fix <- if (length(meas_clean) > 0) setdiff(exo, indicator_items) else exo
    if (length(exo_to_fix) > 0) {
      add_var <- paste0(exo_to_fix, " ~~ 1*", exo_to_fix)
    }
  }

  # Exogenous–exogenous covariances
  if (correlate_exogenous && length(exo) > 1) {
    pe <- utils::combn(exo, 2)
    add_cov <- c(add_cov, paste0(pe[1,], " ~~ ", pe[2,]))
  }

  # Residual correlations among endogenous variables
  if (saturate_endogenous && length(endo) > 1) {
    pi <- utils::combn(endo, 2)
    add_cov <- c(add_cov, paste0(pi[1,], " ~~ ", pi[2,]))
  }

  add_cov <- setdiff(unique(squish(add_cov)), cov_existing)
  add_var <- setdiff(unique(squish(add_var)), cov_existing)

  # 6) Warn about unknown variables
  rhs_all <- if (length(reg_clean)) trimws(sub(".*~", "", reg_clean)) else character(0)
  rhs_terms <- trimws(unlist(strsplit(paste(rhs_all, collapse = " + "), "\\+")))
  used_vars <- unique(c(endo, rhs_terms))
  used_vars <- used_vars[nzchar(used_vars)]
  bad <- setdiff(used_vars, varnames)
  if (length(bad)) {
    warning("These variables are not in the current data selection: ",
            paste(bad, collapse = ", "))
  }

  # 7) Assemble with headings
  blocks <- list()

  # --- Measurement block ---
  if (length(meas_clean)) {
    blocks <- c(blocks,
                list(if (include_headings) "# MEASUREMENT MODEL" else NULL),
                list(meas_clean))
  }

  # --- Structural + user covariances ---
  user_block <- c(reg_clean, cov_existing)
  if (length(user_block)) {
    blocks <- c(blocks,
                list(if (include_headings) "# USER MODEL SPECIFICATION" else NULL),
                list(user_block))
  }

  # --- Generated covariances ---
  if (length(add_cov)) {
    blocks <- c(blocks,
                list(if (include_headings) "# APP-GENERATED COVARIANCES" else NULL),
                list(add_cov))
  }

  # --- Generated variances ---
  if (length(add_var)) {
    blocks <- c(blocks,
                list(if (include_headings) "# APP-GENERATED VARIANCES" else NULL),
                list(add_var))
  }

  paste(unlist(blocks), collapse = "\n")
}




make_osmasem_summary <- function(tab,
                                 skills,
                                 op = "~",
                                 p_cut = 0.05,
                                 p_star = 0.001,
                                 eff_cut = 0.10,
                                 digits_b = 2,
                                 digits_p = 3) {
  stopifnot(is.data.frame(tab),
            all(c("Predictor","op","Estimate","p") %in% names(tab)))
  # Which rows belong to the paths of interest?
  rows_op <- tab$op == op & !is.na(tab$Estimate)
  # Detect CI columns and build a CI string per row
  has_ci_str   <- "CI_95" %in% names(tab)
  has_ci_bounds <- all(c("CI_95_lb","CI_95_ub") %in% names(tab))
  ci_string <- rep(NA_character_, nrow(tab))
  if (has_ci_str) {
    ci_string <- tab$CI_95
  } else if (has_ci_bounds) {
    ci_string <- paste0("[", round(tab$CI_95_lb, digits_b), ", ",
                        round(tab$CI_95_ub, digits_b), "]")
  } else {
    ci_string <- NA_character_
  }
  # Selection rule: p < p_cut OR |b| ≥ eff_cut
  # selected <- rows_op & (tab$p < p_cut | abs(tab$Estimate) >= eff_cut)
  selected <- rows_op & tab$p < p_cut & abs(tab$Estimate) >= eff_cut
  # Groups
  is_skill <- tab$Predictor %in% skills
  skills_sel <- selected & is_skill
  traits_sel <- selected & !is_skill
  # Non-significant (by the same combined rule)
  nonsel <- rows_op & !selected
  # Formatting helpers
  fmt_b <- function(x) ifelse(is.na(x), NA_character_, formatC(x, digits = digits_b, format = "f"))
  fmt_p <- function(x) {
    ifelse(is.na(x), NA_character_,
           ifelse(x < 10^-digits_p, paste0("< ", formatC(10^-digits_p, digits = digits_p, format = "f")),
                  sub("^0\\.", ".", formatC(x, digits = digits_p, format = "f"))))
  }
  fmt_ci <- function(s) ifelse(is.na(s) | s == "", "NA", s)
  # Build text line for each selected row
  make_line <- function(idx) {
    b  <- fmt_b(tab$Estimate[idx])
    ci <- fmt_ci(ci_string[idx])
    p  <- tab$p[idx]
    p_txt <- ifelse(is.na(p), "",
                    ifelse(p < p_star, "; p < 0.001", paste0("; p = ", fmt_p(p))))
    paste0(tab$Predictor[idx], " (b = ", b, "; 95% CI ", ci, p_txt, ")")
  }
  skill_lines <- if (any(skills_sel)) vapply(which(skills_sel), make_line, "") else character(0)
  trait_lines <- if (any(traits_sel)) vapply(which(traits_sel), make_line, "") else character(0)
  # Counts
  N_skills <- sum(skills_sel, na.rm = TRUE)
  N_traits <- sum(traits_sel, na.rm = TRUE)
  # Max |b| among non-selected
  nonSigMaxBeta <- if (any(nonsel)) max(abs(tab$Estimate[nonsel]), na.rm = TRUE) else NA_real_
  # Return structured output
  list(
    N_skills = N_skills,
    N_traits = N_traits,
    skillText = skill_lines,
    traitText = trait_lines,
    nonSigMaxBeta = nonSigMaxBeta,
    selected_rows = which(selected),
    criteria = list(p_cut = p_cut, p_star = p_star,
                    eff_cut = eff_cut, op = op)
  )
}


# Review filtering and bibliography helpers used by the app.
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
    dplyr::select(-id)

  # Step 4: Review Table
  revInfo <- c("paper_id","author_et_al","year","title","review_topics","doi")
  revTab <- result[,revInfo]
  revTab$year <- as.character(revTab$year)
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
