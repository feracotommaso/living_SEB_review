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
