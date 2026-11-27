# Small MetaSEM helpers. Source matrices, reference Ns and model settings are retained.
masem_require_versions <- function() {
  minimum <- c(metaSEM = "2.0", lavaan = "0.7-3")
  for (package in names(minimum)) {
    if (!requireNamespace(package, quietly = TRUE) || getNamespaceVersion(package) < minimum[[package]])
      stop("Install ", package, " >= ", minimum[[package]], " before running MetaSEM; restart R after updating.")
  }
  vapply(c("metaSEM", "lavaan", "OpenMx"), function(package)
    as.character(getNamespaceVersion(package)), character(1))
}

masem_model_variable_notes <- function(model, variables, labels) {
  # Replace only the app's comment block; retain the user's model and comments.
  lines <- strsplit(if (is.null(model)) "" else model, "\n", fixed = TRUE)[[1]]
  start <- which(lines == "# BEGIN SELECTED VARIABLES")
  end <- which(lines == "# END SELECTED VARIABLES")
  if (length(start) && length(end) && end[1] > start[1])
    lines <- lines[-seq.int(start[1], end[1])]
  if (length(lines) && lines[1] == "") lines <- lines[-1]
  names <- unname(labels[variables])
  names[is.na(names)] <- variables[is.na(names)]
  header <- c("# BEGIN SELECTED VARIABLES",
    "# These are the codes of the variables you selected (code = label):",
    if (length(variables)) paste0("# ", variables, " = ", names) else "# Select variables in the sidebar.",
    "# END SELECTED VARIABLES")
  paste(c(header, "", lines), collapse = "\n")
}

masem_prepare <- function(variables, data, fields) {
  result <- getSEMdata(variables, data, fields)
  if (!nrow(result$basic_info)) stop("No matrices contain correlations for this selection.")
  if (anyDuplicated(result$basic_info$matrix_id)) stop("Matrix identifiers must be unique.")
  if (any(!is.finite(result$basic_info$n) | result$basic_info$n <= 3))
    stop("Reference Ns must be finite and greater than 3. Source Ns have not been changed.")
  result$audit <- do.call(rbind, lapply(seq_along(result$cor_matrices), function(i) {
    matrix <- result$cor_matrices[[i]]
    values <- matrix[upper.tri(matrix)]
    if (any(!is.finite(values[!is.na(values)])) || any(abs(values[!is.na(values)]) > 1))
      stop("Invalid correlation in matrix ", result$basic_info$matrix_id[i], ". Source values have not been changed.")
    complete <- !anyNA(matrix)
    data.frame(Matrix = result$basic_info$matrix_id[i], `Available pairs` = sum(!is.na(values)),
      Status = if (!complete) "Incomplete; missing values preserved" else "Complete",
      check.names = FALSE)
  }))
  result$K <- Reduce(`+`, lapply(result$cor_matrices, function(matrix) 1L * !is.na(matrix)))
  result$N <- Reduce(`+`, Map(function(matrix, n) n * !is.na(matrix), result$cor_matrices, result$basic_info$n))
  index <- which(upper.tri(result$K), arr.ind = TRUE)
  result$pairs <- data.frame(Variable1 = rownames(result$K)[index[, 1]],
    Variable2 = colnames(result$K)[index[, 2]], k = result$K[index], N = result$N[index])
  result
}

masem_require_coverage <- function(data) {
  if (nrow(data$basic_info) < 2) stop("At least two contributing matrices are needed.")
  if (any(data$pairs$k == 0)) stop("Some selected variable pairs have no reported correlation. Reduce the selection before estimating.")
  invisible(TRUE)
}

masem_named_matrix <- function(matrix, labels) {
  names <- rownames(matrix)
  text <- unname(labels[names]); text[is.na(text)] <- names[is.na(text)]
  dimnames(matrix) <- list(text, text)
  matrix
}

masem_sample_table <- function(data) {
  info <- data$basic_info[order(data$basic_info$paper_id, data$basic_info$matrix_id), ]
  table <- info[, c("paper_id", "matrix_id", "author_et_al", "year", "age_class", "n", "title", "doi"), drop = FALSE]
  names(table) <- c("Paper", "Matrix", "Authors", "Year", "Age group", "Reference N", "Title", "DOI")
  table
}

masem_validate_ram <- function(RAM) {
  masem_require_versions()
  A <- metaSEM::as.mxMatrix(RAM$A)
  # Version 2 handles fixed paths and isolated variables; cycles remain unsupported.
  graph <- A$free | (!is.na(A$values) & A$values != 0)
  remaining <- seq_len(nrow(graph))
  while (length(remaining)) {
    roots <- remaining[rowSums(graph[remaining, remaining, drop = FALSE]) == 0]
    if (!length(roots)) stop("Feedback models are not supported by this correlation model; use an acyclic specification.")
    remaining <- setdiff(remaining, roots)
  }
  invisible(TRUE)
}

masem_fit <- function(data, user_model, auto_covariances, labels) {
  software <- masem_require_versions()
  masem_require_coverage(data)
  variables <- rownames(data$cor_matrices[[1]])
  syntax <- augment_lavaan_model(user_model, variables, fix_exo_var = TRUE,
    correlate_exogenous = auto_covariances, saturate_endogenous = auto_covariances)
  warnings <- character()
  result <- withCallingHandlers({
    RAM <- metaSEM::lavaan2RAM(syntax, obs.variables = variables)
    masem_validate_ram(RAM)
    T0 <- metaSEM::create.Tau2(RAM = RAM, RE.type = "Zero", Transform = "expLog", RE.startvalues = 0.05)
    frame <- metaSEM::Cor2DataFrame(data$cor_matrices, data$basic_info$n, acov = "weighted")
    M0 <- metaSEM::create.vechsR(A0 = RAM$A, S0 = RAM$S, F0 = RAM$F)
    model <- metaSEM::osmasem(model.name = "One Stage MASEM (auto_augmented)",
      Mmatrix = M0, Tmatrix = T0, data = frame, intervals.type = "z")
    summary <- summary(model, fitIndices = TRUE)
    list(model = model, summary = summary, RAM = RAM)
  }, warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  code <- result$model$mx.fit$output$status$code
  parameters <- result$summary$parameters
  result$ok <- identical(as.integer(code), 0L) && all(is.finite(parameters$Estimate)) &&
    all(is.finite(parameters$`Std.Error`)) && !isFALSE(result$summary$infoDefinite) &&
    !any(result$summary$seSuspect %in% TRUE)
  result$diagnostics <- unique(c(paste("Optimizer status:", code),
    if (!result$ok) "Convergence or parameter uncertainty is not validated; estimates and report are unavailable.",
    if (any(diag(result$model$mx.fit$Smatrix$result) < 0)) "Negative residual variance: the solution may be inadmissible; values have been retained.", warnings))
  result$data <- data; result$syntax <- syntax; result$labels <- labels
  result$user_model <- user_model; result$auto_covariances <- auto_covariances
  result$created <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  result$software <- software
  result
}

masem_pool <- function(data) {
  software <- masem_require_versions()
  masem_require_coverage(data)
  warnings <- character()
  model <- withCallingHandlers(metaSEM::tssem1(data$cor_matrices, data$basic_info$n),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
  matrix <- metaSEM::vec2symMat(coef(model, "fixed"), diag = FALSE)
  dimnames(matrix) <- dimnames(data$cor_matrices[[1]])
  code <- model$mx.fit$output$status$code
  if (!identical(as.integer(code), 0L) || any(!is.finite(matrix)))
    stop("The exploratory pooled matrix did not converge to a validated solution.")
  if (any(abs(matrix) > 1 + 1e-8)) stop("The pooled matrix contains correlations outside [-1,1]. No automatic repair has been applied.")
  list(model = model, matrix = matrix, diagnostics = unique(warnings), software = software)
}

masem_parameters <- function(run, details = FALSE) {
  table <- run$summary$parameters
  table <- table[table$matrix %in% c("A0", "S0"), , drop = FALSE]
  labels <- function(x) ifelse(x %in% names(run$labels), unname(run$labels[x]), x)
  number <- function(x) formatC(x, digits = 3, format = "f")
  result <- data.frame(Outcome = labels(table$row), Operator = ifelse(table$matrix == "A0", "~", "~~"),
    Predictor = labels(table$col), Estimate = number(table$Estimate),
    `95% CI` = paste0("[", number(table$Estimate - 1.96 * table$`Std.Error`), "; ",
      number(table$Estimate + 1.96 * table$`Std.Error`), "]"),
    p = ifelse(table$`Pr(>|z|)` < .001, "< .001", number(table$`Pr(>|z|)`)),
    SE = number(table$`Std.Error`), z = number(table$`z value`), check.names = FALSE)
  loading <- table$matrix == "A0" & table$col %in% setdiff(rownames(run$RAM$A), rownames(run$data$cor_matrices[[1]]))
  result$Operator[loading] <- "=~"
  result$Outcome[loading] <- labels(table$col[loading]); result$Predictor[loading] <- labels(table$row[loading])
  if (details) result[, c("Outcome", "Operator", "Predictor", "SE", "z")] else result[, 1:6]
}

masem_r2 <- function(run) {
  A <- run$model$mx.fit$Amatrix$result; S <- run$model$mx.fit$Smatrix$result
  variables <- rownames(run$RAM$A)
  endogenous <- which(rowSums(abs(A)) > 0)
  if (!length(endogenous)) return(data.frame(Variable = character(), `Residual variance` = numeric(), R2 = numeric(), Note = character(), check.names = FALSE))
  inverse <- solve(diag(nrow(A)) - A)
  total <- diag(inverse %*% S %*% t(inverse))
  value <- 1 - diag(S) / total
  notes <- rep("", length(total))
  if (any(Mod(eigen(A, only.values = TRUE)$values) > 1e-8)) {
    value[] <- NA_real_; notes[] <- "R² not reported for feedback models"
  }
  for (i in endogenous) if (any(abs(S[i, which(abs(A[i, ]) > 0)]) > 1e-8)) {
    value[i] <- NA_real_; notes[i] <- "Residual correlated with a predictor; R² not reported"
  }
  notes[is.finite(value) & (value < 0 | value > 1)] <- "Outside [0,1]; value retained"
  name <- ifelse(variables %in% names(run$labels), unname(run$labels[variables]), variables)
  data.frame(Variable = name[endogenous], `Residual variance` = round(diag(S)[endogenous], 4),
    R2 = round(value[endogenous], 3), Note = notes[endogenous], check.names = FALSE)
}

masem_fit_table <- function(run) {
  summary <- run$summary
  data.frame(Chi2 = summary$Chi, df = summary$ChiDoF, p = summary$p,
    CFI = summary$CFI, TLI = summary$TLI, RMSEA = summary$RMSEA,
    SRMR = metaSEM::osmasemSRMR(run$model))
}

masem_export_zip <- function(data, file) {
  folder <- tempfile("masem_export_"); dir.create(folder)
  files <- character()
  for (id in names(data$cor_matrices)) {
    if (!grepl("^[A-Za-z0-9_-]+$", id)) stop("Unsafe matrix identifier for export.")
    matrix <- data$cor_matrices[[id]]
    path <- file.path(folder, paste0(id, ".csv"))
    utils::write.csv(data.frame(variable = rownames(matrix), matrix, check.names = FALSE), path,
      row.names = FALSE, na = "", fileEncoding = "UTF-8")
    files <- c(files, path)
  }
  for (name in c("basic_info", "pairs", "audit")) {
    path <- file.path(folder, paste0(name, ".csv"))
    utils::write.csv(data[[name]], path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    files <- c(files, path)
  }
  settings <- file.path(folder, "selection_and_model.txt")
  text <- c("Variables:", data$selection$variables, "Age groups:", data$selection$ages,
    "Publication years:", data$selection$years, "User model:", data$user_model,
    "Model preview:", data$syntax, "Settings: acov=weighted; RE.type=Zero; intervals.type=z",
    paste("Automatic covariances:", data$auto_covariances),
    paste("Software:", paste(names(data$software), data$software, collapse = "; ")),
    "CSV matrices preserve missing correlations and canonical variable codes. Ns are coded reference sizes, not counts of unique people.")
  writeLines(enc2utf8(text), settings, useBytes = TRUE)
  zip::zip(file, files = c(files, settings), mode = "cherry-pick")
}
