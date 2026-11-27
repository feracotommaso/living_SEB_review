# Small helpers shared by the app and its exported report. Reference Ns and the
# existing REML model are retained; no participant or source value is changed.
meta_prepare_effects <- function(data) {
  data$analysis_status <- rep("Ready", nrow(data))
  data$analysis_status[is.na(data$value)] <- "Missing correlation"
  bad_r <- !is.na(data$value) & (!is.finite(data$value) | abs(data$value) >= 1)
  bad_n <- !is.finite(data$n) | data$n <= 3
  data$analysis_status[bad_r] <- "Correlation outside the Fisher-z range"
  data$analysis_status[bad_n] <- "Reference N must be finite and greater than 3"
  data$yi <- data$vi <- rep(NA_real_, nrow(data))
  ready <- data$analysis_status == "Ready"
  if (any(ready)) {
    effects <- metafor::escalc(measure = "ZCOR", ri = value, ni = n, data = data[ready, ])
    data$yi[ready] <- effects$yi
    data$vi[ready] <- effects$vi
  }
  data
}

meta_coverage <- function(data, skills) {
  rows <- lapply(skills, function(skill) {
    all <- data[data$pred == skill, , drop = FALSE]
    ready <- all[all$analysis_status == "Ready", , drop = FALSE]
    data.frame(skill = skill, papers = length(unique(ready$paper_id)),
      k = length(unique(ready$matrix_id)), N = sum(ready$n),
      unusable = sum(all$analysis_status != "Ready" & !is.na(all$value)))
  })
  do.call(rbind, rows)
}

meta_fit_models <- function(data, skills) {
  results <- meta_coverage(data, skills)
  for (name in c("b", "cil", "ciu", "se", "z", "p", "r", "rcil", "rciu",
                 "rpil", "rpiu", "tau2", "q", "qdf", "qp")) results[[name]] <- NA_real_
  results$status <- "Not estimable"
  results$note <- "Fewer than two usable matrices."
  models <- stats::setNames(vector("list", length(skills)), skills)
  for (i in seq_along(skills)) {
    if (results$k[i] < 2) next
    di <- data[data$pred == skills[i] & data$analysis_status == "Ready", , drop = FALSE]
    warnings <- character()
    fit <- tryCatch(withCallingHandlers({
      # Same model as before: effects nested in matrices nested in papers.
      model <- metafor::rma.mv(yi = yi, V = vi, random = ~ 1 | paper_id / matrix_id,
                              test = "t", method = "REML", data = di)
      mres <- summary(model)
      prediction <- predict(mres, transf = metafor::transf.ztor)
      if (any(!is.finite(c(mres$beta, mres$se, mres$ci.lb, mres$ci.ub))))
        stop("The model did not return finite estimates and confidence limits.")
      list(model = model, summary = mres, prediction = prediction)
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
    if (inherits(fit, "error")) {
      results$note[i] <- paste("Model failed:", conditionMessage(fit))
      next
    }
    mres <- fit$summary
    prediction <- fit$prediction
    results[i, c("b", "cil", "ciu", "se", "z", "p")] <-
      c(mres$beta, mres$ci.lb, mres$ci.ub, mres$se, mres$zval, mres$pval)
    results[i, c("r", "rcil", "rciu", "rpil", "rpiu")] <-
      c(prediction$pred, prediction$ci.lb, prediction$ci.ub, prediction$pi.lb, prediction$pi.ub)
    results[i, c("tau2", "q", "qdf", "qp")] <- c(sum(mres$sigma2), mres$QE, mres$QEdf, mres$QEp)
    results$status[i] <- if (length(warnings)) "Estimated with warning" else "Estimated"
    results$note[i] <- paste(unique(warnings), collapse = " ")
    models[[i]] <- fit$model
  }
  used <- data$analysis_status == "Ready" & data$pred %in% names(models)[!vapply(models, is.null, logical(1))]
  list(metaRes = results, outList = models, data = data, used_data = data[used, , drop = FALSE])
}

meta_sample_table <- function(data) {
  data <- data[data$analysis_status == "Ready", , drop = FALSE]
  data <- data[!duplicated(data$matrix_id), , drop = FALSE]
  data <- data[order(data$paper_id, data$matrix_id), , drop = FALSE]
  result <- data[, c("paper_id", "matrix_id", "author_et_al", "year", "age_class", "n", "title", "doi"), drop = FALSE]
  names(result) <- c("Paper", "Matrix", "Authors", "Year", "Age group", "Reference N", "Title", "DOI")
  result
}

meta_coverage_table <- function(data, skills, labels) {
  coverage <- meta_coverage(data, skills)
  data.frame(Skill = unname(labels[coverage$skill]), Papers = coverage$papers,
    Matrices = coverage$k, `Sum of reference Ns` = coverage$N,
    `Invalid values` = coverage$unusable, check.names = FALSE)
}

meta_results_table <- function(results, labels, details = FALSE) {
  number <- function(x, digits = 3) ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "—")
  ci <- ifelse(is.finite(results$r), paste0("[", number(results$rcil), "; ", number(results$rciu), "]"), "—")
  table <- data.frame(Skill = unname(labels[results$skill]), Papers = results$papers,
    `Matrices (k)` = results$k, `Sum of reference Ns` = results$N,
    r = number(results$r), `95% CI (r)` = ci, `SE (z)` = number(results$se),
    p = ifelse(is.na(results$p), "—", ifelse(results$p < .001, "< .001", number(results$p))),
    `Q (df)` = ifelse(is.finite(results$q), paste0(number(results$q, 2), " (", results$qdf, ")"), "—"),
    `Tau² (z)` = number(results$tau2), Status = results$status, check.names = FALSE)
  # Present the same saved results in a compact table or in the technical details.
  columns <- if (details) c("Skill", "SE (z)", "Tau² (z)", "Q (df)", "Status") else
    c("Skill", "r", "95% CI (r)", "p", "Papers", "Matrices (k)", "Sum of reference Ns")
  table[, columns, drop = FALSE]
}

meta_result_plot <- function(results, labels, outcome) {
  data <- results[is.finite(results$r), , drop = FALSE]
  if (!nrow(data)) return(NULL)
  data$Skill <- factor(unname(labels[data$skill]), levels = rev(unname(labels[results$skill])))
  ggplot2::ggplot(data, ggplot2::aes(x = r, y = Skill)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = rcil, xmax = rciu), width = .15) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(x = paste0("Correlation with ", outcome, " (r)"), y = NULL) +
    ggplot2::theme_bw(base_size = 14)
}

meta_forest_plot <- function(model, skill) {
  rows <- paste(model$data$author_et_al, model$data$year, paste0("[", model$data$matrix_id, "]"))
  metafor::forest(model, slab = rows, atransf = metafor::transf.ztor,
    xlab = "Correlation (r; Fisher z axis)", main = skill)
}
