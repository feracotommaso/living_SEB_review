# SERVER ####
server <- function(input, output, session) {
## INTRODUCTION ----
  observeEvent(input$go_to_metasem,   {updateNavbarPage(session, "pages", "MetaSEM") })
  observeEvent(input$go_to_metacor,   { updateNavbarPage(session, "pages", "Meta-analysis") })
  observeEvent(input$go_to_review,    { updateNavbarPage(session, "pages", "Review") })
  observeEvent(input$go_to_res,       { updateNavbarPage(session, "pages", "Resources") })
  observeEvent(input$go_to_refs,      { updateNavbarPage(session, "pages", "About & Citation") })
  
  # META-ANALYSIS ----
  study_info <- reactive({c("download_date", "doi", "year", "age_class", "paper_id",
    "matrix_id", "author_et_al", "n", "title", input$Moderators)})
  outlist <- reactive({
    req(input$outcomeTopics)
    variable_search_choices(varlist[varlist$Topic == input$outcomeTopics, ])
  })
  output$dynamic_outcome_select <- renderUI({
    selectInput("metaOutcomes", "Select the specific outcome:", choices = outlist(),
                multiple = FALSE, selected = NULL)
  })
  meta_selection <- reactive({
    list(outcome = input$metaOutcomes,
      ages = sort(unique(as.character(input$age_filter))),
      years = sort(unique(as.character(input$year_filter))))
  })
  dm <- reactive({
    req(input$metaOutcomes)
    data <- combined_data
    if (length(input$age_filter)) data <- data[data$age_class %in% input$age_filter, ]
    if (length(input$year_filter)) data <- data[data$year %in% input$year_filter, ]
    meta_prepare_effects(getMETAdata(pred_vars, input$metaOutcomes, data, study_info()))
  })
  output$metaCoverage <- renderTable({ meta_coverage_table(dm(), pred_vars, labels_app) }, digits = 0)
  metaStudyTab <- reactive({ meta_sample_table(dm()) })
  output$metaStudy_table <- DT::renderDT({
    data <- metaStudyTab()
    data$DOI <- doi_links(data$DOI)
    table <- DT::datatable(data, rownames = FALSE, escape = setdiff(names(data), "DOI"),
      options = list(pageLength = 10, scrollX = TRUE, order = list(list(0, "asc"), list(1, "asc"))))
    table <- DT::formatRound(table, c("Year", "Reference N"), digits = 0, mark = "")
    table <- DT::formatStyle(table, names(data), minWidth = "75px", verticalAlign = "top")
    DT::formatStyle(table, "Title", minWidth = "300px", maxWidth = "400px", whiteSpace = "normal")
  }, server = FALSE)
  output$studycount_banner <- renderUI({
    coverage <- meta_coverage(dm(), pred_vars)
    messages <- character()
    if (any(coverage$k < 2)) messages <- c(messages,
      paste("Fewer than two usable matrices:", paste(labels_app[coverage$skill[coverage$k < 2]], collapse = ", ")))
    if (any(coverage$unusable > 0)) messages <- c(messages,
      "Some observed values cannot be analyzed. The CSV identifies invalid correlations or reference Ns; source values are retained.")
    if (!length(messages)) return(NULL)
    div(class = "alert alert-warning", lapply(messages, p))
  })
  output$downloadMetaData <- downloadHandler(
    filename = function() paste0(Sys.Date(), "_livingSEBmetaData.csv"),
    content = function(file) write.csv(dm(), file, row.names = FALSE, fileEncoding = "UTF-8"))

  # Save one complete analysis when the button is clicked.
  metaAnalysis <- eventReactive(input$meta_analysis, {
    run <- meta_fit_models(dm(), pred_vars)
    run$selection <- meta_selection()
    run$labels <- labels_app
    run$outcome_label <- unname(labels_app[input$metaOutcomes])
    run$created <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
    run
  })
  meta_current <- reactive({
    validate(need(isTruthy(input$meta_analysis), "Run the meta-analysis for the current selection."))
    run <- metaAnalysis()
    validate(need(identical(run$selection, meta_selection()),
      "Selection changed. Run the meta-analysis again to update these results."))
    run
  })
  # Outputs stay empty until ready; instructions/errors are shown once in the status area.
  meta_ready <- reactive({isTruthy(input$meta_analysis) &&
    identical(metaAnalysis()$selection, meta_selection())})
  output$meta_run_status <- renderUI({
    if (!isTruthy(input$meta_analysis)) return(helpText("Select an outcome and filters, then run the meta-analysis."))
    run <- metaAnalysis()
    if (!identical(run$selection, meta_selection()))
      return(div(class = "alert alert-warning", "Selection changed. Results and report are unavailable until you run the meta-analysis again."))
    div(class = "alert alert-info", paste("Analysis:", run$outcome_label, "—", run$created))
  })
  shinyjs::disable("meta_analysis")
  observe({
    req(input$metaOutcomes)
    shinyjs::toggleState("meta_analysis", condition = any(meta_coverage(dm(), pred_vars)$k >= 2))
  })
  shinyjs::disable("report")
  observe({
    valid <- FALSE
    if (isTruthy(input$meta_analysis)) {
      run <- metaAnalysis()
      valid <- identical(run$selection, meta_selection()) && any(is.finite(run$metaRes$r))
    }
    shinyjs::toggleState("report", condition = valid)
  })
  observeEvent(metaAnalysis(), {
    run <- metaAnalysis()
    skills <- names(run$outList)[!vapply(run$outList, is.null, logical(1))]
    updateSelectInput(session, "forestSelected", choices = stats::setNames(skills, unname(run$labels[skills])),
      selected = if (length(skills)) skills[1] else character())
  })
  output$metaResults <- DT::renderDT({
    req(meta_ready())
    run <- meta_current()
    data <- meta_results_table(run$metaRes, run$labels)
    table <- DT::datatable(data, rownames = FALSE,
      selection = "none", escape = TRUE,
      options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
    table <- DT::formatRound(table, c("Papers", "Matrices (k)", "Sum of reference Ns"), digits = 0, mark = "")
    DT::formatStyle(table, names(data), minWidth = "75px", whiteSpace = "nowrap")
  }, server = FALSE)
  output$meta_messages <- renderUI({
    req(meta_ready())
    run <- meta_current()
    notes <- run$metaRes[nzchar(run$metaRes$note), , drop = FALSE]
    if (!nrow(notes)) return(NULL)
    div(class = "alert alert-warning", tags$ul(lapply(seq_len(nrow(notes)), function(i)
      tags$li(tags$strong(paste0(run$labels[notes$skill[i]], ": ")), notes$note[i]))))
  })
  output$metaDetails <- renderTable({
    req(meta_ready())
    run <- meta_current()
    meta_results_table(run$metaRes, run$labels, details = TRUE)
  })
  output$metaPlot <- renderPlot({
    req(meta_ready())
    run <- meta_current()
    validate(need(any(is.finite(run$metaRes$r)), "No skill could be estimated for this selection."))
    meta_result_plot(run$metaRes, run$labels, run$outcome_label)
  })
  output$forestPlot <- renderPlot({
    req(meta_ready())
    run <- meta_current()
    req(input$forestSelected)
    model <- run$outList[[input$forestSelected]]
    validate(need(!is.null(model), "This skill has no estimated model. Select an available skill."))
    meta_forest_plot(model, run$labels[input$forestSelected])
  }, height = 650)

  # The report receives the saved run, never a mixture of old results and new inputs.
  output$report <- downloadHandler(filename = "report_meta.html", content = function(file) {
    run <- meta_current()
    validate(need(nrow(run$used_data) > 0, "No estimated model is available for this report."))
    report_dir <- tempfile("meta_report_")
    dir.create(report_dir)
    bib_file <- file.path(report_dir, "references.bib")
    filtered_bib <- generateBib(data = run$used_data, bibAll = bibAll, review = review)
    RefManageR::WriteBib(filtered_bib, file = bib_file, verbose = FALSE)
    temp_report <- file.path(report_dir, "report_meta.Rmd")
    file.copy("report_meta.Rmd", temp_report)
    file.copy("R/meta_analysis.R", file.path(report_dir, "meta_analysis.R"))
    file.copy("R/utils.R", file.path(report_dir, "utils.R"))
    file.copy("report.css", file.path(report_dir, "report.css"))
    rmarkdown::render(temp_report, output_file = file,
      params = list(run = run, bibfile = bib_file), envir = new.env(parent = globalenv()), quiet = TRUE)
  })

  # META-SEM ----
  target_vars <- reactive({sort(unique(na.omit(c(input$SEBdomains, input$SEBfacets,
    input$PersonalityTraits, input$Outcomes))))})
  observeEvent(target_vars(), {
    updateTextAreaInput(session, "lavmodel", value = masem_model_variable_notes(
      input$lavmodel, target_vars(), labels_app))
  }, ignoreNULL = FALSE)
  masem_selection <- reactive({list(variables = target_vars(),
    ages = sort(unique(as.character(input$age_filter_masem))),
    years = sort(unique(as.character(input$year_filter_masem))))})
  masem_data_run <- eventReactive(input$k_info, {
    tryCatch({
      selection <- masem_selection()
      data <- combined_data
      if (length(selection$ages)) data <- data[data$age_class %in% selection$ages, ]
      if (length(selection$years)) data <- data[data$year %in% selection$years, ]
      loaded <- masem_prepare(selection$variables, data, study_info())
      loaded$selection <- selection; loaded$labels <- labels_app
      loaded
    }, error = function(e) list(error = conditionMessage(e)))
  })
  metaD <- reactive({
    validate(need(isTruthy(input$k_info), "Select variables and click Get info."))
    loaded <- masem_data_run()
    validate(need(is.null(loaded$error), loaded$error))
    validate(need(identical(loaded$selection, masem_selection()), "Selection changed. Click Get info again."))
    loaded
  })
  masem_data_ready <- reactive({
    if (!isTruthy(input$k_info)) return(FALSE)
    loaded <- masem_data_run()
    is.null(loaded$error) && identical(loaded$selection, masem_selection())
  })
  output$masem_state <- renderUI({
    if (!isTruthy(input$k_info)) return(helpText("Choose at least two variables, then click Get info."))
    loaded <- masem_data_run()
    if (!is.null(loaded$error)) return(div(class = "alert alert-warning", loaded$error))
    if (!identical(loaded$selection, masem_selection()))
      return(div(class = "alert alert-warning", "Selection changed. Click Get info again; earlier matrices, models and reports are unavailable."))
    div(class = "alert alert-info", sprintf("Current data: %d articles, %d matrices, %d variables.",
      length(unique(loaded$basic_info$paper_id)), nrow(loaded$basic_info), length(loaded$selection$variables)))
  })
  output$masem_variables <- renderTable({
    req(masem_data_ready())
    loaded <- metaD()
    data.frame(Code = loaded$selection$variables, Label = unname(loaded$labels[loaded$selection$variables]))
  })
  output$K_table <- DT::renderDT({
    req(masem_data_ready())
    data <- metaD(); matrix <- masem_named_matrix(data$K, data$labels)
    DT::datatable(data.frame(Variable = rownames(matrix), matrix, check.names = FALSE), rownames = FALSE,
      selection = "none", options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
  }, server = FALSE)
  output$N_table <- DT::renderDT({
    req(masem_data_ready())
    data <- metaD(); matrix <- masem_named_matrix(data$N, data$labels)
    DT::datatable(data.frame(Variable = rownames(matrix), matrix, check.names = FALSE), rownames = FALSE,
      selection = "none", options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
  }, server = FALSE)
  output$zero_banner <- renderUI({
    req(masem_data_ready())
    data <- metaD(); pairs <- data$pairs
    messages <- character()
    if (any(pairs$k == 0)) {
      zero <- pairs[pairs$k == 0, ]
      shown <- head(zero, 6)
      messages <- c(messages, paste("No reported correlation for:",
        paste(paste(data$labels[shown$Variable1], data$labels[shown$Variable2], sep = " — "), collapse = "; "),
        if (nrow(zero) > 6) paste("and", nrow(zero) - 6, "more pairs; see the exported pair table.")))
    }
    if (any(pairs$k > 0 & pairs$k < 3)) messages <- c(messages, "Some pairs have fewer than three contributing matrices.")
    if (any(pairs$N > 0 & pairs$N < 1000)) messages <- c(messages, "Some pairs have a sum of reference Ns below 1000. This is an advisory warning, not an exclusion rule.")
    if (!length(messages)) return(NULL)
    div(class = "alert alert-warning", lapply(messages, p))
  })
  output$masem_audit <- renderTable({req(masem_data_ready()); metaD()$audit}, digits = 0)
  output$Study_table <- DT::renderDT({
    req(masem_data_ready())
    data <- masem_sample_table(metaD())
    data$DOI <- doi_links(data$DOI)
    table <- DT::datatable(data, rownames = FALSE, escape = setdiff(names(data), "DOI"),
      options = list(pageLength = 10, scrollX = TRUE, order = list(list(0, "asc"))))
    DT::formatRound(table, c("Year", "Reference N"), digits = 0, mark = "")
  }, server = FALSE)

  # The pooled matrix is exploratory and is not an input to the one-stage model.
  pooled_run <- eventReactive(input$s1_matrix, {
    tryCatch({
      data <- metaD(); result <- masem_pool(data)
      result$selection <- data$selection; result$labels <- data$labels
      result
    }, error = function(e) list(error = conditionMessage(e)))
  })
  pooled_current <- reactive({
    data <- metaD()
    validate(need(isTruthy(input$s1_matrix), "Estimate the exploratory pooled matrix first."))
    result <- pooled_run()
    validate(need(is.null(result$error), result$error))
    validate(need(identical(result$selection, data$selection), "Data selection changed. Estimate this matrix again."))
    result
  })
  pooled_ready <- reactive({
    if (!masem_data_ready() || !isTruthy(input$s1_matrix)) return(FALSE)
    result <- pooled_run()
    is.null(result$error) && identical(result$selection, masem_selection())
  })
  SumTable <- reactive({masem_named_matrix(pooled_current()$matrix, pooled_current()$labels)})
  output$s1_diagnostics <- renderUI({
    req(masem_data_ready())
    if (!isTruthy(input$s1_matrix)) return(helpText("Optionally estimate the exploratory pooled matrix using the button on the left."))
    result <- pooled_run()
    if (!is.null(result$error)) return(div(class = "alert alert-warning", result$error))
    if (!identical(result$selection, masem_selection()))
      return(helpText("Data selection changed. Estimate the exploratory pooled matrix again."))
    notes <- pooled_current()$diagnostics
    if (length(notes)) div(class = "alert alert-warning", lapply(notes, p))
  })
  output$s1_table <- DT::renderDT({
    req(pooled_ready())
    matrix <- round(SumTable(), 3)
    DT::datatable(data.frame(Variable = rownames(matrix), matrix, check.names = FALSE), rownames = FALSE,
      selection = "none", options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
  }, server = FALSE)
  output$s1_figure <- renderPlot({req(pooled_ready()); corrplot::corrplot(SumTable(), method = "ellipse", addCoef.col = "black",
    tl.col = "black", tl.srt = 45, number.cex = .8)})
  output$download_s1_figure_png <- downloadHandler(
    filename = function() paste0("correlation_plot_", Sys.Date(), ".png"),
    content = function(file) {
      matrix <- SumTable()
      png(file, width = 2000, height = 2000, res = 300); on.exit(dev.off())
      corrplot::corrplot(matrix, method = "ellipse", addCoef.col = "black", tl.col = "black", tl.srt = 45)
    })

  masem_specification <- reactive({list(user_model = input$lavmodel, auto_covariances = isTRUE(input$auto_saturate))})
  augmented_model <- reactive({
    data <- metaD()
    tryCatch({
      syntax <- augment_lavaan_model(input$lavmodel, data$selection$variables,
        correlate_exogenous = isTRUE(input$auto_saturate), saturate_endogenous = isTRUE(input$auto_saturate))
      masem_validate_ram(metaSEM::lavaan2RAM(syntax, obs.variables = data$selection$variables))
      syntax
    },
      error = function(e) {validate(need(FALSE, conditionMessage(e)))})
  })
  output$model_preview <- renderText({req(masem_data_ready()); augmented_model()})
  sem_run <- eventReactive(input$fitSEM, {
    tryCatch({
      data <- metaD()
      run <- masem_fit(data, input$lavmodel, isTRUE(input$auto_saturate), data$labels)
      run$selection <- data$selection; run$specification <- masem_specification()
      run
    }, error = function(e) list(error = conditionMessage(e)))
  })
  sem_attempt <- reactive({
    data <- metaD()
    validate(need(isTruthy(input$fitSEM), "Fit the model for the current data and syntax."))
    run <- sem_run()
    validate(need(is.null(run$error), run$error))
    validate(need(identical(run$selection, data$selection), "Data selection changed. Fit the model again."))
    validate(need(identical(run$specification, masem_specification()), "Model syntax or automatic covariances changed. Fit the model again."))
    run
  })
  sem_current <- reactive({
    run <- sem_attempt()
    validate(need(isTRUE(run$ok), paste(run$diagnostics, collapse = " ")))
    run
  })
  sem_ready <- reactive({
    if (!masem_data_ready() || !isTruthy(input$fitSEM)) return(FALSE)
    run <- sem_run()
    is.null(run$error) && identical(run$selection, masem_selection()) &&
      identical(run$specification, masem_specification()) && isTRUE(run$ok)
  })
  output$sem_diagnostics <- renderUI({
    req(masem_data_ready())
    if (!isTruthy(input$fitSEM)) return(helpText("Enter the model syntax, then click Fit model."))
    run <- sem_run()
    if (!is.null(run$error)) return(div(class = "alert alert-warning", run$error))
    if (!identical(run$selection, masem_selection()) || !identical(run$specification, masem_specification()))
      return(div(class = "alert alert-warning", "Data selection or model syntax changed. Fit the model again."))
    div(class = if (run$ok && length(run$diagnostics) == 1) "alert alert-info" else "alert alert-warning",
      lapply(run$diagnostics, p))
  })
  output$fit_ui <- renderUI({
    req(sem_ready())
    run <- sem_current()
    if (isTRUE(run$summary$ChiDoF == 0))
      return(helpText("Saturated model (df = 0): global fit indices are not informative. Inspect the estimates and uncertainty."))
    tableOutput("fit_table")
  })
  output$fit_table <- renderTable({req(sem_ready()); masem_fit_table(sem_current())}, digits = 3)
  output$SEMresults <- renderTable({req(sem_ready()); masem_parameters(sem_current())})
  output$SEMdetails <- renderTable({req(sem_ready()); masem_parameters(sem_current(), details = TRUE)})
  output$R2_table <- renderTable({req(sem_ready()); masem_r2(sem_current())}, digits = 3)

  export_masem_data <- reactive({
    data <- metaD()
    data$software <- masem_require_versions()
    data$user_model <- input$lavmodel; data$auto_covariances <- isTRUE(input$auto_saturate)
    data$syntax <- tryCatch(augmented_model(), error = function(e) paste("Preview unavailable:", conditionMessage(e)))
    data
  })
  output$downloadList <- downloadHandler(
    filename = function() paste0(Sys.Date(), "_livingSEB_SEM_data_list.rds"),
    content = function(file) saveRDS(export_masem_data(), file))
  output$downloadZip <- downloadHandler(
    filename = function() paste0(Sys.Date(), "_livingSEB_SEM_data.zip"),
    content = function(file) masem_export_zip(export_masem_data(), file), contentType = "application/zip")
  output$report_masem <- downloadHandler(filename = "report_masem.html", content = function(file) {
    run <- sem_current()
    run$pooled <- tryCatch(pooled_current(), error = function(e) NULL)
    folder <- tempfile("masem_report_"); dir.create(folder)
    bib <- file.path(folder, "references.bib")
    RefManageR::WriteBib(generateBib(data = run$data$basic_info, bibAll = bibAll, review = review), bib, verbose = FALSE)
    template <- file.path(folder, "report_masem.Rmd")
    file.copy("report_masem.Rmd", template)
    file.copy("R/masem_workflow.R", file.path(folder, "masem_workflow.R"))
    file.copy("R/utils.R", file.path(folder, "utils.R"))
    file.copy("report.css", file.path(folder, "report.css"))
    rmarkdown::render(template, output_file = file, params = list(run = run, bibfile = bib),
      envir = new.env(parent = globalenv()), quiet = TRUE)
  })

  for (button in c("downloadList", "downloadZip", "s1_matrix", "download_s1_figure_png", "fitSEM", "report_masem")) shinyjs::disable(button)
  observe({
    shinyjs::toggleState("k_info", condition = length(target_vars()) >= 2)
    data <- tryCatch(metaD(), error = function(e) NULL)
    ready <- !is.null(data)
    estimable <- ready && nrow(data$basic_info) >= 2 && all(data$pairs$k > 0)
    syntax_ready <- estimable && !is.null(tryCatch(augmented_model(), error = function(e) NULL))
    shinyjs::toggleState("downloadList", condition = ready)
    shinyjs::toggleState("downloadZip", condition = ready)
    shinyjs::toggleState("s1_matrix", condition = estimable)
    shinyjs::toggleState("fitSEM", condition = syntax_ready)
    shinyjs::toggleState("download_s1_figure_png", condition = isTruthy(input$s1_matrix) && !is.null(tryCatch(pooled_current(), error = function(e) NULL)))
    shinyjs::toggleState("report_masem", condition = isTruthy(input$fitSEM) && !is.null(tryCatch(sem_current(), error = function(e) NULL)))
  })

  # REVIEW ####
  ### Data selection ----
  # Choices for specific topics
  spec_top_list <- reactive({
    # Return empty vector instead of stopping; makes UI easier to manage.
    if (is.null(input$topics) || !length(input$topics)) return(character(0))
    sort(unique(topics_list$Topic_labs[topics_list$Broad_topic_labs %in% input$topics]))
  })
  # Dynamic UI generation
  output$dynamic_topic_select <- renderUI({
    # Show a helpful message until a broad topic is selected
    if (!length(spec_top_list())) {
      return(helpText("Select at least one broad topic to filter specific topics."))
    }
    selectizeInput(
      "filtTopic", "Filtered based on broad topic:",
      choices = spec_top_list(), multiple = TRUE,
      options = list(placeholder = "Type to filter…")
    )
  })
  ### Review results ----
  # Review table
  output$review_coding_filters <- renderUI({
    if(!'constructs_analyzed'%in%names(review))return(NULL)
    tokens<-function(f)unique(unlist(lapply(review[[f]],function(v)if(is.na(v))character() else trimws(strsplit(v,';',fixed=TRUE)[[1]]))))
    codes<-tokens('constructs_analyzed');codes<-intersect(admcol$column_name,codes)
    choices<-variable_search_choices(as.data.frame(admcol[admcol$column_name%in%codes,]))
    designs<-tokens('study_design');labels<-review_coding_options()$study_design
    design_choices<-stats::setNames(designs,ifelse(designs%in%names(labels),labels[designs],designs))
    tagList(
      p(sprintf('Detailed coding available for %d of %d articles. These filters search the coded articles; the complete list remains available without filters.',
        sum(!is.na(review$main_objective)&nzchar(review$main_objective)),nrow(review))),
      selectInput('review_constructs','Analyzed constructs (any selected):',choices=choices,multiple=TRUE),
      tags$details(tags$summary('More filters: context, measurement, contribution and design'),
        selectInput('review_context','Sample context:',choices=c(tokens('sample_context'),'Not yet assessed'),multiple=TRUE),
        selectInput('review_measurement','SEB measurement:',choices=c(tokens('seb_measurement'),'Not yet assessed'),multiple=TRUE),
        selectInput('review_contribution','Scientific contribution:',choices=c(tokens('study_contribution'),'Not yet assessed'),multiple=TRUE),
        selectInput('review_design','Study design:',choices=design_choices,multiple=TRUE)))
  })
  revSelection <- reactive({
    # Inputs from UI
    topLab    <- input$topics
    subLab    <- input$subtopics
    filtTopic <- input$filtTopic
    # Expand labels to actual topic names
    topics <- if (is.null(topLab) || !length(topLab)) NULL else
      unique(topics_list$Broad_topic[topics_list$Broad_topic_labs %in% topLab])
    # Combine subLab and filtTopic, then match to Topic
    allSubLab <- unique(c(subLab, filtTopic))
    subtopics <- if (is.null(allSubLab) || !length(allSubLab)) NULL else
      unique(topics_list$Topic[topics_list$Topic_labs %in% allSubLab])
    
    search_data <- review_search_data(review, input$review_search, admcol, topics_list, combined_data)
    makeReviewTable(subtopics = subtopics, topics = topics, data = search_data,
      constructs=input$review_constructs,context=input$review_context,measurement=input$review_measurement,
      contribution=input$review_contribution,design=input$review_design)
  })
  # Disable download if nothing to download
  shinyjs::disable("downloadFiltered")
  observe({
    rs <- revSelection()
    has_data <- !is.null(rs$filteredData) && NROW(rs$filteredData) > 0
    shinyjs::toggleState("downloadFiltered", condition = has_data)
  })
  
  review_articles <- reactive({
    data <- revSelection()$filteredData
    data[order(as.character(data$paper_id)), , drop = FALSE]
  })
  output$revTable <- DT::renderDT({
    articles <- review_articles()
    data <- review_browser_data(articles, input$review_columns, admcol, topics_list, combined_data)
    doi <- doi_code(articles$doi[match(data$Code, articles$paper_id)])
    data$Code <- vapply(seq_len(nrow(data)), function(i) {
      code <- data$Code[i]
      if (!nzchar(doi[i])) return(as.character(htmltools::htmlEscape(code)))
      as.character(tags$a(code,
        href = paste0("https://doi.org/", doi[i]),
        target = "_blank", rel = "noopener noreferrer",
        title = "Open article via DOI (new tab)",
        `aria-label` = paste("Open article", code, "via DOI (new tab)")))
    }, character(1))
    # Plain links use DT's built-in behavior: opening a DOI does not select the row.
    table <- DT::datatable(data, rownames = FALSE, escape = setdiff(names(data), "Code"),
      selection = list(mode = "single", target = "row"),
      class = "compact stripe hover",
      options = list(pageLength = 10, lengthMenu = c(10, 25, 50), scrollX = TRUE,
        scrollY = "45vh", scrollCollapse = TRUE, autoWidth = TRUE,
        searching = FALSE,
        order = list(list(0, "asc")),
        language = list(emptyTable = "No matching articles. Adjust the filters.")))
    # Scroll rows inside a bounded area: headers and the horizontal bar stay in view.
    # Minimum widths keep added columns readable; scrollX handles the extra width.
    widths <- stats::setNames(rep("220px", ncol(data)), names(data))
    compact <- c(Code = "55px", Authors = "140px", Year = "55px", Title = "300px")
    common <- intersect(names(compact), names(data))
    widths[common] <- compact[common]
    for (column in names(data)) {
      table <- DT::formatStyle(table, column, minWidth = widths[[column]],
        maxWidth = "360px", verticalAlign = "top", whiteSpace = "normal")
    }
    table
  }, server = FALSE)

  selected_review_id <- reactiveVal(NULL)
  observeEvent(input$revTable_rows_selected, {
    row <- input$revTable_rows_selected
    if (length(row) == 1 && row <= nrow(review_articles()))
      selected_review_id(as.character(review_articles()$paper_id[row]))
  })
  observeEvent(revSelection(), {
    selected_review_id(NULL)
    DT::selectRows(DT::dataTableProxy("revTable"), NULL)
  })
  # Changing columns redraws the table; clear the selection to keep it unambiguous.
  observeEvent(input$review_columns, {
    selected_review_id(NULL)
    DT::selectRows(DT::dataTableProxy("revTable"), NULL)
  })

  output$review_article <- renderUI({
    id <- selected_review_id()
    if (is.null(id)) return(helpText("Select an article in the table to view its details."))
    article <- review_articles()
    article <- article[as.character(article$paper_id) == id, , drop = FALSE]
    if (nrow(article) != 1) return(NULL)
    text <- function(field) review_display_text(article[[field]], field, admcol, topics_list)
    field <- function(label, name) tags$p(tags$strong(paste0(label, ": ")), text(name))
    # Article coding is shared across components; instruments and Ns are sample-level.
    samples <- combined_data[as.character(combined_data$paper_id) == id, , drop = FALSE]
    sample_text <- function(name) {
      if (!nrow(samples)) return(text(name))
      values <- unique(as.character(samples[[name]]))
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values)) paste(values, collapse = "; ") else "Not reported"
    }
    matrix_table <- NULL
    if (nrow(samples)) {
      matrix_data <- samples[, c("matrix_id", "n", "seb_measure", "seb_measure_type"), drop = FALSE]
      names(matrix_data) <- c("Matrix", "Reference N", "SEB instruments", "Instrument format/version")
      matrix_table <- tags$table(class = "table table-striped table-condensed",
        tags$thead(tags$tr(lapply(names(matrix_data), tags$th))),
        tags$tbody(lapply(seq_len(nrow(matrix_data)), function(i)
          tags$tr(lapply(matrix_data[i, ], function(value)
            tags$td(if (is.na(value) || !nzchar(as.character(value))) "Not reported" else as.character(value)))))))
    }
    doi <- text("doi")
    doi_url <- if (grepl("^https?://doi\\.org/", doi)) doi else
      if (grepl("^10\\.[0-9]{4,9}/", doi)) paste0("https://doi.org/", doi) else NULL
    bslib::card(
      bslib::card_header(paste("Article", id)),
      bslib::card_body(
        h3(text("title")),
        p(paste(text("author_et_al"), text("year"), sep = " — ")),
        field("Journal", "journal"), field("Inclusion", "final_decision"),
        if (!is.null(doi_url)) tags$a("Open publication (DOI)", href = doi_url, target = "_blank", rel = "noopener"),
        hr(), h4("Research focus"),
        field("Main objective", "main_objective"),
        field("Scientific contribution", "study_contribution"),
        field("Study design", "study_design"), field("Topics", "review_topics"),
        h4("Constructs, measures and context"),
        tags$details(open = if (nchar(text("constructs_analyzed")) < 600) "open" else NULL,
          tags$summary("Analyzed constructs"), p(text("constructs_analyzed"))),
        helpText("Constructs analyzed in the article; availability in the quantitative matrices may differ."),
        p(tags$strong("SEB instruments: "), sample_text("seb_measure")),
        p(tags$strong("Instrument format/version: "), sample_text("seb_measure_type")),
        field("SEB measurement", "seb_measurement"),
        field("Sample context", "sample_context"), p(tags$strong("Country: "), sample_text("country")),
        tags$details(tags$summary("Abstract"), p(text("abstract"))),
        if (nrow(samples)) tagList(h4("Included quantitative matrices"), matrix_table,
          helpText("Reference N is the coded sample size; available N can vary between correlations. Matrices may include documented sample overlaps.")),
        hr(), h4("Data access and coding notes"),
        field("Open data", "open_data"),
        if (nrow(samples)) tags$details(tags$summary("Notes by quantitative matrix"),
          lapply(seq_len(nrow(samples)), function(i) tagList(
            h5(as.character(samples$matrix_id[i])),
            p(review_display_text(samples$notes[i], "notes", admcol, topics_list))))) else
          tagList(field("Notes", "notes"), field("Quantitative exclusion reason", "exclusion_meta")),
        field("Additional information", "additional")))
  })
  output$revSummary <- renderUI({
    rs <- revSelection()
    n <- if (!is.null(rs$revTab)) NROW(rs$revTab) else 0
    tags$div(
      role = "status", `aria-live` = "polite",
      class = if (n > 0) "alert alert-info" else "alert alert-warning",
      if (n > 0) sprintf("Found %d records matching your filters.", n)
      else "No matching records. Adjust the selected filters."
    )
  })
  ### Clean selection ---
  observeEvent(input$clean_rev, {
    updateTextInput(session, "review_search", value = "")
    updateSelectInput(session, "topics",    selected = character(0))
    updateSelectInput(session, "subtopics", selected = character(0))
    for(id in c('review_constructs','review_context','review_measurement','review_contribution','review_design'))
      updateSelectInput(session,id,selected=character(0))
    if ("filtTopic" %in% names(input)) {
      updateSelectInput(session, "filtTopic", selected = character(0))
    }
  })
  ### Data download ----
  output$downloadFiltered <- downloadHandler(
    filename = function() sprintf("%s_topicFilteredSebStudies.csv", Sys.Date()),
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      dat <- revSelection()$filteredData
      validate(need(NROW(dat) > 0, "Nothing to download."))
      write.csv(dat, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # CITATION ####
  # Show APA (keeps line breaks)
  output$apa_cite <- renderUI({
    shiny::markdown(apa_text)
  })
  
  # Show BibTeX (preformatted)
  output$bib_cite <- renderText({
    bib_text
  })
  
  # Optional: let users download the .bib
  output$download_bib <- downloadHandler(
    filename = function() "livingSEB.bib",
    content  = function(file) writeLines(bib_text, file, useBytes = TRUE)
  )
  
}
