# SERVER ####
server <- function(input, output, session) {
## INTRODUCTION ####
  observeEvent(input$go_to_metasem, {
    updateNavbarPage(session = session, inputId = "pages", selected = "MetaSEM")
  })
  observeEvent(input$go_to_metacor, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Meta-analysis")
  })
  
  # META-ANALYSIS ####
  # PAGE a ####
  # Basic info and moderators
  study_info <- reactive({c("download_date","doi","year","age_class","paper_id", "matrix_id", "author_et_al", "n", input$Moderators)})
  
  # Reactive list of choices
  outlist <- reactive({
    req(input$outcomeTopics)  # ensure it's available
    varlist$column_name[varlist$Topic == input$outcomeTopics]
  })
  
  # Dynamic UI generation
  output$dynamic_outcome_select <- renderUI({
    selectInput("metaOutcomes", "Select the specific outcome:",
                choices = outlist(),
                multiple = FALSE, selected = NULL)
  })
  
  # Selected outcome
  meta_outcome <- reactive({input$metaOutcomes})
  
  # Filter by year
  
  
  # Get the requested data
  dm <- reactive({
    # Get outcome variable
    out_vars <- input$metaOutcomes
    age_filter <- input$age_filter
    year_filter <- input$year_filter
    # Filter by age class if selected
    if (!is.null(input$age_filter)) {
      combined_data <- combined_data[combined_data$age_class %in% age_filter, ]
    }
    # Filter by year if selected
    if (!is.null(input$year_filter)) {
      combined_data <- combined_data[combined_data$year %in% year_filter, ]
    }
    # Compute metadata using your custom function
    dm <- getMETAdata(pred_vars, out_vars, combined_data, study_info())
    # Compute effect sizes
    dm <- escalc(measure = "ZCOR", ri = value, ni = n, data = dm)
    
    return(dm)
  })
  
  # Study_table
  metaStudyTab <- reactive({
    uniqueD <- dm()[!duplicated(dm()$matrix_id), ]
    data.frame(StudyID = uniqueD$matrix_id, 
               Authors = uniqueD$author_et_al,
               N = uniqueD$n)})
  output$metaStudy_table <- renderTable({metaStudyTab()})
  
  #PAGE b ####
  metaAnalysis <- eventReactive({input$meta_analysis},{
    req(dm())
    dm <- dm()
    # Loop for each skill
    outList <- list()
    metaCol <- c("b","cil","ciu","se","z","p",
                 "r","rcil","rciu","rpil","rpiu",
                 "tau2",
                 "k","N",
                 "q","qdf","qp")
    metaRes <- matrix(nrow = length(pred_vars), ncol = length(metaCol))
    colnames(metaRes) <- metaCol
    rownames(metaRes) <- pred_vars
    
    for (i in 1:length(pred_vars)) {
      di <- dm[dm$pred == pred_vars[i] & is.na(dm$yi) == F,]
      outList[[i]] <- rma.mv(yi = yi, V = vi, random = ~ 1 | paper_id / matrix_id,
                             test = "t", data = di)
      mres <- summary(outList[[i]])
      pred <- predict(mres, transf=transf.ztor)
      tau2 <- sum(mres$sigma2)
      metaRes[i,] <- c(mres$beta,mres$ci.lb,mres$ci.ub,mres$se,mres$zval,mres$pval,
                       pred$pred,pred$ci.lb,pred$ci.ub,pred$pi.lb,pred$pi.ub,
                       tau2,
                       nrow(di),sum(di$n[di$matrix_id %in% unique(di$matrix_id)]),
                       mres$QE,mres$QEdf,mres$QEp
      )
    }
    names(outList) <- pred_vars
    
    metaRes <- data.frame(metaRes)
    metaRes$skill = pred_vars
    
    list(metaRes = metaRes, outList = outList)
  })
  
  output$metaResults <- renderTable({
    metaRes <- metaAnalysis()$metaRes
    metaResTable <- data.frame(
      Skill = metaRes$skill,
      N = paste0(metaRes$N, " k = (", metaRes$k, ")"),
      r = format(round(metaRes$r, 3),nsmall=3),
      CI = paste0("[",format(round(metaRes$rcil,3),nsmall=3),"; ",
                  format(round(metaRes$rciu,3),nsmall=3),"]"),
      se = format(round(metaRes$se,3),nsmall=3),
      p = format(round(metaRes$p,4),3),
      q = paste0(round(metaRes$q), "(",metaRes$qdf,")",ifelse(metaRes$qp < .01, "*", "")),
      tau2 = format(round(metaRes$tau2,3),nsmall=3))
    # Replace factor levels
    metaResTable$Skill <- labels_app[as.character(metaResTable$Skill)]
    metaResTable
  })
  
  output$metaPlot <- renderPlot({
    outcomemeta <- meta_outcome()
    outcomelabel <- varlist$label[varlist$column_name == outcomemeta]
    metaRes <- metaAnalysis()$metaRes
    metaRes$skill <- labels_app[as.character(metaRes$skill)]
    ggplot(metaRes, aes(x = r, y = reorder(skill,length(pred_vars):1))) +
       geom_point(shape = 18,
                  color = "black",
                  size = 4) +
       geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0, linewidth = .8) +
       geom_vline(xintercept = c(.00), linetype = "dashed") +
       labs(x = paste0("Meta-analytical association with ", outcomelabel), y = "Skill") +
       theme_bw(base_size = 18) +
       theme(strip.text = element_text(size = 18)) +
       coord_cartesian(xlim = c(ifelse(min(metaRes$rcil) > -.05, -.15,
                                       min(metaRes$rcil)),
                                max(metaRes$rciu)))
  })
  
  #PAGE c ####
  output$forestPlot <- renderPlot({
    outList <- metaAnalysis()$outList
    x = input$forestSelected
    forest(outList[[x]], slab = paste0(outList[[x]]$data$author_et_al, " ",
    #                                    outList[[x]]$data$year, " ",
                                        outList[[x]]$data$matrix_id),
            header = T,
            main = x)
    # forest(outList[[x]])
  })
  
  # d. REPORT ####
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(descriptives = metaStudyTab(),
                     metaRes = metaAnalysis()$metaRes)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # META-SEM ####
  # PAGE a ####
  # Select vars
  target_vars <- reactive({
    # Combine all selections
    all_selections <- c(input$SEBdomains, input$SEBfacets, input$PersonalityTraits, input$Outcomes)
    # Remove NULLs or NA values
    clean_selections <- na.omit(all_selections)
    # Remove empty strings if needed
    clean_selections <- clean_selections[clean_selections != ""]
    clean_selections
  })
  # Basic info and moderators
  # study_info <- reactive({c("paper_id", "matrix_id", "author_et_al", "n", input$Moderators)})
  
  # Get the requested data
  metaD <- eventReactive({input$k_info}, {
    # Filter
    age_filter <- input$age_filter_masem
    year_filter <- input$year_filter_masem
    # Filter by age class if selected
    if (!is.null(input$age_filter_masem)) {
      combined_data <- combined_data[combined_data$age_class %in% age_filter, ]
    }
    # Filter by year if selected
    if (!is.null(input$year_filter_masem)) {
      combined_data <- combined_data[combined_data$year %in% year_filter, ]
    }
    # Get the data
    getSEMdata(target_vars(),combined_data,study_info())
  })
  
  # K_table
  output$K_table <- renderPrint({
    ktab<-pattern.na(metaD()$cor_matrices, show.na = FALSE) # k cors 
    rownames(ktab) <- paste0(1:nrow(ktab),".",rownames(ktab))
    colnames(ktab) <- paste0(1:ncol(ktab),".")
    ktab
  })
  # N_table
  output$N_table <- renderPrint({
    ntab<-pattern.n(metaD()$cor_matrices, metaD()$basic_info$n) # k cors 
    rownames(ntab) <- paste0(1:nrow(ntab),".",rownames(ntab))
    colnames(ntab) <- paste0(1:ncol(ntab),".")
    ntab
  }) 
  # Study_table
  StudyTab <- reactive({data.frame(
    # Dataset = metaD()$basic_info$download_date,
    StudyID = metaD()$basic_info$matrix_id,
    Authors = metaD()$basic_info$author_et_al,
    Year = as.character(metaD()$basic_info$year),
    Age = as.character(metaD()$basic_info$age_class),
    N = metaD()$basic_info$n,
    doi = metaD()$basic_info$doi)})
  output$Study_table <- renderTable({StudyTab()})
  
  # PAGE b ####
  # Run the first-stage SEM
  cfa1 <- eventReactive({input$s1_matrix}, {
    metaSEM::tssem1(metaD()$cor_matrices, metaD()$basic_info$n)
  })
  SumTable <- reactive({x <- round(metaSEM::vec2symMat(coef(cfa1(),"fixed"),diag=FALSE),3)
  dimnames(x) <- list(paste0(1:nrow(metaD()$cor_matrices[[1]]),".",rownames(metaD()$cor_matrices[[1]])),
                      paste0(1:nrow(metaD()$cor_matrices[[1]]),"."))
  x})
  # Summary table
  output$s1_table <- renderPrint({SumTable()}) 
  output$s1_figure <- renderPlot({corrplot(SumTable(), method = "ellipse", 
                                           addCoef.col = "black",
                                           tl.col = 'black', tl.srt = 45)})
  
  # PAGE c ####  
  # Fit the SEM model
  SEM <- eventReactive({input$fitSEM}, {
    model <- input$lavmodel
    varnames <- rownames(metaD()$cor_matrices[[1]]) # Get the names of the variables
    nvar <- nrow(metaD()$cor_matrices[[1]]) # Get their number
    RAM1 <- metaSEM::lavaan2RAM(model, obs.variables=varnames) # Generate RAM sintax from lavaan
    T0 <- metaSEM::create.Tau2(RAM=RAM1, RE.type="Diag", Transform="expLog", RE.startvalues=0.05)
    my.df <- metaSEM::Cor2DataFrame(metaD()$cor_matrices, metaD()$basic_info$n, acov = "weighted")
    # Fit the model using one-Stage MASEM with or without test of indirect effect
    M0 <- create.vechsR(A0=RAM1$A, S0=RAM1$S, F0 = RAM1$F)
    # ind <- mxAlgebra(beta1*beta2, name="IndirectEffect") 
    oss <- osmasem(model.name="One Stage MASEM", Mmatrix=M0, Tmatrix=T0, 
                   data=my.df, intervals.type = "z")
  })
  
  # summary of results
  sumfit <- reactive({
    req(SEM())
    summary(SEM(), fitIndices = TRUE)
  })
  
  # # Show results of one-stage MASEM
  # output$status <- renderText({
  #   if(sumfit()$statusCode == "OK"){"<font color=\"#008000\"><b>Converged!</b></font>"}
  #   else{"<font color=\"#FF0000\"><b>No convergence..., maybe rerunning the model helps.</b></font>"
  #   }
  # })
  
  # Fit indices and model fit
  output$fit_ui <- renderUI({
    if (sumfit()$ChiDoF == 0) {
      textOutput("fit_text")  # Show text
    } else {
      tableOutput("fit_table")  # Show table
    }
  })
  # Text output when ChiDoF == 0
  output$fit_text <- renderText({
    "This is a saturated model (df = 0), so the fit statistics are not informative, but the parameter estimates are (see below)."
  })
  # Table output when ChiDoF ≠ 0
  output$fit_table <- renderTable({
    data.frame(CFI = sumfit()$CFI,
               TLI = sumfit()$TLI,
               RMSEA = sumfit()$RMSEA,
               SRMR = round(osmasemSRMR(SEM()),3)
    )
  })
  # metaSEM results
  output$results <- renderPrint({sumfit()$parameters})
  # Table of model estimates
  output$SEMresults <- renderTable({
    parTab <- sumfit()$parameters
    parTab <- parTab[parTab$matrix %in% c("A0","S0"),
                     c("row", "matrix", "col", "Estimate", "Std.Error", "z value", "Pr(>|z|)")]
    colnames(parTab) <- c("outcome", "op", "predictor", "Estimate", "se", "z", "p")
    parTab[,4:ncol(parTab)] <- round(parTab[,4:ncol(parTab)],3)
    parTab$op <- ifelse(parTab$op == "A0", "~", "~~")
    parTab <- parTab[order(parTab$outcome, parTab$op), ]#, -df$var2
    parTab$p <- ifelse(parTab$p < .001, "<.001", parTab$p)
    data.frame(parTab)
    
    # ciTab <- sumfit()$CI
    # ciTab <- ciTab[ciTab$estimate %in% parTab$Estimate, ]
    # ciTab <- ciTab[!duplicated(ciTab$estimate),]
    # parTab$CI_lower <- ciTab$lbound[match(parTab$Estimate, ciTab$estimate)]
    # parTab$CI_upper <- ciTab$ubound
    # colnames(parTab) <- c("outcome", "op", "predictor", "Estimate", "se", "z", "CI_lower", "CI_upper", "p")
    # parTab[,4:ncol(parTab)] <- round(parTab[,4:ncol(parTab)],3)
    # parTab$op <- ifelse(parTab$op == "A0", "~", "~~")
    # parTab <- parTab[order(parTab$outcome, parTab$op), ]#, -df$var2
    # parTab$p <- ifelse(parTab$p < .001, "<.001", parTab$p)
    # data.frame(parTab)
  })
  
  # d. REPORT ####
  output$report_masem <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_masem.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_masem.Rmd")
      file.copy("report_masem.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        target_vars = target_vars(),
        metaD = metaD(),
        age_filter = input$age_filter_masem,
        year_filter = input$year_filter_masem,
        cfa1 = cfa1(),
        SumTable = SumTable()
        # ,
        # lavmodel = input$lavmodel,
        # sumfit = sumfit()
        )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}