# SERVER ####
server <- function(input, output, session) {
## INTRODUCTION ----
  observeEvent(input$go_to_metasem,   {updateNavbarPage(session, "pages", "MetaSEM") })
  observeEvent(input$go_to_metacor,   { updateNavbarPage(session, "pages", "Meta-analysis") })
  observeEvent(input$go_to_review,    { updateNavbarPage(session, "pages", "Review") })
  observeEvent(input$go_to_res,       { updateNavbarPage(session, "pages", "Resources") })
  observeEvent(input$go_to_refs,      { updateNavbarPage(session, "pages", "About & Citation") })
  
  # META-ANALYSIS ----
  # PAGE a ----
  ### Data selection ----
  # Basic info and moderators
  study_info <- reactive({c("download_date","doi","year","age_class","paper_id", "matrix_id", "author_et_al", "n", "title", input$Moderators)})
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
  ### Data summary ----
  # ---- Check study count
  too_few_studies <- reactive({
    d <- dm()
    if (is.null(d)) return(TRUE)
    k <- length(unique(d$matrix_id))   # number of unique studies
    k <= 1
  })
  # ---- Red banner UI
  output$studycount_banner <- renderUI({
    if (!too_few_studies()) return(NULL)
    
    tags$div(
      class = "alert alert-danger",
      role = "alert",
      tags$strong("STOP — insufficient data for meta-analysis."),
      tags$br(),
      "Only ", length(unique(dm()$matrix_id)), " study/studies available. ",
      "Meta-analysis requires at least 2 studies.",
      tags$br(),
      "Please adjust your outcome selection or filters before proceeding."
    )
  })
  # Study_table
  metaStudyTab <- reactive({
    uniqueD <- dm()[!duplicated(dm()$matrix_id), ]
    data.frame(StudyID = uniqueD$matrix_id, 
               Authors = uniqueD$author_et_al,
               Year = as.character(uniqueD$year),
               Age = uniqueD$age_class,
               N = as.character(uniqueD$n),
               doi = uniqueD$doi
               )})
  output$metaStudy_table <- renderTable({
    rbind(metaStudyTab(),
          c("Total","","","",sum(as.numeric(metaStudyTab()$N)),""))
    })
  ### Data download ----
  # Downloadable csv of meta-analysis dataset
  output$downloadMetaData <- downloadHandler(
    filename = function() {
      paste(today(),"_livingSEBmetaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dm(), file, row.names = FALSE)
    }
  )
  
  #PAGE b ####
  ### Meta analysis ----
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
  ### Output ----
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
  meta_outcome_selected <- eventReactive(input$meta_analysis, {
    input$metaOutcomes
  })
  output$metaPlot <- renderPlot({
    outcomemeta <- meta_outcome_selected()
    outcomelabel <- varlist$label[varlist$column_name == outcomemeta]
    metaRes <- metaAnalysis()$metaRes
    metaRes$skill <- labels_app[as.character(metaRes$skill)]
    metaRes$CI <- paste0("[",format(round(metaRes$rcil,3),nsmall=3),"; ",
                         format(round(metaRes$rciu,3),nsmall=3),"]")
    ggplot(metaRes, aes(x = r, y = reorder(skill,length(pred_vars):1))) +
       geom_point(shape = 18,
                  color = "black",
                  size = 4) +
       geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0, linewidth = .8) +
       geom_vline(xintercept = c(.00), linetype = "dashed") +
       labs(x = paste0("Meta-analytical association with ", outcomelabel), y = "Skill") +
       theme_bw(base_size = 25) +
       # theme(strip.text = element_text(size = 18)) +
       annotate("text", x = metaRes$r,
                y = length(pred_vars):1+.20,
                label = paste0(round(metaRes$r,3)," ",metaRes$CI), 
                hjust = "center", size = 5) +
       coord_cartesian(xlim = c(ifelse(min(metaRes$rcil) > -.05, -.15,
                                       min(metaRes$rcil)),
                                 max(metaRes$rciu)))
  })
  
  #PAGE c ####
  ### Forest plots ----
  output$forestPlot <- renderPlot({
    outList <- metaAnalysis()$outList
    x = input$forestSelected
    forest(outList[[x]], 
           slab = outList[[x]]$data$author_et_al,
           ilab = cbind(outList[[x]]$data$year,
                        outList[[x]]$data$matrix_id),
           ilab.lab = c("Year","ID"),
           header = T,
           main = paste0(x))
  })
  
  # PAGE d. REPORT ####
  output$report <- downloadHandler(
    filename = "report_meta.html",
    
    content = function(file) {
      # Generate filtered bib based on current data
      filtered_bib <- generateBib(data = dm(), bibAll = bibAll)
      #filtered_bib_path <- tempfile(fileext = ".bib")
      filtered_bib_path <- file.path(tempdir(), "filtered_refs.bib")
      RefManageR::WriteBib(filtered_bib, file = filtered_bib_path)
      
      # Copy the report template to a temporary location
      tempReport <- file.path(tempdir(), "report_meta.Rmd")
      file.copy("report_meta.Rmd", tempReport, overwrite = TRUE)
      
      # Parameters passed to Rmd
      params <- list(
        descriptives = metaStudyTab(),
        metaRes = metaAnalysis()$metaRes,
        outcome = meta_outcome(),
        outlist = metaAnalysis()$outList,
        dm = dm(),
        bibfile = filtered_bib_path
      )
      
      # Render the report
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # META-SEM ####
  # PAGE a ####
  ### Data selection ----
  target_vars <- reactive({
    # Combine all selections
    all_selections <- c(input$SEBdomains, input$SEBfacets, input$PersonalityTraits, input$Outcomes)
    # Remove NULLs or NA values
    clean_selections <- na.omit(all_selections)
    # Remove empty strings if needed
    clean_selections <- clean_selections[clean_selections != ""]
    clean_selections
  })
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
  ### Data summary ----
  # K_table
  Ktab <- reactive({
    req(metaD())
    pattern.na(metaD()$cor_matrices, show.na = FALSE)  # counts of studies per correlation
  })
  output$K_table <- renderPrint({
    ktab <- Ktab()
    rownames(ktab) <- paste0(1:nrow(ktab),".",rownames(ktab))
    colnames(ktab) <- paste0(1:ncol(ktab),".")
    ktab
  })
  # ---- Find zero-coverage pairs
  zero_pairs <- reactive({
    ktab <- Ktab()
    idx <- which(ktab == 0, arr.ind = TRUE)
    if (nrow(idx) == 0) return(NULL)
    data.frame(
      var1 = rownames(ktab)[idx[,1]],
      var2 = colnames(ktab)[idx[,2]],
      stringsAsFactors = FALSE
    )
  })
  zero_any <- reactive({ !is.null(zero_pairs()) })
  # ---- Red banner UI
  output$zero_banner <- renderUI({
    if (!zero_any()) return(NULL)
    pairs <- zero_pairs()
    
    # Show a concise list of the first few offending pairs
    max_show <- 6
    n <- nrow(pairs)
    shown <- head(pairs, max_show)
    pair_labels <- paste0(shown$var1, " \u2013 ", shown$var2)
    extra <- if (n > max_show) paste0(" +", n - max_show, " more") else ""
    tags$div(
      class = "alert alert-danger",
      role = "alert",
      tags$strong("STOP — some correlations have ZERO coverage (k = 0)."),
      tags$br(),
      HTML(paste0("No studies report: ", paste(pair_labels, collapse = ", "), extra, ".")),
      tags$br(),
      "Please adjust your variable selection or filters before proceeding."
    )
  })
  # Warnings
  observeEvent(metaD(), {
    req(metaD())
    ktab<-pattern.na(metaD()$cor_matrices, show.na = FALSE)
    if (sum(ktab<3)>0) {
      showNotification("Some correlations will be based on few observations (< 3). Please proceed cautiously",
                       type = "warning", duration = 6)
    }
    if (sum(ktab==0)>0) {
      showNotification("Some correlations will be based on ZERO observations. Please DO NOT proceed",
                       type = "error", duration = 6)
    }
  }, ignoreInit = TRUE)
  # N_table
  output$N_table <- renderPrint({
    ntab<-pattern.n(metaD()$cor_matrices, metaD()$basic_info$n) 
    rownames(ntab) <- paste0(1:nrow(ntab),".",rownames(ntab))
    colnames(ntab) <- paste0(1:ncol(ntab),".")
    ntab
  }) 
  # Warnings
  observeEvent(metaD(), {
    req(metaD())
    ntab<-pattern.n(metaD()$cor_matrices, metaD()$basic_info$n) 
    
    if (sum(ntab<1000)>0) {
      showNotification("Some correlations will be based on small sample size (<1000). Please proceed cautiously",
                       type = "warning", duration = 6)
    }
    if (sum(ntab==0)>0) {
      showNotification("Some correlations will be based on no samples. Please DO NOT proceed",
                       type = "error", duration = 6)
    }
  }, ignoreInit = TRUE)
  
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
  ### Data download ----
  # Download list as RDS
  output$downloadList <- downloadHandler(
    filename = function() {
      paste0(today(), "_livingSEB_SEM_data_list.rds")
    },
    content = function(file) {
      saveRDS(metaD(), file)
    }
  )
  # Download dataframes as separate csv
  output$downloadZip <- downloadHandler(
    filename = function() {
      paste0(today(), "_livingSEB_SEM_data.zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      mylist <- metaD()
      
      # Track files explicitly
      files_to_zip <- c()
      
      for (nm in names(mylist)) {
        fpath <- file.path(tmpdir, paste0(nm, ".csv"))
        write.csv(mylist[[nm]], fpath, row.names = FALSE)
        files_to_zip <- c(files_to_zip, fpath)
      }
      
      # Zip only the files we just wrote
      zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )

  # PAGE b ####
  ### First-stage SEM ----
  cfa1 <- eventReactive({input$s1_matrix}, {
    metaSEM::tssem1(metaD()$cor_matrices, metaD()$basic_info$n)
  })
  ### Results ----
  SumTable <- reactive({
    x <- round(metaSEM::vec2symMat(coef(cfa1(),"fixed"),diag=FALSE),3)
    dimnames(x) <- list(paste0(1:nrow(metaD()$cor_matrices[[1]]),".",rownames(metaD()$cor_matrices[[1]])),
                        paste0(1:nrow(metaD()$cor_matrices[[1]]),"."))
    #dimnames(x) <- rep(list(labels_app[rownames(metaD()$cor_matrices[[1]])]), 2)
    x})
  # Summary table
  output$s1_table <- renderPrint({SumTable()}) 
  output$s1_figure <- renderPlot({corrplot(SumTable(), method = "ellipse", 
                                           addCoef.col = "black",
                                           tl.col = 'black', tl.srt = 45)})
  output$download_s1_figure_png <- downloadHandler(
    filename = function() paste0("correlation_plot_", Sys.Date(), ".png"),
    content = function(file) {
      # high-res PNG (2000x2000 px at 300 dpi ≈ ~6.7 in)
      png(file, width = 2000, height = 2000, res = 300)
      corrplot(SumTable(), method = "ellipse",
               addCoef.col = "black",
               tl.col = "black", tl.srt = 0)
      dev.off()
    }
  )
  
  # PAGE c ####  
  ### Model specification ----
  augmented_model <- reactive({
    req(input$lavmodel, metaD())
    augment_lavaan_model(
      user_model          = input$lavmodel,
      varnames            = rownames(metaD()$cor_matrices[[1]]),
      fix_exo_var         = TRUE,
      correlate_exogenous = input$auto_saturate,
      saturate_endogenous = input$auto_saturate
    )
  })
  output$model_preview <- renderText({ augmented_model() })
  ### Model fit ----
  SEM <- eventReactive({input$fitSEM}, {
    user_model <- input$lavmodel
    varnames   <- rownames(metaD()$cor_matrices[[1]])
    
    model_aug <- augment_lavaan_model(
      user_model          = user_model,
      varnames            = varnames,
      fix_exo_var         = TRUE,
      correlate_exogenous = input$auto_saturate,
      saturate_endogenous = input$auto_saturate
    )
    
    RAM1 <- metaSEM::lavaan2RAM(model_aug, obs.variables = varnames)
    T0   <- metaSEM::create.Tau2(RAM = RAM1, RE.type = "Diag",
                                 Transform = "expLog", RE.startvalues = 0.05)
    my.df <- metaSEM::Cor2DataFrame(metaD()$cor_matrices, metaD()$basic_info$n, acov = "weighted")
    M0    <- create.vechsR(A0 = RAM1$A, S0 = RAM1$S, F0 = RAM1$F)
    
    osmasem(model.name = "One Stage MASEM (auto_augmented)",
            Mmatrix = M0, Tmatrix = T0, data = my.df, intervals.type = "z")
    
    # model <- input$lavmodel
    # varnames <- rownames(metaD()$cor_matrices[[1]]) # Get the names of the variables
    # nvar <- nrow(metaD()$cor_matrices[[1]]) # Get their number
    # RAM1 <- metaSEM::lavaan2RAM(model, obs.variables=varnames) # Generate RAM sintax from lavaan
    # T0 <- metaSEM::create.Tau2(RAM=RAM1, RE.type="Diag", Transform="expLog", RE.startvalues=0.05)
    # my.df <- metaSEM::Cor2DataFrame(metaD()$cor_matrices, metaD()$basic_info$n, acov = "weighted")
    # # Fit the model using one-Stage MASEM with or without test of indirect effect
    # M0 <- create.vechsR(A0=RAM1$A, S0=RAM1$S, F0 = RAM1$F)
    # # ind <- mxAlgebra(beta1*beta2, name="IndirectEffect") 
    # oss <- osmasem(model.name="One Stage MASEM", Mmatrix=M0, Tmatrix=T0, 
    #                data=my.df, intervals.type = "z")
  })
  ### Results ----
  sumfit <- reactive({
    req(SEM())
    summary(SEM(), fitIndices = TRUE)
  })
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
  SEMresults_df <- reactive({
    req(sumfit())
    sem_results_df(
      sumfit    = sumfit(),
      sem       = SEM(),
      metaD     = list(
        cor_matrices = metaD()$cor_matrices,  
        basic_info   = list(n = metaD()$basic_info$n)
      ),
      labels_app = labels_app   
    )
    # my.df <- metaSEM::Cor2DataFrame(metaD()$cor_matrices, metaD()$basic_info$n, acov = "weighted")
    # 
    # ## 1) Build your fixed-effects table as before, but keep a copy with IDs
    # fixedRaw <- subset(sumfit()$parameters, matrix %in% c("A0","S0"),
    #                    select = c("matrix","row","col","Estimate","Std.Error","z value","Pr(>|z|)"))
    # 
    # # create a parameter ID that matches metaSEM naming
    # fixedRaw$param_id <- ifelse(fixedRaw$matrix == "A0",
    #                             paste0(fixedRaw$col, "_",  fixedRaw$row),
    #                             paste0(fixedRaw$row, "_", fixedRaw$col))
    # 
    # ## 2) Get VarCorr and make a named vector of tau variances
    # tauMat   <- metaSEM::VarCorr(SEM())              # random-effects VCV
    # vcor <- round(diag(metaSEM::VarCorr(SEM())),3)
    # tau_diag <- diag(tauMat)
    # tau_ids  <- my.df$ylabels                   # names of parameters with random effects
    # tau_var  <- setNames(as.numeric(tau_diag), tau_ids)
    # tau_sd   <- sqrt(tau_var)
    # 
    # ## 3) Map tau back to fixed effects by name; non-random params become NA
    # match_idx <- match(fixedRaw$param_id, names(tau_var))
    # fixedRaw$tau_var <- tau_var[match_idx]
    # fixedRaw$tau_sd  <- tau_sd[match_idx]
    # 
    # ## 4) Format the combined "tau (sd)" cell
    # fmt3 <- function(x) ifelse(is.na(x), NA, sprintf("%.3f", x))
    # fixedRaw$Tau <- ifelse(is.na(fixedRaw$tau_var), "NaN",
    #                        paste0(fmt3(fixedRaw$tau_var), " (", fmt3(fixedRaw$tau_sd), ")"))
    # 
    # ## 5) Now rebuild your presentation table and merge Tau by keys (row/col/op)
    # parTab <- subset(sumfit()$parameters, matrix %in% c("A0","S0"),
    #                  select = c("matrix","row","col","Estimate","Std.Error","z value","Pr(>|z|)"))
    # parTab$op <- ifelse(parTab$matrix == "A0", "~", "~~")
    # parTab$outcome   <- parTab$row
    # parTab$predictor <- parTab$col
    # 
    # zcrit <- 1.96
    # parTab$CI_lower <- parTab$Estimate - zcrit * parTab$`Std.Error`
    # parTab$CI_upper <- parTab$Estimate + zcrit * parTab$`Std.Error`
    # parTab$CI95 <- paste0("[",round(parTab$CI_lower, 2),"; ",
    #                       round(parTab$CI_upper, 2),"]")
    # is_resid_var <- parTab$matrix == "S0" & parTab$row == parTab$col
    # parTab$op[is_resid_var] <- "~~ (resid var)"
    # parTab$predictor[is_resid_var] <- parTab$outcome[is_resid_var]
    # 
    # parTab <- parTab[, c("matrix","outcome","op","predictor","Estimate","Std.Error",
    #                      "z value","Pr(>|z|)","CI95")]
    # names(parTab) <- c("matrix","outcome","op","predictor","Estimate","SE",
    #                    "z","p","CI_95")
    # 
    # # merge Tau by the unambiguous key (matrix,row,col)
    # parTab <- merge(parTab,
    #                 fixedRaw[, c("matrix","row","col","Tau")],
    #                 by.x = c("matrix","outcome","predictor"),
    #                 by.y = c("matrix","row","col"),
    #                 all.x = TRUE,
    #                 sort  = FALSE)
    # 
    # # pretty formatting
    # parTab$Estimate <- round(parTab$Estimate, 3)
    # parTab$SE       <- round(parTab$SE, 3)
    # parTab$z        <- round(parTab$z, 3)
    # parTab$p        <- ifelse(is.na(parTab$p), NA,
    #                           ifelse(parTab$p < .001, "<.001", sprintf("%.3f", parTab$p)))
    # parTab$outcome <- labels_app[parTab$outcome]
    # parTab$predictor <- labels_app[parTab$predictor]
    # 
    # # rename the column at the end (so you can use parTab$Tau internally)
    # names(parTab)[names(parTab) == "Tau"] <- "tau (sd)"
    # 
    # # final ordering for display
    # op_order <- c("~","~~","~~ (resid var)")
    # parTab$op <- factor(parTab$op, levels = op_order)
    # parTab <- parTab[order(parTab$op, parTab$outcome, parTab$predictor), ]
    # 
    # names(parTab)[names(parTab) == "predictor"] <- "Predictor"
    # names(parTab)[names(parTab) == "matrix"] <- "Matrix"
    # names(parTab)[names(parTab) == "outcome"] <- "Outcome"
    # data.frame(parTab)
  })
  output$SEMresults <- renderTable({
    SEMresults_df()
  })
  # Residual variances + R2
  R2_table <- reactive({
    req(SEM())
    # extract residual (unique) variances from the fitted OSMASEM model
    Sres_mat <- SEM()$mx.fit$Smatrix$result
    Sres <- diag(Sres_mat)
    
    # variable names from your RAM S matrix labels
    var_names <- colnames(SEM()$Mmatrix$S0$labels)
    
    # optional: replace with user-friendly labels if you have labels_app
    if (exists("labels_app") && !is.null(labels_app)) {
      clean_names <- ifelse(var_names %in% names(labels_app), labels_app[var_names], var_names)
    } else {
      clean_names <- var_names
    }
    
    # build table
    out <- data.frame(
      variable = clean_names,
      residual_var = round(Sres, 4),
      R2 = round(pmax(0, 1 - Sres), 3),
      stringsAsFactors = FALSE
    )
    
    # helpful ordering: endogenous first (those appearing on LHS of A paths)
    parTab <- sumfit()$parameters
    endo_raw <- unique(parTab$row[parTab$matrix == "A0"])
    endo <- if (exists("labels_app") && !is.null(labels_app)) {
      ifelse(endo_raw %in% names(labels_app), labels_app[endo_raw], endo_raw)
    } else endo_raw
    out$endo <- out$variable %in% endo
    out <- out[order(!out$endo, out$variable), c("variable","residual_var","R2")]
    colnames(out) <- c("Variable","Residual var", "R2")
    rownames(out) <- NULL
    out
  })
  
  output$R2_table <- renderTable({
    R2_table()
  }, striped = TRUE, bordered = TRUE, digits = 3)
  # PAGE d. REPORT ####
  output$report_masem <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_masem.html",
    
    content = function(file) {
      
      # Generate filtered bib based on current data
      filtered_bib <- generateBib(data = metaD()$basic_info, bibAll = bibAll)
      #filtered_bib_path <- tempfile(fileext = ".bib")
      filtered_bib_path <- file.path(tempdir(), "filtered_refs.bib")
      RefManageR::WriteBib(filtered_bib, file = filtered_bib_path)
      
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
        SumTable = SumTable(),
        lavmodel = input$lavmodel,
        augmented_model = augmented_model(),
        SEM = SEM(),
        sumfit = sumfit(),
        SEMresults_df = SEMresults_df(),
        R2_table = R2_table(),
        bibfile = filtered_bib_path
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
    
    makeReviewTable(subtopics = subtopics, topics = topics)
  })
  # Disable download if nothing to download
  shinyjs::disable("downloadFiltered")
  observe({
    rs <- revSelection()
    has_data <- !is.null(rs$filteredData) && NROW(rs$filteredData) > 0
    shinyjs::toggleState("downloadFiltered", condition = has_data)
  })
  
  output$revTable <- renderTable({
    rs <- revSelection()
    validate(
      need(!is.null(rs$revTab), "No table available yet."),
      need(NROW(rs$revTab) > 0,
           "No results for the current combination. Try using the filtered specific topic that matches the selected broad topic(s).")
    )
    rs$revTab
  })
  output$revSummary <- renderUI({
    rs <- revSelection()
    n <- if (!is.null(rs$revTab)) NROW(rs$revTab) else 0
    tags$div(
      role = "status", `aria-live` = "polite",
      class = if (n > 0) "alert alert-info" else "alert alert-warning",
      if (n > 0) sprintf("Found %d records matching your topic filters.", n)
      else "No matching records. Adjust your broad/specific topic filters."
    )
  })
  ### Clean selection ---
  observeEvent(input$clean_rev, {
    updateSelectInput(session, "topics",    selected = character(0))
    updateSelectInput(session, "subtopics", selected = character(0))
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
  output$apa_cite <- renderText({
    apa_text
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