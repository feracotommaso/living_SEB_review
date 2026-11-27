# Necessary libraries and utilities
source("R/utils.R")
library(shiny)
library(shinyjs)
library(bslib)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(corrplot)
library(metaSEM)
library(metafor)
library(ggplot2)
library(readxl)
library(RefManageR)
library(stringdist)

# UI ####
ui <- navbarPage(
  header = tagList(
    useShinyjs(),
    tags$style(HTML("
    .home-cta { display:flex; flex-direction:column; gap:10px; align-items:flex-start; }
    .home-cta .btn { width:auto; }   /* don't stretch */
    .model-details { margin:16px 0; border:1px solid #cbd2d9; border-radius:6px; }
    .model-details > summary { display:flex; align-items:center; gap:10px; padding:12px 14px;
      background:#f3f5f7; font-weight:600; cursor:pointer; list-style:none; border-radius:6px; }
    .model-details > summary::-webkit-details-marker { display:none; }
    .model-details > summary::before { content:'›'; font-size:24px; line-height:1; }
    .model-details[open] > summary::before { transform:rotate(90deg); }
    .model-details[open] > summary { border-bottom:1px solid #cbd2d9; border-radius:6px 6px 0 0; }
    .model-details > summary:hover { background:#e7ecf1; }
    .model-details > summary:focus-visible { outline:2px solid #517c9c; outline-offset:2px; }
    .model-details > summary small { margin-left:auto; color:#586571; font-weight:400; }
    .model-details-body { padding:0 14px 10px; overflow-x:auto; }
    .citation-code pre { white-space:pre-wrap; overflow-wrap:anywhere; word-break:normal; }
    #revTable td:first-child > a { white-space:nowrap; font-weight:600; text-decoration:underline; }
    #revTable td:first-child > a::after { content:'↗'; margin-left:3px; font-size:0.85em; }
    #revTable tr.selected td:first-child > a { color:inherit; }
                    "))
  ),
#  useShinyjs(),
  title = "Making SEB research living",
  id = "pages",

## INTRODUCTION ####
  tabPanel(
    title = "Home",
    br(),
    tags$div(
      class = "alert alert-info",
      tags$strong("Exploratory evidence mapping. "),
      "Use the curated data to explore the literature and plan analyses. Check sample overlap, coverage and measurement choices before a dedicated meta-analysis."
    ),
    h1("Explore the Living SEB database", align = "left"),
    p(
      "Explore peer-reviewed research on SEB skills: run classic meta-analyses of correlations, ",
      "estimate meta-analytic SEMs, and browse/download curated literature subsets."
    ),
    tags$a("Read the manual", href = manual_html_url, target = "_blank", rel = "noopener",
           class = "btn btn-outline-dark"),
    tags$a("Download manual (PDF)", href = manual_pdf_url, target = "_blank", rel = "noopener",
           class = "btn btn-outline-dark"),
    h3("What you can do"),

    # Use a 2-column layout
    bslib::layout_columns(
      col_widths = c(4, 8), # left 2/3, right 1/3
      gap = "10px",

      # LEFT COLUMN (all your workflow cards)
      div(
        bslib::card(
          bslib::card_header(tags$strong("Meta-analysis of correlations")),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Choose outcomes:"), " pick a broad group and a specific outcome."),
              tags$li(tags$b("Filter:"), " restrict by age class and publication year."),
              tags$li(tags$b("Run models:"), " one multilevel random-effects model per BESSI domain."),
              tags$li(tags$b("Inspect:"), " correlations and confidence intervals, article/matrix counts, expandable model details and forest plots."),
              tags$li(tags$b("Export:"), " CSV dataset and an HTML report.")
            )
          )
        ),
        br(),
        bslib::card(
          bslib::card_header(tags$strong("Meta-analytic SEM (one-stage MASEM)")),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Select variables:"), " SEB domains/facets, traits, and outcomes."),
              tags$li(tags$b("Filter:"), " restrict by age class and publication year."),
              tags$li(tags$b("Check coverage:"), " matrix counts and reference-N tables with coverage alerts."),
              tags$li(tags$b("Pooled matrix:"), " optionally explore pooled correlations and a plot."),
              tags$li(tags$b("Specify model:"), " use the selected variable codes listed in the model editor."),
              tags$li(tags$b("Fit & interpret:"), " convergence, model parameters, fit indices and an R² table."),
              tags$li(tags$b("Model:"), " one-stage estimation with between-study heterogeneity fixed at zero."),
              tags$li(tags$b("Export:"), " RDS/ZIP of data and an HTML report.")
            )
          )
        ),
        br(),
        bslib::card(
          bslib::card_header(tags$strong("Review browser")),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Find articles:"), " search text, topics, analyzed constructs and study characteristics."),
              tags$li(tags$b("Browse:"), " articles initially ordered by code; sort headings and choose columns."),
              tags$li(tags$b("Read details:"), " select a row to open the article card, including review-only papers."),
              tags$li(tags$b("Export:"), " every matching article and all coded fields as CSV.")
            )
          )
        )
      ),

      # RIGHT COLUMN (resources card only)
      bslib::card(
        bslib::card(
          bslib::card_header(tags$strong("Quick start")),
          bslib::card_body(
            p("Jump straight to a workflow:"),
            div(
              style = "display:flex; flex-direction:column; gap:10px; max-width:320px;",
              actionButton("go_to_metacor", "Meta-analysis",    class = "btn btn-dark"),
              actionButton("go_to_metasem", "metaSEM",          class = "btn btn-dark"),
              actionButton("go_to_review",  "Review browser",   class = "btn btn-outline-dark"),
              actionButton("go_to_res",     "Resources",        class = "btn btn-outline-dark"),
              actionButton("go_to_refs",    "About & Citation", class = "btn btn-outline-dark")
            )
          )
        )
      )
    )
  ), #END PAGE 1

## META-ANALYSIS ####
  tabPanel(
    title="Meta-analysis",
### .a vars selection ####
    sidebarLayout(
      sidebarPanel(h3("Select the outcome of interest"),
                   radioButtons("outcomeTopics", "Select the broad outcome grouping:",
                               unique(c(varlist$Topic[!varlist$Topic %in% c("SKILLS", "BESSI-based SEB skills")])), selected = if ("Education" %in% varlist$Topic) "Education" else "SCHOOL" ),
                   uiOutput("dynamic_outcome_select"),
                   selectInput("age_filter","Filter by age",
                               choices = unique(combined_data$age_class),
                               multiple = TRUE),
                   selectInput("year_filter","Filter by year of publication",
                               choices = unique(combined_data$year),
                               multiple = TRUE)
                   # selectInput("metaModerators", "Moderators?",
                   #             c(list("XXXX","YYYY","ZZZZ")),
                   #             multiple = FALSE )
      ),
      mainPanel(
        uiOutput("meta_run_status"),
        tabsetPanel(
          tabPanel(
            "Summary",
            h4("Coverage by skill"),
            tableOutput("metaCoverage"),
            helpText("Invalid values counts observed correlations with non-finite r, r outside −1 < r < 1, or a non-finite reference N / N ≤ 3. Ordinary missing correlations are not counted. See analysis_status in the CSV for the reason."),
            helpText("Ns are the reference sizes already coded for each matrix. Their sum is not a count of unique people; available N can vary by correlation and some samples overlap."),
            h4("Included matrices"),
            DT::DTOutput("metaStudy_table"),
            uiOutput("studycount_banner"),
            # Button
            downloadButton("downloadMetaData", "Download current selection")
          ),

### .b meta-matrix ####
          tabPanel(
            title="Meta results",
            actionButton("meta_analysis", "Run the meta-analysis",
                         style="color: #fff; background-color: black; border-color: black"),
            br(),
            h4("Estimated correlations"),
            DT::DTOutput("metaResults"),
            helpText("Correlations and their confidence intervals are on the r scale. N is the sum of coded reference sample sizes, not a count of unique people."),
            uiOutput("meta_messages"),
            tags$details(class = "model-details",
              tags$summary(tags$span("Model details"), tags$small("SE · τ² · Q")),
              div(class = "model-details-body",
                helpText("SE and heterogeneity variance (tau-squared) are on the Fisher z scale. Q is the heterogeneity test statistic."),
                tableOutput("metaDetails"))),
            # verbatimTextOutput("s1_table"),
            plotOutput("metaPlot")
          ),

### .c forest plots ####
          tabPanel(
            title="Forest plots",
            selectInput("forestSelected", "Skill to show",
                        stats::setNames(pred_vars, unname(labels_app[pred_vars])),
                        multiple = FALSE),
            plotOutput("forestPlot")
          ),

### .d report ####
          tabPanel(
            title = "Report",
            p("Run the meta-analysis for the current selection to enable the report. The report uses the same saved data, filters, labels and results."),
            br(),
            downloadButton("report", "Generate a brief report of the results")
          )
        )
      )
    )
  ),

# ---------------------------------------------------------------------------------------------------------------- #

##META-SEM ####
  tabPanel(
    title="MetaSEM",
### .a vars selection ####
    sidebarLayout(
      sidebarPanel(h3("Select the variables of interest"),
                   selectInput("SEBdomains", "SEB domains:",
                               variable_search_choices(varlist[varlist$broad == "BESSI",]),
                               multiple = TRUE ),
                   selectInput("SEBfacets", "SEB facets:",
                               variable_search_choices(varlist[varlist$broad == "BESSI_facet",]),
                               multiple = TRUE ),
                   selectInput("PersonalityTraits", "Personality traits:",
                               variable_search_choices(varlist[varlist$broad == "BIGFIVE",]),
                               multiple = TRUE ),
                   selectInput("Outcomes", "Outcomes:",
                               variable_search_choices(varlist[!(varlist$Topic %in% c("SKILLS", "TRAITS", "BESSI-based SEB skills", "Personality traits")),]),
                               multiple = TRUE ),
                   selectInput("age_filter_masem","Filter by age",
                               choices = unique(combined_data$age_class),
                               multiple = TRUE),
                   selectInput("year_filter_masem","Filter by year of publication",
                               choices = unique(combined_data$year),
                               multiple = TRUE),
                   # selectInput("Moderators", "Moderators?",
                   #             c(list("XXXX","YYYY","ZZZZ")),
                   #             multiple = TRUE ),
                   actionButton("k_info", "Get info",
                                style="color: #fff; background-color: black; border-color: black")
      ),

      mainPanel(
        uiOutput("masem_state"),
        tabsetPanel(
          tabPanel("Summary",
                   # Button
                   downloadButton("downloadList", "Download the data as R list"),
                   downloadButton("downloadZip", "Download the data as csv files"),
                   br(),

                   h4("Contributing matrices per correlation (k)"),
                   uiOutput("zero_banner"),   # persistent red banner when k=0 pairs exist
                   DT::DTOutput("K_table"),

                   h4("Sum of reference Ns per correlation"),
                   DT::DTOutput("N_table"),
                   helpText("Ns are the coded reference sample sizes, not pair-specific counts or counts of unique people. Missing correlations and documented sample overlaps are retained."),
                   tags$details(class = "model-details", tags$summary("Selected variable codes and matrix checks"),
                     div(class = "model-details-body", tableOutput("masem_variables"), tableOutput("masem_audit"))),

                   h4("Studies included"),
                   DT::DTOutput("Study_table")
          ),

### .b meta-matrix ####
          tabPanel(
            title="SEM matrix",
            br(),  # a capo
            h3("Explore the pooled correlation matrix"),
            p("This optional TSSEM calculation is separate from the one-stage model. The one-stage model uses the study matrices directly."),
            br(),
            h4("Estimated correlation matrix"),
            actionButton("s1_matrix", "Get the meta-matrix",
                         style="color: #fff; background-color: black; border-color: black"),
            uiOutput("s1_diagnostics"),
            DT::DTOutput("s1_table"),
            withSpinner(plotOutput("s1_figure")),
            downloadButton("download_s1_figure_png", "Download correlation plot (PNG)")
          ),

### .c meta-SEM ####
          tabPanel(
            title = "metaSEM",
            br(),
            textAreaInput(
              "lavmodel",
              label = "Model syntax (use the variable codes listed in the comments below):",
              width = "100%",
              height = "300px",
              value =
                "
# ADD THE REGRESSIONS YOU WANT TO ESTIMATE
# Regression coefficients as 'y ~ x1 + x2'
academicachievement ~ selfmanagement + socialengagement + conscientiousness + extraversion

# OPTIONAL COVARIANCES CAN BE ENTERED EXPLICITLY OR ADDED WITH THE CHECKBOX
# Covariances as 'x1 ~~ x2'

# EXOGENOUS VARIANCES DEFAULT TO 1 UNLESS EXPLICITLY SPECIFIED
"
            ),
            helpText("Lines starting with # are comments and do not enter the model. The variable list updates when your selection changes; your model below it is retained."),
#            checkboxInput("auto_fix_exo", "Fix exogenous variances to 1", TRUE),
            checkboxInput("auto_saturate", "Add exogenous covariances and covariances between endogenous residuals", TRUE),
            helpText("Fixed paths and variables without directed paths are supported. Feedback loops are not supported. For measurement models, switch off automatic covariances and specify them explicitly."),
            verbatimTextOutput("model_preview"),
            br(),
            actionButton("fitSEM","Fit the specified SEM model",
                         style="color: #fff; background-color: black; border-color: black"),
            h4("Model fit"),
            p("One-stage MASEM; between-study heterogeneity is fixed at zero (the existing model setting)."),
            uiOutput("sem_diagnostics"),
            uiOutput("fit_ui"),  # This will show either text or table dynamically
            h4("Model estimates"),
            withSpinner(tableOutput("SEMresults")), #withSpinner
            helpText("Regression coefficients, loadings, covariances and variances are different parameters; they are not all correlations. Fixed coefficients remain visible in the model syntax; this table lists estimated parameters."),
            tags$details(class = "model-details", tags$summary("Model details: standard errors and Wald tests"),
              div(class = "model-details-body", tableOutput("SEMdetails"))),
            h4("Residual variance"),
            withSpinner(tableOutput("R2_table")),
            helpText("R² is shown for endogenous variables where applicable, using their model-implied total variance. Out-of-range values are retained and flagged.")

          ),

### .d report ####
          tabPanel(
            title = "Report",
            p("Fit a validated one-stage model for the current data and syntax to enable the report. The exploratory pooled matrix is optional and is included only if calculated for the same data selection."),
            br(),
            downloadButton("report_masem", "Generate a brief report of the maSEM results")
          )
        )
      )
    )
  ), #END PAGE META-SEM


# ---------------------------------------------------------------------------------------------------------------- #

##REVIEW ####
  tabPanel(
    title="Review",
### .a topic selection ####
    sidebarLayout(
      sidebarPanel(h3("Find articles"),
                   textInput("review_search", "Search articles:",
                     placeholder = "Any author, title, abstract, topic or construct"),
                   selectInput("topics", "BROAD topics:",
                               choices = unique(topics_list$Broad_topic_labs),
                               multiple = TRUE ),
                   selectInput("subtopics", "Specific topics:",
                               choices = unique(topics_list$Topic_labs),
                               multiple = TRUE ),
                   uiOutput("dynamic_topic_select"),
                   uiOutput('review_coding_filters'),
                   actionButton("clean_rev", "Clean selection",
                                style="color: #fff; background-color: black; border-color: black"),
                   br(),br(),
                   downloadButton("downloadFiltered", "Download the filtered data")
                   ),
      mainPanel(
        h3("Included articles"),
        uiOutput("revSummary"),
        selectizeInput("review_columns", "Columns to show (code is always included):",
          choices = stats::setNames(setdiff(names(review_column_labels), "paper_id"),
            unname(review_column_labels[names(review_column_labels) != "paper_id"])),
          selected = c("author_et_al", "year", "title"), multiple = TRUE),
        helpText("Click an article code ↗ to open its DOI in a new tab; click elsewhere in the row to read its details here. Click a heading to sort. Scroll within the table to browse rows; column headings and the horizontal scrollbar stay in place. Search and filters also include review-only articles; the download contains every matching article and all coded fields."),
        DT::DTOutput("revTable"),
        hr(),
        uiOutput("review_article")
        )
      )
  ), #END PAGE REVIEW

# ---------------------------------------------------------------------------------------------------------------- #

## RESOURCES ####
  tabPanel(
    title = "Resources",
    bslib::card(
      bslib::card_header(tags$strong("Resources")),
      bslib::card_body(
        h4("Learn to use the app"),
        p(tags$a("Manual (HTML)", href = manual_html_url, target = "_blank", rel = "noopener", class = "btn btn-dark"),
          " ", tags$a("Manual (PDF)", href = manual_pdf_url, target = "_blank", rel = "noopener", class = "btn btn-outline-dark")),
        p(tags$a("R analysis tutorials", href = "https://github.com/feracotommaso/living_SEB_review/tree/main/tutorialAnalysis",
          target = "_blank", rel = "noopener"), " - simple scripts for local meta-analysis and one-stage MASEM."),
        h4("Data, methods and results"),
        p(tags$a("GitHub repository, codebooks and data", href = "https://github.com/feracotommaso/living_SEB_review/",
          target = "_blank", rel = "noopener"), " - CSV data and XLSX companions for human review."),
        p(tags$a("Living results and database overview", href = "https://feracotommaso.github.io/living_SEB_review/paper/livingSEBresults.html",
          target = "_blank", rel = "noopener")),
        p(tags$a("Preregistration", href = "https://feracotommaso.github.io/living_SEB_review/preregistration/Preregistration_protocol_livingSEB.pdf",
          target = "_blank", rel = "noopener")),
        p(tags$a("Project site", href = "https://feracotommaso.github.io/living_SEB_review/",
          target = "_blank", rel = "noopener"))
      ),
      br(),

      # Contact & feedback
      bslib::card(
        bslib::card_header(tags$strong("Contact & feedback")),
        bslib::card_body(
          p("Questions or suggestions?"),
          div(
            style = "display:flex; gap:10px; flex-wrap:wrap;",
            tags$a(
              href = "mailto:tommaso.feraco@unipd.it",
              class = "btn btn-dark btn-sm", "Email the author"
            ),
            tags$a(
              href = "https://github.com/feracotommaso/living_SEB_review/issues",
              target = "_blank", class = "btn btn-outline-dark btn-sm", "Report an issue"
            ),
            tags$a(
              href = "https://github.com/feracotommaso/living_SEB_review",
              target = "_blank", class = "btn btn-outline-dark btn-sm", "Open the repository"
            )
          )
        )
      ),
    )
  ),

# ---------------------------------------------------------------------------------------------------------------- #

## ABOUT ####
  tabPanel(
    title = "About & Citation",
    br(),
    h1("About this app", align = "left"),
    p(
      "This app was developed by Tommaso Feraco to help researchers explore SEB literature, run classic meta-analyses of correlations, ",
      "and estimate meta-analytic SEMs."
    ),
    # Two-column layout
    bslib::layout_columns(
      col_widths = c(9, 3), # left 2/3, right 1/3
      gap = "10px",

      # ===== LEFT COLUMN ===
      div(
        # How to cite
        bslib::card_header(tags$strong("Cite this app")),
        bslib::card_body(
          p("If you use this app or dataset, cite the Living SEB Skills Project:"),
          p("Also record the app version, selected variables and filters, and save the analysis data and report. The project citation does not identify a particular data snapshot."),
          uiOutput("apa_cite"),
          p("BibTeX:"),
          div(class = "citation-code", verbatimTextOutput("bib_cite")),
          br(),
          downloadButton("download_bib", "Download .bib")
          ),
        br(),

        # Version / data snapshot
        bslib::card(
          bslib::card_header(tags$strong("Version & loaded data")),
          bslib::card_body(
            p(paste("App interface version:", app_version)),
            p("Data source: the published GitHub main branch, loaded when the app starts."),
            p(paste("Data loaded at:", data_loaded_at)),
            helpText("This is the retrieval time, not the date of the latest scientific database change. Data releases and app releases are updated separately."),
            p(sprintf("Loaded database: %d review articles; %d quantitative articles; %d correlation matrices.",
              length(unique(review$paper_id)), length(unique(combined_data$paper_id)), length(unique(combined_data$matrix_id)))),
            tags$details(class = "model-details", tags$summary("Analysis environment"),
              div(class = "model-details-body",
              p(paste("R", getRversion())),
              p(paste(names(masem_require_versions()), masem_require_versions(), collapse = "; ")),
              p(paste("metafor", as.character(getNamespaceVersion("metafor"))))))
          )
        )
      ), # <-- CLOSE LEFT COLUMN DIV

      # ===== RIGHT COLUMN ===
      div(
        bslib::card(
          # Credits & acknowledgments
          bslib::card(
            bslib::card_header(tags$strong("Credits & acknowledgments")),
            br(),
            bslib::card_body(
              tags$ul(
                tags$li(HTML("<b>Packages:</b> metafor, metaSEM, lavaan, OpenMx, ggplot2, corrplot, shiny, DT, bslib, shinyjs")),
                tags$li(HTML("<b>Inspiration:</b> webMASEM — Jak et al. (2021), <a href='https://doi.org/10.1002/jrsm.1498' target='_blank'>https://doi.org/10.1002/jrsm.1498</a>")),
                tags$li(HTML("<b>Acknowledgments:</b> Thanks to Chris Soto and Brent Roberts for supporting the project, and to Enrico Toffalini, Filippo Gambarota and Margherita Calderan for their guidance and contributions. Thanks also to Jak and colleagues for the webMASEM inspiration."))
              )
            )
          ),
        )
      ) # <-- CLOSE RIGHT COLUMN DIV
    )
  )
) #END LAST PAGE

# # rsconnect::deployApp(getwd())
