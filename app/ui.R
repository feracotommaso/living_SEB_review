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
  # theme = bs_theme(
  #   version   = 5,              # Bootstrap 5
  #   bootswatch = "flatly",      # or "minty", "darkly", NULL for default
  #   primary   = "#0d6efd",      # brand color
  #   base_font = font_google("Inter"),
  #   heading_font = font_google("Inter Tight")
  # ),
  header = tagList(
    useShinyjs(),
    tags$style(HTML("
    .home-cta { display:flex; flex-direction:column; gap:10px; align-items:flex-start; }
    .home-cta .btn { width:auto; }   /* don't stretch */
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
      class = "alert alert-danger",
      tags$strong("WARNING:"), " THIS IS STILL A DEMO"
    ),
    h1("The living SEB app - Introduction and functionalities", align = "left"),
    p(
      "Explore peer-reviewed research on SEB skills: run classic meta-analyses of correlations, ",
      "estimate meta-analytic SEMs, and browse/download curated literature subsets."
    ),
    tags$a("DOWNLOAD THE APP MANUAL HERE",
           href = "https://feracotommaso.github.io/living_SEB_review/appManual/livingSEBapp_manual.pdf",
           target = "_blank"),
    h3("What you can do"),
  
    # Use a 2-column layout
    bslib::layout_columns(
      col_widths = c(8, 4), # left 2/3, right 1/3
      gap = "20px",
    
      # LEFT COLUMN (all your workflow cards)
      div(
        bslib::card(
          bslib::card_header("Meta-analysis of correlations"),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Choose outcomes:"), " pick a broad group and a specific outcome."),
              tags$li(tags$b("Filter:"), " restrict by ", tags$code("age_class"), " and publication year."),
              tags$li(tags$b("Run models:"), " multilevel random-effects via ", tags$code("metafor::rma.mv()"), "."),
              tags$li(tags$b("Inspect:"), " results table, dot–CI plot, forest plots."),
              tags$li(tags$b("Export:"), " CSV dataset and an HTML report.")
            )
          )
        ),
        br(),
        bslib::card(
          bslib::card_header("Meta-analytic SEM (one-stage MASEM)"),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Select variables:"), " SEB domains/facets, traits, and outcomes."),
              tags$li(tags$b("Check coverage:"), " K/N tables with alerts for k=0 / small N."),
              tags$li(tags$b("Pooled matrix:"), " compute meta-analytic correlations + plot."),
              tags$li(tags$b("Specify model:"), " write lavaan syntax; app can auto-augment."),
              tags$li(tags$b("Fit & interpret:"), " fit indices, parameters (with τ), and an R² table."),
              tags$li(tags$b("Export:"), " RDS/ZIP of data and an HTML report.")
            )
          )
        ),
        br(),
        bslib::card(
          bslib::card_header("Review browser"),
          bslib::card_body(
            tags$ul(
              tags$li(tags$b("Filter by topics:"), " broad and specific topic labels."),
              tags$li(tags$b("Dynamic filtering:"), " specific topics follow your broad choice."),
              tags$li(tags$b("Results summary:"), " live count with helpful empty states."),
              tags$li(tags$b("Export:"), " download the filtered studies as CSV.")
            )
          )
        ),
        br(),
        bslib::card(
          bslib::card_header("Quick start"),
          bslib::card_body(
            p("Jump straight to a workflow:"),
            div(
              style = "display:flex; flex-direction:column; gap:10px; max-width:320px;",
              actionButton("go_to_metacor", "Meta-analysis",    class = "btn btn-dark"),
              actionButton("go_to_metasem", "metaSEM",          class = "btn btn-dark"),
              actionButton("go_to_review",  "Review browser",   class = "btn btn-outline-dark"),
              actionButton("go_to_refs",    "About & Citation", class = "btn btn-outline-dark")
            )
          )
        )
      ),
    
      # RIGHT COLUMN (resources card only)
      bslib::card(
        bslib::card_header(tags$strong("Resources & acknowledgments")),
        bslib::card_body(
          p(
            "Code and data: ",
            tags$a("GitHub repository",
                   href = "https://github.com/feracotommaso/living_SEB_review",
                   target = "_blank")
          ),
          p(
            "We thank prior authors and ",
            tags$a("Jak et al. (2021)", href = "https://doi.org/10.1002/jrsm.1498", target = "_blank"),
            " for the webMASEM inspiration."
          ),
          p(
            "Contact: ",
            tags$a("tommaso.feraco@unipd.it", href = "mailto:tommaso.feraco@unipd.it")
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
                               unique(c(varlist$Topic[varlist$Topic != "SKILLS"])), selected = "SCHOOL" ),
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
        tabsetPanel(
          tabPanel(
            "Summary",
            h4("Samples included"),
            tableOutput("metaStudy_table"),
            h5("Note: N is the max sample size and may vary across skill domains"),
            # Button
            downloadButton("downloadMetaData", "Download the data")
          ),
          
### .b meta-matrix ####
          tabPanel(
            title="Meta results",
            actionButton("meta_analysis", "Run the meta-analysis",
                         style="color: #fff; background-color: black; border-color: black"),
            
            br(),
            h4("Estimated correlations"),
            tableOutput("metaResults"),
            # verbatimTextOutput("s1_table"),
            plotOutput("metaPlot")
          ),

### .c forest plots ####
          tabPanel(
            title="Forest plots",
            selectInput("forestSelected", "Skill to show",
                        pred_vars, 
                        multiple = FALSE),
            plotOutput("forestPlot")
          ),

### .d report ####
          tabPanel(
            title = "Report",
            h4("You can download a report only if you run the meta-analysis first", 
               align = "left", style = "color: red; font-weight: bold;"), # WARNING
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
                               c(varlist$column_name[varlist$broad == "BESSI"]), 
                               multiple = TRUE ),
                   selectInput("SEBfacets", "SEB facets:",
                               c(varlist$column_name[varlist$broad == "BESSI_facet"]), 
                               multiple = TRUE ),
                   selectInput("PersonalityTraits", "Personality traits:",
                               c(varlist$column_name[varlist$broad == "BIGFIVE"]), 
                               multiple = TRUE ),
                   selectInput("Outcomes", "Outcomes:",
                               c(varlist$column_name[!(varlist$Topic %in% c("SKILLS", "TRAITS"))]), 
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
        tabsetPanel(
          tabPanel("Summary",
                   # Button
                   downloadButton("downloadList", "Download the data as R list"),
                   downloadButton("downloadZip", "Download the data as csv files"),
                   br(),
                   
                   h4("K correlations"),
                   uiOutput("zero_banner"),   # persistent red banner when k=0 pairs exist
                   verbatimTextOutput("K_table"),

                   h4("Sample size per correlation"),
                   verbatimTextOutput("N_table"),

                   h4("Studies included"),
                   tableOutput("Study_table")
          ),
          
### .b meta-matrix ####
          tabPanel(
            title="SEM matrix",
            br(),  # a capo
            h1("Explore the meta-analytical matrix",align="left"), # titoletto di primo livello
            br(),
            h4("Estimated correlation matrix"),
            actionButton("s1_matrix", "Get the meta-matrix",
                         style="color: #fff; background-color: black; border-color: black"),
            verbatimTextOutput("s1_table"),
            withSpinner(plotOutput("s1_figure")),
            downloadButton("download_s1_figure_png", "Download correlation plot (PNG)")
          ),

### .c meta-SEM ####
          tabPanel(
            title = "metaSEM",
            br(),
            textAreaInput(
              "lavmodel",
              label = "Modify the model using lavaan syntax. The full model sintax is printed below",
              width = "1000px",
              height = "300px",
              value =
                "
# ADD THE REGRESSIONS YOU WANT TO ESTIMATE
# Regression coefficients as 'y ~ x1 + x2' 
academicachievement ~ selfmanagement + socialengagement + conscientiousness + extraversion

# YOU CAN SATURATE THE MODEL (ESTIMATE ALL COVARIANCES BY THICKING THE BOX BELOW)
# Covariances as 'x1 ~~ x2'

# EXOGENOUS VARIANCE IS SET TO 1 BY DEFAULT. DO NOT ADD IT
"
            ),
#            checkboxInput("auto_fix_exo", "Fix exogenous variances to 1", TRUE),
            checkboxInput("auto_saturate", "Saturate the model by adding residual correlations among all variables", TRUE),
            verbatimTextOutput("model_preview"),
            br(),
            actionButton("fitSEM","Fit the specified SEM model",
                         style="color: #fff; background-color: black; border-color: black"),
            h4("Model fit"),
            uiOutput("fit_ui"),  # This will show either text or table dynamically
            h4("Model estimates"),
            withSpinner(tableOutput("SEMresults")), #withSpinner
            h4("Residual variance"),
            withSpinner(tableOutput("R2_table"))#withSpinner
            
          ),

### .d report ####
          tabPanel(
            title = "Report",
            h4("You can download a report only if you run both the SEM matrix and the metaSEM", 
               align = "left", style = "color: red; font-weight: bold;"), # WARNING
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
      sidebarPanel(h3("Select the topics of interest"),
                   selectInput("topics", "BROAD topics:",
                               choices = unique(topics_list$Broad_topic_labs),
                               multiple = TRUE ),
                   selectInput("subtopics", "Specific topics:",
                               choices = unique(topics_list$Topic_labs),
                               multiple = TRUE ),
                   uiOutput("dynamic_topic_select"),
                   actionButton("clean_rev", "Clean selection",
                                style="color: #fff; background-color: black; border-color: black"),
                   br(),br(),
                   downloadButton("downloadFiltered", "Download the filtered data")
                   ),
      mainPanel(
        h3("Filtered studies"),
        uiOutput("revSummary"),
        #uiOutput("revTableUI"),
        tableOutput("revTable")
        )
      )
  ), #END PAGE REVIEW

# ---------------------------------------------------------------------------------------------------------------- #

## THANKS ####
  tabPanel(
    title = "About & Citation",
    br(),
    h1("About this app", align = "left"),
    p(
      "This app helps researchers explore SEB literature, run classic meta-analyses of correlations, ",
      "and estimate meta-analytic SEMs (one-stage MASEM)."
    ),
    # How to cite
    bslib::card(
      bslib::card_header("How to cite"),
      bslib::card_body(
        p("If you use this app or dataset, please cite it as:"),
        tags$pre(
          "Feraco, T. (2025). The Living SEB Project: A Living Systematic Review and Meta-Analysis of Social, Emotional, and Behavioral Skills. 
          https://github.com/feracotommaso/living_SEB_review"
        ),
        p("BibTeX:"),
        tags$pre(
          "@misc{Feraco2025LivingSEB,
  author       = {Feraco, Tommaso},
  title        = {The Living SEB Project: A Living Systematic Review and Meta-Analysis of Social, Emotional, and Behavioral Skills},
  year         = {2025},
  howpublished = {GitHub repository},
  publisher    = {NA},
  url          = {https://github.com/feracotommaso/living_SEB_review}
}"
        )
      )
    ),
    br(),
    # Contact & feedback
    bslib::card(
      bslib::card_header("Contact & feedback"),
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
    br(),
    # Credits & acknowledgments
    bslib::card(
      bslib::card_header("Credits & acknowledgments"),
      bslib::card_body(
        tags$ul(
          tags$li(HTML("<b>Packages:</b> metafor, metaSEM, ggplot2, corrplot, shiny, bslib, shinyjs")),
          tags$li(HTML("<b>Inspiration:</b> webMASEM — Jak et al. (2021), <a href='https://doi.org/10.1002/jrsm.1498' target='_blank'>https://doi.org/10.1002/jrsm.1498</a>")),
          tags$li(HTML("<b>Data & code:</b> see the GitHub repository for sources, scripts, and license"))
        )
      )
    ),
    
    br(),
    # Version / data snapshot (optional placeholder text)
    bslib::card(
      bslib::card_header("Version & data snapshot"),
      bslib::card_body(
        p("App version: v0.1 (demo)"),
        p("Dataset last updated: see repository README for the latest snapshot/date.")
        # If you want this dynamic, you can replace these with textOutput()s and set them in server.
      )
    )
  ) #END LAST PAGE
) #END

# # rsconnect::deployApp(getwd())
