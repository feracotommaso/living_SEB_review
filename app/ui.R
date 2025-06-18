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

# UI ####
ui <- navbarPage(
  title = "Making SEB research living",
  id = "pages",
  
## INTRODUCTION ####
  tabPanel(
    title = "Home",
    br(),  # Line break
    h1("Introduction to the Shiny app", align = "left"),  # Title
    br(),
    tags$p("Welcome to our Shiny app for navigating peer-reviewed research on SEB skills,
      summarize their results and obtain meta-analytical estimates or metaSEM.
      Here's what you can do in this app:"),
    
    br(),
    h3("What you can do"),
    tags$ul(
      tags$li(tags$b("Meta-analyze correlation coefficients:"), 
              "This section allows you to estimate the meta-analytical [cross-sectional] 
              association between SEB skills and outcome variables of your choice 
              depending on the full set of correlates included in studies using the BESSI
              or alternative versions of the BESSI."),
      tags$li(tags$b("Estimate metaSEM:"), 
              "This section allows you to estimate more complex models using a meta-sem approach.
              Using lavaan syntax you will be able to estimate uni- and multi-variate regression models
              and path analysis. SEM models can also be estimated but item-level correlations are not provided.")
    ),
    tags$p(tags$b("Want more complex analyses?"), "Download the data from the GitHub repository and get full power."),
    
    br(),
    h3("Info"),
    tags$p(
      "Code, data, and all the information needed to run the application locally or for other personal uses ",
      "are available on the GitHub repository of this project: ",
      tags$a("https://github.com/feracotommaso/living_SEB_review",
             href = "https://github.com/feracotommaso/living_SEB_review",
             target = "_blank")
      ),
    tags$p(
      "We thank all the authors who inspired this work 
      and Jak and colleagues (2021) for freely sharing the webMASEM app:",
      tags$a("https://doi.org/10.1002/jrsm.1498",
             href = "https://doi.org/10.1002/jrsm.1498",
             target = "_blank")
    ),
    br(),
    h3("Begin working"),
    sidebarPanel(
      actionButton(inputId = "go_to_metacor", label = "Go to classic meta analysis"),
      br(),br(),
      actionButton(inputId = "go_to_metasem", label = "Go to metaSEM")
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
            tableOutput("metaStudy_table")
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
                        multiple = FALSE, selected = TRUE ),
            plotOutput("forestPlot")
          ),

### .d report ####
          tabPanel(
            title = "Report",
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
          # tabPanel("Summary",
          #   accordion(
          #     accordion_panel("K correlations", verbatimTextOutput("K_table")),
          #     accordion_panel("Sample size per correlation", verbatimTextOutput("N_table")),
          #     accordion_panel("Studies included", tableOutput("Study_table")),
          #   )
          # ),
          tabPanel("Summary",
                   # Conditionally display the warning message in red

                   h4("K correlations"),
                   verbatimTextOutput("K_table"),

                   h4("Sample size per correlation"),
                   verbatimTextOutput("N_table"),

                   h4("Studies included"),
                   tableOutput("Study_table")
          ),
          
### .b meta-matrix ####
          tabPanel(
            title="First stage SEM",
            br(),  # a capo
            h1("Explore the meta-analytical matrix",align="left"), # titoletto di primo livello
            br(),
            h4("Estimated correlation matrix"),
            actionButton("s1_matrix", "Get the meta-matrix",
                         style="color: #fff; background-color: black; border-color: black"),
            verbatimTextOutput("s1_table"),
            plotOutput("s1_figure")
          ),

### .c meta-SEM ####
          tabPanel(
            title = "metaSEM",
            br(),
            textAreaInput(
              "lavmodel",
              label = "Specify the model using lavaan syntax",
              width = "1000px",
              height = "300px",
              value =
                "
# Regression coefficients as 'y ~ x1 + x2' 
academicachievement ~ selfmanagement + socialengagement + conscientiousness + extraversion

# Covariances as 'x1 ~~ x2'
selfmanagement ~~ socialengagement + conscientiousness + extraversion
socialengagement ~~ conscientiousness + extraversion
conscientiousness ~~ extraversion

# REMEMBER THAT ALL EXOGENOUS VARS SHOULD HAVE VARIANCE FIXED TO 1
# Variances as 'x1 ~~ 1*x1' 
socialengagement ~~ 1*socialengagement
selfmanagement ~~ 1*selfmanagement 
conscientiousness ~~ 1*conscientiousness
extraversion ~~ 1*extraversion
"
            ),
            actionButton("fitSEM","Fit the specified SEM model",
                         style="color: #fff; background-color: black; border-color: black"),
            h4("Model fit"),
            uiOutput("fit_ui"),  # This will show either text or table dynamically
            h4("Model estimates"),
            withSpinner(verbatimTextOutput("results")),
            withSpinner(tableOutput("SEMresults"))#withSpinner
          ),

### .d report ####
          tabPanel(
            title = "Report",
            downloadButton("report_masem", "Generate a brief report of the maSEM results")
          )
        )
      )
    )
  ), #END PAGE META-SEM
  
## THANKS ####
  tabPanel(
    title = "References",
    br(),  # Line break
    h1("Closing remarks", align = "left"),  # Title
    br(),
    tags$p(tags$b("Thank you for using this app!"), "Give us your feedback if you want"),
    tags$ul(
      tags$li(tags$b("Contacts:"), 
              "Contact tommaso.feraco@unipd.it for any request or feedback."),
      tags$li(tags$b("People:"), 
              "Tommaso Feraco worked to this project alone (at the moment)."),
      tags$li(tags$b("Next steps:"), 
              "We will keep the dataset up-to-date and will try to expand the functionalities
              of this app toward new kind of analyses once the data will allow more complexity.")
      
    )
  ) #END LAST PAGE
) #END

# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# # rsconnect::setAccountInfo(name='feracoshiny',
# #                           token='A47AC911721175295B55E72B4BBDBA64',
# #                           secret='RDBYEXaqWAtSZDRAFdq1s2WgiKe+Gn9pC6fbLoKQ')
# # 
# # rsconnect::deployApp(getwd())
