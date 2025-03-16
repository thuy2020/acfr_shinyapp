library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
source("charts.R")

# Define the UI
shinyUI(navbarPage(
    title = tags$div(
        tags$img(src = "logo.png", height = "30px", 
                 style = "margin-top: -5px; margin-right: 10px;"),
        span("Annual Comprehensive Financial Reports by States", class = "main-title")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        # Custom CSS to add spacing between sections
        tags$style(HTML("
      .custom-spacing {
        margin-top: 30px;  /* Adjust this value to increase/decrease space */
      }
      .custom-section-spacing {
        margin-top: 40px;  /* Extra space between major sections */
      }
    "))
    ),
    
    # Main Layout
    fluidRow(
        column(12,
               sidebarLayout(
                   sidebarPanel(
                       # Select inputs for state, entity type, and year
                       tags$div(class = "select-input-custom",
                                selectInput("selected_state", "Select State:", 
                                            choices = unique(all_entities$state.name), 
                                            selected = NULL, multiple = TRUE)
                       ),
                       tags$div(class = "select-input-custom",
                                selectInput("selected_entity_type", "Select Entity Type:", 
                                            choices = unique(all_entities$category), 
                                            selected = NULL, multiple = TRUE)
                       ),
                       tags$div(class = "select-input-custom",
                                selectInput("selected_year", "Select Year:", 
                                            choices = unique(all_entities$year), 
                                            selected = NULL, multiple = TRUE)
                       )
                   ),
                   
                   mainPanel(
                       # Value Boxes with additional space
                       fluidRow(class = "custom-section-spacing",  # Adds space before value boxes
                                valueBoxOutput("box_net_pension", width = 4),  
                                valueBoxOutput("box_net_opeb", width = 4),
                                valueBoxOutput("box_total_liability", width = 4)
                       ),
                       
                       # Table Section with space after value boxes
                       fluidRow(class = "custom-section-spacing",
                                tabsetPanel(
                                    
                                    tabPanel("Summary Table", 
                                             div(style = "font-size: 20px; font-weight: bold; text-align: center; color: darkblue; margin-top: 20px; margin-bottom: 20px;",
                                                textOutput("summary_table_title")),
                                             DTOutput("summary_table"),
                                             textOutput("caption")),
                                    tabPanel("Entity Table", 
                                             div(style = "font-size: 20px; font-weight: bold; text-align: center; color: darkblue; margin-top: 20px; margin-bottom: 20px;",
                                                 textOutput("entity_table_title")),
                                             DTOutput("entity_table"), 
                                             textOutput("caption")),
                                    tabPanel("Counties' Population Covered", 
                                             div(style = "font-size: 20px; font-weight: bold; text-align: center; color: darkblue; margin-top: 20px; margin-bottom: 20px;",
                                                 textOutput("population_counties_title")),
                                             DTOutput("population_covered_counties"),
                                             textOutput("caption")),
                                    
                                    tabPanel("Municipalities' Population Covered", 
                                             div(style = "font-size: 20px; font-weight: bold; text-align: center; color: darkblue; margin-top: 20px; margin-bottom: 20px;",
                                                 textOutput("population_municipalities_title")),
                                             DTOutput("population_covered_municipalities"),
                                             textOutput("caption")),
                                    
                                    tabPanel("SD' Students Covered", 
                                             div(style = "font-size: 20px; font-weight: bold; text-align: center; color: darkblue; margin-top: 20px; margin-bottom: 20px;",
                                                 textOutput("population_sd_title")),
                                             DTOutput("population_covered_sd"),
                                             textOutput("caption"))
                                )
                       ),
                       
                       # Space before the download button
                       fluidRow(class = "custom-spacing",
                                downloadButton("download_data", "Download Data")
                       ),
                       
                       # Add the new h3 title "National Data"
                       fluidRow(
                         column(12, tags$h3("National Data for FY 2022", class = "custom-section-spacing"))
                       ),
                       
                       # Plot Section with additional space
                       fluidRow(class = "custom-section-spacing",
                                tabsetPanel(
                                    tabPanel("Net Pension Liability", plotlyOutput("net_pension_plot")),
                                    tabPanel("Net OPEB Liability", plotlyOutput("opeb_plot")),
                                    tabPanel("Total Liabilities", plotlyOutput("total_liabilities_plot"))
                                )
                       )
                   )
               )
        )
    ),
    
    # Section for Top 10 Entities table with space
    fluidRow(class = "custom-section-spacing",
             column(12,
                    tags$h3("Top 10 Entities"),  
                    DTOutput("top10_table")
             )
    )
))
