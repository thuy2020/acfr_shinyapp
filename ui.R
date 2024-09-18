library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
library(bs4Dash)

# Define the UI
shinyUI(navbarPage(
    title = tags$div(
        tags$img(src = "logo.png", height = "30px", 
                 style = "margin-top: -5px; margin-right: 10px;"),
        span("Annual Comprehensive Financial Reports by States", class = "main-title")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
                       
                       fluidRow(
                           valueBoxOutput("box_net_pension", width = 4),  
                           valueBoxOutput("box_net_opeb", width = 4),
                           valueBoxOutput("box_total_liability", width = 4)
                       ),
                       # Tabset panel for different tables
                       tabsetPanel(
                           tabPanel("Entity Table", DTOutput("entity_table"), 
                                    textOutput("caption")),
                           tabPanel("Summary Table", DTOutput("summary_table"),
                                    textOutput("caption"))
                       ),
                       
                       downloadButton("download_data", "Download Data"),
                       # Tabbed panel for different plots
                       tabsetPanel(
                           tabPanel("Net Pension Liability", plotlyOutput("net_pension_plot")),
                           tabPanel("Net OPEB Liability", plotlyOutput("opeb_plot")),
                           tabPanel("Total Liabilities", plotlyOutput("total_liabilities_plot"))
                       )
                   )
               )
        )
    ),
    
    # Section for Top 10 Entities table
    fluidRow(
        column(12,
               tags$h3("Top 10 Entities"),  
               DTOutput("top10_table")
        )
    )
))
