library(shiny)
library(DT)
library(shinydashboard)
library(plotly)

# Define the UI
shinyUI(navbarPage(
    title = tags$div(
        tags$img(src = "logo.png", height = "30px", style = "margin-top: -5px; margin-right: 10px;"),
        span("School Districts Debt", class = "main-title")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    fluidRow(
        column(12,
               sidebarLayout(
                   sidebarPanel(
                       tags$div(class = "select-input-custom",
                                selectInput("selected_state", "Select State:", choices = NULL)
                       ),
                       uiOutput("school_district_ui")
                       
                   ),
                   mainPanel(
                       fluidRow(
                           valueBoxOutput("box_2020", width = 2.5),
                           valueBoxOutput("box_2021", width = 2.5),
                           valueBoxOutput("box_2022", width = 2.5)
                          # valueBoxOutput("box_2023", width = 2.5)
                       ),
 
                       DTOutput("totals_table"),
                       tags$div(style = "margin-top: 20px;", 
                                downloadButton("download_data", "Download")
                       ),
                       tabsetPanel(
                           tabPanel("Net Pension Liability", plotlyOutput("net_pension_plot")),
                           tabPanel("Net OPEB Liability", plotlyOutput("opeb_plot")),
                           tabPanel("Total Liabilities", plotlyOutput("total_liabilities_plot"))
                       )
                   )
               )
        )
    ),
    # New section for Top 100 School Districts table
    fluidRow(
        column(12,
               tags$h3("Top 100 School Districts"),  # Add a header for the new section
               DTOutput("top100_table")  # Placeholder for the Top 100 table
        )
    )
    
    
    
    

))
