library(shiny)
library(DT)
library(shinydashboard)

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
                       )
                   )
               )
        )
    )
))
