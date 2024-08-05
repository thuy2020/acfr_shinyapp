library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
source("charts.R")  

# Define the server logic
shinyServer(function(input, output, session) {
  
  # Populate the selectInput choices dynamically
  observe({
    updateSelectInput(session, "selected_state", 
                      choices = unique(state_tot$state.name))
  })
  
  # school district choices based on selected state
  observe({
    req(input$selected_state)
    school_districts <- all_school_districts %>%
      filter(state.name == input$selected_state) %>%
      pull(name)
    updateSelectInput(session, "selected_district", 
                      choices = c("All", school_districts))
  })
  
  output$school_district_ui <- renderUI({
    tags$div(class = "select-input-custom",
            selectInput("selected_district", "Select School District:", choices = NULL)
            )
  })
  
  # Filter the data based on the selected state
  filtered_data <- reactive({
    req(input$selected_state, input$selected_district)
    if(is.null(input$selected_district) || input$selected_district == "All"){
      state_tot %>% filter(state.name == input$selected_state)
    } else{
      all_school_districts %>% filter(state.name == input$selected_state, 
                                      name == input$selected_district)
    }
   
  })
  
  # Render the table
  output$totals_table <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 10, order = list(list(1, 'asc'))))
  })
  
  
  # Render value boxes
  output$box_2020 <- renderValueBox({
    valueBox(
      value = "12345",,
      subtitle = "Total Liabilities",
      icon = icon("chart-line")
    )
  })
  
  output$box_2021 <- renderValueBox({
    valueBox(
      value = "12345",
      subtitle = "Total Assets",
      icon = icon("briefcase")
    )
  })
  
  output$box_2022 <- renderValueBox({
    valueBox(
      value = "12345",
      subtitle = "Total Revenues",
      icon = icon("money-bill")
    )
  })
  
  
  output$box_2023 <- renderValueBox({
    valueBox(
      value = "12345",
      subtitle = "Total students",
      icon = icon("users")
    )
  })
  
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Reason-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  

})
