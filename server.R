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
    updateSelectizeInput(session, "selected_district", 
                      choices = c("All", school_districts))
  })
  
  output$school_district_ui <- renderUI({
    tags$div(class = "select-input-custom",
             selectizeInput("selected_district", "Select School District:", 
                        choices = NULL,
                        options = list(
                          placeholder = 'Start typing to search...',
                          maxOptions = 1000  # Adjust the number of options to display
                        )
                        )
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
  
  # net_pension_liability_allyears
  net_pension_liability_allyears <- reactive({
    req(input$selected_state)
    valuebox_data %>%
      filter(state.name == input$selected_state, category == "net_pension_liability") %>%
      pull(allyears)
  })
  
  output$box_2020 <- renderValueBox({
    valueBox(
      value = net_pension_liability_allyears(),  
      subtitle = "NPL in 4 years",
      icon = icon("chart-line")
    )
  })
  
  # net_opeb_liability all years
  
  net_opeb_liability_allyears <- reactive({
    req(input$selected_state)
    valuebox_data %>%
      filter(state.name == input$selected_state, category == "net_opeb_liability") %>%
      pull(allyears)
  })
  output$box_2021 <- renderValueBox({
    valueBox(
      value = net_opeb_liability_allyears(),
      subtitle = "OPEB Liability in 4 years",
      icon = icon("briefcase")
    )
  })
  
  # total_liabilities all years
  
  total_liabilities_allyears <- reactive({
    req(input$selected_state)
    valuebox_data %>%
      filter(state.name == input$selected_state, category == "total_liabilities") %>%
      pull(allyears)
  })
  
  output$box_2022 <- renderValueBox({
    valueBox(
      value = total_liabilities_allyears(),
      subtitle = "Total Liabilities in 4 years",
      icon = icon("money-bill")
    )
  })
  
  # output$box_2023 <- renderValueBox({
  #   valueBox(
  #     value = "12345",
  #     subtitle = "Total students",
  #     icon = icon("users")
  #   )
  # })
  
  
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
