library(shiny)
library(DT)
library(dplyr)
library(plotly)
library(tidyr)
library(forcats)
source("charts.R")

# Define the server logic
shinyServer(function(input, output, session) {
  
  # Reactive function to filter data based on input selections for the totals table
  filtered_data <- reactive({
    data <- all_entities
    
    # Apply filters based on user input
    if (!is.null(input$selected_state) && length(input$selected_state) > 0) {
      data <- data %>% filter(state.name %in% input$selected_state)
    }
    if (!is.null(input$selected_entity_type) && length(input$selected_entity_type) > 0) {
      data <- data %>% filter(category %in% input$selected_entity_type)
    }
    
    # If no year is selected, default to 2022
    if (is.null(input$selected_year) || length(input$selected_year) == 0) {
      data <- data %>% filter(year == 2022)
    } else {
      data <- data %>% filter(year %in% input$selected_year)
    }
    
    return(data)
  })
  
  # Entity table
  output$entity_table <- renderDT({
    data <- filtered_data()
    datatable(data, escape = FALSE, options = list(pageLength = 10)) %>% 
                formatStyle(columns = 5:14,'text-align' = 'right') %>% 
                formatRound(columns = 5:14, digits = 0)
  })
  
  # Reactive function to calculate the summary table data
  summary_data <- reactive({
    year_to_filter <- if (is.null(input$selected_year) || length(input$selected_year) == 0) 2022 
    else input$selected_year
    
    d <- all_entities %>% filter(year %in% year_to_filter)
    summarize_data(d)
    
  })
  
  # Render the summary table
  output$summary_table <- renderDT({
    data <- summary_data()
    datatable(data, options = list(pageLength = 10)) %>% 
                formatStyle(columns = 2:5,'text-align' = 'right') %>% 
                formatRound(columns = 2:5, digits = 0)
              
  })
  
  output$caption <- renderText({
    selected_state <- ifelse(length(input$selected_state) == 1, input$selected_state, "all states")
    selected_year <- ifelse(length(input$selected_year) == 1, input$selected_year, "all years")
    
    paste0("Source: Annual comprehensive financial reports for ", selected_state, 
           " governmental units, fiscal year ", selected_year, ".", 
           " Note: Net-Net Pension Liability = Net Pension Liability - Net Pension Assets",
           " Note: Net-Net OPEB Liability = Net OPEB Liability - Net OPEB Assets")
  })
  
  # Download handler for both totals and summary tables in one Excel file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Reason-ACFR-data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Get the totals data and summary table data
      entity_data <- filtered_data()
      summary_data_to_download <- summary_data()  # Use the same data as rendered in the summary_table
      
      # Write both dataframes to separate sheets in an Excel file
      writexl::write_xlsx(list("Entity Table" = entity_data, 
                               "Summary Table" = summary_data_to_download), path = file)
    }
  )
  
  # Render top 10 entities table
  output$top10_table <- renderDT({
    data <- filtered_data() %>%
      arrange(desc(net_pension_liability)) %>%
      head(10)
    datatable(data, options = list(pageLength = 10))
  })
  
  # Render value boxes for net pension liability
  output$box_net_pension <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$`Net-Net Pension Liability`), 
                     format = "f", big.mark = ",", digit = 0),
             "Net Pension Liabily", icon = icon("dollar-sign"))
  })
  
  # Render value box for net OPEB liability
  output$box_net_opeb <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$`Net-Net OPEB Liability`), 
                     format = "f", big.mark = ",", digit = 0),
             "Net OPEB Liability", icon = icon("dollar-sign"))
  })
  
  # Render value box for total liabilities
  output$box_total_liability <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$`Total Liabilities`), 
                     format = "f", big.mark = ",", digit = 0),
             "Total Liabilities", icon = icon("dollar-sign"))
  })
  
  # Render the net pension liability plot
  output$net_pension_plot <- renderPlotly({
   ggplotly(p_net_pension_liability)
  })
  
  # Plot for Net OPEB Liability
  output$opeb_plot <- renderPlotly({
    ggplotly(p_net_opeb)
  })
  
  # Plot for Total Liabilities
  output$total_liabilities_plot <- renderPlotly({
    ggplotly(p_total_liabilities)
  })
  
})