library(shiny)
library(DT)
library(dplyr)
library(plotly)

# Define the server logic
shinyServer(function(input, output, session) {
  
  # Reactive function to filter data based on input selections
  filtered_data <- reactive({
    data <- all_entities
    
    # Apply filters based on user input
    if (!is.null(input$selected_state) && length(input$selected_state) > 0) {
      data <- data %>% filter(state.name %in% input$selected_state)
    }
    if (!is.null(input$selected_entity_type) && length(input$selected_entity_type) > 0) {
      data <- data %>% filter(category %in% input$selected_entity_type)
    }
    if (!is.null(input$selected_year) && length(input$selected_year) > 0) {
      data <- data %>% filter(year %in% input$selected_year)
    }
    
    return(data)
  })
  
  # Render the summary table
  output$totals_table <- renderDT({
    data <- filtered_data()
    datatable(data, options = list(pageLength = 10))
  })
  
  # Render top 10 entities table
  output$top10_table <- renderDT({
    data <- filtered_data() %>%
      arrange(desc(net_pension_liability)) %>%
      head(10)
    datatable(data, options = list(pageLength = 10))
  })
  
  # Reactive function to summarize data for value boxes
  summary_data <- reactive({
    data <- filtered_data()
    
    # Summarize data by category and year (if year is selected)
    if (!is.null(input$selected_year) && length(input$selected_year) > 0) {
      data <- data %>% filter(year %in% input$selected_year)
    }
    
    data %>% 
      group_by(category) %>%
      summarize(total_liabilities = sum(total_liabilities, na.rm = TRUE),
                net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
                net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
                netted_net_pension_liability = sum(net_pension_liability - net_pension_assets, na.rm = TRUE),
                net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
                net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
                netted_net_opeb_liability = sum(net_opeb_liability - net_opeb_assets, na.rm = TRUE))
  })
  
  # Render value boxes for net pension liability
  output$box_net_pension <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$netted_net_pension_liability), format = "f", big.mark = ",", digits = 2),
             "Net Pension Liabilities", icon = icon("dollar-sign"), color = "blue")
  })
  
  # Render value box for net OPEB liability
  output$box_net_opeb <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$netted_net_opeb_liability), format = "f", big.mark = ",", digits = 2),
             "Net OPEB Liabilities", icon = icon("dollar-sign"), color = "green")
  })
  
  # Render value box for total liabilities
  output$box_total_liability <- renderValueBox({
    data <- summary_data()
    
    valueBox(formatC(sum(data$total_liabilities), format = "f", big.mark = ",", digits = 2),
             "Total Liabilities", icon = icon("dollar-sign"), color = "red")
  })
  
  # Plot for Net Pension Liability
  output$net_pension_plot <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~year, y = ~net_pension_liability, color = ~state.name, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Net Pension Liability Over Time",
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Net Pension Liability"))
  })
  
  # Plot for Net OPEB Liability
  output$opeb_plot <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~year, y = ~net_opeb_liability, color = ~state.name, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Net OPEB Liability Over Time",
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Net OPEB Liability"))
  })
  
  # Plot for Total Liabilities
  output$total_liabilities_plot <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~year, y = ~total_liabilities, color = ~state.name, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Total Liabilities Over Time",
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Total Liabilities"))
  })
  
  # Download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
})
