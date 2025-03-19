library(shiny)
library(DT)
library(dplyr)
library(plotly)
library(tidyr)
library(flextable)
library(officer)
library(ggplot2)

source("charts.R")

# Define the server logic
shinyServer(function(input, output, session) {
  
  filter_data <- function(data, input_state, input_entity, input_year) {
    if (!is.null(input_state) && length(input_state) > 0) {
      data <- data %>% filter(state.name %in% input_state)
    }
    if (!is.null(input_entity) && length(input_entity) > 0) {
      data <- data %>% filter(category %in% input_entity)
    }
    if (is.null(input_year) || length(input_year) == 0) {
      data <- data %>% filter(year == 2023)
    } else {
      data <- data %>% filter(year %in% input_year)
    }
    return(data)
  }
  
  filtered_data <- reactive({
    filter_data(all_entities, input$selected_state, input$selected_entity_type, input$selected_year)
  })
  
  
  # # Reactive function to filter data based on input selections for the totals table
  # filtered_data <- reactive({
  #   data <- all_entities
  #   
  #   # Apply filters based on user input
  #   if (!is.null(input$selected_state) && length(input$selected_state) > 0) {
  #     data <- data %>% filter(state.name %in% input$selected_state)
  #   }
  #   if (!is.null(input$selected_entity_type) && length(input$selected_entity_type) > 0) {
  #     data <- data %>% filter(category %in% input$selected_entity_type)
  #   }
  #   
  #   # If no year is selected, default to 2023
  #   if (is.null(input$selected_year) || length(input$selected_year) == 0) {
  #     data <- data %>% filter(year == 2023)
  #   } else {
  #     data <- data %>% filter(year %in% input$selected_year)
  #   }
  #   return(data)
  # })

  #Adding dynamic titles for each table with selected filters
  output$entity_table_title <- renderText({
    paste("Entity Table - State:", paste(input$selected_state, collapse = ", "),
          "| Year:", paste(input$selected_year, collapse = ", "),
          "| Entity Type:", paste(input$selected_entity_type, collapse = ", "))
  })
  
  output$summary_table_title <- renderText({
    paste("Summary Table - State:", paste(input$selected_state, collapse = ", "), 
          "| Year:", paste(input$selected_year, collapse = ", "), 
          "| Entity Type:", paste(input$selected_entity_type, collapse = ", "))
  })
  
  output$population_counties_title <- renderText({
    "Counties' Population Covered by State | Year: 2023"
  })
  
  output$population_municipalities_title <- renderText({
    "Municipalities' Population Covered by State | Year: 2023"
  })
  
  output$population_sd_title <- renderText({
    "School Districts' Students Covered by State | Year: 2023"
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
    year_to_filter <- if (is.null(input$selected_year) || length(input$selected_year) == 0) 2023 
    else input$selected_year
    
    d <- filtered_data() %>% 
      filter(year %in% year_to_filter)
    
    # Get the summarized data using the existing function
    result <- summarize_data(d)
    
    # If a single state is selected, incorporate the additional logic
    if (!is.null(input$selected_state) && length(input$selected_state) == 1) {
      selected_state <- input$selected_state
      
      result <- result %>% 
        left_join(census_pop_by_category %>% filter(state.name == selected_state), 
                  by = c("Public Employers")) %>%  
        mutate(`Population Covered (%)` = round(population / census_population * 100, 2)) %>% 
        select(-c(state.abb, population, state.name, census_population))
    }
    
    return(result)
    
  })
  
  # Render the summary table
  
  output$summary_table <- renderDT({
    data <- summary_data() %>% 
      mutate(`Public Employers` = factor(`Public Employers`, 
                                         levels = c("State", "Counties", "Municipalities", 
                                                     "School Districts", "Total"))) %>%
      arrange(`Public Employers`) 
    
    datatable(data, options = list(pageLength = 10)) %>% 
                formatStyle(columns = 2:6,'text-align' = 'right') %>% 
                formatRound(columns = 2:6, digits = 0)
              
  })
  
  # Render the population_covered table
  output$population_covered_counties <- renderDT({
    data <- population_covered_counties
    datatable(data, options = list(pageLength = 10)) %>% 
      formatStyle(columns = 2:3,'text-align' = 'right') %>% 
      formatRound(columns = 2:3, digits = 0)
    
  }) 
  
  output$population_covered_municipalities <- renderDT({
    data <- population_covered_municipalities
    datatable(data, options = list(pageLength = 10)) %>% 
      formatStyle(columns = 2:3,'text-align' = 'right') %>% 
      formatRound(columns = 2:3, digits = 0)
    
  }) 
  
  output$population_covered_sd <- renderDT({
    data <- population_covered_sd
    datatable(data, options = list(pageLength = 10)) %>% 
      formatStyle(columns = 2:3,'text-align' = 'right') %>% 
      formatRound(columns = 2:3, digits = 0)
    
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
  
# Summary table
  
  format_billions <- function(x) {
    paste0(round(x / 1e9, 1), " billion")  # Convert to billions 
  }
  
  format_percentage <- function(x) {
    paste0(round(x, 0), "%")  # appends "%"
  }
  
  output$download_summary_table <- downloadHandler(
    filename = function() {
      paste("Summary_Table_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      summary_data_to_download <- summary_data() %>%     # Get the filtered summary table data
        select(-c(`Net-Net Pension & OPEB Liability`, population)) %>% 
        mutate(`Public Employers` = factor(`Public Employers`, levels = c("State", "Counties", "Municipalities", "School Districts", "Total"))) %>%
        arrange(`Public Employers`)
        
        # Apply number formatting to billions
      numeric_cols <- c("Net-Net Pension Liability", "Net-Net OPEB Liability", 
                        "Total Liabilities")
      summary_data_to_download[numeric_cols] <- lapply(summary_data_to_download[numeric_cols], format_billions)
  
      summary_data_to_download$`Retirement Share of Total Liabilities (%)` <- 
        format_percentage(summary_data_to_download$`Retirement Share of Total Liabilities (%)`)
      # Rename columns
      colnames(summary_data_to_download) <- gsub("Net-Net Pension Liability", "Net Pension Liability", 
                                                 colnames(summary_data_to_download))
      colnames(summary_data_to_download) <- gsub("Net-Net OPEB Liability", "Net OPEB Liability", 
                                                 colnames(summary_data_to_download))
      colnames(summary_data_to_download) <- gsub("Retirement Share of Total Liabilities \\(\\%\\)", "Pension + OPEB Share of Total", 
                                                 colnames(summary_data_to_download))  
      
      # Convert the dataframe to a flextable 
      table_ft <- flextable(summary_data_to_download) %>%
        set_table_properties(layout = "autofit") %>%
        theme_vanilla() %>%
        align(align = "center", part = "all") %>%
        fontsize(size = 12, part = "all") %>%
        autofit()
      
      # Word document 
      doc <- read_docx()
      doc <- body_add_par(doc, "Summary Table", style = "heading 1")
      doc <- body_add_flextable(doc, value = table_ft)
      print(doc, target = file)
    }
  )
####Value boxes####
  # value box net pension 
  output$box_net_pension <- renderValueBox({
    data <- summary_data() %>% slice_tail(n = 1)
    valueBox(formatC(sum(data$`Net-Net Pension Liability`), 
                     format = "f", big.mark = ",", digit = 0),
             "Net-Net Pension Liability", icon = icon("money-bill"))
  })
  
  # Value box OPEB 
  output$box_net_opeb <- renderValueBox({
    data <- summary_data() %>% slice_tail(n = 1)
    
    valueBox(formatC(sum(data$`Net-Net OPEB Liability`), 
                     format = "f", big.mark = ",", digit = 0),
             "Net-Net OPEB Liability", icon = icon("chart-line"))
  })
  
  # Value box total liabilities
  output$box_total_liability <- renderValueBox({
    data <- summary_data()%>% slice_tail(n = 1)
    
    valueBox(formatC(sum(data$`Total Liabilities`), 
                     format = "f", big.mark = ",", digit = 0),
             "Total Liabilities", icon = icon("briefcase"))
  })
  
####Plot####  
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
  
  
  # Render top 10 entities table
  output$top10_table <- renderDT({
    data <- filtered_data() %>%
      arrange(desc(net_pension_liability)) %>%
      head(10)
    datatable(data, escape = FALSE, options = list(pageLength = 10))
  })
  
})