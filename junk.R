
###CUrrent server.R@@

library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)
source("charts.R")  

# Define the server logic
shinyServer(function(input, output, session) {
  
  # Populate the selectInput choices dynamically
  observe({
    updateSelectInput(session, "selected_state", 
                      choices = unique(all_entities$state.name))
  })
  
  observe({
    updateSelectInput(session, "selected_entity_type", 
                      choices = c("State",
                                  "County",
                                  "Municipality", 
                                  "School District")) 
  })
  
  
  observe({
    req(input$selected_state, input$selected_entity_type)
    entities <- all_entities %>%
      filter(state.name == input$selected_state, 
             entity.type == input$selected_entity_type) %>%
      pull(name)
    updateSelectizeInput(session, "selected_entity", 
                         choices = c("All", entities),
                         server = TRUE)
  })
  
  output$entity_ui <- renderUI({
    tags$div(class = "select-input-custom",
             selectizeInput("selected_entity", "Select Entity:", 
                            choices = NULL,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 1000  
                            )
             )
    )
  })
  
  # Filter the data based on the selected state
  filtered_data <- reactive({
    req(input$selected_state, 
        input$selected_entity_type, 
        input$selected_entity)
    if (input$selected_entity == "All") {
      all_entities %>% filter(state.name == input$selected_state, 
                              entity.type == input$selected_entity_type)
    } else {
      all_entities %>% filter(state.name == input$selected_state, 
                              entity.type == input$selected_entity_type, 
                              name == input$selected_entity)
    }
  })
  
  # Render the table
  output$totals_table <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 10, 
                             order = list(list(1, 'asc'))))
  })
  
  
})

##########

state_tot <- readRDS("state_tot.RDS")

# data %>% 
#   filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
#   select(state.name, category, `2022`) %>% distinct() %>% drop_na() %>% 
#   
#   
#   #pivot_longer(3:5, names_to = "year", values_to = "value") %>% select(-year) %>% distinct() %>% 
#   #group_by(state.name, category) %>%
#  # summarise(allyears = sum(value, na.rm = TRUE)) %>% 
#   mutate(across(where(is.numeric), ~ comma(.))) %>% 
#   saveRDS("valuebox_data.RDS")

valuebox_data <- readRDS("valuebox_data.RDS")


### Save to an RDS object to reduce run time 
# all_school_districts <- data %>%
#    select(2:revenues, enrollment_22) %>%
#    group_by(state.name, name, year) %>%
#    mutate(across(5:revenues, list(tot = ~ sum(., na.rm = TRUE)), .names = "sum_{col}")) %>%
#    select(state.name, name, year, contains("sum_"), enrollment_22) %>%
#    distinct() %>%
#    pivot_longer(cols = 4:25,
#                 names_to = "category",
#                 values_to = "value") %>%
#    pivot_wider(names_from = year,
#                values_from = value) %>%
#    mutate(category = str_remove(category, "sum_")) 
#mutate(across(where(is.numeric) & !all_of("enrollment_22"), ~ comma(.))) %>% distinct()

# all_school_districts %>% saveRDS("all_school_districts.RDS")

all_school_distrcts_raw <- readRDS("all_school_districts.RDS") 

all_school_districts <- all_school_distrcts_raw %>% 
  arrange(state.name, desc(enrollment_22)) %>% 
  #mutate(across(where(is.numeric) & !all_of("enrollment_22"), ~ comma(.))) %>% 
  distinct()


#Top 10 each state

top10_each_state <- all_school_distrcts_raw %>% 
  select(state.name, name, enrollment_22) %>% 
  distinct() %>% 
  arrange(state.name, desc(enrollment_22)) %>%      
  group_by(state.name) %>%                              
  slice_head(n = 10)

top10_chart_data <- all_school_distrcts_raw %>%  
  select(-enrollment_22) %>% 
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
  inner_join(top10_each_state, by = c("state.name", "name")) %>% 
  filter(!state.name %in% c("Guam", "Puerto Rico")) %>% 
  mutate(name = str_to_title(name))

# top 100 sd

top100_sd <- read.csv("data/top100_sd.csv") %>% 
  select(state.name, name, year, 
         net_pension_liability, net_opeb_liability, total_liabilities,
         enrollment_20, enrollment_21, enrollment_22) 




#   # net_pension_liability_allyears
#   net_pension_liability_allyears <- reactive({
#     req(input$selected_state)
#     valuebox_data %>%
#       filter(state.name == input$selected_state, 
#              category == "net_pension_liability") %>%
#       pull(`2022`)
#   })
#   
#   output$box_2020 <- renderValueBox({
#     valueBox(
#       value = net_pension_liability_allyears(),  
#       subtitle = "NPL 2022",
#       icon = icon("chart-line")
#     )
#   })
#   
#   # net_opeb_liability all years
#   net_opeb_liability_allyears <- reactive({
#     req(input$selected_state)
#     valuebox_data %>%
#       filter(state.name == input$selected_state, category == "net_opeb_liability") %>%
#       pull(`2022`)
#   })
#   output$box_2021 <- renderValueBox({
#     valueBox(
#       value = net_opeb_liability_allyears(),
#       subtitle = "OPEB Liability 2022",
#       icon = icon("briefcase")
#     )
#   })
#   
#   # total_liabilities all years
#   
#   total_liabilities_allyears <- reactive({
#     req(input$selected_state)
#     valuebox_data %>%
#       filter(state.name == input$selected_state, category == "total_liabilities") %>%
#       pull(`2022`)
#   })
#   
#   output$box_2022 <- renderValueBox({
#     valueBox(
#       value = total_liabilities_allyears(),
#       subtitle = "Total Liabilities 2022",
#       icon = icon("money-bill")
#     )
#   })
# 
#   
#   # Download handler
#   output$download_data <- downloadHandler(
#     filename = function() {
#       paste("Reason-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(filtered_data(), file, row.names = FALSE)
#     }
#   )
#   
#   
#   # Render net_pension_plot
#   output$net_pension_plot <- renderPlotly({
#     req(input$selected_state)
#     
#     # Filter the data based on selected state
#     filtered_data <- top10_chart_data %>%
#       filter(state.name == input$selected_state) %>%
#       filter(category == "net_pension_liability") %>%
#       select(name, `2022`)
#     
#     p <- filtered_data %>%
#       ggplot(aes(fct_reorder(name, `2022`), `2022`)) +
#       geom_col(fill = "#55C5E6") +
#       coord_flip() +
#       scale_y_continuous(labels = comma) +
#       labs(x = "", y = "", 
#            title = paste("Top 10 School Districts in ", input$selected_state, "in 2022")) +
#       theme_minimal()
#     
#     ggplotly(p)
#   })
#   
#   # Render the Net OPEB Liability plot
#   output$opeb_plot <- renderPlotly({
#     req(input$selected_state)
#     
#     filtered_data <- top10_chart_data %>%
#       filter(state.name == input$selected_state) %>%
#       filter(category == "net_opeb_liability") %>%
#       select(name, `2022`)
#     
#     p <- filtered_data %>%
#       ggplot(aes(fct_reorder(name, `2022`), `2022`)) +
#       geom_col(fill = "#3690CC") +
#       coord_flip() +
#       scale_y_continuous(labels = comma) +
#       labs(x = "", y = "", 
#            title = paste("Top 10 School Districts in ", input$selected_state, "in 2022")) +
#       theme_minimal()
#     
#     ggplotly(p)
#   })
#   
#   # Total Liabilities plot
#   output$total_liabilities_plot <- renderPlotly({
#     req(input$selected_state)
#     
#     filtered_data <- top10_chart_data %>%
#       filter(state.name == input$selected_state) %>%
#       filter(category == "total_liabilities") %>%
#       select(name, `2022`)
#     
#     p <- filtered_data %>%
#       ggplot(aes(fct_reorder(name, `2022`), `2022`)) +
#       geom_col(fill = "#125E9B") +
#       coord_flip() +
#       scale_y_continuous(labels = comma) +
#       labs(x = "", y = "", 
#            title = paste("Top 10 School Districts in ", input$selected_state, "in 2022")) +
#       theme_minimal()
#     
#     ggplotly(p)
#   })
#   
#   #Top100 sd
#   output$top100_table <- renderDataTable({
#     datatable(top100_sd, 
#               options = list(pageLength = 10, autoWidth = TRUE),  
#               rownames = FALSE)
#   
# 
# })