library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(plotly)
options(scipen = 999)

all_entities <- readRDS("data/data_for_acfr_shinyapp.RDS") %>% 
  select(-c(state.abb, id, compensated_absences)) %>% 
  mutate(name = paste0('<a href="', url, '" target="_blank">', name, '</a>')) %>% 
  select(-url)



summarize_data <- function(data) {
  data %>%
    group_by(category) %>%
    summarize(
      net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
      net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
      `Net-Net Pension Liability` = net_pension_liability - net_pension_assets,
      net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
      net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
      `Net-Net OPEB Liability` = net_opeb_liability - net_opeb_assets,
      `Total Liabilities` = sum(total_liabilities, na.rm = TRUE)
    ) %>%
    mutate(
      `Net-Net Pension & OPEB Liability` = `Net-Net Pension Liability` + `Net-Net OPEB Liability`,
      `Retirement Share of Total Liabilities (%)` = round((`Net-Net Pension & OPEB Liability` / `Total Liabilities`) * 100, 1)
    ) %>%
    rename(`Public Employers` = category) %>% 
    select(`Public Employers`, 
           `Net-Net Pension Liability`, 
           `Net-Net OPEB Liability`, 
           `Net-Net Pension & OPEB Liability`, 
           `Total Liabilities`, 
           `Retirement Share of Total Liabilities (%)`)
}

#check function
# summarize_data(all_entities %>%
#                filter(state.name == "Arizona" & year == 2022)) %>% View()


dp <- all_entities %>% 
filter(year == 2022) %>% 
  group_by(category) %>%
  summarize(total_liabilities = sum(total_liabilities, na.rm = TRUE),
            # net_net pension
            net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
            net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
            net_net_pension_liability = sum(net_pension_liability - net_pension_assets, na.rm = TRUE),
            #netted opeb
            net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
            net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
            net_net_opeb_liability = sum(net_opeb_liability - net_opeb_assets, na.rm = TRUE))


p_net_pension_liability <- dp %>% 
  ggplot(aes(fct_reorder(category, netted_net_pension_liability), 
             netted_net_pension_liability)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Net Pension Liability in FY 2022",
       caption = "Note: Net-Net Value") +
  scale_y_continuous(labels = scales::comma)


p_net_opeb <- dp %>% 
  ggplot(aes(fct_reorder(category, netted_net_opeb_liability), 
             netted_net_opeb_liability)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Net OPEP FY 2022", 
       caption = "Note: Net-Net Value") +
  scale_y_continuous(labels = scales::comma)




p_total_liabilities <- dp %>% 
  ggplot(aes(fct_reorder(category, total_liabilities), 
             total_liabilities)) +
  geom_col(fill = "#B38F50") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Total Liabilities FY 2022") +
  scale_y_continuous(labels = scales::comma)


######
# Reactive function to summarize data for value boxes
# summary_data <- reactive({
#   data <- filtered_data() %>% filter(year == 2022)
#   
#   # Summarize data by category and year (if year is selected)
#   if (!is.null(input$selected_year) && length(input$selected_year) > 0) {
#     data <- data %>% filter(year %in% input$selected_year)
#   }
#   
#   data %>% 
#     group_by(category) %>%
#     summarize(total_liabilities = sum(total_liabilities, na.rm = TRUE),
#               # net_net pension
#               net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
#               net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
#               net_net_pension_liability = sum(net_pension_liability - net_pension_assets, na.rm = TRUE),
#               #net_net opeb
#               net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
#               net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
#               net_net_opeb_liability = sum(net_opeb_liability - net_opeb_assets, na.rm = TRUE))
# })
######
