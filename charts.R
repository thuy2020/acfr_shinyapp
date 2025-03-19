library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(plotly)
options(scipen = 9999)


df <- data.frame(state.abb, state.name)
census_pop_by_category <- readRDS("data/census_pop_by_category.RDS") %>% 
  rename(census_population = population) %>% left_join(df) %>% 
  mutate(state.name = ifelse(state.abb == "DC", "District of Columbia", state.name)) %>% 
  rename(`Public Employers` = category)


all_entities <- readRDS("data/data_for_acfr_shinyapp.RDS") %>%
  # avoid double count detroit public schools & detroit public schools community district
  mutate(population = ifelse(name == "detroit public schools community district", 0, population)) %>% 
  select(-c(state.abb, id, compensated_absences)) %>% 
  mutate(name = paste0('<a href="', url, '" target="_blank">', name, '</a>')) %>% 
  select(-url)


summarize_data <- function(data) {
  data %>%
    group_by(category) %>%
    summarize(
      population = sum(population, na.rm = TRUE),
      #net-net pension
      net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
      net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
      `Net-Net Pension Liability` = net_pension_liability - net_pension_assets,
      #net-net opeb
      net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
      net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
      `Net-Net OPEB Liability` = net_opeb_liability - net_opeb_assets,
      #total
      `Total Liabilities` = sum(total_liabilities, na.rm = TRUE)) %>%
    
    mutate(`Net-Net Pension & OPEB Liability` = `Net-Net Pension Liability` + `Net-Net OPEB Liability`) %>% 
    
    
    #style
    
    rename(`Public Employers` = category) %>% 
    select(`Public Employers`, 
           `Net-Net Pension Liability`, 
           `Net-Net OPEB Liability`, 
           `Net-Net Pension & OPEB Liability`, 
           `Total Liabilities`, population) %>% 
    #add the last total row
    bind_rows(summarise(., across(2:5, sum, na.rm = TRUE)) %>% 
                mutate(`Public Employers` = "Total")) %>% 
    #percent
    mutate(`Retirement Share of Total Liabilities (%)` = round((`Net-Net Pension & OPEB Liability` / `Total Liabilities`) * 100, 2)) 
  
}

# population_covered table

population_covered <- all_entities %>% 
  filter(year == 2023) %>% 
  select(state.name, category, population) %>% 
  group_by(state.name, category) %>% 
  mutate(population_covered = sum(population, na.rm = TRUE)) %>% select(-population) %>% 
  distinct() %>% 
  left_join(census_pop_by_category, 
            by = c("state.name" = "state.name", "category" = "Public Employers")) %>%
 mutate(pct_covered = round(population_covered/census_population*100,2)) 

population_covered_counties <- population_covered %>% 
  filter(category == "Counties") %>% ungroup() %>% 
  select(-c(category, state.abb))
  
population_covered_municipalities <- population_covered %>% 
  filter(category == "Municipalities") %>% ungroup() %>% 
  select(-c(category, state.abb))  

population_covered_sd <- population_covered %>% 
  filter(category == "School Districts") %>% ungroup() %>% 
  select(-c(category, state.abb)) 


###TODO: #Why MI has high sd pop covered while having few count of sd?
# mi_sd_acfr <- readRDS("data/data_for_acfr_shinyapp.RDS") %>% 
#   filter(state.name == "Michigan" & year == 2022 & category == "School Districts") %>% 
#   select(id, name, population) %>% 
#   filter(name != "detroit public schools community district") %>% 
#   distinct()
# 
# mi_sd_acfr %>% filter(duplicated(population))
# 
# census_pop_by_category %>% 
#   filter(state.name == "Michigan" & `Public Employers` == "School Districts")
# 
# mi_sd_acfr %>% summarise(pop = sum(population, na.rm = TRUE))

#########

dp <- all_entities %>% 
filter(year == 2023) %>% 
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
  ggplot(aes(fct_reorder(category, net_net_pension_liability), 
             net_net_pension_liability)) +
  geom_col(fill = "#CB644F") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Net-Net Pension Liability in FY 2023",
       caption = "Note: Net-Net Pension = Net Pension Liability - Net Pension Assets") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), 
                                                   accuracy = 1))


p_net_opeb <- dp %>% 
  ggplot(aes(fct_reorder(category, net_net_opeb_liability), 
             net_net_opeb_liability)) +
  geom_col(fill = "#C9950E") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Net-Net OPEP FY 2023", 
       caption = "Note: Net-Net OPEP = Net OPEP Liability - Net OPEB Assets") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), 
                                                   accuracy = 1)) 


p_total_liabilities <- dp %>% 
  ggplot(aes(fct_reorder(category, total_liabilities), 
             total_liabilities)) +
  geom_col(fill = "#7C5C77") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Nationwide Total Liabilities FY 2023") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), 
                                                   accuracy = 1)) 

