library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(plotly)
options(scipen = 999)
data <- readRDS("data/school_districts_for_shinyapp.RDS") 

state_tot <- readRDS("state_tot.RDS")

data %>% 
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
  select(-c(name, enrollment_22, `2023`)) %>% 
  pivot_longer(3:5, names_to = "year", values_to = "value") %>% select(-year) %>% distinct() %>% 
  group_by(state.name, category) %>%
  summarise(allyears = sum(value, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~ comma(.))) %>% saveRDS("valuebox_data.RDS")

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

# top10_chart_data %>% 
#   filter(state.name == "California") %>% 
#   filter(category == "net_pension_liability") %>%  
#   select(name, 2020) %>% 
#   ggplot(aes(fct_reorder(name, 2020), 2020)) +
#   geom_col(fill = "steelblue")+
#   coord_flip()+
#   scale_y_continuous(labels = comma)+
#   labs(x = "",
#        y = "")+
#   theme_minimal() -> top10_chart
# 
# top10_chart_plotly <- ggplotly(top10_chart)