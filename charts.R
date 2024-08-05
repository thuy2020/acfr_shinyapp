library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

d <- readRDS("data/school_districts_for_shinyapp.RDS")

# all_school_districts <- d %>% 
#   select(2:revenues) %>% 
#   group_by(name, year) %>% 
#   mutate(across(5:revenues, list(tot = ~ sum(., na.rm = TRUE)), .names = "sum_{col}")) %>% 
#   select(state.name, name, year, contains("sum_")) %>% 
#   distinct() %>% 
#   pivot_longer(cols = 4:26,
#                names_to = "category", 
#                values_to = "value") %>% 
#   pivot_wider(names_from = year, 
#               values_from = value) %>% 
#   mutate(category = str_remove(category, "sum_")) %>% 
#   mutate(across(where(is.numeric), ~ comma(.))) %>% distinct()

#all_school_districts %>% saveRDS("all_school_districts.RDS")

all_school_districts <- readRDS("all_school_districts.RDS")
  
#d %>% colnames()

state_tot <- d %>% filter(state.name %in% c("California", "Ohio")) %>% 
  select(2:revenues) %>% 
  group_by(state.name, year) %>% 
  mutate(across(5:revenues, list(tot = ~ sum(., na.rm = TRUE)), .names = "sum_{col}")) %>% 
  select(state.name, contains("sum_")) %>% 
  distinct() %>% 
  pivot_longer(cols = 3:25,
               names_to = "category", 
               values_to = "value") %>% 
  pivot_wider(names_from = year, 
                          values_from = value) %>% 
  mutate(category = str_remove(category, "sum_")) %>% 
  mutate(across(where(is.numeric), ~ comma(.)))
  


