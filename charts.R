library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

data <- readRDS("data/school_districts_for_shinyapp.RDS") 

d <- data %>% 
  arrange(state.name) %>% 
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
  mutate(category = str_remove(category, "sum_")) 

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

all_school_districts <- readRDS("all_school_districts.RDS") %>% 
  arrange(state.name, desc(enrollment_22)) %>% 
  mutate(across(where(is.numeric) & !all_of("enrollment_22"), ~ comma(.))) %>% 
  distinct()

state_tot <- d %>% 
  mutate(across(where(is.numeric), ~ comma(.)))

valuebox_data <- d %>% 
  pivot_longer(3:6, names_to = "year", values_to = "value") %>% 
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
  group_by(state.name, category) %>% 
  summarise(allyears = sum(value)) %>% 
  mutate(across(where(is.numeric), ~ comma(.)))



#Top 10 each state

top10_each_state <- readRDS("all_school_districts.RDS") %>% 
  select(state.name, name, enrollment_22) %>% 
  distinct() %>% 
  arrange(state.name, desc(enrollment_22)) %>%      
  group_by(state.name) %>%                              
  slice_head(n = 10)

top10 <- readRDS("all_school_districts.RDS") %>%  
  select(-enrollment_22) %>% 
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
  #filter(state.name == "California")  %>% 
  inner_join(top10_each_state, by = c("state.name", "name"))

#top10 %>% filter(category == "net_pension_liability") 
# ggplot(aes())