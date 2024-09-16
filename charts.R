library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(plotly)
options(scipen = 999)

all_entities <- readRDS("data/data_for_acfr_shinyapp.RDS") %>% 
  select(-c(state.abb, id, url, compensated_absences))

