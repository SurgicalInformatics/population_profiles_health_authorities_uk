#
library(tidyverse)
library(janitor)

#read csv in
ccp_april_25_centre_lookup = read_csv('lookup_centres/ccp_dag_id_lookup_25-April-2020.csv')

#read ethnicity files

#For england
eng_ethnicity_data = read_csv('ethnicity_data/england_ethnicity.csv') %>% clean_names() %>% 
  filter(indicator_name == 'Asian or Asian British ethnic group: % of population' | 
           indicator_name == 'Black and Minority Ethnic (BME) Population' | indicator_name == "Percentage of population whose ethnicity is not 'White UK'") %>% 
  select(area_code, area_name, sex, age, time_period, indicator_name, value)

eng_ethnicity_data = eng_ethnicity_data %>% 
  pivot_wider(names_from = indicator_name, values_from = value) %>% 
  clean_names()

#Finally combine with DAG data
ccp_april_25_centre_lookup %>% 
  left_join(eng_ethnicity_data, by = c('ccg' = 'area_code'))