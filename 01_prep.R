#
library(tidyverse)
library(janitor)

#read csv in
ccp_april_26_centre_lookup = read_csv('lookup_centres/ccp_dag_id_lookup_26-April-2020.csv')

#read ethnicity files

#For england
eng_ethnicity_data = read_csv('ethnicity_data/england_ethnicity.csv') %>% clean_names() %>% 
  filter(indicator_name == 'Asian or Asian British ethnic group: % of population' | 
           indicator_name == 'Black and Minority Ethnic (BME) Population' | indicator_name == "Percentage of population whose ethnicity is not 'White UK'") %>% 
  select(area_code, area_name, sex, age, time_period, indicator_name, value)

eng_ethnicity_data = eng_ethnicity_data %>% 
  pivot_wider(names_from = indicator_name, values_from = value) %>% 
  clean_names() %>% 
  mutate(perc_asian = asian_or_asian_british_ethnic_group_percent_of_population,
         perc_black = black_and_minority_ethnic_bme_population,
         perc_white = 100-percentage_of_population_whose_ethnicity_is_not_white_uk,
         perc_other = 100 - (perc_asian + perc_black + perc_white),
         perc_other = ifelse(perc_other < 0, 0, perc_other)) %>% 
  select(area_code, area_name, perc_asian, perc_black, perc_white, perc_other)

#for scotland 
scotland_new_lookup_codes = read_csv('lookup_centres/scotland_hb_code_conversion.csv') 

scotland_ethnicity_data = read_csv('ethnicity_data/scotland_ethnicity_2011.csv') %>% clean_names() %>% 
                          rename(health_board_id = x1) %>% 
                          select(health_board_id, all_people, white, asian_asian_scottish_or_asian_british, african, caribbean_or_black, other_ethnic_groups) %>% 
                          mutate(perc_asian_scot = (asian_asian_scottish_or_asian_british / all_people)*100,
                                 perc_black_scot = ((caribbean_or_black + african) / all_people)*100,
                                 perc_white_scot = (white / all_people)*100,
                                 perc_other_scot = (other_ethnic_groups / all_people)*100) %>% 
                          select(health_board_id, perc_asian_scot, perc_white_scot, perc_black_scot, perc_other_scot) %>% 
                          left_join(scotland_new_lookup_codes, by = c('health_board_id' = 'HB')) %>% 
                          mutate(health_board_id = HB19) %>% 
                          select(health_board_id, perc_asian_scot, perc_white_scot, perc_black_scot, perc_other_scot) 
  unique(ccp_april_26_centre_lookup$redcap_data_access_group)


#Finally combine with DAG data
ccp_ethnicity_centre_lookup = ccp_april_26_centre_lookup %>% 
  left_join(eng_ethnicity_data, by = c('ccg' = 'area_code')) %>% 
  left_join(scotland_ethnicity_data, by = c('ccg' = 'health_board_id')) %>% 
  mutate(perc_asian = ifelse(country == 'Wales' & redcap_data_access_group == 'Cardiff and Vale University Health Board', 5.562, perc_asian),
         perc_black = ifelse(country == 'Wales' & redcap_data_access_group == 'Cardiff and Vale University Health Board', 1.665, perc_black),
         perc_white = ifelse(country == 'Wales' & redcap_data_access_group == 'Cardiff and Vale University Health Board', 88.845, perc_white),
         perc_other = ifelse(country == 'Wales' & redcap_data_access_group == 'Cardiff and Vale University Health Board', 3.8965, perc_other),
         perc_asian = ifelse(country == 'Wales' & redcap_data_access_group != 'Cardiff and Vale University Health Board', 1.2767, perc_asian),
         perc_black = ifelse(country == 'Wales' & redcap_data_access_group != 'Cardiff and Vale University Health Board', 0.4882, perc_black),
         perc_white = ifelse(country == 'Wales' & redcap_data_access_group != 'Cardiff and Vale University Health Board', 96.7669, perc_white),
         perc_other = ifelse(country == 'Wales' & redcap_data_access_group != 'Cardiff and Vale University Health Board', 1.47572, perc_other)) %>% 
  mutate(perc_asian = ifelse(country == 'Scotland', perc_asian_scot, perc_asian),
         perc_black = ifelse(country == 'Scotland', perc_black_scot, perc_black),
         perc_white = ifelse(country == 'Scotland', perc_white_scot, perc_white),
         perc_other = ifelse(country == 'Scotland', perc_other_scot, perc_other)) %>% 
  select(- perc_black_scot, - perc_white_scot, -perc_other_scot, -perc_asian_scot) %>% 
  mutate(perc_asian = ifelse(is.na(perc_asian), (100-perc_white)/2 , perc_asian),
         perc_other = ifelse(is.na(perc_other), (100-perc_white)/2 , perc_other))

#write csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(ccp_ethnicity_centre_lookup, paste0('ccp_ethnicity_out_', save_date, '.csv'))