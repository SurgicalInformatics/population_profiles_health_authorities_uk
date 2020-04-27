#
library(tidyverse)
library(janitor)
library(rlang)

`%ni%` = Negate(`%in%`)

#read csv in
ccp_april_26_centre_lookup = read_csv('lookup_centres/ccp_dag_id_lookup_26-April-2020.csv') %>% 
                              mutate(ccg = ifelse(ccg == 'E38000230' & 
                                                    (place_name == 'Derriford Hospital' |
                                                       place_name == 'Royal Devon And Exeter Hospital (Wonford)' |
                                                       place_name == 'Torbay Hospital') , 'E38000152', ccg),
                                     ccg = ifelse(ccg == 'E38000230' & place_name == 'North Devon District Hospital', 'E38000129', ccg),
                                     ccg = ifelse(ccg == 'E38000229' & place_name == 'Chesterfield Royal Hospital', 'E38000115', ccg))

#For england
eng_wal_ethnicity_data = read_csv('ethnicity_data/england_wales_census.csv') %>% clean_names() %>% 
  rename(ccg = mnemonic,
         all_people = all_usual_residents) %>% 
  mutate(n_char_code = nchar(ccg)) %>% 
  filter(n_char_code == 9) %>% 
  select(-n_char_code, -area) %>% 
  rename(white_gypsy_traveller = white_gypsy_or_irish_traveller) %>% 
  mutate(mixed_or_multiple_ethnic_groups = mixed_multiple_ethnic_groups_white_and_black_caribbean + 
           mixed_multiple_ethnic_groups_white_and_black_african + mixed_multiple_ethnic_groups_white_and_asian + 
           mixed_multiple_ethnic_groups_other_mixed,
          white = white_english_welsh_scottish_northern_irish_british + white_irish + white_gypsy_traveller + white_other_white,
          african_caribbean = black_african_caribbean_black_british_african + black_african_caribbean_black_british_caribbean + black_african_caribbean_black_british_other_black,
          asian_asian_british = asian_asian_british_indian + asian_asian_british_pakistani + asian_asian_british_bangladeshi + asian_asian_british_chinese + asian_asian_british_other_asian)

vars_eng_to_perc = colnames(eng_wal_ethnicity_data)[colnames(eng_wal_ethnicity_data) %ni% c('ccg', 'all_people')]

perc_eng_wal_ethnicity_data = eng_wal_ethnicity_data %>% mutate_at(vars(all_of(vars_eng_to_perc)), .funs = list(perc = ~(. / all_people)*100))

#for scotland 
scotland_new_lookup_codes = read_csv('lookup_centres/scotland_hb_code_conversion.csv') 

scotland_ethnicity_data = read_csv('ethnicity_data/scotland_ethnicity_2011.csv') %>% clean_names() %>% 
                           rename(health_board_id = x1,
                                  asian_asian_british = asian_asian_scottish_or_asian_british,
                                  asian_asian_british_pakistani = asian_asian_scottish_or_asian_british_pakistani_pakistani_scottish_or_pakistani_british,
                                  asian_asian_british_indian = asian_asian_scottish_or_asian_british_indian_indian_scottish_or_indian_british,
                                  asian_asian_british_bangladeshi = asian_asian_scottish_or_asian_british_bangladeshi_bangladeshi_scottish_or_bangladeshi_british,
                                  asian_asian_british_chinese = asian_asian_scottish_or_asian_british_chinese_chinese_scottish_or_chinese_british,
                                  asian_asian_british_other_asian = asian_asian_scottish_or_asian_british_other_asian, 
                                  other_ethnic_group_arab = other_ethnic_groups_arab_arab_scottish_or_arab_british,
                                  other_ethnic_group_any_other_ethnic_group = other_ethnic_groups_other_ethnic_group) %>% 
  mutate(white_english_welsh_scottish_northern_irish_british = white_scottish + white_other_british,
         african_caribbean = african + caribbean_or_black)

vars_scot_to_perc = colnames(scotland_ethnicity_data)[colnames(scotland_ethnicity_data) %ni% c('health_board_id', 'all_people')]

as.varchar = function(x){
  as.numeric(as.character(x))
}

scotland_ethnicity_data = scotland_ethnicity_data %>% 
  mutate_at(c('african_other_african', 'caribbean_or_black_other_caribbean_or_black'), as.numeric)

perc_scotland_ethnicity_data = scotland_ethnicity_data %>% 
                                             mutate_at(vars(all_of(vars_scot_to_perc)), .funs = list(perc = ~(. / all_people)*100)) %>% 
                                             left_join(scotland_new_lookup_codes, by = c('health_board_id' = 'HB')) %>% 
                                             filter(!is.na(health_board_id)) %>% 
                                             mutate(health_board_id = HB19) %>% 
                                             select(-HB19) %>% 
                                             rename(ccg = health_board_id)
                                    

#bind_rows
perc_combined_eng_wal_sco = bind_rows(perc_scotland_ethnicity_data, perc_eng_wal_ethnicity_data)

#Finally combine with DAG data
ccp_ethnicity_centre_lookup = ccp_april_26_centre_lookup %>% 
  left_join(perc_combined_eng_wal_sco, by = c('ccg' = 'ccg')) %>% 
  rename(dag_id_e = dag_id,
         redcap_data_access_group_e = redcap_data_access_group)

#write csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(ccp_ethnicity_centre_lookup, paste0('ccp_ethnicity_out_', save_date, '.csv'))