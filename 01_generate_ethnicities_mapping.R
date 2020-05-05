#April 2020, Surgical Informatics, Tom Drake. Uses NHS data and census data (scottish and an England / Wales custom query through nomis) to identify ethnicity by CCG/HB area
library(tidyverse)
library(janitor)
library(rlang)

`%ni%` = Negate(`%in%`)

#read csv in
ccp_from_location_centre_lookup = read_csv('lookup_centres/ccp_dag_id_lookup_05-May-2020.csv') #%>% 
                              # mutate(ccg = ifelse(ccg == 'E38000230' & 
                              #                       (place_name == 'Derriford Hospital' |
                              #                          place_name == 'Royal Devon And Exeter Hospital (Wonford)' |
                              #                          place_name == 'Torbay Hospital') , 'E38000152', ccg),
                              #        ccg = ifelse(ccg == 'E38000230' & place_name == 'North Devon District Hospital', 'E38000129', ccg),
                              #        ccg = ifelse(ccg == 'E38000229' & place_name == 'Chesterfield Royal Hospital', 'E38000115', ccg))
                              
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

#Now update estimates for 2018
#update population numbers
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2018
#https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Health-Boards/populationestimates-by-lhb-age
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates
scotland_new_lookup_codes = read_csv('lookup_centres/scotland_hb_code_conversion.csv') %>% select(-Country)

mid_2018_estimates = read_csv('updated_population_estimates/SAPE21DT5_mid2018_pop_estimates.csv') %>% clean_names() %>% #includes scottish and welsh estimates
  rename(ccg = area_codes,
         ccg_name = area_names,
         updated_all_age_mid_2018 = all_ages) %>% 
  left_join(scotland_new_lookup_codes, by = c('ccg' = 'HB')) %>% 
  mutate(ccg = ifelse(!is.na(HB19), HB19, ccg))

#Lets merge the CCGs
#make census ccgs equal to the ccgs 2019
eng_wal_ethnicity_data = eng_wal_ethnicity_data %>% 
  mutate(ccg = ifelse(ccg == 'E38000032', 'E38000217', ccg),
         ccg = ifelse(ccg == 'E38000123', 'E38000217', ccg),
        ccg = ifelse(ccg == 'E38000158', 'E38000217', ccg),
        ccg = ifelse(ccg == 'E38000060', 'E38000226', ccg),
        ccg = ifelse(ccg == 'E38000065', 'E38000227', ccg),
        ccg = ifelse(ccg == 'E38000093', 'E38000228', ccg),
        ccg = ifelse(ccg == 'E38000041', 'E38000215', ccg),
        ccg = ifelse(ccg == 'E38000061', 'E38000212', ccg),
        ccg = ifelse(ccg == 'E38000111', 'E38000212', ccg),
        ccg = ifelse(ccg == 'E38000112', 'E38000212', ccg),
        ccg = ifelse(ccg == 'E38000094', 'E38000225', ccg),
        ccg = ifelse(ccg == 'E38000095', 'E38000225', ccg),
        ccg = ifelse(ccg == 'E38000096', 'E38000225', ccg),
        ccg = ifelse(ccg == 'E38000012', 'E38000220', ccg),
        ccg = ifelse(ccg == 'E38000013', 'E38000220', ccg),
        ccg = ifelse(ccg == 'E38000149', 'E38000220', ccg),
        ccg = ifelse(ccg == 'E38000213', 'E38000213', ccg),#changes in april 2020
        ccg = ifelse(ccg == 'E38000058', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000071', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000115', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000169', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000131', 'E38000218', ccg),
        ccg = ifelse(ccg == 'E38000159', 'E38000219', ccg),
        ccg = ifelse(ccg == 'E38000022', 'E38000222', ccg),
        ccg = ifelse(ccg == 'E38000125', 'E38000222', ccg),
        ccg = ifelse(ccg == 'E38000155', 'E38000222', ccg),
        ccg = ifelse(ccg == 'E38000129', 'E38000230', ccg),
        ccg = ifelse(ccg == 'E38000152', 'E38000230', ccg),
        ccg = ifelse(ccg == 'E38000036', 'E38000213', ccg),#changes in april 2020
        ccg = ifelse(ccg == 'E38000067', 'E38000214', ccg),#changes in april 2020
        ccg = ifelse(ccg == 'E38000003', 'E38000223', ccg),
        ccg = ifelse(ccg == 'E38000017', 'E38000224', ccg),
        ccg = ifelse(ccg == 'E38000033', 'E38000223', ccg),
        ccg = ifelse(ccg == 'E38000110', 'E38000221', ccg),
        ccg = ifelse(ccg == 'E38000114', 'E38000221', ccg),
        ccg = ifelse(ccg == 'E38000148', 'E38000224', ccg),
        ccg = ifelse(ccg == 'E38000160', 'E38000221', ccg),
        ccg = ifelse(ccg == 'E38000207', 'E38000224', ccg),
        ccg = ifelse(ccg == 'E38000209', 'E38000221', ccg),
        ccg = ifelse(ccg == 'E38000058', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000071', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000115', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000169', 'E38000229', ccg),
        ccg = ifelse(ccg == 'E38000129', 'E38000230', ccg),
        ccg = ifelse(ccg == 'W11000026', 'W11000031', ccg),
        ccg = ifelse(ccg == 'W11000027', 'W11000030', ccg)) %>% 
  group_by(ccg) %>% 
  mutate_at(vars(-ccg), sum) %>% 
  distinct(ccg, .keep_all = T)

eng_wal_ethnicity_data = eng_wal_ethnicity_data %>% left_join(mid_2018_estimates, by = 'ccg') %>% mutate(multiplication_factor = updated_all_age_mid_2018 / all_people)

update_numbers_eng_wal_ethnicity_data = eng_wal_ethnicity_data %>% mutate_at(vars(all_of(c('all_people', vars_eng_to_perc))),  ~(. *multiplication_factor))

perc_eng_wal_ethnicity_data = update_numbers_eng_wal_ethnicity_data %>% mutate_at(vars(all_of(vars_eng_to_perc)), .funs = list(perc = ~(. / all_people)*100))

#perc_eng_wal_ethnicity_data = eng_wal_ethnicity_data %>% mutate_at(vars(all_of(vars_eng_to_perc)), .funs = list(perc = ~(. / all_people)*100))

#for scotland 

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

scotland_ethnicity_data = scotland_ethnicity_data %>% 
                          left_join(scotland_new_lookup_codes, by = c('health_board_id' = 'HB')) %>%
                          mutate(health_board_id = HB19) %>% 
                          select(-HB19, -HBName) %>% 
                          left_join(mid_2018_estimates, by = c('health_board_id' = 'ccg')) %>% mutate(multiplication_factor = updated_all_age_mid_2018 / all_people) %>% 
                          select(-HB19, -HBName)
  

update_numbers_scotland_ethnicity_data = scotland_ethnicity_data %>% mutate_at(vars(all_of(c('all_people', vars_scot_to_perc))),  ~(. *multiplication_factor))

perc_scotland_ethnicity_data = update_numbers_scotland_ethnicity_data %>%
                                             mutate_at(vars(all_of(vars_scot_to_perc)), .funs = list(perc = ~(. / all_people)*100)) %>%
                                             filter(!is.na(health_board_id)) %>%
                                             rename(ccg = health_board_id)

# perc_scotland_ethnicity_data = scotland_ethnicity_data %>% 
#                                              mutate_at(vars(all_of(vars_scot_to_perc)), .funs = list(perc = ~(. / all_people)*100)) %>% 
#                                              left_join(scotland_new_lookup_codes, by = c('health_board_id' = 'HB')) %>% 
#                                              filter(!is.na(health_board_id)) %>% 
#                                              mutate(health_board_id = HB19) %>% 
#                                              select(-HB19) %>% 
#                                              rename(ccg = health_board_id)
#                                     

#bind_rows
perc_combined_eng_wal_sco = bind_rows(perc_scotland_ethnicity_data, perc_eng_wal_ethnicity_data)

#Finally combine with DAG data
ccp_ethnicity_centre_lookup = ccp_from_location_centre_lookup %>% 
  left_join(perc_combined_eng_wal_sco, by = c('ccg' = 'ccg')) %>% 
  rename(dag_id_e = dag_id,
         redcap_data_access_group_e = redcap_data_access_group)

#Now make White, Asian, Black and ME proportions
ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  mutate(white_perc_out = white_perc,
         asian_perc_out = asian_asian_british_perc,
         black_perc_out = african_caribbean_perc,
         minority_ethnic_out = other_ethnic_group_any_other_ethnic_group_perc + other_ethnic_group_arab_perc + mixed_or_multiple_ethnic_groups_perc,
         check_val = white_perc_out + asian_perc_out + black_perc_out + minority_ethnic_out) %>% distinct(dag_id_e, .keep_all = T)

#ccp_ethnicity_centre_lookup %>% filter(is.na(white_english_welsh_scottish_northern_irish_british_perc)) %>% select(place_name, ccg) %>% distinct(ccg, .keep_all = T)

#write csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(ccp_ethnicity_centre_lookup, paste0('ccp_ethnicity_out_', save_date, '.csv'))
