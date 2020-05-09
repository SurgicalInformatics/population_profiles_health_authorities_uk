#April 2020, Surgical Informatics, Tom Drake. Uses NHS data and census data (scottish and an England / Wales custom query through nomis) to identify ethnicity by CCG/HB area
library(tidyverse)
library(janitor)
library(rlang)

`%ni%` = Negate(`%in%`)

#read csv in
ccp_from_location_centre_lookup = read_csv('https://raw.githubusercontent.com/SurgicalInformatics/ccp_location_lookups/master/data_out_ccp_lookups/ccp_dag_id_lookup.csv') #%>% 
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

#Finally, map IMD scores on

#IMDs
england_imd_19 = read_csv('imd_lookups/imd_lookup_england_2019.csv')
wales_imd_19 = read_csv('imd_lookups/imd_lookup_wales_2019.csv')
scotland_imd_20 = read_csv('imd_lookups/imd_lookup_scotland_2020.csv')
ni_imd_17 = read_csv('imd_lookups/nimdm_2017.csv')

#Data zones/ LSOAs
scotland_datazones = read_csv('imd_lookups/scottish_data_areas.csv')
england_lsoas = read_csv('imd_lookups/england_lsoa_to_ccg.csv') %>% clean_names()
ni_sa_to_lgd = readxl::read_xls('imd_lookups/ni_sa_to_lgd.xls')
ni_soa_to_hsct = read_csv('imd_lookups/soa_to_hsct_ni.csv')
wales_hb_names = read_csv('imd_lookups/wales_hb_names.csv')

#To get wales (no direct linkages across lsoa and HB)
#This is derived from all postcodes of the UK https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020
#Needs unzipped into imd_lookups folder
#wales_lsoa_lookup = read_csv('imd_lookups/Data/NSPL_FEB_2020_UK.csv') %>%  filter(startsWith(lsoa11, 'W')) %>% select(lsoa11, ccg, hlthau)
# write_csv(wales_lsoa_lookup, 'imd_lookups/wales_lsoa_to_hb.csv')
wales_lsoa_lookup = read_csv('imd_lookups/wales_lsoa_to_hb.csv')

wales_imd = wales_imd_19 %>% 
  left_join(wales_lsoa_lookup, by = c('lsoa11cd' = 'lsoa11')) %>% 
  group_by(hlthau) %>% 
  mutate(wa_average_imd = median(wimd_2019)) %>% 
  ungroup() %>% 
  left_join(wales_hb_names, by = c('hlthau' = 'LHB19CD')) %>% 
  distinct(hlthau, .keep_all = T) %>% 
  select(hlthau, LHB19NM, wa_average_imd) %>% 
  arrange(desc(wa_average_imd)) %>% 
  mutate(wales_average_imd_rank = 1:n()) %>% 
  select(-wa_average_imd) 

#make ni lookup
ni_imd = ni_soa_to_hsct %>% 
  left_join(ni_sa_to_lgd, by = c('SOA' = 'SOA2001')) %>% 
  left_join(ni_imd_17, by = c('LGD2014' = 'LGD2014code')) %>% 
  group_by(HSCT) %>% 
  mutate(avg_income_per_hsct = median(Income_perc)) %>%  #done on income alone!!!
  distinct(HSCT, .keep_all = T) %>% 
  select(HSCT, avg_income_per_hsct) %>% 
  filter(!is.na(HSCT)) %>% 
  ungroup() %>% 
  arrange(desc(avg_income_per_hsct)) %>% 
  mutate(ni_average_imd_rank = 1:n()) %>% 
  select(-avg_income_per_hsct) %>% 
  mutate(HSCT = ifelse(HSCT == 'Western HSCT', 'WHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Southern HSCT', 'SHSCT', HSCT),
         HSCT = ifelse(HSCT == 'South Eastern HSCT', 'SEHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Belfast HSCT', 'BHSCT', HSCT),
         HSCT = ifelse(HSCT == 'Northern HSCT', 'NHSCT', HSCT))# This is v. generalised and not enough data granularity

#Build big lookups
scotland_imd_20 %>% 
  rename(DataZone = Data_Zone) %>% 
  left_join(scotland_datazones, by = 'DataZone') -> scotland_imd_lookup

england_imd_19 %>% 
  left_join(england_lsoas, by = c('ccg19cd' = 'ccg18cd')) -> england_ccg_imd_lookup

#group_by and select most useful things

scotland_imd_lookup %>% 
  select(HB, DataZone, SIMD2020_Rank) %>% 
  group_by(HB) %>% 
  mutate(average_imd_rank = median(SIMD2020_Rank)) %>% 
  select(-SIMD2020_Rank, -DataZone) %>% 
  distinct(HB, .keep_all = T) %>% 
  ungroup() %>% 
  arrange(-desc(average_imd_rank)) %>% 
  mutate(scotland_average_imd_rank = 1:n()) %>% 
  select(-average_imd_rank) -> scotland_hb_imd

england_ccg_imd_lookup %>% 
  select(ccg19cd, RAvgRank) %>% 
  rename(average_imd_rank = RAvgRank) %>% 
  distinct(ccg19cd, .keep_all = T)  -> england_ccg_imd

#now map these to redcap dataset
ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(scotland_hb_imd, by = c('ccg' = 'HB')) %>% 
  left_join(england_ccg_imd,  by = c('ccg' = 'ccg19cd')) %>% 
  left_join(wales_imd,  by = c('ccg' = 'hlthau')) %>% 
  mutate(average_imd_rank = ifelse(country == 'Scotland', scotland_average_imd_rank, average_imd_rank),
         average_imd_rank = ifelse(country == 'Wales', wales_average_imd_rank, average_imd_rank)) %>% select(-LHB19NM, -check_val)

#map on the IMD15 calculations
imd_update_15 = read_csv('imd_lookups/lookup_hospital_imd_england.csv')

#Create a lookup for incorrectly entered ods codes
incorrect_ods_postcode_mapping = ccp_ethnicity_centre_lookup %>% 
  select(postcode, dag_id_e, lat, lon) %>% 
  mutate(lat = round(lat, digits = 2),
         lon = round(lon, digits = 2)) %>% arrange(desc(lon)) %>% 
  distinct(postcode, .keep_all = T) %>% 
  rename(postcode_correction = postcode,
         ods_corrected = dag_id_e) %>% select(-lat, -lon)

#lookup IMD15

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(imd_update_15 %>% select(postcode, wIMD15), by = c('postcode' = 'postcode'))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(imd_update_15 %>% select(ods, wIMD15) %>% rename(wIMD15_2 = wIMD15), by = c('dag_id_e' = 'ods'))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(incorrect_ods_postcode_mapping, by = c('postcode' = 'postcode_correction')) %>% 
  mutate(ods_corrected = ifelse(ods_corrected == 'CBS25', 'RBS25', ods_corrected),
         ods_corrected = ifelse(ods_corrected == 'RA009', 'RA901', ods_corrected),
         ods_corrected = ifelse(ods_corrected == 'RAJ02', 'RAJ01', ods_corrected),
         ods_corrected = ifelse(ods_corrected == 'RCX01', 'RXC01', ods_corrected),
         ods_corrected = ifelse(ods_corrected == 'RJ2319', 'RJ231', ods_corrected)) %>% 
  left_join(imd_update_15 %>% select(ods, wIMD15) %>% rename(wIMD15_3 = wIMD15), by = c('ods_corrected' = 'ods'))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(imd_update_15 %>% select(postcode, wIMD15) %>% rename(wIMD15_4 = wIMD15) %>% 
              mutate(postcode_start = gsub("[[:space:]].*", '', postcode)) %>% select(-postcode), by = c('postcode_start'))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  left_join(imd_update_15 %>% select(postcode, wIMD15) %>% rename(wIMD15_5 = wIMD15) %>% 
              mutate(postcode_start = gsub("[[:space:]].*", '', postcode)) %>% select(-postcode), by = c('postcode_start'))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  mutate(wIMD15 = ifelse(is.na(wIMD15), wIMD15_2, wIMD15),
         wIMD15 = ifelse(is.na(wIMD15), wIMD15_3, wIMD15),
         wIMD15 = ifelse(is.na(wIMD15), wIMD15_4, wIMD15),
         wIMD15 = ifelse(is.na(wIMD15), wIMD15_5, wIMD15))

ccp_ethnicity_centre_lookup = ccp_ethnicity_centre_lookup %>% 
  distinct(dag_id_e, .keep_all = T) %>% 
  select(-wIMD15_2, -wIMD15_3, -wIMD15_4, -wIMD15_5) %>% 
  rename(wimd15 = wIMD15) %>% 
  select(-HBName, -HB19)

ccp_ethnicity_centre_lookup_lite = ccp_ethnicity_centre_lookup %>% select(-contains('white'),
                                                                     -contains('asian'),
                                                                     -contains('perc'),
                                                                     -contains('black'),
                                                                     -contains('mixed'),
                                                                     -contains('other'),
                                                                     #-contains('imd_rank'),
                                                                     -contains('minority'),
                                                                     -contains('afric'),
                                                                     -ods_corrected)


#write csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

#Missing IMD15
missing_imd15 = ccp_ethnicity_centre_lookup_lite %>% distinct(dag_id_e, .keep_all = T) %>% 
  filter(is.na(wimd15) & country == 'England')

write_csv(missing_imd15, paste0('data_out_ccp_lookup_with_population_level_estimate/ccp_needs_imd15', save_date, '.csv'))

#Export all files
write_csv(ccp_ethnicity_centre_lookup, paste0('data_out_ccp_lookup_with_population_level_estimate/ccp_ethnicity_out_', save_date, '.csv'))
write_csv(ccp_ethnicity_centre_lookup, paste0('data_out_ccp_lookup_with_population_level_estimate/ccp_ethnicity_out.csv'))


#Export lite
write_csv(ccp_ethnicity_centre_lookup_lite, paste0('data_out_ccp_lookup_with_population_level_estimate/ccp_ethnicity_lite_out_', save_date, '.csv'))
write_csv(ccp_ethnicity_centre_lookup_lite, paste0('data_out_ccp_lookup_with_population_level_estimate/ccp_ethnicity_lite_out.csv'))