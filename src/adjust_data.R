library(here)
library(dplyr)
library(readxl)
library(sf)

data <- st_read(here("data", "okc_zone_data_scenario.geojson"))
new <- read_excel(here("data", "okc_zone_data_scenario.xlsx"), sheet="Adjustments", range = cell_rows(1:8))


data1 <- left_join(data, new, by=c("GEOID", "NAME", "ALAND")) %>%
  mutate(no_vehE = ifelse(is.na(no_vehE.y), no_vehE.x, no_vehE.y)) %>%	
  mutate(one_vehE = ifelse(is.na(one_vehE.y), one_vehE.x, one_vehE.y)) %>%	
  mutate(two_vehE = ifelse(is.na(two_vehE.y), two_vehE.x, two_vehE.y)) %>%	
  mutate(three_vehE = ifelse(is.na(three_vehE.y), three_vehE.x, three_vehE.y)) %>%	
  mutate(fourplus_vehE = ifelse(is.na(fourplus_vehE.y), fourplus_vehE.x, fourplus_vehE.y)) %>%	
  mutate(total_hhsE = ifelse(is.na(total_hhsE.y), total_hhsE.x, total_hhsE.y)) %>%	
  mutate(hh_1personE = ifelse(is.na(hh_1personE.y), hh_1personE.x, hh_1personE.y)) %>%	
  mutate(hh_2personE = ifelse(is.na(hh_2personE.y), hh_2personE.x, hh_2personE.y)) %>%	
  mutate(hh_3personE = ifelse(is.na(hh_3personE.y), hh_3personE.x, hh_3personE.y)) %>%	
  mutate(hh_4person_plusE = ifelse(is.na(hh_4person_plusE.y), hh_4person_plusE.x, hh_4person_plusE.y)) %>%	
  mutate(total_under6E = ifelse(is.na(total_under6E.y), total_under6E.x, total_under6E.y)) %>%	
  mutate(total_6to17E = ifelse(is.na(total_6to17E.y), total_6to17E.x, total_6to17E.y)) %>%	
  mutate(median_incomeE = ifelse(is.na(median_incomeE.y), median_incomeE.x, median_incomeE.y)) %>%	
  mutate(total_emp = ifelse(is.na(total_emp.y), total_emp.x, total_emp.y)) %>%	
  mutate(basic_emp = ifelse(is.na(basic_emp.y), basic_emp.x, basic_emp.y)) %>%	
  mutate(retail_emp = ifelse(is.na(retail_emp.y), retail_emp.x, retail_emp.y)) %>%	
  mutate(service_emp = ifelse(is.na(service_emp.y), service_emp.x, service_emp.y)) %>%	
  mutate(total_pop = ifelse(is.na(total_pop.y), total_pop.x, total_pop.y)) %>%	
  mutate(emp_density = ifelse(is.na(emp_density.y), emp_density.x, emp_density.y)) %>%	
  mutate(pop_density = ifelse(is.na(pop_density.y), pop_density.x, pop_density.y)) %>%	
  mutate(act_density = ifelse(is.na(act_density.y), act_density.x, act_density.y)) %>%
  select(GEOID, NAME, ALAND, no_vehE,
         one_vehE,
         two_vehE,
         three_vehE,
         fourplus_vehE,
         total_hhsE,
         hh_1personE,
         hh_2personE,
         hh_3personE,
         hh_4person_plusE,
         total_under6E,
         total_6to17E,
         median_incomeE,
         total_emp,
         basic_emp,
         retail_emp,
         service_emp,
         total_pop,
         emp_density,
         pop_density,
         act_density)

st_write(data1, here("data", "okc_zone_data_scenario1.geojson"))
