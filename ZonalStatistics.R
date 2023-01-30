library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

# Download longitudinal employer-household dynamica dataset
lehd_blocks <- read_csv('https://lehd.ces.census.gov/data/lodes/LODES7/ok/wac/ok_wac_S000_JT00_2015.csv.gz', show_col_types=FALSE) %>%
  rename(total_emp = C000) %>%
  mutate(basic_emp = CNS01+CNS02+CNS03+CNS04+CNS05+CNS06+CNS08+CNS09) %>%
  rename(retail_emp = CNS07) %>%
  mutate(service_emp = total_emp - basic_emp - retail_emp) %>%
  select(w_geocode, total_emp, basic_emp, retail_emp, service_emp)

# Collapse data from block to tract level
lehd_tracts <- lehd_blocks %>%
  mutate(w_geocode = as.character(w_geocode)) %>%
  mutate(GEOID = substr(w_geocode, 1, 11)) %>%
  select(-w_geocode) %>%
  group_by(GEOID) %>%
  summarize(across(everything(), ~sum(.)))

# Download census tract data
hh_vars = c(no_veh = "B08201_002",
            total_hhs = "B08201_001",
            hh_1person = "B08201_007",
            hh_2person = "B08201_013",
            hh_3person = "B08201_019",
            hh_4person_plus = "B08201_025",
            inc_lt_10k = "B19001_002",
            inc_btw_10k_15k = "B19001_003",
            inc_btw_15k_20k = "B19001_004",
            inc_btw_20k_25k = "B19001_005",
            inc_btw_25k_30k = "B19001_006",
            inc_btw_30k_35k = "B19001_007",
            inc_btw_35k_40k = "B19001_008",
            inc_btw_40k_45k = "B19001_009",
            inc_btw_45k_50k = "B19001_010",
            inc_btw_50k_60k = "B19001_011",
            inc_btw_60k_75k = "B19001_012",
            inc_btw_75k_100k = "B19001_013",
            inc_btw_100k_125k = "B19001_014",
            inc_btw_125k_150k = "B19001_015",
            inc_btw_150k_200k = "B19001_016",
            inc_gt_200k = "B19001_017")

census <- get_acs(geography = "tract",
                  state = "OK",
                  county = c("Oklahoma", "Lincoln", "Logan", "Canadian", "Grady", "McClain", "Cleveland"),
                  variables = hh_vars,
                  output = "wide", 
                  geometry = TRUE) %>%
  filter(!st_is_empty(geometry))

areas <- tracts(state = "OK",
                         county = c("Oklahoma", "Lincoln", "Logan", "Canadian", "Grady", "McClain", "Cleveland")) %>%
  select(GEOID, ALAND) %>%
  st_drop_geometry()

zones <- left_join(census, lehd_tracts) %>%
  left_join(areas)


