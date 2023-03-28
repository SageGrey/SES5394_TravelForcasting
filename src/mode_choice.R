library(here)
library(tidyverse)
library(sf)
library(knitr)
library(kableExtra)
library(survey)
library(srvyr)

fare <- 1465794 / 2136274

# Calculate travel time and costs
vehs <- read_csv(here("data", "NHTS_csv", "vehpub.csv"), show_col_types = F) %>%
  filter(HH_CBSA == "36420")

trips <- read_csv(here("data", "NHTS_csv", "trippub.csv"), show_col_types = F) %>%
  filter(HH_CBSA == "36420")

car_trips <- trips %>% filter(PSGR_FLG == "02")

car_trips_svy <- car_trips %>% as_survey(weights = WTTRDFIN)

veh_svy <- vehs %>% as_survey(weights = WTHHFIN)
  
total_time <- car_trips_svy %>%
  summarise(total_time = survey_total(TRVLCMIN))

total_gas_cost <- veh_svy %>%
  summarise(total_cost = survey_total(GSTOTCST))

cost_per_minute <- total_gas_cost$total_cost[1] / total_time$total_time[1]

# Assumptions of gas cost and transit fare
# Carpool occupancy from Table 4.16 of NCHRP 716
skim <- read_csv(here("data", "trip_flows.csv")) %>%
  mutate(drive_cost = car_time * cost_per_minute) %>%
  mutate(transit_cost = fare * transfers) %>%
  mutate(carpool_cost_hbo = drive_cost / 2.71,
         carpool_cost_hbw = drive_cost / 2.42,
         carpool_cost_nhb = drive_cost / 2.75) 


# Estimate existing mode-shares
trips <- trips %>%
  mutate(home_based = case_when(WHYTO == "01" ~ TRUE,
                                WHYTO == "02" ~ TRUE,
                                WHYFROM == "01" ~ TRUE,
                                WHYFROM == "02" ~ TRUE,
                                TRUE ~ FALSE)) %>%
  mutate(work = ifelse(WHYTO == "03" | WHYFROM == "03", TRUE, FALSE)) %>%
  mutate(purpose = case_when(home_based & work ~ "HBW",
                             home_based ~ "HBO",
                             TRUE ~ "NHB")) %>%
  mutate(mode = case_when(TRPTRANS == "03" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "04" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "05" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "06" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "08" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "17" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "18" & NUMONTRP > 1 ~ "HOV",
                          TRPTRANS == "03" ~ "SOV",
                          TRPTRANS == "04" ~ "SOV",
                          TRPTRANS == "05" ~ "SOV",
                          TRPTRANS == "06" ~ "SOV",
                          TRPTRANS == "08" ~ "SOV",
                          TRPTRANS == "17" ~ "SOV",
                          TRPTRANS == "18" ~ "SOV",
                          TRPTRANS == "10" ~ "transit",
                          TRPTRANS == "11" ~ "transit",
                          TRPTRANS == "12" ~ "transit",
                          TRPTRANS == "13" ~ "transit",
                          TRPTRANS == "16" ~ "transit",
                          TRUE ~ "other")) %>%
  filter(mode != "other")

trips_svy <- trips %>%
  as_survey(weights = WTTRDFIN)

mode_by_purpose <- trips_svy %>%
  group_by(purpose, mode) %>%
  survey_tally() %>%
  select(-n_se) %>%
  pivot_wider(names_from = mode,
              values_from = n,
              names_prefix = "n_",) %>%
  replace_na(list(n_transit = 0)) %>%
  mutate(n_trips = n_SOV + n_HOV + n_transit) %>%
  mutate(pct_SOV = n_SOV / n_trips) %>%
  mutate(pct_HOV = n_HOV / n_trips) %>%
  mutate(pct_transit = n_transit / n_trips) %>%
  select(purpose, pct_SOV, pct_HOV, pct_transit)


# Apply mode-choice model
# We'd rather use nested logit models, but some of them aren't available

## HBO using Model G because we don't want non-motorized or transit submodes

SOV_share_HBO <- mode_by_purpose$pct_SOV[mode_by_purpose$purpose == "HBO"]

HOV_share_HBO <- mode_by_purpose$pct_HOV[mode_by_purpose$purpose == "HBO"]

transit_share_HBO <- mode_by_purpose$pct_transit[mode_by_purpose$purpose == "HBO"]

SOV_const_HBO <- log(SOV_share_HBO / (1 - SOV_share_HBO))
HOV_const_HBO <- log(HOV_share_HBO / (1 - HOV_share_HBO))
transit_const_HBO <- log(transit_share_HBO / (1 - transit_share_HBO))

skim <- skim %>%
  mutate(utility_transit_HBO = transit_const_HBO +
           IVTT * -0.010  +
           (transit_time - IVTT) * -0.046 +
           transit_cost * -0.029,
         utility_SOV_HBO = SOV_const_HBO +
           car_time * -0.010 +
           drive_cost * -0.029,
         utility_HOV_HBO = HOV_const_HBO +
           car_time * -0.010 +
           carpool_cost_hbo * -0.029) %>%
  mutate(exp_u_SOV_HBO = exp(utility_SOV_HBO),
         exp_u_HOV_HBO = exp(utility_HOV_HBO),
         exp_u_transit_HBO = exp(utility_transit_HBO)) %>%
  rowwise() %>%
  mutate(total_utility_HBO = sum(exp_u_SOV_HBO,
                                 exp_u_HOV_HBO,
                                 exp_u_transit_HBO,
                                 na.rm = TRUE)) %>%
  ungroup()


## HBW using model G bc it's nested logit
SOV_share_HBW <- mode_by_purpose$pct_SOV[mode_by_purpose$purpose == "HBW"]

HOV_share_HBW <- mode_by_purpose$pct_HOV[mode_by_purpose$purpose == "HBW"]

transit_share_HBW <- mode_by_purpose$pct_transit[mode_by_purpose$purpose == "HBW"]

SOV_const_HBW <- log(SOV_share_HBW / (1 - SOV_share_HBW))
HOV_const_HBW <- log(HOV_share_HBW / (1 - HOV_share_HBW))
transit_const_HBW <- log(transit_share_HBW / (1 - transit_share_HBW))

beta_car_nest_HBW = 0.5

skim <- skim %>%
  mutate(utility_transit_HBW = transit_const_HBW +
           IVTT * -0.028  +
           (transit_time - IVTT) * -0.065 +
           transit_cost * -0.0055,
         utility_SOV_HBW = SOV_const_HBW +
           car_time * -0.028 +
           drive_cost * -0.0055,
         utility_HOV_HBW = HOV_const_HBW +
           car_time * -0.028 +
           carpool_cost_hbw * -0.0055) %>%
  mutate(exp_u_SOV_HBW = exp(utility_SOV_HBW),
         exp_u_HOV_HBW = exp(utility_HOV_HBW),
         exp_u_transit_HBW = exp(utility_transit_HBW)) %>%
  rowwise() %>%
  mutate(utility_car_HBW_total = log(sum(exp_u_SOV_HBW,
                                         exp_u_HOV_HBW,
                                         na.rm = TRUE)),
         utility_car_HBW_nest = beta_car_nest_HBW * utility_car_HBW_total) %>%
  mutate(exp_u_car_HBW = exp(utility_car_HBW_nest)) %>%
  mutate(total_utility_HBW = sum(exp_u_car_HBW,
                                 exp_u_transit_HBW,
                                 na.rm = TRUE)) %>%
  ungroup()

## NHB using model G bc we don't have detailed walk and transfer wait time
SOV_share_NHB <- mode_by_purpose$pct_SOV[mode_by_purpose$purpose == "NHB"]

HOV_share_NHB <- mode_by_purpose$pct_HOV[mode_by_purpose$purpose == "NHB"]

transit_share_NHB <- mode_by_purpose$pct_transit[mode_by_purpose$purpose == "NHB"]

SOV_const_NHB <- log(SOV_share_NHB / (1 - SOV_share_NHB))
HOV_const_NHB <- log(HOV_share_NHB / (1 - HOV_share_NHB))
transit_const_NHB <- log(transit_share_NHB / (1 - transit_share_NHB))

skim <- skim %>%
  mutate(utility_transit_NHB = transit_const_HBO +
           IVTT * -0.006 +
           (transit_time - IVTT) * -0.068 +
           transit_cost * -0.008,
         utility_SOV_NHB = SOV_const_NHB +
           car_time * -0.006 +
           drive_cost * -0.008,
         utility_HOV_NHB = HOV_const_NHB +
           car_time * -0.006 +
           carpool_cost_nhb * -0.008) %>%
  mutate(exp_u_SOV_NHB = exp(utility_SOV_NHB),
         exp_u_HOV_NHB= exp(utility_HOV_NHB),
         exp_u_transit_NHB = exp(utility_transit_NHB)) %>%
  rowwise() %>%
  mutate(total_utility_NHB = sum(exp_u_SOV_NHB,
                                 exp_u_HOV_NHB,
                                 exp_u_transit_NHB,
                                 na.rm = TRUE)) %>%
  ungroup()
