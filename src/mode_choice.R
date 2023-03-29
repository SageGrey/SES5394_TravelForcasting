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

SOV_const_HBO <- 0.01
HOV_const_HBO <- 0.19
transit_const_HBO <- 2

SOV_const_HBW <- 1.8
HOV_const_HBW <- -0.6
transit_const_HBW <- 5
beta_car_nest_HBW = 0.5

SOV_const_NHB <- 0.95
HOV_const_NHB <- 0.9
transit_const_NHB <- 5

## HBO using Model G because we don't want non-motorized or transit submodes

SOV_share_HBO <- mode_by_purpose$pct_SOV[mode_by_purpose$purpose == "HBO"]
HOV_share_HBO <- mode_by_purpose$pct_HOV[mode_by_purpose$purpose == "HBO"]
transit_share_HBO <- mode_by_purpose$pct_transit[mode_by_purpose$purpose == "HBO"]

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

## Probability of each modes
### HBO
skim <- skim %>%
  mutate(p_transit_HBO = exp(utility_transit_HBO) / total_utility_HBO,
         p_SOV_HBO = exp(utility_SOV_HBO) / total_utility_HBO,
         p_HOV_HBO = exp(utility_HOV_HBO) / total_utility_HBO) %>%
  replace_na(list(p_transit_HBO = 0,
                  p_SOV_HBO = 0,
                  p_HOV_HBO = 0))


### NHB
skim <- skim %>%
  mutate(p_transit_NHB = exp(utility_transit_NHB) / total_utility_NHB,
         p_SOV_NHB = exp(utility_SOV_NHB) / total_utility_NHB,
         p_HOV_NHB = exp(utility_HOV_NHB) / total_utility_NHB) %>%
  replace_na(list(p_transit_NHB = 0,
                  p_SOV_NHB = 0,
                  p_HOV_NHB = 0))


### HBW
skim <- skim %>%
  mutate(p_transit_HBW = exp(utility_transit_HBW) / total_utility_HBW,
         p_car_HBW = exp(utility_car_HBW_nest) / total_utility_HBW) %>%
  replace_na(list(p_transit_HBW = 0,
                  p_car_HBW = 0))

skim <- skim %>%
  mutate(p_SOV_if_car_HBW = exp(utility_SOV_HBW) / exp(utility_car_HBW_total),
         p_HOV_if_car_HBW = exp(utility_HOV_HBW) / exp(utility_car_HBW_total)) %>%
  mutate(p_SOV_HBW = p_car_HBW * p_SOV_if_car_HBW,
         p_HOV_HBW = p_car_HBW * p_HOV_if_car_HBW)


## Calculate number of trips
skim <- skim %>%
  mutate(n_transit_HBO = round(HBO_flow * p_transit_HBO),
         n_SOV_HBO = round(HBO_flow * p_SOV_HBO),
         n_HOV_HBO = round(HBO_flow * p_HOV_HBO),
         n_transit_HBW = round(HBW_flow * p_transit_HBW),
         n_SOV_HBW = round(HBW_flow * p_SOV_HBW),
         n_HOV_HBW = round(HBW_flow * p_HOV_HBW),
         n_transit_NHB = round(NHB_flow * p_transit_NHB),
         n_SOV_NHB = round(NHB_flow * p_SOV_NHB),
         n_HOV_NHB = round(NHB_flow * p_HOV_NHB)) %>%
  replace_na(list(n_transit_HBO = 0,
                  n_SOV_HBO = 0,
                  n_HOV_HBO = 0,
                  n_transit_HBW = 0,
                  n_SOV_HBW = 0,
                  n_HOV_HBW = 0,
                  n_transit_NHB = 0,
                  n_SOV_NHB = 0,
                  n_HOV_NHB = 0))


modeled_mode_by_purpose_1 <- tibble(
  purpose = c("HBO_model 1", "HBW_model 1", "NHB_model 1"),
  pct_transit = c(sum(skim$n_transit_HBO) /
                    sum(skim$HBO_flow),
                  sum(skim$n_transit_HBW) /
                    sum(skim$HBW_flow),
                  sum(skim$n_transit_NHB) /
                    sum(skim$NHB_flow)),
  pct_SOV = c(sum(skim$n_SOV_HBO) /
                sum(skim$HBO_flow),
              sum(skim$n_SOV_HBW) /
                sum(skim$HBW_flow),
              sum(skim$n_SOV_NHB) /
                sum(skim$NHB_flow)),
  pct_HOV = c(sum(skim$n_HOV_HBO) /
                sum(skim$HBO_flow),
              sum(skim$n_HOV_HBW) /
                sum(skim$HBW_flow),
              sum(skim$n_HOV_NHB) /
                sum(skim$NHB_flow)))

model_compare <- rbind(mode_by_purpose, modeled_mode_by_purpose_1) 

model_compare


