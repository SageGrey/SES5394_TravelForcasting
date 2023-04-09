library(here)
library(tidyverse)
library(tidycensus)
library(downloader)
library(tidycensus)
library(sf)

OKC_CBSA <- 36420 #OKC


okc_trips <- read_csv("C:\\Users\\sageg\\OneDrive\\Documents\\SES5394_TravelForcasting\\data\\NHTS_csv\\trippub.csv") %>%
  filter(HH_CBSA == OKC_CBSA) %>% #Filter to only OKC Stat Area
  filter(TRPTRANS != "01" & # Filter Out Walking
           TRPTRANS != "02" & # Filter Out Biking
           TRPTRANS != "19") # Filter out Airplane


bos_trips <- read_csv("C:\\Users\\sageg\\OneDrive\\Documents\\SES5394_TravelForcasting\\data\\NHTS_csv\\trippub.csv") %>%
  filter(HH_CBSA == BOS_CBSA) %>% #Filter to only Bos Stat Area
  filter(TRPTRANS != "19") # Filter out Airplane


## Classify Trip Types into 3 Meta Categories

#HBW: Home-Based Work
#HBO: Home-Based Other
#NHB: Non-Home-Based

okc_trips <- okc_trips %>%
  mutate(home_based = (WHYFROM == "01" |
                         WHYFROM == "02" |
                         WHYTO == "01" |
                         WHYTO == "02"),
         work = (WHYFROM == "03" |
                   WHYTO == "03")) %>%
  mutate(purpose = case_when(home_based & work ~ "HBW",
                             home_based ~ "HBO",
                             TRUE ~ "NHB"))
#
zone_data <- st_read(here("data", "okc_zones_with_centroids.geojson"))


# Get data for OKC counties
census <- get_acs(
  geography = "tract",
  state = "OK",
  county = c(
    "Oklahoma",
    "Lincoln",
    "Logan",
    "Canadian",
    "Grady",
    "McClain",
    "Cleveland"
  ),
  variables = c(total_hispanic_latino = "B03001_003", total_non_hispanic_latino="B03001_002"),
  output = "wide",
  geometry = TRUE
) %>%
  filter(!st_is_empty(geometry)) %>%
  replace(is.na(.), 0)

zone_data$hisp_origin <- census$total_hispanic_latinoE
zone_data$non_hisp_origin <- census$total_non_hispanic_latinoE


##Show Table
table(okc_trips$purpose)

hh_OKC_trip_counts <- okc_trips %>%
  group_by(HOUSEID) %>%
  summarise(HBO_trips = sum(purpose == "HBO"),
            HBW_trips = sum(purpose == "HBW"),
            NHB_trips = sum(purpose == "NHB"))

## Load Household Data

OKC_hhs <- read_csv("C:\\Users\\sageg\\OneDrive\\Documents\\SES5394_TravelForcasting\\data\\NHTS_csv\\hhpub.csv") %>%
  filter(HH_CBSA == OKC_CBSA) %>% #Filter to only OKC Stat Area
  left_join(hh_OKC_trip_counts) %>%
  replace_na(list(HBO_trips = 0,
                  HBW_trips = 0,
                  NHB_trips = 0))



#### Assigning 

##Household Income as Continuous


##Household Size

OKC_hhs <- OKC_hhs %>%
  mutate(household_size_3_or_bigger = case_when(HHSIZE == "1" ~ "1 person",
                                                HHSIZE == "2" ~ "2 people",
                                                HHSIZE == "3" ~ "3 plus people",
                                                HHSIZE == "4" ~ "3 plus people",
                                                HHSIZE == "5" ~ "3 plus people",
                                                HHSIZE == "4" ~ "3 plus people",
                                                HHSIZE == "5" ~ "3 plus people",
                                                HHSIZE == "6" ~ "3 plus people",
                                                TRUE ~ "3 plus people")) 


OKC_hhs <- OKC_hhs %>%
  mutate(inc_k = case_when(HHFAMINC == "01" ~ 5,
                           HHFAMINC == "02" ~ 12.5,
                           HHFAMINC == "03" ~ 17.5,
                           HHFAMINC == "04" ~ 20,
                           HHFAMINC == "05" ~ 37.5,
                           HHFAMINC == "06" ~ 62.5,
                           HHFAMINC == "07" ~ 82.5,
                           HHFAMINC == "08" ~ 112.5,
                           HHFAMINC == "09" ~ 132.5,
                           HHFAMINC == "10" ~ 175,
                           HHFAMINC == "11" ~ 300,
                           TRUE ~ 99)) %>%
  mutate(inc_k = na_if(inc_k, 99))

OKC_hhs <- OKC_hhs %>%
  mutate(hisp = case_when(HH_HISP == "01" ~"Yes", 
                          HH_HISP == "02" ~ "No",
                          TRUE ~ "No"))

## Vehicle Ownership
OKC_hhs <- OKC_hhs %>%
  mutate(veh_cat = case_when(HHVEHCNT == 0 ~ "zero",
                             HHVEHCNT == 1 ~ "one",
                             TRUE ~ "two-plus")) %>%
  
  mutate(no_vehicles = case_when(veh_cat == "zero" ~ "No Vehicles",
                                 veh_cat == "one" ~ "Has Vehicles",
                                 veh_cat == "two-plus" ~ "Has Vehicles"))

## Are there Children? 
OKC_hhs <- OKC_hhs %>%
  mutate(any_kids = LIF_CYC != "01" &
           LIF_CYC != "02" &
           LIF_CYC != "09" &
           LIF_CYC != "10")

## Linear Regression Carole's 
#hbo_trip_prod <- lm(HBO_trips ~ log2(inc_k) +
#                             veh_cat +
#                              any_kids,
#                  data = OKC_hhs)

#hbw_trip_prod <- lm(HBW_trips ~ log2(inc_k) +
#                               veh_cat +
#                               any_kids,
#                   data = OKC_hhs)
#nhb_trip_prod <- lm(NHB_trips ~ log2(inc_k) +
#                          veh_cat +
#                          any_kids,
#              data = OKC_hhs)


## Linear Regression Carole's 
hbo_trip_prod <- lm(HBO_trips ~ log2(inc_k) +
                      any_kids +
                      hisp + 
                      no_vehicles +
                      household_size_3_or_bigger,
                    data = OKC_hhs)

hbw_trip_prod <- lm(HBW_trips ~ log2(inc_k) +
                      any_kids +
                      hisp +
                      no_vehicles +
                      household_size_3_or_bigger,
                    data = OKC_hhs)
nhb_trip_prod <- lm(NHB_trips ~ log2(inc_k) +
                      any_kids + 
                      hisp +
                      no_vehicles +
                      household_size_3_or_bigger,
                    data = OKC_hhs)



## Show Summary of Linear Regression
summary(hbo_trip_prod)
summary(hbw_trip_prod)
summary(nhb_trip_prod)


## Estimate Trip Productions HBO
zone_data <- zone_data %>%
  #mutate(hhs_2plus_veh = total_hhsE - no_vehE - one_vehE) %>%
  mutate(hhs_3plus_people = total_hhsE - hh_1personE - hh_2personE) %>%
  mutate(hhs_with_kidsE = total_under6E + total_6to17E) %>%
  mutate(hh_noVehicles = no_vehE) %>%
  
  
  mutate(hbo_prod_per_hh = 0.827 + 
           #  0.151 * log2(zone_data$median_incomeE) - 
           #  1.251 * ((hisp_origin)/total_hhsE) -
           #  1.01  * (hh_noVehicles/total_hhsE) +
           2.837 * (hhs_3plus_people/total_hhsE) +
           1.933 *  (hh_2personE/total_hhsE)) %>%
  #   0.678 * (hhs_with_kidsE/total_hhsE)) 
  
  mutate(hbw_prod_per_hh = -0.218 + 
           # 0.098 * log2(zone_data$median_incomeE) - 
           # 1.0719 * ((hisp_origin)/total_hhsE) -
           # 0.300  * (hh_noVehicles/total_hhsE) +
           1.9509 * (hhs_3plus_people/total_hhsE) +
           .8997 *  (hh_2personE/total_hhsE) 
         -.9849 * (hhs_with_kidsE/total_hhsE)) %>%
  
  mutate(nhb_prod_per_hh = .908 + 
           # 0.129 * log2(zone_data$median_incomeE) - 
           # 1.0719 * ((hisp_origin)/total_hhsE) -
           # 0.300  * (hh_noVehicles/total_hhsE) +
           # -.587 * (hhs_3plus_people/total_hhsE) +
           #.780 *  (hh_2personE/total_hhsE) +
           3.400 * (hhs_with_kidsE/total_hhsE)) 

zone_data <- zone_data %>%
  mutate(hbo_trip_prod = total_hhsE * hbo_prod_per_hh) %>%
  mutate(hbw_trip_prod = total_hhsE * hbw_prod_per_hh) %>%
  mutate(nhb_trip_prod = total_hhsE * nhb_prod_per_hh)


# GEOID with no median income: 40109107407, 78,000.04 (median income)

## Estimate Trip Attractions
zone_data <- zone_data %>%
  mutate(hbo_trip_attr = 1.0 * total_hhsE +
           0.3 * basic_emp +
           5.9 * retail_emp +
           2.3 * service_emp) %>%
  
  # Add home-based work trips
  mutate(hbw_trip_attr = 1.2 * total_emp) %>%
  
  # Add non-home-based trips
  mutate(nhb_trip_attr = 0.6 * total_hhsE +
           0.7 * basic_emp +
           2.6 * retail_emp + 
           1.0 * service_emp)


simplified_okc_zone_data <- subset(zone_data, select=c(GEOID, NAME, hbo_trip_attr, hbw_trip_attr, nhb_trip_attr, nhb_trip_prod, hbo_trip_prod, hbw_trip_prod, hbo_prod_per_hh, hhs_with_kidsE,  total_hhsE))

simplified_okc_zone_data <- simplified_okc_zone_data %>%
  filter( hbo_trip_prod != "NaN")

zone_data <- zone_data %>%
  filter(hbo_trip_prod != "NaN")


#Home Based Others
sum(zone_data$hbo_trip_prod)
sum(zone_data$hbo_trip_attr)

sum(zone_data$hbw_trip_prod)
sum(zone_data$hbw_trip_attr)



zone_data <- zone_data %>%
  mutate(hbo_bal_attr = hbo_trip_attr * 
           sum(hbo_trip_prod) / sum(hbo_trip_attr)) %>%
  mutate(hbw_bal_attr = hbw_trip_attr * 
           sum(hbw_trip_prod) / sum(hbw_trip_attr)) %>%
  mutate(nhb_bal_attr = nhb_trip_attr * 
           sum(nhb_trip_prod) / sum(nhb_trip_attr))


sum(zone_data$hbo_bal_attr)
sum(zone_data$hbo_trip_prod)

sum(zone_data$hbw_bal_attr)
sum(zone_data$hbw_trip_prod)

sum(zone_data$nhb_bal_attr)
sum(zone_data$nhb_trip_prod)





zone_data_export <- subset(zone_data, select=c(GEOID, NAME, hbo_trip_prod, hbo_bal_attr, hbw_trip_prod, hbw_bal_attr, nhb_trip_prod, nhb_bal_attr))


st_write(zone_data_export, here("data",
                                "trip-gen-newVariables.geojson"))