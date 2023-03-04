library(here)
library(tidyverse)
library(sf)
library(survey)
library(srvyr)
library(od)
library(ggspatial)
library(knitr)
library(readxl)
library(tigris)
library(scenRios)

OKC_CBSA <- "36420" #OKC

#### Import NHTS travel data by purpose ####
trips <- read_csv(here("data", "nhts", "trippub.csv"), show_col_types = FALSE) %>%
  filter(HH_CBSA == OKC_CBSA) %>%
  filter(TRPTRANS == "03" | # Car
         TRPTRANS == "04" | # SUV
         TRPTRANS == "05" | # Van
         TRPTRANS == "06") # pickup truck


# Recode based on trip type
trips <- trips %>%
  mutate(home_based = case_when(WHYTO == "01" ~ TRUE,
                                WHYTO == "02" ~ TRUE,
                                WHYFROM == "01" ~ TRUE,
                                WHYFROM == "02" ~ TRUE,
                                TRUE ~ FALSE)) %>%
  mutate(work = ifelse(WHYTO == "03" | WHYFROM == "03", TRUE, FALSE)) %>%
  mutate(purpose = case_when(home_based & work ~ "HBW",
                             home_based ~ "HBO",
                             TRUE ~ "NHB"))

# Create a survey object
trips_svy <- trips %>%
  filter(TRVLCMIN > 0) %>%
  as_survey(weights = WTTRDFIN)

ttime_by_purpose <- trips_svy %>%
  group_by(purpose) %>%
  summarise(avg_time = survey_mean(TRVLCMIN))

#### Import skim and trip generation data ####
# Convert from centroids to GEOIDs
# car_centroid_dict <- read_csv(here("data", "car_centroids.csv")) %>%
#   select(ID, Centroid) %>%
#   rename(geoid=ID) %>%
#   rename(centroid=Centroid)
# 
# match_geoid <- function(v, dict) {
#   sapply(v, function(id) {
#     # row <- dict[dict$centroid_id,]
#     # row$GEOID
#     dict$geoid[id]
#     
#   })
# }
# 
# skim <- read_csv(here("data", "okc_full_skim.csv")) %>%
#   mutate(Origin=match_geoid(Origin, car_centroid_dict)) %>%
#   mutate(Destination=match_geoid(Destination, car_centroid_dict))
# 
# write_csv(skim, here("data", "okc_full_skim_geoid.csv"))

skim <- read_csv(here("data", "okc_full_skim_geoid.csv"))
trip_gen <- st_read(here("data", "trip-gen.geojson"))

#### Balancing ####
# Add friction factors
skim <- skim %>%
  filter(!is.na(car_time)) %>%
  select(Origin, Destination, car_time) %>%
  mutate(Origin = as.character(Origin)) %>%
  mutate(Destination = as.character(Destination)) %>%
  rename(from_GEOID=Origin) %>%
  rename(to_GEOID=Destination) %>%
  mutate(F_HBO = car_time^-2) %>%
  mutate(F_HBW = car_time^-2) %>%
  mutate(F_NHB = car_time^-2)

# Run Carole's tool
hbo_dist <- grvty_balancing(od_zones = trip_gen,
                            friction = skim,
                            zone_id = "GEOID",
                            zone_o = "hbo_trip_prod",
                            zone_d = "hbo_bal_attr",
                            friction_o_id = "from_GEOID",
                            friction_d_id = "to_GEOID",
                            friction_factor = "F_HBO",
                            tolerance = 0.01,
                            max_iter = 50000)

tail(hbo_dist$convergence)
head(hbo_dist$flows)
