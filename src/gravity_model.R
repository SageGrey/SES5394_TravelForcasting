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
# 
# other_dict <- zone_data %>%
#   select(centroid_id, GEOID) %>%
#   rename(centroid=centroid_id)
# 
# car_centroid_dict <- read_csv(here("data", "car_centroids.csv")) %>%
#   select(ID, Centroid) %>%
#   rename(geoid=ID) %>%
#   rename(centroid=Centroid) %>%
#   left_join(other_dict) #%>%
#   # select(centroid, GEOID) %>%
  # rename(geoid=GEOID)
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
# write_csv(skim, here("data", "okc_full_skim_GEOID.csv"))
# write_csv(car_centroid_dict, here("data", "full_geoid_centroid_dict.csv"))

skim <- read_csv(here("data", "okc_full_skim_geoid.csv"))
trip_gen <- st_read(here("data", "trip-gen.geojson")) %>%
  mutate(hbo_trip_prod=replace(hbo_trip_prod, hbo_trip_prod < 0, 0))

#### Balancing ####
# Add friction factors
skim <- skim %>%
  filter(!is.na(car_time)) %>%
  select(Origin, Destination, car_time) %>%
  mutate(Origin = as.character(Origin)) %>%
  mutate(Destination = as.character(Destination)) %>%
  rename(from_GEOID=Origin) %>%
  rename(to_GEOID=Destination)

skim <- skim %>%
  mutate(F_HBO = car_time^-3) %>%
  mutate(F_HBW = car_time^-0.5) %>%
  mutate(F_NHB = car_time^-2.9)

# Exponential friction factor
# skim <- skim %>%
#   mutate(F_HBO = exp(-16.30407 * car_time)) %>%
#   mutate(F_HBW = exp(-26.10191 * car_time)) %>%
#   mutate(F_NHB = exp(-15.94483* car_time))

# Run Carole's tool
HBO_dist <- grvty_balancing(od_zones = trip_gen,
                            friction = skim,
                            zone_id = "GEOID",
                            zone_o = "hbo_trip_prod",
                            zone_d = "hbo_bal_attr",
                            friction_o_id = "from_GEOID",
                            friction_d_id = "to_GEOID",
                            friction_factor = "F_HBO",
                            tolerance = 0.01,
                            max_iter = 10000)

tail(HBO_dist$convergence)

HBW_dist <- grvty_balancing(od_zones = trip_gen,
                            friction = skim,
                            zone_id = "GEOID",
                            zone_o = "hbw_trip_prod",
                            zone_d = "hbw_bal_attr",
                            friction_o_id = "from_GEOID",
                            friction_d_id = "to_GEOID",
                            friction_factor = "F_HBW",
                            tolerance = 0.01,
                            max_iter = 10000)

tail(HBW_dist$convergence)

NHB_dist <- grvty_balancing(od_zones = trip_gen,
                            friction = skim,
                            zone_id = "GEOID",
                            zone_o = "nhb_trip_prod",
                            zone_d = "nhb_bal_attr",
                            friction_o_id = "from_GEOID",
                            friction_d_id = "to_GEOID",
                            friction_factor = "F_NHB",
                            tolerance = 0.01,
                            max_iter = 10000)

tail(NHB_dist$convergence)

HBO_flows <- HBO_dist$flows %>%
  rename(from_GEOID = o_id,
         to_GEOID = d_id,
         HBO_flow = flow)

HBW_flows <- HBW_dist$flows %>%
  rename(from_GEOID = o_id,
         to_GEOID = d_id,
         HBW_flow = flow)

NHB_flows <- NHB_dist$flows %>%
  rename(from_GEOID = o_id,
         to_GEOID = d_id,
         NHB_flow = flow)

skim <- skim %>%
  left_join(HBO_flows) %>%
  left_join(HBW_flows) %>%
  left_join(NHB_flows)

sum(skim$HBO_flow * skim$car_time) / sum(skim$HBO_flow)
sum(skim$HBW_flow * skim$car_time) / sum(skim$HBW_flow)
sum(skim$NHB_flow * skim$car_time) / sum(skim$NHB_flow)

desire_lines_HBO_threshold <- od_to_sf(skim, trip_gen, silent = TRUE) %>%
  filter(HBO_flow > 400)

ggplot(desire_lines_HBO_threshold) +
  geom_sf(data=zone_data) +
  # annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(aes(alpha = HBO_flow)) +
  theme_void()


