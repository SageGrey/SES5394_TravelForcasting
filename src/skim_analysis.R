library(tidyverse)
library(here)
library(sf)
library(ggplot2)
library(readxl)

library(RColorBrewer)

#### Import data ####
car_centroid_dict <- read_csv(here("data", "car_centroids.csv")) %>%
  select(ID, Centroid) %>%
  rename(geoid=ID) %>%
  rename(centroid=Centroid)

match_centroid_id <- function(v, dict) {
  sapply(v, function(id) {
    row <- dict[dict$geoid==id,]
    row$centroid
  })
}
match_geoid <- function(v, dict) {
  sapply(v, function(id) {
    row <- dict[dict$centroid_id,]
    row$GEOID
  })
}

car_skim <- read_csv(here("data", "best_skim.csv")) %>%
  pivot_longer(cols=-Origin) %>%
  rename(Destination=name) %>%
  mutate(value = as.numeric(value)) %>%
  rename(car_time=value) %>%
  mutate(Origin= match_centroid_id(Origin, car_centroid_dict)) %>%
  mutate(Destination=match_centroid_id(Destination, car_centroid_dict))

transit_centroid_dict <- read_csv(here("data", "transit_node_centroid.csv")) %>%
  select(ID, Centroid) %>%
  rename(geoid=ID) %>%
  rename(centroid=Centroid)

transit_time_skim <- read_xlsx(here("data", "nur_transit_skim.xlsx"), sheet="Total Time") %>%
  pivot_longer(cols=-RCIndex) %>%
  rename(Destination=name) %>%
  rename(Origin=RCIndex) %>%
  mutate(value = as.numeric(value)) %>%
  rename(transit_time=value) %>%
  mutate(Origin=match_centroid_id(Origin, transit_centroid_dict)) %>%
  mutate(Destination=match_centroid_id(Destination, transit_centroid_dict))

transit_IVTT_skim <- read_xlsx(here("data", "nur_transit_skim.xlsx"), sheet="In-Vehicle Time") %>%
  pivot_longer(cols=-RCIndex) %>%
  rename(Destination=name) %>%
  rename(Origin=RCIndex) %>%
  mutate(value = as.numeric(value)) %>%
  rename(IVTT=value) %>%
  mutate(Origin=match_centroid_id(Origin, transit_centroid_dict)) %>%
  mutate(Destination=match_centroid_id(Destination, transit_centroid_dict))

transit_transfers_skim <- read_xlsx(here("data", "nur_transit_skim.xlsx"), sheet="Number of Transfers") %>%
  pivot_longer(cols=-RCIndex) %>%
  rename(Destination=name) %>%
  rename(Origin=RCIndex) %>%
  mutate(value = as.numeric(value)) %>%
  rename(transfers=value) %>%
  mutate(Origin=match_centroid_id(Origin, transit_centroid_dict)) %>%
  mutate(Destination=match_centroid_id(Destination, transit_centroid_dict))

transit_fare_skim <- read_xlsx(here("data", "nur_transit_skim.xlsx"), sheet="Fare") %>%
  pivot_longer(cols=-RCIndex) %>%
  rename(Destination=name) %>%
  rename(Origin=RCIndex) %>%
  mutate(value = as.numeric(value)) %>%
  rename(fare=value) %>%
  mutate(Origin=match_centroid_id(Origin, transit_centroid_dict)) %>%
  mutate(Destination=match_centroid_id(Destination, transit_centroid_dict))

full_skim <- full_join(car_skim, transit_time_skim) %>%
  full_join(transit_IVTT_skim) %>%
  full_join(transit_transfers_skim) %>%
  full_join(transit_fare_skim)

zones_with_centroid_ids <- zones %>%
  mutate(centroid_id=match_centroid_id(centroid_id, car_centroid_dict))
  
  
write_csv(full_skim, here("data", "okc_full_skim.csv"))
st_write(zones_with_centroid_ids, here("data", "okc_zones_with_centroids.geojson"), append=FALSE, delete_layer=TRUE)

skim <- read_csv(here("data", "best_skim.csv")) %>%
  mutate(centroid_id = Origin)
centroids <- read_csv(here("data", "centroids.csv")) %>%
  

zones <- st_read(here("data", "okc_zone_data.geojson")) 

zones <- zones %>%
  mutate(Centroid = as.double(row.names(zones))) %>%
  left_join(centroids, by='Centroid') %>%
  rename(centroid_id = ID) %>%
  select(-Elevation) %>%
  select(-Centroid)

# Chloropleth map of travel times to the University of OK
# Centroid ID is 900625

uok_zone <- "900625"

okc_travel_zones <- left_join(zones, skim) %>%
  select(centroid_id, geometry, "900625") %>%
  mutate_at("900625", funs(as.double(.))) %>%
  rename(travel_time_to_uok="900625")


#### Create reusable themes ####
# Create clear theme for maps
clear_map_theme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  title = element_blank()
)

#### Create Chloropleth Maps ####
chlor_pal <- brewer.pal(5, "BrBG")
chlor_pal_grey <- brewer.pal(5, "Greys")
chlor_pal_greens <- brewer.pal(5, "BuGn")

ggplot(okc_travel_zones) +
  geom_sf(aes(fill = travel_time_to_uok)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Travel Time to University of Oklahoma (minutes") +
  theme(legend.title = element_text("Travel Time")) +
  clear_map_theme

  
#### Find info about skim 

min(skim)
