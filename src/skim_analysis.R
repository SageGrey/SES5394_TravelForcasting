library(tidyverse)
library(here)
library(sf)
library(ggplot2)

library(RColorBrewer)

library(tidytransit)
library(here)
library(ggthemes)
library(tigris)


#### Import data ####
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


#### Map Transit Skim ALL COUNTIES


OKC_tracts <- tracts(state = "OK", county = c("Oklahoma","Cleveland", "McClain", "Lincoln", "Logan", "Canadian", "Grady"))

OKC_rta_gtfs <- read_gtfs(
  "https://embarkok.com/data/gtfs/google_transit.zip")

route_shapes <- shapes_as_sf(OKC_rta_gtfs$shapes)

ggplot() +
  geom_sf(data = OKC_tracts,
          fill = "cornsilk",
          color = "gray") +
  geom_sf(data = route_shapes,
          aes(color = shape_id)) +
  theme_map() +
  theme(legend.position = "none")

#### Map Transit Skim Oklahoma + Cleveland


OKC_tracts <- tracts(state = "OK", county = c("Oklahoma","Cleveland"))

OKC_rta_gtfs <- read_gtfs(
  "https://embarkok.com/data/gtfs/google_transit.zip")

route_shapes <- shapes_as_sf(OKC_rta_gtfs$shapes)

ggplot() +
  geom_sf(data = OKC_tracts,
          fill = "ivory2",
          color = "gray") +
  geom_sf(data = route_shapes,
          aes(color = shape_id)) +
  theme_map() +
  theme(legend.position = "none")

