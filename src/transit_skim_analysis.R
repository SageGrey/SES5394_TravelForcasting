library(tidyverse)
library(tidytransit)
library(ggthemes)
library(tigris)
library(dplyr)
library(sf)
library(RColorBrewer)
library(ggplot2)

okc_transit_tracts <- tracts(state="Oklahoma", county = c(
  "Oklahoma",
  "Cleveland"
))

okc_no_transit_tracts <- tracts(state="Oklahoma", county = c(
  "Lincoln",
  "Logan",
  "Canadian",
  "Grady",
  "McClain"
))

okc_transit <- read_gtfs("https://embarkok.com/data/gtfs/google_transit.zip")

transit_routes <- shapes_as_sf(okc_transit$shapes)

ggplot() +
  geom_sf(data=okc_transit_tracts, fill='lightblue', alpha=1, color='grey') +
  geom_sf(data=okc_no_transit_tracts, fill='white', alpha=0.2, color='lightgrey') +
  # geom_sf(data=transit_routes, aes(color=shape_id)) +
  theme_map() +
  theme(legend.position="none")

####

uok_centroid <- 171
from_uok_skim <- transit_time_skim %>%
  filter(Origin==uok_centroid) %>%
  # mutate(Destination=as.double(Destination)) %>%
  rename(centroid_id=Destination)

zones_skim <- left_join(zones_with_centroid_ids, from_uok_skim) %>%
  mutate_at("transit_time", funs(as.double(.)))

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

ggplot(zones_skim) +
  geom_sf(aes(fill = transit_time)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Travel Time from University of Oklahoma (minutes") +
  theme(legend.title = element_text("Travel Time")) +
  clear_map_theme

