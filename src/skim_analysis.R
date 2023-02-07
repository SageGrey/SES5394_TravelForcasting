library(tidyverse)
library(here)
library(sf)

#### Import data ####
skim <- read_csv(here("data", "best_skim.csv")) %>%
  tibble::column_to_rownames('Origin')
centroids <- read_csv(here("data", "centroids.csv")) 
zones <- st_read(here("data", "okc_zone_data.geojson")) %>%
  mutate(Centroid = as.double(row.names(zones)))  %>%
  left_join(centroids) %>%
  rename(centroid_id = ID) %>%
  select(-Elevation) %>%
  select(-Centroid)