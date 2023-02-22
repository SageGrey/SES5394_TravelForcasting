library(tidyverse)
library(here)
library(sf)

full_skim <- read_csv(here("data", "okc_full_skim.csv"))
zone_data <- st_read(here("data", "okc_zones_with_centroids.geojson"))

# Calculate perceived travel time
full_skim <- mutate(full_skim, OVTT = transit_time - IVTT) %>%
  mutate(percieved_time = (IVTT + 2.5* OVTT))

# Add employment data to skim
employment_data <- zone_data %>%
  st_drop_geometry() %>%
  select(centroid_id, GEOID, total_emp)

accessibility_df <- full_skim %>%
  left_join(employment_data, by=join_by(Destination==centroid_id))

# Define decay function with inflection=25, stdev=5
logistic_decay <- function(travel_time) {
  1/(1 + exp((travel_time - 25)/5))
}

# Calculate accessibility measures
accessibility_df <- accessibility_df %>%
  mutate(car_jobs=total_emp*logistic_decay(car_time)) %>%
  mutate(transit_jobs=total_emp*logistic_decay(percieved_time))

accessibility_summary <- accessibility_df %>%
  group_by(Origin) %>%
  summarise(car_access=sum(car_jobs, na.rm=TRUE),
            transit_access=sum(transit_jobs, na.rm=TRUE))


# Plot

