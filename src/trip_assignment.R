library(tidyverse)
library(sf)
library(here)
library(tigris)

# Total SOV trips by purpose
skim <- skim %>%
  mutate(total_SOV_veh_trips=n_SOV_HBO + n_SOV_HBW + n_SOV_NHB)

# Total HOV trips by purpose (Divide by assumed vehicle occupancy)
## Different assumed occupancies by mode (from Table 4.16 of NCHRP 716)
hbo_occupancy <- 2.71
hbw_occupancy <- 2.42
nhb_occupancy <- 2.75

skim <- skim %>%
  mutate(total_HOV_veh_trips=(n_HOV_HBO/hbo_occupancy) + (n_HOV_HBW/hbw_occupancy) + (n_HOV_NHB/nhb_occupancy))

# Total vehicles travelling between p-a pair
skim <- skim %>%
  mutate(total_veh_trips=total_SOV_veh_trips + total_HOV_veh_trips)

# Match TransCAD Centroid IDs
geoid_centroid_dict <- read_csv(here("data", "full_geoid_centroid_dict.csv")) %>%
  select(geoid, GEOID)

# Convert from centroids to GEOIDs

xwalk_from <- geoid_centroid_dict %>%
  rename(from_node=geoid, from_GEOID=GEOID)

xwalk_to <- geoid_centroid_dict %>% 
  rename(to_node=geoid, to_GEOID=GEOID)

PA_mat_transcad <- left_join(skim, xwalk_from) %>%
  left_join(xwalk_to) %>%
  rename(flow=total_veh_trips) %>%
  select(from_node, to_node, flow)

write_csv(PA_mat_transcad, here("data", "TransCAD_Data", "PA_mat_total_vehs.csv"))


# Export simplified P-A matrix
skim_pa
