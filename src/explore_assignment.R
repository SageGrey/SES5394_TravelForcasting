library(tidyverse)
library(here)

base <- read_csv(here("data", "okc_analysis_roads+okcLinkFlows.csv"))
scenario <- read_csv(here("data", "okc_analysis_roads+LinkFlows_scenario.csv"))

sum(base$capacity)
sum(scenario$capacity)

sum(base$Tot_VMT, na.rm=TRUE)
sum(scenario$Tot_VMT, na.rm=TRUE)

land_rush_tracts <- c("40081961402","40081961300","40109109001","40109109004","40109109003","40081961401")
zone_geometry <- zone_geometry %>%
  mutate(rush = ifelse(GEOID %in% land_rush_tracts, "ivory2", "ivory2")) %>%
  mutate(rush = ifelse(GEOID == "40109101900", "seagreen", "ivory2"))

ggplot() +
  geom_sf(data = zone_geometry,
          fill = zone_geometry$rush,
          color = "gray") +
  theme_map() +
  theme(legend.position = "none")

base