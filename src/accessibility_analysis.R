library(tidyverse)
library(here)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggthemes)


clear_map_theme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  title = element_blank()
)

chlor_pal <- brewer.pal(5, "BrBG")
chlor_pal_grey <- brewer.pal(5, "Greys")
chlor_pal_greens <- brewer.pal(5, "BuGn")


full_skim <- read_csv(here("data", "okc_full_skim.csv"))
zone_data <- st_read(here("data", "okc_zones_with_centroids.geojson"))

# Calculate perceived travel time
full_skim <- mutate(full_skim, OVTT = transit_time - IVTT) %>%
  mutate(percieved_time = (IVTT + 2.5* OVTT))

# Add employment data to skim
employment_data <- zone_data %>%
  select(centroid_id, GEOID, total_emp)

accessibility_df <- full_skim %>%
  left_join(employment_data, by=join_by(Destination==centroid_id))

# Define decay function with inflection=25, stdev=5
logistic_decay <- function(travel_time) {
  1/(1 + exp((travel_time - 25)/5))  # 25 Minutes travel time, standard deviation of 5
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

ggplot(accessibility_summary) +
  geom_histogram(aes(x = car_access),
                 bins = 35,
                 color = "blue",
                 fill = "lightblue") +
  scale_x_continuous(name = "Accessibility Total by Car") +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal()
# Analysis, There are a group of OKC census tracts that have a low degree of accessibility with a large number of census tracts with high accessibility


ggplot(accessibility_summary) +
  geom_histogram(aes(x = transit_access),
                 bins = 35,
                 color = "blue",
                 fill = "lightblue") +
  scale_x_continuous(name = "Accessibility Total by Transit") +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal()


ggplot(accessibility_summary) +
  geom_point(aes(x = car_access,
                 y = transit_access),
             alpha = 0.7,
             color = "black",
             shape = "o") +
  scale_x_continuous(name = "Car accessibility index") +
  scale_y_continuous(name = "Transit accessibility index") +
  theme_minimal()

## Spatial Distribution

## Car 
options(scipen=999)
zone_data <- zone_data %>%
  left_join(accessibility_summary, by=join_by(centroid_id == Origin))

ggplot(zone_data) +
annotation_map_tile(type = "osm",
                      zoomin = 0,
                      progress = "none") +
  geom_sf(aes(fill =car_access),
          color = NA,
          alpha = 0.6) +
  #clear_map_theme +
  scale_fill_gradientn(colors = chlor_pal_greens,
                       name = "Car Accessibility",
                       # trans = "log",
                      # breaks = c(0.000001,100),
                      # labels = c("Low", "High")
                      ) +

  theme_map()


## Transit 
options(digits=3)
zone_data <- zone_data %>%
  left_join(accessibility_summary, by=join_by(centroid_id == Origin))

ggplot(zone_data) +
   annotation_map_tile(type = "osm",
                       zoomin = 0,
                     progress = "none") +
  geom_sf(aes(fill = transit_access),
          color = NA,
          alpha = 0.6) +
  
  scale_fill_gradientn(colors = chlor_pal_greens,
                       name = "Transit Accessibility (log)",
                       trans = "log",
                       #breaks = c(0.000001,100),
                       #labels = c("Low","High")
                       ) +

  theme_map()
  #clear_map_theme
