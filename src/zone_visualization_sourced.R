#### Load Libraries and Data ####
library(sf)
library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(RColorBrewer)
library(treemapify)
library(here)
library(dots)
library(viridis)
library(tidytransit)
library(tigris)
library(kableExtra)

# Load data created in zone_data_collection.R
zones <- st_read(here("data", "okc_zones_with_centroids.geojson"))
zones_scenario <- st_read(here("data", "okc_zone_data_scenario.geojson"))

#### Create reusable elements ####

# Create football stadium element
football_stadium <- function()
  list(geom_point(
    aes(x = -97.444191, y = 35.205841),
    color = 'black',
    size = 4,
    shape = 18
  ), geom_text(
    aes(x = -96.8, y = 35.205841, label = "Football\nStadium"),
    size = 5,
    color = "black"
  ), geom_segment(
    aes(x = -97.444191, y = 35.205841,
        xend = -97, yend = 35.205841,
        linetype = "dashed",
    ),
    show_guide=FALSE
  ))

# Create map color bar legend
map_colorbar <- guide_colorbar(
  direction = "horizontal",
  barheight = unit(2, units = "mm"),
  barwidth = unit(50, units = "mm"),
  draw.ulim = T,
  title.position = 'top'
)

# #### Create Chloropleth Maps ####
# chlor_pal <- brewer.pal(5, "BrBG")
# chlor_pal_grey <- brewer.pal(5, "Greys")
# chlor_pal_greens <- brewer.pal(5, "BuGn")


#### University of Oklahoma Travel Data
uok_geoid <- "40027201202"
# uok_geoid <- "40027201201"
# uok_centroid <- "171"

zone_geometry <- zones %>%
  select(GEOID, geometry) %>%
  mutate_at("GEOID", funs(as.double(.)))

uok_origin_skim <- read_csv(here("data", "okc_full_skim_geoid.csv")) %>%
  filter(Destination==uok_geoid) %>%
  left_join(zone_geometry, by=join_by(Origin==GEOID)) %>%
  st_as_sf(sf_column_name = "geometry")
  
uok_dest_skim <- read_csv(here("data", "okc_full_skim_geoid.csv")) %>%
  filter(Origin==uok_geoid) %>%
  left_join(zone_geometry, by=join_by(Destination==GEOID)) %>%
  st_as_sf(sf_column_name = "geometry")

# zones_centroids <- st_read(here("data", "okc_zones_with_centroids.geojson")) %>%
#   select(centroid_id, geometry)
# 
# uok_dest_skim <- read_csv(here("data", "okc_full_skim.csv")) %>%
#   filter(Destination==uok_centroid) %>%
#   left_join(zones_centroids, by=join_by(Origin==centroid_id))
#   st_as_sf(sf_column_name = "geometry")
# 
# ggplot(uok_dest_skim) +
#   geom_sf(aes(fill=transit_time)) +
#   theme_map() +
#   theme(
#     rect = element_rect(fill = "transparent"),
#     legend.position=c(0.5,0)
#   )
# 
# 

#### Map Transit Routes with GTFS Data
OKC_rta_gtfs <- read_gtfs("https://embarkok.com/data/gtfs/google_transit.zip")
route_shapes <- shapes_as_sf(OKC_rta_gtfs$shapes)

# Tracts that were part of the 1895 Land Rush
land_rush_tracts <- c("40081961402","40081961300","40109109001","40109109004","40109109003","40081961401")

OKC_tracts <- tracts(state = "OK", county = c("Oklahoma","Cleveland", "McClain", "Lincoln", "Logan", "Canadian", "Grady")) %>%
  mutate(color=case_when(
    COUNTYFP=="109" | COUNTYFP=="027" ~ "ivory2",
    TRUE ~ "cornsilk")) %>%
  mutate(rush= ifelse(GEOID %in% land_rush_tracts, "darkgoldenrod2", ifelse(GEOID == "40109108508", "seagreen", "ivory2")))


OKC_transit_tracts <- OKC_tracts  %>%
  filter((COUNTYFP=="109" | COUNTYFP=="027"))


trip_gen <- st_read(here("data", "trip-gen-newVariables.geojson"))
trip_gen_scenario <- st_read(here("data", "trip-gen-newVariables_scenario.geojson"))
# 
# total_trips <- trip_gen %>%
#   summarise("Home Based Work"=round(sum(hbw_trip_prod)),
#             "Home Based Other"=round(sum(hbo_trip_prod)),
#             "Non Home Based"=round(sum(nhb_trip_prod))) %>%
#   st_drop_geometry() %>%
#   t()


# # Employment Density Map
# ggplot(zones) +
#   geom_sf(aes(fill = emp_density)) +
#   scale_fill_gradientn(colors = chlor_pal_greens, name = "Employees per Tract") +
#   ggtitle("Employment Density") +
#   football_point +
#   football_text +
#   theme(legend.title = element_text("Median Income")) +
#   clear_map_theme
# 
# 
# #Activity Density Map
# ggplot(zones) +
#   geom_sf(aes(fill = act_density)) +
#   scale_fill_gradientn(colors = chlor_pal_greens, name = "Activity per tract") +
#   ggtitle("Activity Density") +
#   
#   ##Label Football Stadium
#   football_point +
#   football_text +
#   theme(legend.title = element_text("Median Income")) +
#   clear_map_theme
# 
# 
# #### Create Histograms ####
# # 4+ Person Households
# zones %>%
#   ggplot(aes(x = hh_4person_plusE)) +
#   geom_histogram(
#     binwidth = 30,
#     fill = "#69b3a2",
#     color = "#e9ecef",
#     alpha = 0.9
#   ) +
#   ggtitle("Histogram of Number of 4+ Person Households") +
#   xlab("# of 4+ Person Households (Estimated)") +
#   ylab("# of Census Tracts") +
#   theme_ipsum() +
#   theme(plot.title = element_text(size = 12))
# 
# # Households with no cars
# zones %>%
#   ggplot(aes(x = no_vehE)) +
#   geom_histogram(
#     binwidth = 15,
#     fill = "sienna",
#     color = "#e9ecef",
#     alpha = 0.9
#   ) +
#   ggtitle("Histogram of Number of Households with no cars") +
#   xlab("# of households without cars") +
#   ylab("# of Census Tracts") +
#   
#   theme_ipsum() +
#   theme(plot.title = element_text(size = 10))
# 
# 
#### Scatter Plot ####
#Income & Children Living at Home
ggplot(zones,
       aes(x = total_18to34E, y = median_incomeE, color = pop_density)) +
  geom_point(size = 3) +
  ggtitle("Census Tracts by Income, Household Structure, and Density ") +
  xlab("# of households where adults live with their parents") +
  ylab("Median income") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 10))
# 

#### Dot Density Map ####
# Using a library
veh_cols <- c("no_vehE", "one_vehE", "two_vehE", "three_vehE", "fourplus_vehE")

# dots_points(shp=zones_cars, cols=all_of(veh_cols), divisor=100) %>%
#   ggplot() +
#   geom_sf(data=zones, color="white") +
#   geom_sf(
#     aes(color=dots_type),
#     alpha=0.3,
#     size=0.1
#   ) +
#   scale_color_brewer("Vehicles Owned\n(each points represents\n100 households)",
#     palette = "Set1") +
#   theme_map() +
#   guides(color = guide_legend(override.aes = list(size = 5, alpha = 0.6)))

zones_cars <- zones %>%
  mutate(total_cars=one_vehE + 2*two_vehE + 3*three_vehE + 4*fourplus_vehE)
# 
# dots_points(shp=zones_cars, cols=c("total_cars"), divisor=100) %>%
#   ggplot() +
#   geom_sf(data=zones, color="white") +
#   geom_sf(
#     alpha=0.3,
#     size=0.1,
#     color="darkorange"
#   ) +
#   theme_map()
# 
# # Doing it by hand
# scale_factor <- 1000
# columns <- c("no_vehE", "one_vehE", "two_vehE", "three_vehE", "fourplus_vehE")
# 
# no_veh_pct <- st_sample(zones, size = ceiling(zones[["no_vehE"]] / scale_factor))
# 
# one_veh_pts <- st_sample(zones, size = ceiling(zones$one_vehE / scale_factor))
# 
# two_veh_pts <- st_sample(census, size = ceiling(zones$two_vehE / scale_factor))
# 
# three_veh_pts <- st_sample(census, size = ceiling(zones$three_vehE / scale_factor))
# 
# fourPlus_veh_pts <- st_sample(zones, size = ceiling (zones$fourplus_vehE / scale_factor))
# 
# # Create Df of Dots
# no_veh_df <- tibble(vehOwn = rep("No Vehicles",
#                                  length(no_veh_pct))) %>%
#   st_sf(geom = no_veh_pct)
# 
# one_veh_df <- tibble(vehOwn = rep("One Vehicle",
#                                   length(one_veh_pts))) %>%
#   st_sf(geom = one_veh_pts)
# 
# two_veh_df <- tibble(vehOwn = rep("Two Vehicles",
#                                   length(two_veh_pts))) %>%
#   st_sf(geom = two_veh_pts)
# 
# three_veh_df <- tibble(vehOwn = rep("Three Vehicles",
#                                     length(three_veh_pts))) %>%
#   st_sf(geom = three_veh_pts)
# 
# four_veh_df <- tibble(vehOwn = rep("Four or More Vehicles",
#                                    length(fourPlus_veh_pts))) %>%
#   st_sf(geom = fourPlus_veh_pts)
# 
# vehOwn_df <-
#   rbind(no_veh_df, one_veh_df, two_veh_df, three_veh_df, four_veh_df)
# 
# # Plot
# ggplot(zones) +
#   geom_sf(color = "white") +
#   geom_sf(data = vehOwn_df,
#           aes(color = vehOwn),
#           alpha = 0.3,
#           size = 0.1) +
#   scale_color_brewer("Vehicles Owned\n(each points represents\n100 households)",
#                      palette = "Set1") +
#   theme_void()  +
#   guides(color = guide_legend(override.aes = list(size = 5, alpha = 0.6)))


#### Tree Map ####
# Calculate Employment Breakdown
total_employment <- sum(zones$total_emp, na.rm = TRUE)
total_employment_type <- zones %>%
  summarise(
    retail = sum(retail_emp, na.rm = TRUE) / total_employment,
    basic = sum(basic_emp, na.rm = TRUE) / total_employment,
    service = sum(service_emp, na.rm = TRUE) / total_employment
  ) %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = c("basic", "service", "retail"),
    names_to = "emp_type",
    values_to = "pct"
  )
