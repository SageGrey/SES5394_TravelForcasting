#### Load Libraries and Data ####
library(sf)
library(ggplot2)
library(hrbrthemes)
library(devtools)
library(ggthemes)
library(RColorBrewer)
library(treemapify)
library(here)

# Load data created in zone_data_collection.R
zones <- st_read(here("data", "okc_zone_data.geojson"))

# zones_undupe <- zones[!duplicated(zones), ]

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

# Create football stadium element
football_point <- geom_point(
  aes(x = -97.444191, y = 35.205841),
  color = 'black',
  size = 4,
  shape = 18
)
football_text <- geom_text(
  aes(x = -97.20, y = 35.205841, label = "Football Stadium"),
  size = 2,
  color = "black"
)

#### Create Chloropleth Maps ####
chlor_pal <- brewer.pal(5, "BrBG")
chlor_pal_grey <- brewer.pal(5, "Greys")
chlor_pal_greens <- brewer.pal(5, "BuGn")

# Median Income Map
ggplot(zones) +
  geom_sf(aes(fill = median_incomeE)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Median Income") +
  
  ##Label Football Stadium
  football_point +
  football_text +
  
  theme(legend.title = element_text("Median Income")) +
  clear_map_theme

# Number of Households
ggplot(zones) +
  geom_sf(aes(fill = total_hhsE)) +
  scale_fill_gradientn(colors = chlor_pal, name = "# of Households") +
  ggtitle("Number of Households") +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text("Median Income")) +
  football_point +
  football_text +
  clear_map_theme +
  scale_colour_solarized()


# Total Employment
ggplot(zones) +
  geom_sf(aes(fill = total_emp), color = NA) +
  ggtitle("Total Employment") +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text("Median Income"))  +
  clear_map_theme


# Employment Density Map
ggplot(zones) +
  geom_sf(aes(fill = emp_density)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Employees per Tract") +
  ggtitle("Employment Density") +
  football_point +
  football_text +
  theme(legend.title = element_text("Median Income")) +
  clear_map_theme


#Activity Density Map
ggplot(zones) +
  geom_sf(aes(fill = act_density)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Activity per tract") +
  ggtitle("Activity Density") +
  
  ##Label Football Stadium
  football_point +
  football_text +
  theme(legend.title = element_text("Median Income")) +
  clear_map_theme


#### Create Histograms ####
# 4+ Person Households
zones %>%
  ggplot(aes(x = hh_4person_plusE)) +
  geom_histogram(
    binwidth = 30,
    fill = "#69b3a2",
    color = "#e9ecef",
    alpha = 0.9
  ) +
  ggtitle("Histogram of Number of 4+ Person Households") +
  xlab("# of 4+ Person Households (Estimated)") +
  ylab("# of Census Tracts") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 12))

# Households with no cars
zones %>%
  ggplot(aes(x = no_vehE)) +
  geom_histogram(
    binwidth = 15,
    fill = "sienna",
    color = "#e9ecef",
    alpha = 0.9
  ) +
  ggtitle("Histogram of Number of Households with no cars") +
  xlab("# of households without cars") +
  ylab("# of Census Tracts") +
  
  theme_ipsum() +
  theme(plot.title = element_text(size = 10))


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


#### Dot Density Map ####
# Create Points
no_veh_pct <- st_sample(zones,
                        size = ceiling(zones$no_vehE / 100))

one_veh_pts <- st_sample(zones,
                         size = ceiling(zones$one_vehE / 100))

two_veh_pts <- st_sample(census,
                         size = ceiling(zones$two_vehE / 100))

three_veh_pts <- st_sample(census,
                           size = ceiling(zones$three_vehE / 100))
fourPlus_veh_pts <- st_sample(zones,
                              size = ceiling (zones$fourplus_vehE / 100))

# Create Df of Dots
no_veh_df <- tibble(vehOwn = rep("No Vehicles",
                                 length(no_veh_pct))) %>%
  st_sf(geom = no_veh_pct)

one_veh_df <- tibble(vehOwn = rep("One Vehicle",
                                  length(one_veh_pts))) %>%
  st_sf(geom = one_veh_pts)

two_veh_df <- tibble(vehOwn = rep("Two Vehicles",
                                  length(two_veh_pts))) %>%
  st_sf(geom = two_veh_pts)

three_veh_df <- tibble(vehOwn = rep("Three Vehicles",
                                    length(three_veh_pts))) %>%
  st_sf(geom = three_veh_pts)

four_veh_df <- tibble(vehOwn = rep("Four or More Vehicles",
                                   length(fourPlus_veh_pts))) %>%
  st_sf(geom = fourPlus_veh_pts)

vehOwn_df <-
  rbind(no_veh_df, one_veh_df, two_veh_df, three_veh_df, four_veh_df)

# Plot
ggplot(zones) +
  geom_sf(color = "white") +
  geom_sf(data = vehOwn_df,
          aes(color = vehOwn),
          alpha = 0.3,
          size = 0.1) +
  scale_color_brewer("Vehicles Owned\n(each points represents\n100 households)",
                     palette = "Set1") +
  theme_void()  +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 0.6)))


#### Tree Map ####
# Employment Breakdown
## Sum data
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

# Plot
ggplot(total_employment_type, aes(area = pct, fill = emp_type)) +
  geom_treemap(show.legend = FALSE, color = NA) +
  geom_treemap_text(aes(label = paste(
    emp_type, "\n",
    prettyNum(pct * 100, digits = 1),
    "%", sep = ""
  )),
  color = "white") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("OKC Employment Breakdown")


## devtools::install_github("katiejolly/nationalparkcolors")