#Load Libraries
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(hrbrthemes)
library(devtools)
library(viridis)
library(ggthemes)
library(RColorBrewer)

# Download longitudinal employer-household dynamic dataset
lehd_blocks <-
  read_csv(
    'https://lehd.ces.census.gov/data/lodes/LODES7/ok/wac/ok_wac_S000_JT00_2015.csv.gz',
    show_col_types = FALSE
  ) %>%
  rename(total_emp = C000) %>%
  mutate(basic_emp = CNS01 + CNS02 + CNS03 + CNS04 + CNS05 + CNS06 + CNS08 +
           CNS09) %>%
  rename(retail_emp = CNS07) %>%
  mutate(service_emp = total_emp - basic_emp - retail_emp) %>%
  select(w_geocode, total_emp, basic_emp, retail_emp, service_emp)

# Collapse data from block to tract level
lehd_tracts <- lehd_blocks %>%
  mutate(w_geocode = as.character(w_geocode)) %>%
  mutate(GEOID = substr(w_geocode, 1, 11)) %>%
  select(-w_geocode) %>%
  group_by(GEOID) %>%
  summarize(across(everything(), ~ sum(.)))

# Download census tract data
hh_vars = c(
  ## Number of Vehicles
  no_veh = "B08201_002",
  total_veh = "B08201_002",
  one_veh = "B08201_003",
  two_veh = "B08201_004",
  three_veh = "B08201_005",
  fourplus_veh = "B08201_006",
  
  ## Total Households
  total_hhs = "B08201_001",
  
  ## Household Structure
  hh_1person = "B08201_007",
  hh_2person = "B08201_013",
  hh_3person = "B08201_019",
  hh_4person_plus = "B08201_025",
  
  ## Children
  total_under6 = "B05009_002",
  total_6to17 = "B05009_020",
  # adults living with parents
  total_18to34 = "B09021_012",
  
  ## Income
  total_household_income = "B19001_001",
  inc_lt_10k = "B19001_002",
  inc_btw_10k_15k = "B19001_003",
  inc_btw_15k_20k = "B19001_004",
  inc_btw_20k_25k = "B19001_005",
  inc_btw_25k_30k = "B19001_006",
  inc_btw_30k_35k = "B19001_007",
  inc_btw_35k_40k = "B19001_008",
  inc_btw_40k_45k = "B19001_009",
  inc_btw_45k_50k = "B19001_010",
  inc_btw_50k_60k = "B19001_011",
  inc_btw_60k_75k = "B19001_012",
  inc_btw_75k_100k = "B19001_013",
  inc_btw_100k_125k = "B19001_014",
  inc_btw_125k_150k = "B19001_015",
  inc_btw_150k_200k = "B19001_016",
  inc_gt_200k = "B19001_017"
)

census <- get_acs(
  geography = "tract",
  state = "OK",
  county = c(
    "Oklahoma",
    "Lincoln",
    "Logan",
    "Canadian",
    "Grady",
    "McClain",
    "Cleveland"
  ),
  variables = hh_vars,
  output = "wide",
  geometry = TRUE
) %>%
  filter(!st_is_empty(geometry))

## Get the land area in sq meters
areas <- tracts(
  state = "OK",
  
  county = c(
    "Oklahoma",
    "Lincoln",
    "Logan",
    "Canadian",
    "Grady",
    "McClain",
    "Cleveland"
  )
) %>%
  select(GEOID, ALAND) %>%
  st_drop_geometry()


## Get population data
population <- get_decennial(
  geography = "tract",
  state = "OK",
  year = 2020,
  county = c(
    "Oklahoma",
    "Lincoln",
    "Logan",
    "Canadian",
    "Grady",
    "McClain",
    "Cleveland"
  ),
  variables = "P1_001N",
  output = "wide",
  geometry = FALSE
) %>%
  mutate(total_pop = P1_001N) %>%
  select(c("total_pop", "GEOID"))

## Join datasets and calculate densities
zones <- left_join(census, lehd_tracts) %>%
  left_join(areas) %>%
  left_join(population) %>%
  mutate(emp_density = 1e6 * total_emp / ALAND) %>% # calculate employment density per km2
  mutate(pop_density = 1e6 * total_pop / ALAND) %>% # calculate population density per km2
  mutate(act_density = emp_density + pop_density) # calculate activity density per km2



### Chloropleths
chlor_pal <- brewer.pal(5, "BrBG")
chlor_pal_grey <- brewer.pal(5,"Greys" )
chlor_pal_greens <- brewer.pal(5, "BuGn")

#Median Income Map
  ## Data
  median_income_map <- ggplot(zones) +
  geom_sf(aes(fill = median_incomeE)) +
  scale_fill_gradientn(colors = chlor_pal_greens, name = "Median Income")+
  ggtitle("Median Income") +
  
  ##Label Football Stadium
      geom_point(aes(x=-97.444191, y=35.205841),
                 color = 'black', size = 4, shape =18) +
      geom_text(aes(x=-97.20, y=35.205841, label = "football stadium"), size =2, color ="black")
 
    ## Plot Map
    median_income_map 


hh_size_map <- ggplot(zones) +
  geom_sf(aes(fill = total_hhsE)) +
  scale_fill_gradientn(colors = chlor_pal, name = "# of Households") +
  ggtitle("Number of Households") +
  #scale_fill_continuous_tableau(name = "Total Households")+
  theme_ipsum() +
  theme(plot.title = element_text(size = 12)) +
  geom_point(
    aes(x = -97.444191, y = 35.205841),
    color = 'black',
    size = 4,
    shape = 18
  ) +
  # geom_label(aes(x=-97.444191, y=35.205841, label= "football stadium"))
  geom_text(
    aes(x = -97.20, y = 35.205841, label = "football stadium"),
    size = 2,
    color = "black"
  )
hh_size_map + theme_solarized() + scale_colour_solarized()



total_employment_map <- ggplot(zones) +
  geom_sf(aes(fill = total_emp), color = NA) +
  ggtitle("Total Employment") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 12))
total_employment_map


hh_size_map

### Historgrams
hist_hhs <- zones %>%
  ggplot(aes(x = total_hhsE)) +
  geom_histogram(
    binwidth = 30,
    fill = "#69b3a2",
    color = "#e9ecef",
    alpha = 0.9
  ) +
  ggtitle("Census Tracts with 'X' Num. of Households") +
  xlab("Number of Households (Estimated)") +
  ylab("# of Census Tracts") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 12))
hist_hhs


Hist_NoVehAvail <- zones %>%
  ggplot(aes(x = no_vehE)) +
  geom_histogram(
    binwidth = 30,
    fill = "sienna",
    color = "#e9ecef",
    alpha = 0.9
  ) +
  ggtitle("Census Tracts with 'X' Num. of Households with no cars") +

  theme_ipsum() +
  theme(plot.title = element_text(size = 10))
Hist_NoVehAvail


#ScatterPlot

#Income & Children Living at Home

Living_At_Home_By_Income <- ggplot(zones, aes(x=total_18to34E, y=median_incomeE, color= no_vehE)) + 
  geom_point(size=3) +
  xlab("Number of adults living with parents per census tract") +
  ylab("Median income for census tract") +
  theme_ipsum()

Living_At_Home_By_Income

myvars_acs <- load_variables(2020, "pl")
population <- get_decennial(
  geography = "tract",
  state = "OK",
  year = 2020,
  county = c(
    "Oklahoma",
    "Lincoln",
    "Logan",
    "Canadian",
    "Grady",
    "McClain",
    "Cleveland"
  ),
  variables = "P1_001N",
  output = "wide",
  geometry = FALSE
)


# Dot Density Map
##Create Points
no_veh_pct <- st_sample(zones, 
                            size = ceiling(zones$no_vehE/100))

one_veh_pts <- st_sample(zones, 
                            size = ceiling(zones$one_vehE/100))

two_veh_pts <- st_sample(census, 
                            size = ceiling(zones$two_vehE/100))

three_veh_pts <- st_sample(census, 
                            size = ceiling(zones$three_vehE/100))
fourPlus_veh_pts <- st_sample(zones, 
                              size = ceiling (zones$fourplus_vehE/100))

## Create Df of Dots
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

 vehOwn_df<- rbind(no_veh_df, one_veh_df, two_veh_df, three_veh_df, four_veh_df)

 
 ## Plot
 
 ggplot(zones) + 
   geom_sf(color = "white") +
   geom_sf(data = vehOwn_df, 
           aes(color = vehOwn), 
           alpha = 0.3,
           size = 0.1) +
   scale_color_brewer("Vehicles Owned\n(each points represents\n100 households)",
                      palette = "Set1") +
   theme_void()  +
   guides(color = guide_legend(override.aes = list(size=5, alpha = 0.6)))
 
 
### Reference Space

## devtools::install_github("katiejolly/nationalparkcolors")
