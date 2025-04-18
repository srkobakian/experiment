# Create maps for pilot study
# This script considers the sa3 areas of Australia.
# It creates lineups by simulating a null model spatial for 20 plots,
# It then adds a data with a spatial trend to a randomly selected plot 

# libraries
library(gstat)
library(purrr)
library(sp)
library(sf)
library(sugarbag)
library(tidyverse)
library(viridis)

set.seed(19941030)

###########################################################
####################     DATA    ##########################
# Geography of sa3 areas
sa3 <- absmapsdata::sa32016
sa3 <- rmapshaper::ms_simplify(sa3, 
  keep = 0.5, keep_shapes = TRUE, explode = FALSE, drop_null_geometries = TRUE)

sa3_neighbours <- sf::st_intersects(sa3$geometry,sa3$geometry)  %>% 
  as_tibble() %>% 
  filter(row.id != col.id) %>% 
  rename(sa3neighbours = col.id) %>% 
  group_by(row.id) %>% 
  count(name = "sa3")


# filter out Islands
Islands <- c("Cocos (Keeling) Islands",	"Christmas Island", "Norfolk Island", "Lord Howe Island")
sa3 <- sa3 %>% 
  filter(!is.na(cent_long)) %>% 
  filter(!(sa3_name_2016 %in% Islands))

# Make list of Tasmanian sa3 areas
Tasmania <- sa3 %>% st_drop_geometry() %>% 
  filter(state_name_2016 == "Tasmania") %>%
  select(sa3_name_2016) %>% pull()

# derive centroids
sa3_centroids <- sa3 %>%
  select(sa3_name_2016, longitude = cent_long, latitude = cent_lat) %>%
  sf::st_drop_geometry() %>% filter(!is.na(longitude))

# add trend models to simulated errors
sa3_centroids$logsize <- log(sa3$areasqkm_2016)


###########################################################
################    AUSTRALIA    ##########################

aus_underlay <- st_union(sa3$geometry)


###########################################################
####################    HEXMAP   ##########################
# Create hexagon map of sa3
allocated <- create_hexmap(
  shp = sa3,
  buffer_dist = 2,
  sf_id = "sa3_name_2016",
  hex_size = 0.6, # same size used in create_grid
  hex_filter = 10,
  neighbours = sa3,
  f_width = 30,
  focal_points = capital_cities,
  verbose = TRUE)

# same column used in create_hexmap
hexagons <- fortify_hexagon(data = allocated, sf_id = "sa3_name_2016", hex_size = 0.6)

# Convert hexagons to polygons for plotting
# This will order the areas by the sf_id, this results in alphabetical order
hexagons_sf <- hexagons %>% 
  select(sa3_name_2016, long, lat, points, focal_dist) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(sa3_name_2016) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON")

# Add hexagons to centroids
hex <- left_join(hexagons_sf, sa3_centroids)

hex_neighbours <- st_intersects(hex, hex) %>% 
  as_tibble() %>% 
  filter(row.id != col.id) %>% 
  rename(hexneighbours = col.id) %>% 
  group_by(row.id) %>% 
  count(name = "hex") 

# Compare neighbours of hexmap and geo 
hex_neighbours <- hex_neighbours %>%
  left_join(sa3_neighbours, by = "row.id") %>% 
  mutate(diff = sa3 - hex)
  
###########################################################
###########################################################
############ CREATE SPATIAL RELATIONSHIPS #################
###########################################################
###########################################################

# Create a set of spatial trend data relationships

# Manual null data creation
# Change this parameter to change strength of spatial dependency
# Specify the spatial model
var.g.dummy <- gstat(formula = z ~ 1, 
  locations = ~ longitude + latitude, 
  dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
  nmax = 12)

# Create underlying spatially dependent data for 12 null plots
var.sim <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))


###########################################################
###########################################################
################### FIND NEIGHBOURS #######################
###########################################################
###########################################################

# use st_intersects 
sa3_neighbours <- st_intersects(sa3, sa3)
# smooth spatial relationship function:
spatial_smoother <- function(area_number, values_vector, area_weight = 0.5, neighbours_list){
  
  stopifnot( area_weight >= 0 && area_weight <= 1)
  # List of the neighbours
  neighbours <- as.vector(neighbours_list[[area_number]])
  # Remove the current area
  neighbours <- neighbours[!neighbours == area_number]
  if ((length(values_vector[neighbours]))==0){
    smoothed_value <- values_vector[area_number]
  } else {
    # Weighted value: area value and neighbours
    ave_of_neighbours <- mean(values_vector[neighbours], na.rm = TRUE)
    smoothed_value <- (area_weight)*values_vector[area_number] + (1-area_weight)*ave_of_neighbours
  }
  return(smoothed_value)
}

sims <- colnames(var.sim)[5:length(colnames(var.sim))]


###########################################################
############### Smooth null disributions ##################
###########################################################
sa3_sims1 <- as_tibble(var.sim) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))

sa3_sims2 <- sa3_sims1 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))

sa3_sims3 <- sa3_sims2 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))


smoothing <- bind_rows(
  "smooth1" = sa3_sims1,
  "smooth2" = sa3_sims2, 
  "smooth3" = sa3_sims3, .id = "groups")


sa3_long <- smoothing %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016, -groups) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))

smoothed_alpha <- c(
    smooth1 = "#7a0177",
    smooth2 = "#c51b8a",
    smooth3 = "#f768a1",
    smooth4 = "#fbb4b9",
    smooth5 = "#feebe2")

s_plot <- sa3_long %>% 
  mutate(groups = factor(groups, levels = c("smooth1", "smooth2", "smooth3"))) %>% 
  group_by(groups, simulation) %>% 
  mutate(mean_value  = mean(value)) %>% 
  ggplot() + 
  geom_density(aes(x= value, fill = groups), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_value), colour = "black") +
  facet_wrap(~ simulation) + 
  scale_fill_manual(values = smoothed_alpha)

s_plot

ggsave(filename = "figures/simulation_densities.png", 
  plot = s_plot, device = "png", dpi = 300,
  height = 12, width = 12)
