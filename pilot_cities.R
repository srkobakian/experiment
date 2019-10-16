
###############################################################################
######################          CITY DIFFERENCE       #########################

# First source the simulated points
# source('~/experiment/R/00_pilot.R', echo = TRUE)

# only use the most smoothed
sa3_min <- sa3_long %>% 
  filter(groups == "smooth3") %>%
  pull(value) %>% min()
sa3_max <- sa3_long %>% 
  filter(groups == "smooth3") %>%
  pull(value) %>% max()
sa3_mean <- sa3_long %>% 
  filter(groups == "smooth3") %>%
  pull(value) %>% mean()


# use an underlying spatial covariance model
# increase the values of Brisbane, depending on distance from Brisbane city
# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_cities <- allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(cities = (max_dist - focal_dist)^8,
         dist_rank = rank(focal_dist),
         cities = ifelse(dist_rank == 50, -2, 
                         scales::rescale(cities, to = c(mean(c(sa3_min, sa3_mean)), sa3_max))))

ggplot(sa3_cities) + geom_histogram(aes(x = cities))

### Start with shapes - geographies
aus_geo_cities <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_cities)

### Start with shapes - hexagons
aus_hex_cities <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_cities)


############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
set.seed(19941031)
pos <- sample(1:12, 1)

aus_geo_sa3_cities <- aus_geo_cities %>%
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, 
                        scales::rescale((value+true), c(1, sa3_max)), 
                        scales::rescale((value), c(sa3_min, sa3_max))))

aus_hex_sa3_cities <- aus_hex_cities %>% 
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the new data around the distribution of null data
  mutate(value = ifelse(simulation == pos, 
                        scales::rescale((value+true), c(1, sa3_max)), 
                        scales::rescale((value), c(sa3_min, sa3_max))))


############################################################################### 
############################                       ############################
############################################################################### 

ggplot(aus_hex_sa3_cities) + geom_histogram(aes(x=value)) + facet_wrap(~simulation)

############################################################################### 
############################                       ############################
############################################################################### 

aus_geo_cities_plot <- aus_geo_sa3_cities %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    strip.text.x = element_text(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
aus_geo_cities_plot

ggsave(filename = "figures/cities/aus_geo_cities.png", plot = aus_geo_cities_plot, device = "png", dpi = 300,
  height = 18, width = 18)

aus_hex_cities_plot <- aus_hex_sa3_cities %>% 
  ggplot() + 
  geom_sf(data = aus_underlay, colour = "lightgrey", fill = NA, size = 0.01) + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    strip.text.x = element_text(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
aus_hex_cities_plot

ggsave(filename = "figures/cities/aus_hex_cities.png", plot = aus_hex_cities_plot, device = "png", dpi = 300,
  height = 18, width = 18)

