
###############################################################################
######################          CITY DIFFERENCE       #########################

# Which map shows the most areas coloured red?

set.seed(2015)
var.g.dummy <- gstat(formula = z ~ 1, 
                     locations = ~ longitude + latitude, 
                     dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
                     nmax = 12)

# Create underlying spatially dependent data for 12 null plots
var.sim <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))

# use an underlying spatial covariance model

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


# only use the least smoothed
sa3_min <- sa3_long %>% 
  filter(groups == "smooth1") %>%
  pull(value) %>% min()
sa3_max <- sa3_long %>% 
  filter(groups == "smooth1") %>%
  pull(value) %>% max()

sa3_mean <- sa3_long %>% 
  filter(groups == "smooth1") %>%
  pull(value) %>% mean()

#####################################################################

# increase the values of Brisbane, depending on distance from Brisbane city
# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_cities <- allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(city_distance = (max_dist - focal_dist)^8,
         dist = scales::rescale(city_distance,
                                     to = c(0,1)),
         cities = ifelse(dist < 0.9, NA, 
                         scales::rescale(city_distance,
                        to = c(sa3_mean, sa3_max))))


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
pos <- 6

aus_geo_sa3_cities <- aus_geo_cities %>%
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  group_by(simulation) %>% 
  mutate(value = ifelse(simulation == pos, 
         # for the true data plot, keep random values if not close enough to cities
         ifelse(is.na(cities), value, true),
         # for all others rescale to the same range
         scales::rescale((value), c(sa3_min, sa3_max))))

pos <- 6

aus_hex_sa3_cities <- aus_hex_cities %>% 
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the new data around the distribution of null data
  group_by(simulation) %>% 
  mutate(value = ifelse(simulation == pos, 
                        # for the true data plot, keep random values if not close enough to cities
                        ifelse(is.na(cities), value, true),
                        # for all others rescale to the same range
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
  facet_wrap(~ simulation) + theme_minimal() + guides(fill = FALSE) +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    strip.text.x = element_text(colour = "white", size = 40),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
aus_geo_cities_plot

ggsave(filename = "figures/cities/aus_geo_cities.pdf", plot = aus_geo_cities_plot, device = "pdf", dpi = 300,
  height = 14, width = 18)

aus_hex_cities_plot <- aus_hex_sa3_cities %>% 
  ggplot() + 
  geom_sf(data = aus_underlay, colour = "grey", fill = NA, size = 0.01) + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() + guides(fill = FALSE) +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    strip.text.x = element_text(colour = "white", size = 40),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
aus_hex_cities_plot

ggsave(filename = "figures/cities/aus_hex_cities.pdf", plot = aus_hex_cities_plot, device = "pdf", dpi = 300,
  height = 14, width = 18)

