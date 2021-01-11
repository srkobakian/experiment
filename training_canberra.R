
###############################################################################
######################            Clusters            #########################


# Example of three maps

set.seed(2010)
var.g.dummy <- gstat(formula = z ~ 1, 
  locations = ~ longitude + latitude, 
  dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
  nmax = 12)


# Create underlying spatially dependent data for 12 null plots
var.sim <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 3) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))
sims <- c("sim1", "sim2", "sim3")

# use an underlying spatial covariance model

sa3_sims1 <- as_tibble(var.sim) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))



# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_ade <- allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(city_distance = (max_dist - focal_dist)^8,
    # only for desired three
    dist = ifelse(points %in% "Adelaide", 
      scales::rescale(city_distance, to = c(0,1.5)), 
      scales::rescale(city_distance, to = c(0,1))))






sa3_long <- sa3_sims1 %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))


### Start with shapes - geographies
aus_geo_ade_data <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ade)

### Start with shapes - hexagons
aus_hex_ade_data <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ade)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- 3

aus_geo_sa3_ade <- aus_geo_ade_data %>%
  mutate(true = dist) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(-1,1)), 
    scales::rescale((value), c(-1,1))))

pos <- 2

aus_hex_sa3_ade <- aus_hex_ade_data %>%
  mutate(true = dist) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(-1,1)), 
    scales::rescale((value), c(-1,1))))



aus_geo_ade <- aus_geo_sa3_ade %>% 
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
ggsave(filename = "figures/train/aus3_geo_ade.png", plot = aus_geo_ade, device = "png", dpi = 300,
  height = 9, width = 18)


aus_hex_ade <- aus_hex_sa3_ade %>% 
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
ggsave(filename = "figures/train/aus3_hex_ade.png", plot = aus_hex_ade, device = "png", dpi = 300,
  height = 9, width = 18)


