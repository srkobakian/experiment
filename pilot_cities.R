
###############################################################################
######################     cities CITY DIFFERENCE     #########################

# First source the simulated points
# source('~/experiment/R/00_pilot.R', echo = TRUE)

# only use the most smoothed
sa3_min <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% min()
sa3_max <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% max()


# use an underlying spatial covariance model
# increase the values of Brisbane, depending on distance from Brisbane city
# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_cities <- allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(cities = sqrt(max_dist - focal_dist)) %>%
  # keep a reasonable scale to match null plots
  mutate(cities = scales::rescale(cities, to = c(sa3_min, sa3_max)))

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
pos <- sample(1:20, 1)

aus_geo_sa3 <- aus_geo_cities %>%
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true, 
    scales::rescale((value), c(sa3_min, sa3_max))))

aus_hex_sa3 <- aus_hex_cities %>% 
  mutate(true = cities) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the new data around the distribution of null data
  mutate(value = ifelse(simulation == pos, true, 
    scales::rescale((value), c(sa3_min, sa3_max))))


############################################################################### 
############################   Population ns       ############################
############################################################################### 

aus_geo_cities_plot <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/cities/aus_geo_cities.png", plot = aus_geo_cities_plot, device = "png", dpi = 300,
  height = 9, width = 18)


aus_hex_cities_plot <- aus_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/cities/aus_hex_cities.png", plot = aus_hex_cities_plot, device = "png", dpi = 300,
  height = 9, width = 18)




############################################################################### 
############################       Tasmania        ############################
############################################################################### 


tas_geo_sa3 <- aus_geo_sa3 %>%
  filter(sa3_name_2016 %in% Tasmania)

tas_hex_sa3 <- aus_hex_sa3 %>%
  filter(sa3_name_2016 %in% Tasmania)

###############################################################################
tas_geo_sa3 %>% 
  ggplot() + geom_density(aes(x = cities)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu")
ggsave(filename = "figures/cities/density.png", plot = tas_cities, device = "png", dpi = 300,
  height = 6, width = 6)


spop_plot <- tas_geo_sa3 %>%
  group_by(groups) %>% 
  mutate(mean_value  = mean(cities)) %>% 
  ggplot() + 
  geom_density(aes(x= cities, fill = groups), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_value), colour = "black") + 
  scale_fill_manual(values = cities_alpha)
spop_plot

tas_cities <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = cities)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") +
  facet_wrap(~groups)
ggsave(filename = "figures/cities/tas_cities.png", plot = tas_cities, device = "png", dpi = 300,
  height = 6, width = 6)


hex_cities <- tas_hex_sa3 %>% 
  ggplot() + geom_sf(aes(fill = cities)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu") +
  facet_wrap(~groups)
ggsave(filename = "figures/cities/hex_cities.png", plot = hex_cities, device = "png", dpi = 300,
  height = 6, width = 6)

gridExtra::grid.arrange(tas_cities, hex_cities)


###############################################################################
##########################    Population density    ###########################

# Add Tasmania population density to null model

tas_cities <- ggplot(tas_geo_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas_cities.png", plot = tas_cities, dpi=300, device = "png", width = 12, height = 6)

hex_cities <- ggplot(tas_hex_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/hex_cities.png", plot = hex_cities, dpi=300, device = "png", width = 12, height = 6)



# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model

tas_cities <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/cities_geo.png", 
  plot = tas_cities, dpi=300, device = "png", width = 12, height = 12)

hex_cities <- tas_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "RdYlBu") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/cities_hex.png", 
  plot = hex_cities, dpi=300, device = "png", width = 12, height = 12)

