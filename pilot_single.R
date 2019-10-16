
###############################################################################
######################     SINGLE CITY DIFFERENCE     #########################

# First source the simulated points
# source('~/experiment/R/00_pilot.R', echo = TRUE)

# only use the most smoothed
sa3_min <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% min()
sa3_max <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% max()

sa3_mean <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% mean()


# use an underlying spatial covariance model
# increase the values of Brisbane, depending on distance from Brisbane city
# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area with Brisbane focal point

sa3_single <- allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(single = ifelse(points == "Brisbane", scales::rescale(sqrt(max_dist - focal_dist), to = c(sa3_mean, sa3_max)), NA))
    
    
### Start with shapes - geographies
aus_geo_single <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_single)

### Start with shapes - hexagons
aus_hex_single <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_single)


############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- sample(1:12, 1)

aus_geo_sa3 <- aus_geo_single %>%
  mutate(true = single) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, ifelse(!is.na(true), true, value), 
    scales::rescale((value), c(sa3_min, sa3_max))))

aus_hex_sa3 <- aus_hex_single %>% 
  mutate(true = single) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the new data around the distribution of null 
  mutate(value = ifelse(simulation == pos, ifelse(!is.na(true), true, value), 
    scales::rescale((value), c(sa3_min, sa3_max))))


############################################################################### 
############################   Population ns       ############################
############################################################################### 

aus_geo_single_plot <- aus_geo_sa3 %>% 
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
ggsave(filename = "figures/single/aus_geo_single.png", plot = aus_geo_single_plot, device = "png", dpi = 300,
  height = 18, width = 18)


aus_hex_single_plot <- aus_hex_sa3 %>% 
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
ggsave(filename = "figures/single/aus_hex_single.png", plot = aus_hex_single_plot, device = "png", dpi = 300,
  height = 18, width = 18)



