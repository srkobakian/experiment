
###############################################################################
######################    NORTH WEST TO SOUTH EAST    #########################

# First source the simulated points
source('~/experiment/R/00_pilot.R', echo = TRUE)

# use an underlying spatial covariance model
# add a north to south model
sa3_nwse_pop <- sa3_centroids %>% 
  mutate(lat = abs(latitude-min(latitude)),
    long = abs(longitude-min(longitude)),
    nwse = lat*long) %>% select(-lat, -long)

###############################################################################
######################         NORTH TO SOUTH         #########################

# First source the simulated points
source('~/experiment/R/00_pilot.R', echo = TRUE)

# use an underlying spatial covariance model
# add a north to south model
sa3_nwse_pop <- sa3_centroids %>% mutate(nwse = abs(latitude-min(latitude)))

### Start with shapes - geographies
aus_geo_nwse_pop <- sa3 %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_nwse_pop)

### Start with shapes - hexagons
aus_hex_nwse_pop <- hexagonwse_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_nwse_pop)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

aus_geo_sa3 <- aus_geo_nwse_pop %>%
  mutate(true = nwse) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

aus_hex_sa3 <- aus_hex_nwse_pop %>% 
  mutate(true = nwse) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))


############################################################################### 
############################   Population ns       ############################
############################################################################### 

aus_geo_nwse <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_geo_nwse.png", plot = aus_geo_nwse, device = "png", dpi = 300,
  height = 9, width = 18)


aus_hex_nwse <- aus_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_hex_nwse.png", plot = aus_hex_nwse, device = "png", dpi = 300,
  height = 9, width = 18)


############################################################################### 
#########################    Population density ns    #########################
############################################################################### 

aus_geo_nwse <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_geo_nwse.png", plot = aus_geo_nwse, device = "png", dpi = 300,
  height = 12, width = 12)


aus_hex_nwse <- aus_hex_sa3 %>%
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_hex_nwse.png", plot = aus_hex_nwse, device = "png", dpi = 300,
  height = 12, width = 12)




############################################################################### 
############################       Tasmania        ############################
############################################################################### 


tas_geo_sa3 <- aus_geo_sa3 %>%
  filter(state_name_2016 == "Tasmania")

tas_hex_sa3 <- aus_hex_sa3 %>%
  filter(state_name_2016 == "Tasmania")

###############################################################################
tas_geo_sa3 %>% 
  ggplot() + geom_density(aes(x = nwse)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn")
ggsave(filename = "figures/pop/density.png", plot = tas_nwse, device = "png", dpi = 300,
  height = 6, width = 6)

spop_plot <- tas_geo_sa3 %>%
  group_by(iteration) %>% 
  mutate(mean_value  = mean(ns)) %>% 
  ggplot() + 
  geom_density(aes(x= nwse, fill = groups), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_value), colour = "black") + 
  scale_fill_manual(values = nwse_alpha)
spop_plot

tas_nwse <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = nwse)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~iteration)
ggsave(filename = "figures/pop/tas_nwse.png", plot = tas_nwse, device = "png", dpi = 300,
  height = 6, width = 6)


hex_nwse <- tas_hex_sa3 %>% 
  ggplot() + geom_sf(aes(fill = nwse)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~iteration)
ggsave(filename = "figures/pop/hex_nwse.png", plot = hex_nwse, device = "png", dpi = 300,
  height = 6, width = 6)

gridExtra::grid.arrange(tas_nwse, hex_nwse)


###############################################################################
##########################    Population density    ###########################

# Add Tasmania population density to null model

tas_nwse <- ggplot(tas_geo_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas_nwse.png", plot = tas_nwse, dpi=300, device = "png", width = 12, height = 6)

hex_nwse <- ggplot(tas_hex_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/hex_nwse.png", plot = hex_nwse, dpi=300, device = "png", width = 12, height = 6)



# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model

tas_nwse <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/nwse_tas.png", 
  plot = tas_nwse, dpi=300, device = "png", width = 12, height = 6)

hex_nwse <- tas_hex_sa3 %>%  
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/nwse_hex.png", 
  plot = hex_nwse, dpi=300, device = "png", width = 12, height = 6)
