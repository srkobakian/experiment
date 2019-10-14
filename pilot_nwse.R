
###############################################################################
######################    NORTH WEST TO SOUTH EAST    #########################

# First source the simulated points
#source('~/experiment/R/00_pilot.R', echo = TRUE)

# only use the most smoothed
sa3_min <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% min()
sa3_max <- sa3_long %>% 
  filter(groups == "smooth5") %>%
  pull(value) %>% max()

# use an underlying spatial covariance model
# add a north to south model
sa3_nwse <- sa3_centroids %>% 
  mutate(lat = abs(latitude-min(latitude)),
    long = abs(longitude-min(longitude)),
    nwse = lat*long) %>% 
  mutate(nwse = scales::rescale(nwse, to = c(sa3_min, sa3_max))) %>%
  select(-lat, -long)

###############################################################################
######################         NORTH TO SOUTH         #########################

# First source the simulated points
source('~/experiment/R/00_pilot.R', echo = TRUE)

# use an underlying spatial covariance model
# add a north to south model
sa3_nwse <- sa3_centroids %>% mutate(nwse = abs(latitude-min(latitude)))

### Start with shapes - geographies
aus_geo_nwse <- sa3 %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_nwse)

### Start with shapes - hexagons
aus_hex_nwse <- hexagonwse_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_nwse)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

aus_geo_sa3 <- aus_geo_nwse %>%
  mutate(true = nwse) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

aus_hex_sa3 <- aus_hex_nwse %>% 
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
ggsave(filename = "figures/nwse/aus_geo_nwse.png", plot = aus_geo_nwse, device = "png", dpi = 300,
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
ggsave(filename = "figures/nwse/aus_hex_nwse.png", plot = aus_hex_nwse, device = "png", dpi = 300,
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
ggsave(filename = "figures/nwse/aus_geo_nwse.png", plot = aus_geo_nwse, device = "png", dpi = 300,
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
ggsave(filename = "figures/nwse/aus_hex_nwse.png", plot = aus_hex_nwse, device = "png", dpi = 300,
  height = 12, width = 12)




############################################################################### 
############################       Tasmania        ############################
############################################################################### 


tas_geo_sa3 <- aus_geo_sa3 %>%
  filter(sa3_name_2016 %in% Tasmania)

tas_hex_sa3 <- aus_hex_sa3 %>%
  filter(sa3_name_2016 %in% Tasmania)

###############################################################################
tas_geo_sa3 %>% 
  ggplot() + geom_density(aes(x = nwse)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn")
ggsave(filename = "figures/nwse/density.png", plot = tas_nwse, device = "png", dpi = 300,
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
ggsave(filename = "figures/nwse/tas_nwse.png", plot = tas_nwse, device = "png", dpi = 300,
  height = 6, width = 6)


hex_nwse <- tas_hex_sa3 %>% 
  ggplot() + geom_sf(aes(fill = nwse)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~iteration)
ggsave(filename = "figures/nwse/hex_nwse.png", plot = hex_nwse, device = "png", dpi = 300,
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
