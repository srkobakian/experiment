
###############################################################################
######################         NORTH TO SOUTH         #########################

# First source the simulated points
source('~/experiment/R/00_pilot.R', echo = TRUE)

# use an underlying spatial covariance model
# add a north to south model
sa3_ns_pop <- sa3_centroids %>% mutate(ns = abs(latitude-min(latitude)))

### Start with shapes - geographies
aus_geo_ns_pop <- sa3 %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ns_pop)

### Start with shapes - hexagons
aus_hex_ns_pop <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ns_pop)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

aus_geo_sa3 <- aus_geo_ns_pop %>%
  mutate(true = ns) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

aus_hex_sa3 <- aus_hex_ns_pop %>% 
  mutate(true = ns) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))


############################################################################### 
############################   Population ns       ############################
############################################################################### 

aus_geo_ns <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_geo_ns.png", plot = aus_geo_ns, device = "png", dpi = 300,
  height = 9, width = 18)


aus_hex_ns <- aus_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_hex_ns.png", plot = aus_hex_ns, device = "png", dpi = 300,
  height = 9, width = 18)


############################################################################### 
#########################    Population density ns    #########################
############################################################################### 

aus_geo_ns <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_geo_ns.png", plot = aus_geo_ns, device = "png", dpi = 300,
  height = 12, width = 12)


aus_hex_ns <- aus_hex_sa3 %>%
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_hex_ns.png", plot = aus_hex_ns, device = "png", dpi = 300,
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
  ggplot() + geom_density(aes(x = ns)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn")
ggsave(filename = "figures/pop/density.png", plot = tas_ns, device = "png", dpi = 300,
  height = 6, width = 6)


spop_plot <- tas_geo_sa3 %>%
  group_by(iteration) %>% 
  mutate(mean_value  = mean(ns)) %>% 
  ggplot() + 
  geom_density(aes(x= ns, fill = groups), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_value), colour = "black") + 
  scale_fill_manual(values = ns_alpha)
spop_plot

tas_ns <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = ns)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~iteration)
ggsave(filename = "figures/pop/tas_ns.png", plot = tas_ns, device = "png", dpi = 300,
  height = 6, width = 6)


hex_ns <- tas_hex_sa3 %>% 
  ggplot() + geom_sf(aes(fill = ns)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~iteration)
ggsave(filename = "figures/pop/hex_ns.png", plot = hex_ns, device = "png", dpi = 300,
  height = 6, width = 6)

gridExtra::grid.arrange(tas_ns, hex_ns)


###############################################################################
##########################    Population density    ###########################

# Add Tasmania population density to null model

tas_ns <- ggplot(tas_geo_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas_ns.png", plot = tas_ns, dpi=300, device = "png", width = 12, height = 6)

hex_ns <- ggplot(tas_hex_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/hex_ns.png", plot = hex_ns, dpi=300, device = "png", width = 12, height = 6)



# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model

tas_ns <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/ns_tas.png", 
  plot = tas_ns, dpi=300, device = "png", width = 12, height = 6)

hex_ns <- tas_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/ns_hex.png", 
  plot = hex_ns, dpi=300, device = "png", width = 12, height = 6)
