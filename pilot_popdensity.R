###############################################################################
######################       POPULATION DENSITY       #########################

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
# add a population density model

# Use the Estimated Residential Population (ERP)
ERP <- read_csv("data/ERP.csv")
ERP_sa3_16 <- ERP %>% 
  filter(`Geography Level` == "Statistical Area Level 3") %>% 
  filter(Time == 2016) %>% select(sa3_name_2016 = Region, ERP = Value)

ERP_sa3_16 <- sa3 %>% st_drop_geometry() %>% select(sa3_name_2016, areasqkm_2016) %>% 
  left_join(., ERP_sa3_16)

ERP_sa3_16 <- ERP_sa3_16 %>% 
  mutate(pop_density = (ERP/areasqkm_2016))  %>% 
  mutate(pop_density = scales::rescale(pop_density, to = c(sa3_min,sa3_max)))

###########################################################
################# Smooth pop density ######################
###########################################################

# This distribution will be added to one of the null plots

sa3_smooth_pop <- as_tibble(ERP_sa3_16) %>% 
  mutate(s1_popdens16 = map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = pop_density, area_weight = 0.5, neighbours_list = sa3_neighbours)) %>% 
  mutate(s2_popdens16 =  map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = s1_popdens16, area_weight = 0.5, neighbours_list = sa3_neighbours)) %>% 
  mutate(s3_popdens16 =  map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = s2_popdens16, area_weight = 0.5, neighbours_list = sa3_neighbours)) %>% 
  mutate(s4_popdens16 =  map_dbl(1:nrow(sa3), spatial_smoother, 
    values_vector = s3_popdens16, area_weight = 0.5, neighbours_list = sa3_neighbours)) 


### Start with shapes - geographies
aus_geo_smooth_pop <- sa3 %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_smooth_pop) %>% 
  gather("groups", "smoother_density", contains("popdens"))

### Start with shapes - hexagons
aus_hex_smooth_pop <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 20 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_smooth_pop) %>% 
  gather("groups", "smoother_density", contains("popdens"))

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- sample(1:20, 1)

aus_geo_sa3 <- aus_geo_smooth_pop %>%
  mutate(true = smoother_density) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(sa3_min, sa3_max)), 
    scales::rescale((value), c(sa3_min, sa3_max))))

aus_hex_sa3 <- aus_hex_smooth_pop %>% 
  mutate(true = smoother_density) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(sa3_min, sa3_max)), 
    scales::rescale((value), c(sa3_min, sa3_max))))


############################################################################### 
############################ Population smoothing  ############################
############################################################################### 

aus_geo_smoothed <- aus_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(groups ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_geo_smoothed.png", plot = aus_geo_smoothed, device = "png", dpi = 300,
  height = 9, width = 18)


aus_hex_smoothed <- aus_hex_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_grid(groups ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/pop/aus_hex_smoothed.png", plot = aus_hex_smoothed, device = "png", dpi = 300,
  height = 9, width = 18)


############################################################################### 
######################### Population density smoothed #########################
############################################################################### 

aus_geo_popdens <- aus_geo_sa3 %>% 
  filter(groups == "smooth5") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/aus_geo_popdens.png", plot = aus_geo_popdens, device = "png", dpi = 300,
  height = 12, width = 12)


aus_hex_popdens <- aus_hex_sa3 %>%
  filter(groups == "smooth5") %>%  
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/aus_hex_popdens.png", plot = aus_hex_popdens, device = "png", dpi = 300,
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
  ggplot() + geom_density(aes(x = smoother_density)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn")+
ggsave(filename = "figures/pop/density.png", plot = tas_smoothed, device = "png", dpi = 300,
  height = 6, width = 6)

smoothed_alpha <- c(
  s1_popdens16 = "#7a0177",
  s2_popdens16 = "#c51b8a",
  s3_popdens16 = "#f768a1",
  s4_popdens16 = "#fbb4b9",
  s5_popdens16 = "#feebe2",
  s6_popdens16 = "#ffffff")         

spop_plot <- tas_geo_sa3 %>%
  mutate(groups = factor(groups, levels = c("s1_popdens16", "s2_popdens16", "s3_popdens16","s4_popdens16", "s5_popdens16", "s6_popdens16"))) %>% 
  group_by(groups, groups) %>% 
  mutate(mean_value  = mean(smoother_density)) %>% 
  ggplot() + 
  geom_density(aes(x= smoother_density, fill = groups), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_value), colour = "black") + 
  scale_fill_manual(values = smoothed_alpha)
spop_plot

tas_smoothed <- tas_geo_sa3 %>% 
  ggplot() + 
  geom_sf(aes(fill = smoother_density)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~groups)
ggsave(filename = "figures/pop/tas_smoothed.png", plot = tas_smoothed, device = "png", dpi = 300,
  height = 6, width = 6)


hex_smoothed <- tas_hex_sa3 %>% 
  ggplot() + geom_sf(aes(fill = smoother_density)) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") +
  facet_wrap(~groups)
ggsave(filename = "figures/pop/hex_smoothed.png", plot = hex_smoothed, device = "png", dpi = 300,
  height = 6, width = 6)

gridExtra::grid.arrange(tas_smoothed, hex_smoothed)


###############################################################################
##########################    Population density    ###########################

# Add Tasmania population density to null model

tas_popdens <- ggplot(tas_geo_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(groups ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/tas_popdensity.png", plot = tas_popdens, dpi=300, device = "png", width = 12, height = 6)

hex_popdens <- ggplot(tas_hex_sa3) + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_grid(groups ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/hex_popdensity.png", plot = hex_popdens, dpi=300, device = "png", width = 12, height = 6)



# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model

tas_popdens <- tas_geo_sa3 %>% 
  filter(groups == "smooth5") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/smoothed_tas_popdensity.png", plot = tas_popdens, dpi=300, device = "png", width = 12, height = 6)

hex_popdens <- tas_hex_sa3 %>% 
  filter(groups == "smooth5") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "Spectral") + 
  facet_wrap( ~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.background = element_rect(fill = "black", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
ggsave(filename = "figures/lineups/tas/smoothed_hex_popdensity.png", plot = hex_popdens, dpi=300, device = "png", width = 12, height = 6)
