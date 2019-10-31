
###############################################################################
######################         TRAINING PLOTS         #########################


###############################################################################
######################    NORTH WEST TO SOUTH EAST    #########################


set.seed(2017)
var.g.dummy <- gstat(formula = z ~ 1, 
                     locations = ~ longitude + latitude, 
                     dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
                     nmax = 3)

# Create underlying spatially dependent data for 12 null plots
var.sim <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 3) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))


sims <- colnames(var.sim)[5:length(colnames(var.sim))]

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
# add a north to south model
sa3_red <- sa3_centroids %>% 
  mutate(red = 4)


###############################################################################
######################         NORTH TO SOUTH         #########################

# use an underlying spatial covariance model
# add a north to south model

### Start with shapes - geographies
aus_geo_red <- sa3 %>% 
  select(sa3_name_2016) %>% 
  # Add the 20 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_red)

### Start with shapes - hexagons
aus_hex_red <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 20 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_red)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- 2

aus_geo_sa3_red <- aus_geo_red %>%
  mutate(true = red) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  group_by(simulation) %>% 
  mutate(value = ifelse(simulation == pos,
                        scales::rescale((value+true), c(sa3_mean, sa3_max)), 
                        scales::rescale((value), c(sa3_min, sa3_mean))))

pos <- 3
aus_hex_sa3_red <- aus_hex_red %>% 
  mutate(true = red) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  group_by(simulation) %>% 
  mutate(value = ifelse(simulation == pos,
                        scales::rescale((value+true), c(sa3_mean, sa3_max)), 
                        scales::rescale((value), c(sa3_min, sa3_mean))))

############################################################################### 
############################   Population red     ############################
############################################################################### 

aus_geo_red <- aus_geo_sa3_red %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black", colour = NA),
        strip.background = element_rect(fill = "black", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename = "figures/red/aus_geo_red.png", plot = aus_geo_red, device = "png", dpi = 300,
       height = 9, width = 18)


aus_hex_red <- aus_hex_sa3_red %>% 
  ggplot() + 
  geom_sf(aes(fill = value), colour = NA) + 
  scale_fill_distiller(type = "div", palette = "RdYlGn") + 
  facet_wrap(~ simulation) + theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black", colour = NA),
        strip.background = element_rect(fill = "black", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename = "figures/red/aus_hex_red.png", plot = aus_hex_red, device = "png", dpi = 300,
       height = 9, width = 18)

