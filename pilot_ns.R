
###############################################################################
######################         NORTH TO SOUTH         #########################

# Are areas located in the north more blue than areas in the south?

set.seed(2017)
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

#####################################################################

# add a north to south model
sa3_ns <- sa3_centroids %>% 
  mutate(ns = log(abs(latitude))) %>% 
  mutate(ns = scales::rescale(ns, to = c(sa3_mean,sa3_max)))

ggplot(sa3_ns) + geom_histogram(aes(ns))

### Start with shapes - geographies
aus_geo_ns <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ns)

### Start with shapes - hexagons
aus_hex_ns <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long) %>% 
  left_join(., sa3_ns)


  
############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot
pos <- 8

aus_geo_sa3_ns <- aus_geo_ns %>%
  mutate(true = ns) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(sa3_min, sa3_max)), 
    scales::rescale((value), c(sa3_min, sa3_max))))

pos <- 8

aus_hex_sa3_ns <- aus_hex_ns %>% 
  mutate(true = ns) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the new data around the distribution of null data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(sa3_min, sa3_max)), 
    scales::rescale((value), c(sa3_min, sa3_max))))


############################################################################### 
############################   Population ns       ############################
############################################################################### 

aus_geo_ns <- aus_geo_sa3_ns %>% 
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
ggsave(filename = "figures/ns/aus_geo_ns.pdf", plot = aus_geo_ns, device = "pdf", dpi = 300,
  height = 14, width = 18)


aus_hex_ns <- aus_hex_sa3_ns %>% 
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
ggsave(filename = "figures/ns/aus_hex_ns.pdf", plot = aus_hex_ns, device = "pdf", dpi = 300,
  height = 14, width = 18)
