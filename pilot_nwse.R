###############################################################################
######################  NORTH WEST - SOUTH EAST TREND   #######################

# Which map shows the most areas coloured red?

set.seed(2016)
var.g.dummy <- gstat(formula = z ~ 1, 
                     locations = ~ longitude + latitude, 
                     dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
                     nmax = 12)

# Create underlying spatially dependent data for 12 null plots
# use an underlying spatial covariance model
var.sim1 <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))
var.sim2 <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))
var.sim3 <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))
var.sim4 <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))

###############################################################################
######################          SMOOTH        #########################

sa3_sims1_1 <- as_tibble(var.sim1) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims1_2 <- sa3_sims1_1 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims1_3 <- sa3_sims1_2 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims1_4 <- sa3_sims1_3 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims1_5 <- sa3_sims1_4 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))


smoothing1 <- bind_rows(
  "smooth11" = sa3_sims1_1,
  "smooth12" = sa3_sims1_2, 
  "smooth13" = sa3_sims1_3,
  "smooth14" = sa3_sims1_4, 
  "smooth15" = sa3_sims1_5, .id = "groups")


sa3_long1 <- sa3_sims1_5 %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))
###############################################################################


sa3_sims2_1 <- as_tibble(var.sim2) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims2_2 <- sa3_sims2_1 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims2_3 <- sa3_sims2_2 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims2_4 <- sa3_sims2_3 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims2_5 <- sa3_sims2_4 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))


smoothing2 <- bind_rows(
  "smooth21" = sa3_sims2_1,
  "smooth22" = sa3_sims2_2, 
  "smooth23" = sa3_sims2_3,
  "smooth24" = sa3_sims2_4, 
  "smooth25" = sa3_sims2_5, .id = "groups")


sa3_long2 <- sa3_sims2_5 %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))
###############################################################################

sa3_sims3_1 <- as_tibble(var.sim3) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims3_2 <- sa3_sims3_1 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims3_3 <- sa3_sims3_2 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims3_4 <- sa3_sims3_3 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims3_5 <- sa3_sims3_4 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))


smoothing3 <- bind_rows(
  "smooth31" = sa3_sims3_1,
  "smooth32" = sa3_sims3_2, 
  "smooth33" = sa3_sims3_3,
  "smooth34" = sa3_sims3_4, 
  "smooth35" = sa3_sims3_5, .id = "groups")


sa3_long3 <- sa3_sims3_5 %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))
###############################################################################


sa3_sims4_1 <- as_tibble(var.sim4) %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims4_2 <- sa3_sims4_1 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims4_3 <- sa3_sims4_2 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims4_4 <- sa3_sims4_3 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
sa3_sims4_5 <- sa3_sims4_4 %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))


smoothing4 <- bind_rows(
  "smooth41" = sa3_sims4_1,
  "smooth42" = sa3_sims4_2, 
  "smooth43" = sa3_sims4_3,
  "smooth44" = sa3_sims4_4, 
  "smooth45" = sa3_sims4_5, .id = "groups")


sa3_long4 <- sa3_sims4_5 %>%
  select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))
#####################################################################


###############################################################################
######################           ADD TREND            #########################
###############################################################################
# only use the most smoothed
sa3_min1 <- sa3_long1 %>% pull(value) %>% min()
sa3_max1 <- sa3_long1 %>% pull(value) %>% max()
sa3_mean1 <- sa3_long1 %>% pull(value) %>% mean()


sa3_min2 <- sa3_long2  %>% pull(value) %>% min()
sa3_max2 <- sa3_long2  %>% pull(value) %>% max()
sa3_mean2 <- sa3_long2  %>% pull(value) %>% mean()

sa3_min3 <- sa3_long3  %>% pull(value) %>% min()
sa3_max3 <- sa3_long3  %>% pull(value) %>% max()
sa3_mean3 <- sa3_long3  %>% pull(value) %>% mean()

sa3_min4 <- sa3_long4  %>% pull(value) %>% min()
sa3_max4 <- sa3_long4  %>% pull(value) %>% max()
sa3_mean4 <- sa3_long4  %>% pull(value) %>% mean()

#####################################################################


# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_nwse <-  allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(lat = abs(latitude-min(latitude)),
         long = abs(longitude-min(longitude)),
         nwse = (-2*lat)+long) %>% 
  mutate(nwse = scales::rescale(nwse, to = c(-1, 3))) %>%
  select(-lat, -long)


### Start with shapes - geographies
aus_geo_nwse1 <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long1) %>% 
  left_join(., sa3_nwse)

aus_geo_nwse2 <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long2) %>% 
  left_join(., sa3_nwse)

aus_geo_nwse3 <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long3) %>% 
  left_join(., sa3_nwse)

aus_geo_nwse4 <- sa3 %>%
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long4) %>% 
  left_join(., sa3_nwse)

### Start with shapes - hexagons
aus_hex_nwse1 <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long1) %>% 
  left_join(., sa3_nwse)

aus_hex_nwse2 <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long2) %>% 
  left_join(., sa3_nwse)

aus_hex_nwse3 <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long3) %>% 
  left_join(., sa3_nwse)

aus_hex_nwse4 <- hexagons_sf %>% 
  select(sa3_name_2016) %>% 
  # Add the 16 simulated values for each area
  left_join(., sa3_long4) %>% 
  left_join(., sa3_nwse)

############################################################################### 

# Add the distribution will be added to one of the null plots

# Choose a location for the true data in the plot

geo_plot_nwse <- function(data, position, min, max) {
  aus_geo_nwse_plot <- data %>%
    mutate(true = nwse) %>% 
    mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
    # add the spatial trend model to the null data plot
    # scale the null data around the mean of the data
    group_by(simulation) %>% 
    mutate(value = ifelse(simulation == position, 
                          # for the true data plot, keep random values if not close enough to nwse
                          ifelse(is.na(nwse), value, true),
                          # for all others rescale to the same range
                          scales::rescale((value), c(min, max)))) %>%
    # Plot this data as a geographic map
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
  
  
  ggsave(filename = paste0("figures/final/aus_nwse_", position, "_geo.png"), 
         plot = aus_geo_nwse_plot,
         device = "png", dpi = 300,
         height = 14, width = 18)
  return(aus_geo_nwse_plot)
}


hex_plot_nwse <- function(data, position, min, max) {
  aus_hex_nwse_plot <- data %>%
    mutate(true = nwse) %>% 
    mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
    # add the spatial trend model to the null data plot
    # scale the null data around the mean of the data
    group_by(simulation) %>% 
    mutate(value = ifelse(simulation == position, 
                          # for the true data plot, keep random values if not close enough to nwse
                          ifelse(is.na(nwse), value, true),
                          # for all others rescale to the same range
                          scales::rescale((value), c(min, max)))) %>%
    # Plot this data as a geographic map
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
  
  ggsave(filename = paste0("figures/final/aus_nwse_", position, "_hex.png"), 
         plot = aus_hex_nwse_plot,
         device = "png", dpi = 300,
         height = 14, width = 18)
  return(aus_hex_nwse_plot)
}


nwse_positions <- c(5, 6, 3, 2)
min_list <- list(sa3_min1,sa3_min2,sa3_min3,sa3_min4)
max_list <- list(sa3_max1,sa3_max2,sa3_max3,sa3_max4)
geo_list <- list(aus_geo_nwse1, aus_geo_nwse2, aus_geo_nwse3, aus_geo_nwse4)
hex_list <- list(aus_hex_nwse1, aus_hex_nwse2, aus_hex_nwse3, aus_hex_nwse4)

geo_plot_nwse(aus_geo_nwse1, position = nwse_positions[[1]], min_list[[1]], max_list[[1]])
geo_plot_nwse(aus_geo_nwse2, position = nwse_positions[[2]], min_list[[2]], max_list[[2]])
geo_plot_nwse(aus_geo_nwse3, position = nwse_positions[[3]], min_list[[3]], max_list[[3]])
geo_plot_nwse(aus_geo_nwse4, position = nwse_positions[[4]], min_list[[4]], max_list[[4]])

hex_plot_nwse(aus_hex_nwse1, position = nwse_positions[[1]], min_list[[1]], max_list[[1]])
hex_plot_nwse(aus_hex_nwse2, position = nwse_positions[[2]], min_list[[2]], max_list[[2]])
hex_plot_nwse(aus_hex_nwse3, position = nwse_positions[[3]], min_list[[3]], max_list[[3]])
hex_plot_nwse(aus_hex_nwse4, position = nwse_positions[[4]], min_list[[4]], max_list[[4]])
