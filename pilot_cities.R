
###############################################################################
######################          CITY DIFFERENCE       #########################

# Create and smooth 8 null data simulations: all cities different to rural areas


set.seed(2015)
var.g.dummy <- gstat(formula = z ~ 1, 
                     locations = ~ longitude + latitude, 
                     dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.5),
                     nmax = 12)

# Create underlying spatially dependent data for 12 null plots for 8 simulations
var.sim_cities <- map(1:8, function(seed){
  set.seed(seed)
  predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) %>% 
  left_join(sa3_centroids, ., by=c("longitude", "latitude"))})

# use an underlying spatial covariance model

sa3_cities_sims1 <- map(var.sim_cities, function(data){
  data %>% as_tibble %>% 
  mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                           values_vector = ., area_weight = 0.4, neighbours_list = sa3_neighbours))})

sa3_cities_sims2 <- map(sa3_cities_sims1, function(data){
  data %>% as_tibble %>% 
    mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                             values_vector = ., area_weight = 0.4, neighbours_list = sa3_neighbours))})

sa3_cities_sims3 <- map(sa3_cities_sims2, function(data){
  data %>% as_tibble %>% 
    mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother, 
                             values_vector = ., area_weight = 0.4, neighbours_list = sa3_neighbours))})


sa3_long_cities <- map(sa3_cities_sims3, function(data){
  data %>% select(-longitude, -latitude, -logsize) %>% 
  gather(key = "simulation", value = "value", -sa3_name_2016) %>%
  mutate(simulation = as.numeric(gsub("sim", "", simulation)))
  })

sims <- paste0("sim", 1:12)


# only use the least smoothed
sa3_min <- map(sa3_long_cities, function(data){data %>% pull(value) %>% min()})
sa3_max <- map(sa3_long_cities, function(data){data %>% pull(value) %>% max()})
sa3_mean <- map(sa3_long_cities, function(data){data %>% pull(value) %>% mean()})

#####################################################################

# increase the values of Brisbane, depending on distance from Brisbane city
# allocated: the data set containing the allocated hexagon centroid for each sa3
max_dist <- 1478314 # furthest area from any focal point

sa3_cities <- map2(sa3_mean, sa3_max, function(mean = .x, max = .y){
  allocated %>% 
  select(sa3_name_2016, longitude, latitude, points, focal_dist) %>% 
  mutate(city_distance = (max_dist - focal_dist)^8,
         dist = scales::rescale(city_distance,
                                     to = c(0,1)),
         cities = ifelse(dist < 0.9, NA, 
                         scales::rescale(city_distance,
                        to = c(mean, max))))
  })

############################################################################### 
## Allocate null data sets to be geography or hexagon 
############################################################################### 
set.seed(2015)
geo_list <- sample(1:8, 4)
hex_list <- seq(1:8)[-geo_list]

### Start with shapes - geographies
aus_geo_cities <- map(sa3_cities[geo_list], ~sa3 %>%
                        select(sa3_name_2016) %>% 
                        # Add the 16 simulated values for each area
                        left_join(., sa3_long, by = c("sa3_name_2016")) %>% 
                        left_join(., .x, by = c("sa3_name_2016")))

### Start with shapes - hexagons
aus_hex_cities <- map(sa3_cities[hex_list], ~hexagons_sf %>%
                        select(sa3_name_2016) %>% 
                        # Add the 16 simulated values for each area
                        left_join(., sa3_long, by = c("sa3_name_2016")) %>% 
                        left_join(., .x, by = c("sa3_name_2016")))


############################################################################### 

# Add the distribution to one of the null plots

add_data_plot <- function(num, data, data_pos, min, max){
  data[[num]] %>%
    mutate(true = cities) %>% 
    mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
    # add the spatial trend model to the null data plot
    # scale the null data around the mean of the data
    group_by(simulation) %>% 
    mutate(value = ifelse(simulation == data_pos[[num]], 
                          # for the true data plot, keep random values if not close enough to cities
                          ifelse(is.na(cities), value, true),
                          # for all others rescale to the same range
                          scales::rescale((value), c(min[[num]], max[[num]]))))}


# Choose a location for the true data in the plot
geo_pos <- c(9,9,4,4)
hex_pos <- c(12,12,3,3)

aus_geo_sa3_cities <- map(1:4, function(x) add_data_plot(num = x, aus_geo_cities, geo_pos, sa3_min, sa3_max))
aus_hex_sa3_cities <- map(1:4, function(x) add_data_plot(num = x, aus_hex_cities, hex_pos, sa3_min, sa3_max))

  
############################################################################### 
############################################################################### 

ggplot(map_dfr(aus_hex_sa3_cities, as_tibble)) + geom_histogram(aes(x=value)) + facet_wrap(~simulation)

############################################################################### 
############################################################################### 

geo_plot <- function(data, pos, plotnum){
  plota <- ggplot(data) + 
    geom_sf(aes(fill = value), colour = NA) + 
    scale_fill_distiller(type = "div", palette = "RdYlBu") + 
    facet_wrap(~ simulation) + theme_minimal() + guides(fill = FALSE) +
    theme(plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black", colour = NA),
          strip.background = element_rect(fill = "black", colour = NA),
          strip.text.x = element_text(colour = "white", size = 40),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  ggsave(filename = paste0("figures/cities/aus_geo_cities", plotnum ,"_", pos, ".pdf"), 
         plot = plota, device = "pdf", dpi = 300,
         height = 14, width = 18)
  
  return(plota)
}

geo_plots <- map2(aus_geo_sa3_cities, 1:4, ~geo_plot(.x, geo_pos[.y], .y))



hex_plot <- function(data, pos, plotnum){
  plota <- ggplot(data) + 
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
  
  
  ggsave(filename = paste0("figures/cities/aus_hex_cities", plotnum ,"_", pos, ".pdf"), 
         plot = plota, device = "pdf", dpi = 300,
         height = 14, width = 18)
  
  return(plota)
}

hex_plots <- map2(aus_hex_sa3_cities, 1:4, ~hex_plot(.x, hex_pos[.y], .y))

