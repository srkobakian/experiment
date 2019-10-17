
###############################################################################
######################         NORTH TO SOUTH         #########################

# First source the simulated points
# source('~/experiment/R/00_pilot.R', echo = TRUE)

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
sa3_ns <- sa3_centroids %>% 
  mutate(ns = abs(latitude-min(latitude))) %>% 
  mutate(ns = scales::rescale(ns, to = c(sa3_min,sa3_max)))

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
set.seed(19941030)
pos <- sample(1:12, 1)

aus_geo_sa3 <- aus_geo_ns %>%
  mutate(true = ns) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the spatial trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos,
    scales::rescale((value+true), c(sa3_min, sa3_max)), 
    scales::rescale((value), c(sa3_min, sa3_max))))

aus_hex_sa3 <- aus_hex_ns %>% 
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

aus_geo_ns <- aus_geo_sa3 %>% 
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
  height = 9, width = 18)


aus_hex_ns <- aus_hex_sa3 %>% 
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
ggsave(filename = "figures/ns/aus_hex_ns.pdf", plot = aus_hex_ns, device = "pdf", dpi = 300,
  height = 9, width = 18)
