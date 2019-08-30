# Create maps for pilot study
# This script considers the sa3 areas of Australia.
# It creates lineups by simulating a null model spatial for 20 plots,
# It then adds a data with a spatial trend to a randomly selected plot 

# libraries
library(gstat)
library(purrr)
library(sp)
library(sf)
library(sugarbag)
library(tidyverse)
library(viridis)

set.seed(19941030)

###########################################################
####################     DATA    ##########################
# Geography of sa3 areas
sa3 <- absmapsdata::sa32016
sa3 <- st_simplify(sa3, preserveTopology = TRUE, dTolerance = 0.001)

# filter out Islands
Islands <- c("Cocos (Keeling) Islands",	"Christmas Island", "Norfolk Island", "Lord Howe Island")
sa3 <- sa3 %>% 
  filter(!is.na(cent_long)) %>% 
  filter(!(sa3_name_2016 %in% Islands))

# plot with area sqkm
ggplot(sa3) + geom_sf(aes(fill = areasqkm_2016))

###########################################################
# Create hexagon map of sa3
# Centroids
sa3_centroids <- sa3 %>% 
  select(sa3_name_2016, longitude = cent_long, latitude = cent_lat) %>% 
  sf::st_drop_geometry() %>% filter(!is.na(longitude))

# Create hexagon location grid
grid <- create_grid(centroids = sa3_centroids, hex_size = 0.45, buffer_dist = 2)

# Allocate polygon centroids to hexagon grid points
allocated <- allocate(
  centroids = sa3_centroids,
  sf_id = "sa3_name_2016",
  hex_grid = grid,
  hex_size = 0.45, # same size used in create_grid
  hex_filter = 10,
  width = 30,
  focal_points = capital_cities,
  verbose = TRUE
)

# same column used in create_centroids
hexagons <- fortify_hexagon(data = allocated, sf_id = "sa3_name_2016", hex_size = 0.45)

# Convert hexagons to polygons for plotting
# This will order the areas by the sf_id, this results in alphabetical order
hexagons_sf <- hexagons %>% 
  select(sa3_name_2016, sa3_code_2016, long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(sa3_code_2016) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

# Remove geometry of sa3 geographic areas
sa3_ng <- sf::st_drop_geometry(sa3)

hex <- sa3 %>% 
  # ensure correct order by ordering alphabetically
  arrange(sa3_name_2016) %>% 
  mutate(geometry = hexagons_sf$geometry)

# Plot hexagons
ggplot(hex) + geom_sf(aes(fill = areasqkm_2016), colour = NA)


###########################################################
# Apply a Spatial Relationships z~1 constant errors ( for all null plots )
# Convert to SPDF using coordinates for spatial dependency model
coordinates(sa3_ng) <- ~ cent_long + cent_lat

# Change this parameter to change strength of spatial dependency
cov.Decay <- 0.5
# Specify the spatial model
var.g.dummy <- gstat(formula=z~1, 
  locations=~x+y, 
  dummy=T, beta=1,
  model=vgm(psill=1, model="Gau", range=cov.Decay),
  nmax=20)
var.sim <- predict(var.g.dummy, newdata=sa3_ng, nsim=1)

# pair coordinates with the simulated spatially dependent null model
var_sf <- as_tibble(var.sim)

ggplot(var_sf, aes(x=cent_long, y=cent_lat, colour = sim1)) + 
  geom_point() +
  scale_fill_viridis_c()


###########################################################
# Create a set of spatial trend data relationships

# Manual null data creation
# Change this parameter to change strength of spatial dependency
cov.Decay <- 0.5
# Specify the spatial model
var.g.dummy <- gstat(formula=z~1, 
  locations=~longitude+latitude, 
  dummy=T, beta=1,
  model=vgm(psill=1, model="Gau", range=cov.Decay),
  nmax=20)

# Create underlying spatially dependent data for 20 null plots
var.sim <- predict(var.g.dummy, newdata=sa3_centroids, nsim=16)

# add trend models to simulated errors
sa3_centroids$size <- (sa3$areasqkm_2016)/10000

sa3_centroids <- sa3_centroids %>% 
  as_tibble %>% 
  mutate(ns = latitude/10, # North to south trend
    ns = (-longitude + latitude)/10) # North west to south east trend


###########################################################
# Create data sets for plotting
# Add spatial relationship to geography and hexagons
# Match to sa3 areas
var.sim <- left_join(sa3_centroids, as_tibble(var.sim), 
by = c("longitude","latitude")) 

# Join simulations and true data models 
sa3_sims <- left_join(sa3, as_tibble(var.sim))
hex_sims <- left_join(hex, as_tibble(var.sim))


###############################################################################
######################    NORTH WEST TO SOUTH EAST    #########################

sf_ns <- ggplot() + 
  geom_sf(data = sa3_sims, aes(fill = ns), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/geo_sa3_ns.png", 
  plot = sf_ns, dpi=300, device = "png", width = 12, height = 6)

hex_ns <- ggplot() + 
  geom_sf(data = hex_sims, aes(fill = ns), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/hex_sa3_ns.png", 
  plot = hex_ns, dpi=300, device = "png", width = 12, height = 6)

gridExtra::grid.arrange(sf_ns, hex_ns, nrow=1)


# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

# Create data set to plot
# all plots will be null plots except the one with additional true trend model
sa3_long <- sa3_sims %>% 
  mutate(true = ns) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = sa3_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_ns_geo.png", dpi=300, device = "png", width = 12, height = 6)


# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model
hex_long <- hex_sims %>% 
  mutate(true = ns) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = hex_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_ns_hex.png", dpi=300, device = "png", width = 12, height = 6)

###############################################################################
######################         NORTH TO SOUTH         #########################
# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

# Create data set to plot
# all plots will be null plots except the one with additional true trend model
sa3_long <- sa3_sims %>% 
  mutate(true = ns) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = sa3_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_ns_geo.png", dpi=300, device = "png", width = 12, height = 6)


# Line up hexagons (sf) plot
# all plots will be null plots except the one with additional true trend model
hex_long <- hex_sims %>% 
  mutate(true = ns) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = hex_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_ns_hex.png", dpi=300, device = "png", width = 12, height = 6)



###############################################################################
######################              SIZE              #########################

sf_size <- ggplot() + 
  geom_sf(data = sa3_sims, aes(fill = size), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/geo_sa3_size.png", 
  plot = sf_size, dpi=300, device = "png", width = 12, height = 6)

hex_size <- ggplot() + 
  geom_sf(data = hex_sims, aes(fill = size), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/hex_sa3_size.png", 
  plot = hex_size, dpi=300, device = "png", width = 12, height = 6)

gridExtra::grid.arrange(sf_size, hex_size, nrow=1)


# Choose a location for the true data in the plot
pos <- sample(1:16, 1)

# Create data set to plot
# all plots will be null plots except the one with additional true trend model
sa3_long <- sa3_sims %>% 
  mutate(true = size) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = sa3_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_size_geo.png", dpi=300, device = "png", width = 12, height = 6)


# Line up hexagosize (sf) plot
# all plots will be null plots except the one with additional true trend model
hex_long <- hex_sims %>% 
  mutate(true = size) %>% 
  gather("simulation", "value", starts_with("sim")) %>% 
  mutate(simulation = as.numeric(gsub("sim", "", simulation))) %>% 
  # add the trend model to the null data plot
  # scale the null data around the mean of the data
  mutate(value = ifelse(simulation == pos, true + value, (mean(true) + value*sd(true))))

ggplot() + 
  geom_sf(data = hex_long, aes(fill = value), colour = NA) +
  scale_fill_distiller(type = "div", palette = "BrBG") + 
  facet_wrap(~ simulation) + theme_minimal()
ggsave(filename = "figures/lineups/sa3_size_hex.png", dpi=300, device = "png", width = 12, height = 6)
