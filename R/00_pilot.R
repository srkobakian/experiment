# Create maps for pilot study


# experiment libraries
library(sp)
library(sf)
library(purrr)
library(gstat)
library(tidyverse)
library(viridis)

set.seed(19941030)

###########################################################
# Geography of sa3 areas
sa3 <- absmapsdata::sa32016

# filter out Islands
sa3 <- sa3 %>% filter(cent_long > 109, cent_long < 155)

# plot with area sqkm
ggplot(sa3) + geom_sf(aes(fill = areasqkm_2016))


###########################################################
# Create hexagon map of sa3
library(sugarbag)

# hexmap
# Centroids
sa3_centroids <- sa3 %>% 
  select(sa3_name_2016, longitude = cent_long, latitude = cent_lat) %>% 
  sf::st_drop_geometry() %>% filter(!is.na(longitude))

# Create hexagon location grid
grid <- create_grid(centroids = sa3_centroids, hex_size = 0.35, buffer_dist = 2)

# Allocate polygon centroids to hexagon grid points
allocated <- allocate(
  centroids = sa3_centroids,
  sf_id = "sa3_name_2016",
  hex_grid = grid,
  hex_size = 0.35, # same size used in create_grid
  hex_filter = 10,
  width = 30,
  focal_points = capital_cities,
  verbose = TRUE
)

# same column used in create_centroids
hexagons <- fortify_hexagon(data = allocated, sf_id = "sa3_name_2016", hex_size = 0.35)

# Convert hexagons to polygons for plotting
hexagons_sf <- hexagons %>% 
  select(sa3_name_2016, long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(sa3_name_2016) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

hexagons_plot <- sa3 %>% mutate(geometry = hexagons_sf$geometry)

# Plot hexagons
ggplot(hexagons_plot) + geom_sf(aes(fill = areasqkm_2016), colour = NA)

# generate spatial coordinates in a grid for Australia
x <- seq(min(sa3$cent_long, na.rm= TRUE), max(sa3$cent_long, na.rm= TRUE), 1)
y <- seq(min(sa3$cent_lat, na.rm= TRUE), max(sa3$cent_lat, na.rm= TRUE), 1)
ll <- expand.grid(x, y)

# create dataframe
spdf <- data.frame(id = 1:nrow(ll),
  x = ll$Var1,
  y = ll$Var2)

# convert to spatial points dataframe
coordinates(spdf) <- c("x", "y")
proj4string(spdf) <- CRS("+proj=longlat +datum=WGS84")

###########################################################

# Spatial Relationships z~1 constant errors
cov.Decay <- 0.5 # play with this parameter to change strength of spatial dependency
var.g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1,
  model=vgm(psill=1, model="Gau", range=cov.Decay),
  nmax=20)
var.sim <- predict(var.g.dummy, newdata=spdf, nsim=1)

mysim <- data.frame(id = 1:nrow(ll),
  x = ll$Var1,
  y = ll$Var2, 
  z = var.sim@data$sim1)

ggplot(mysim, aes(x=x, y=y, fill = z)) + 
  geom_tile() +
  scale_fill_viridis()


# Add a trend model
# error ranges from -2 to 5, so use a linear trend, north to south
# use + mysim$z*y to scale the errors for size of plot area
mysim$yz <- mysim$y + mysim$z
ggplot(mysim, aes(x=x, y=y, fill = yz)) + 
  geom_tile() +
  scale_fill_viridis()

# Play with trend parameter
# Also X+Y trend via + mysim$y*mysim$x
# use mysim$y*mysim$x*mysim$z to scale the errors for size of plot area
mysim$zx2 <- mysim$y*2 + mysim$x + mysim$y*mysim$z
ggplot(mysim, aes(x=x, y=y, fill = zx2)) + 
  geom_tile() +
  scale_fill_viridis()

# Also outlier model (a value different from neighbour)
# And a cluster model - for which you may need to look for an 
# approach for this, that will add k modes of similar values at 
# different locations

# Use a north to south trend of 2


###########################################################
# Create data sets for plotting
# Add spatial relationship to geography and hexagons
# Match to sa3 areas

mysim_sf <- sf::st_as_sf(mysim, coords = c("x", "y"),  crs = 4283) 
mysim_ints <- sf::st_nearest_feature(sa3, mysim_sf)

# Join sa3 areas to mysim
sa3$point <- mysim_ints
sa3 <- left_join(sa3, mysim, by = c("point" = "id"))

# Join hexagons to mysim
hexagons_plot$point <- mysim_ints
hexagons_plot <- left_join(hexagons_plot, mysim, by = c("point" = "id"))


tile_yz <- ggplot() + 
  geom_tile(aes(x=x, y=y, fill = yz),
    data = mysim) +
  scale_fill_viridis()
ggsave(filename = "figures/tile_sa3_yz.png", 
  plot = tile_yz, device = "png", width = 12, height = 6)

sf_yz <- ggplot() + 
  geom_sf(data = sa3, aes(fill = yz), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/geo_sa3_yz.png", 
  plot = sf_yz, device = "png", width = 12, height = 6)

hex_yz <- ggplot() + 
  geom_sf(data = hexagons_plot, aes(fill = yz), colour = NA) +
  scale_fill_viridis()
ggsave(filename = "figures/hex_sa3_yz.png", 
  plot = hex_yz, device = "png", width = 12, height = 6)


###
library(nullabor)

sa3_ng <- sf::st_drop_geometry(sa3)

null_d <- lineup(null_permute("yz"), sa3_ng)
#attr(null_d, "pos")

# Line up geography (sf) plots
null_sa3 <- null_d %>%
  arrange(sa3_name_2016) %>% 
    group_by(.sample) %>%
  mutate(sa3$geometry) %>% 
  sf::st_as_sf() %>% 
  ungroup()


ggplot() + 
  geom_point(data = null_sa3, aes(x = cent_long,
    y = cent_lat, colour = yz)) +
  scale_colour_viridis() + facet_wrap(~ .sample)
ggsave(filename = "figures/null_sa3_yz.png", plot = plot_yz, device = "png", width = 12, height = 6)

# Line up hexagons (sf) plot
null_hex <- null_d %>%
  arrange(sa3_name_2016) %>% 
  group_by(.sample) %>%
  mutate(hexagons_sf$geometry) %>% 
  sf::st_as_sf() %>% 
  ungroup()

ggplot() + 
  geom_sf(data = null_hex, aes(fill = yz), colour = NA) +
  scale_fill_viridis() + facet_wrap(~ .sample)
ggsave(filename = "figures/null_hex_yz.png", plot = plot_yz, device = "png", width = 12, height = 6)