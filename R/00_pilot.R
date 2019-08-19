# Create maps for pilot study

# use absmapsdata for shape information
library(tidyverse)
sa3 <- absmapsdata::sa32016

# filter out Islands
sa3 <- sa3 %>% filter(cent_long > 109, cent_long < 155)

# plot with area sqkm
ggplot(sa3) + geom_sf(aes(fill = areasqkm_2016))


# experiment
library(sp)
library(gstat)
library(tidyverse)
library(viridis)

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

cov.Decay <- 0.2 # play with this parameter to change strength of spatial dependency
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
# error ranges from -2 to 5, so use a linear trend, north to south of 2
mysim$z2 <- mysim$y*2 + mysim$z + mysim$z*y
ggplot(mysim, aes(x=x, y=y, fill = z2)) + 
  geom_tile() +
  scale_fill_viridis()

# Play with trend parameter
# Also X+Y trend
mysim$zx2 <- mysim$y*2 + mysim$z + mysim$z*y*-x
ggplot(mysim, aes(x=x, y=y, fill = zx2)) + 
  geom_tile() +
  scale_fill_viridis()

# Also outlier model (a value different from neighbour)
# And a cluster model - for which you may need to look for an 
# approach for this, that will add k modes of similar values at 
# different locations

# Use a north to south trend of 2
# Match to sa3 areas

mysim_sf <- sf::st_as_sf(mysim, coords = c("x", "y"),  crs = 4283) 
mysim_ints <- sf::st_nearest_feature(sa3, mysim_sf)
sa3$point <- mysim_ints

sa3 <- left_join(sa3, mysim, by = c("point" = "id"))

ggplot() + 
  geom_tile(aes(x=x, y=y, fill = zx2),
    data = mysim) +
  scale_fill_viridis()

ggplot() + 
  geom_sf(data = sa3, aes(fill = zx2)) +
  scale_fill_viridis()

