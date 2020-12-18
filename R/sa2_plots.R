#sa2_plots

#sa2_plots

# Create a choropleth of Australia
library(sugarbag)
library(tidyverse)
library(readxl)
library(spData)
library(ggthemes)
library(maptools)
library(sf)
library(cartogram)
library(cowplot)


# Filter for centroids only within longitudes
checkgeom <- function(x){
  xval <- st_coordinates(x)[[2]]
  
  if (xval < 109){return(FALSE)}
  if (xval > 155){return(FALSE)}
  return(TRUE)
}

sa2 <- absmapsdata::sa22011 %>% 
  filter(!st_is_empty(geometry)) %>%
  filter(checkgeom(x = geometry))

sa2 <- sa2 %>% 
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Join with cancer data from Australian Cancer Atlas

SIR <- read_csv("data/SIR Downloadable Data.csv") %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Lung", Sex_name == "Persons")
ERP <- read_csv("data/ABS_ERP_ASGS_26112019122253900.csv") %>%
  filter(REGIONTYPE == "SA2", Time == 2011, Sex == "Persons", Region %in% SIR$SA2_name) %>% 
  dplyr::select(Region, Value)


# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 < 0.5 ~ "#33809d",
    sir_p50 >= 0.5 & sir_p50 < 1 ~ "#aec6c7",
    sir_p50 >= 1 & sir_p50 < 1.5 ~ "#fff4bc",
    sir_p50 >= 1.5 & sir_p50 < 2 ~ "#ff9a64",
    sir_p50 >= 2 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}

# Join with sa2 sf object
sa2lung_ERP <- SIR %>% 
  left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) %>%
  left_join(., ERP %>% dplyr::select(Region, 
                                     Population = Value), by = c("sa2_name_2011"= "Region")) %>% 
  filter(!st_is_empty(geometry))


sa2lung_ERP <- sa2lung_ERP %>% 
  filter(!is.na(Population)) %>% 
  mutate(SIR = map_chr(p50, aus_colours)) %>% 
  st_as_sf() 

sa2lung_ERP <- st_transform(sa2lung_ERP, 3112)

b <- st_bbox(sa2lung_ERP)
b["xmin"]<- -2181807
b["xmax"]<- 1933081

sa2lung_ERP_fort <- fortify_sfc(sa2lung_ERP)

aus <- absmapsdata::state2016 %>% 
  filter(!(state_name_2016 == "Other Territories"))





###############################################################################

invthm <- theme_void() + theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "white", size = 20),
)

# Create a choropleth
aus_ggchoro <- ggplot(sa2lung_ERP) + 
  geom_sf(aes(fill = SIR)) +
  invthm + theme(legend.position ="bottom") +
  labs(fill = "SIR") + scale_fill_identity()
aus_ggchoro
ggsave(filename = "figures/aus_ggchoro.png", device = "png",
       bg = "transparent",  dpi = 300,  width = 7, height = 6)


aus_legend <- get_legend(aus_ggchoro)
save(aus_legend, file = "figures/aus_legend.rda")

###############################################################################
# Cartograms

# Contiguous Cartograms
cont <- sa2lung_ERP %>%
  cartogram_cont(.,
                 weight = "Population", itermax = 20) %>%
  st_as_sf()

save(cont, "data/auscont.rda")
load("data/auscont.rda")
aus_ggcont <-  ggplot(cont) + 
  geom_sf(aes(fill = SIR)) +
  invthm + theme(legend.position ="bottom") +
  labs(fill = "SIR") + scale_fill_identity() +
  invthm +  guides(fill=FALSE)
aus_ggcont
ggsave(filename = "figures/aus_ggcont.png", device = "png",  bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

sa2lung_ERP <- sa2lung_ERP %>% 
  mutate(sva = sqrt(as.numeric(Population)/as.numeric(albers_sqkm)))
sa2lung_ERP %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 7))

ncont <- cartogram_ncont(sa2lung_ERP, k = 1/5,
                         weight = "Population") %>% st_as_sf()
aus_ggncont <- ggplot(ncont) + 
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
  geom_sf(aes(fill = SIR), colour = NA) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim =
             c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  #scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  scale_size_identity() + 
  invthm +guides(fill=FALSE)
aus_ggncont

perth_ggncont <- ggplot(ncont %>% filter(gcc_name_2011 == "Greater Perth")) + 
  geom_rect(aes(xmin =-1736000, xmax = -1639000, ymin=-3824000, ymax=-3672300), fill = NA, colour = "black") +
  geom_sf(data= sa2lung_ERP %>% filter(gcc_name_2011 == "Greater Perth"),
          fill = NA, colour = "grey", size = 0.001) +
  geom_sf(aes(fill = SIR), colour = NA) + 
  scale_fill_identity() + invthm +
  guides(fill=FALSE)
perth_ggncont

library(cowplot)
full_ggncont <- ggdraw() + 
  draw_plot(aus_ggncont, 0, 0, 1, 1) +
  draw_plot(perth_ggncont, 0.33, 0.10, 0.25, 0.25)
full_ggncont
ggsave(filename = "figures/aus_ggncont.png", plot = full_ggncont,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- sa2lung_ERP %>%
  mutate(pop = (Population/max(Population))*10) %>% 
  cartogram_dorling(., k = 0.01, weight = "pop", m_weight = 1) %>% st_as_sf()
d <- st_bbox(dorl)
aus_ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = SIR)) + 
  scale_fill_identity()+
  invthm + guides(fill = FALSE)
aus_ggdorl
ggsave(filename = "figures/aus_ggdorl.png", device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
sa2lung_ERPmap <- st_transform(sa2lung_ERP, "+proj=longlat +datum=WGS84 +no_defs")
centroids <- create_centroids(sa2lung_ERPmap, "sa2_name_2011")
lung_neighbours <- st_intersects(sa2lung_ERPmap,sa2lung_ERPmap)

grid <- create_grid(centroids = centroids, 
                    hex_size = 0.25, buffer_dist = 11)
hexmap <- allocate(
  centroids = centroids, hex_grid = grid, 
  sf_id = "sa2_name_2011", hex_size = 0.25, hex_filter = 11, 
  focal_points = capital_cities, verbose = TRUE, width = 30
)

hexagons <- fortify_hexagon(hexmap, sf_id = "sa2_name_2011", hex_size = 0.25) %>% 
  left_join(st_drop_geometry(sa2lung_ERP))

hexagons_sf <- hexagons %>% 
  select(sa2_name_2011, long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(sa2_name_2011) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

# Remove geometry of sa2 geographic areas
sa2_ng <- sf::st_drop_geometry(sa2lung_ERP)

hex <- sa2lung_ERP %>% 
  # ensure correct order by ordering alphabetically
  arrange(sa2_name_2011) %>% 
  mutate(geometry = hexagons_sf$geometry)

aus_gghexmap <- ggplot(hex) + 
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.001) +
  geom_sf(aes(fill = SIR), colour = NA) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_identity() + invthm + guides(fill=FALSE)
aus_gghexmap
ggsave(filename = "figures/aus_gghexmap.png", plot = aus_gghexmap,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Aus grid

aus_grid <- gridExtra::grid.arrange(aus_ggcont, full_ggncont, aus_ggdorl, aus_gghexmap, nrow = 2)
ggsave(filename = "figures/aus_grid.png", plot = aus_grid,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)



aus_map <- ggplot() +
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
  geom_point(data = capital_cities, aes(x = longitude, latitude), colour = "white", size = 5) + 
  invthm
ggsave(filename = "figures/aus_capcities.png", plot = aus_map,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)



aus_points <- ggplot() +
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
  geom_point(data = capital_cities, aes(x = longitude, latitude), colour = "white", size = 5) +
  geom_point(data = centroids, aes(x = longitude, latitude), colour = "d69d6f", size = 0.1) + 
  invthm
ggsave(filename = "figures/aus_points.png", plot = aus_points,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)









#####
# Animate
library(gganimate)
sa2lung_ERP_order <- fortify_sfc(sa2lung_ERP) %>% 
  left_join(hexmap %>% dplyr::select(sa2_name_2016, focal_dist, rownumber))

ggplot(sa2lung_ERP_order) + geom_sf() + transition_reveal(rownumber)

anim <- ggplot(sa2lung_ERP_order, aes(long, lat, polygon)) +
  transition_layers(layer_length = 1, transition_length = 2) +
  enter_fade() + enter_grow()

anim_save(filename = "sa2_animated.gif", animation = anim, renderer =gifski_renderer())
