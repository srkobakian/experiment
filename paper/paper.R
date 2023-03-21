## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE, 
  message = FALSE,
  cache = FALSE,
  dpi = 300,
  out.width = "100%")


## ----libraries----------------------------------------------
# Load Libraries
library(tidyverse)
library(readxl)
library(broom)
library(broom.mixed)
library(cowplot)
library(png)
library(grid)
library(lme4)
library(ggthemes)
library(RColorBrewer)
library(knitr)
library(kableExtra)

options(knitr.kable.digits = "2")


## ----data---------------------------------------------------
trend_colors <- c(
  "NW-SE" = "#B2DF8A",
  "Three Cities" = "#A6CEE3",
  "All Cities" = "#1F78B4")
  
type_colors <- c(
  "Choro." = "#fcae91",
  "Hex." = "#a50f15")

detect_f_colors <- c(
  "No" = "#66C2A5",
  "Yes" = "#FC8D62")

detect_colors <- c(
  "Detected? No" = "#66C2A5",
  "Detected? Yes" = "#FC8D62")

  # Downloaded data
d <- read_xlsx("data/experiment-export.xlsx", sheet=2) %>%
  filter(!is.na(contributor)) %>%
  mutate(contributor = factor(contributor))

# Check data set 
# Need to clean multiple entries, 48, 24
# remove duplicated entries due to submit button
d <- d %>% group_by(group, contributor, image_name) %>%
  slice(1) %>% ungroup() %>% 
  arrange(group, contributor, plot_order)

# Remove contributors who did not provide answers to most questions
keep <- d %>% count(contributor, sort = TRUE) %>% filter(n > 10)
d <- d %>% 
  filter(contributor %in% keep$contributor) %>%
  filter(contributor != "1234567890")

# Remove contributors who did not provide any choices
# Or an insufficient amount of responses
bad_contribs <- d %>% group_by(contributor) %>% 
  summarise(sum0 = sum(choice)) %>% 
  filter(sum0 < 13) %>% 
  pull(contributor)

d <- d %>% 
  filter(!(contributor %in% bad_contribs))


n_contributors <- d %>% count(contributor, sort=TRUE) %>% 
  summarise(n_contributors = length(contributor))

d <- d %>% mutate(certainty = factor(as.character(certainty),
  levels = c("1", "2", "3", "4","5"), ordered=TRUE))


## ----reps---------------------------------------------------
replicate <- tibble(image_name = c("aus_cities_12_geo.png", "aus_cities_12_hex.png", 
                                   "aus_cities_3_geo.png", "aus_cities_3_hex.png",
                                   "aus_cities_4_geo.png", "aus_cities_4_hex.png",
                                   "aus_cities_9_geo.png", "aus_cities_9_hex.png",
                                   "aus_nwse_2_geo.png", "aus_nwse_2_hex.png",
                                   "aus_nwse_3_geo.png", "aus_nwse_3_hex.png",
                                   "aus_nwse_5_geo.png", "aus_nwse_5_hex.png",
                                   "aus_nwse_6_geo.png", "aus_nwse_6_hex.png",
                                   "aus_three_12_geo.png", "aus_three_12_hex.png",
                                   "aus_three_5_geo.png", "aus_three_5_hex.png",
                                   "aus_three_8_geo.png", "aus_three_8_hex.png",
                                   "aus_three_9_geo.png", "aus_three_9_hex.png"),
                    replicate = c(1, 1, 2, 2, 3, 3, 4, 4, 
                                  1, 1, 2, 2, 3, 3, 4, 4,
                                  1, 1, 2, 2, 3, 3, 4, 4))
# Add rep info to data
d <- d %>% left_join(., replicate, by = "image_name")


## ----pdetection_group---------------------------------------
# Tidy for analysis
d <- d %>% 
  separate(image_name, c("nothing", "trend", "location", "type", "extra"), remove = FALSE) %>%
  select(-nothing, -extra) %>%
  mutate(location = as.numeric(location), 
    # detect measures the accuracy of the choice
         detect = ifelse(location == choice, 1, 0)) %>% 
  mutate(trend = case_when(
    trend == "nwse" ~ "NW-SE",
    trend == "cities" ~ "All Cities",
    trend == "three" ~ "Three Cities")) %>% 
  mutate(trend = fct_relevel(trend,"All Cities","Three Cities", "NW-SE")) %>% 
  mutate(type = case_when(
    type == "hex" ~"Hex.",
    TRUE~"Choro.")) %>% 
    mutate(detect_f = factor(detect, levels = c(0,1), labels = c("Detected? No", "Detected? Yes")))

plots <- d %>% group_by(group, trend, type, location) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) 


## ----makethyroidplots---------------------------------------
library(cartogram)
library(sugarbag)
library(sf)

# Spatial polygons files sourced from: https://github.com/wfmackey/absmapsdata
load("data/sa22011.rda")
load("data/state2011.rda")

invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "black", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    text = element_text(colour = "white"),
    axis.text = element_blank()
  )

# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 <  0.74 ~ "#33809d",
    sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
    sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
    sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
    sir_p50 >= 1.45 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}

sa2 <- sa22011 %>% 
  filter(!st_is_empty(geometry)) %>% 
  filter(!state_name_2011 == "Other Territories") %>% 
  filter(!sa2_name_2011 == "Lord Howe Island")

SIR <- read_csv("data/SIR Downloadable Data.csv") %>% 
  filter(SA2_name %in% sa2$sa2_name_2011) %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Thyroid", Sex_name == "Females")
ERP <- read_csv("data/ERP.csv") %>%
  filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) %>% 
  dplyr::select(Region, Value)
# Alternative maps
# Join with sa2 sf object
sa2thyroid_ERP <- SIR %>% 
  left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) %>%
  left_join(., ERP %>% 
              dplyr::select(Region, 
              Population = Value), by = c("sa2_name_2011"= "Region")) %>% 
  filter(!st_is_empty(geometry))
sa2thyroid_ERP <- sa2thyroid_ERP %>% 
  #filter(!is.na(Population)) %>% 
  filter(!sa2_name_2011 == "Lord Howe Island") %>% 
  mutate(SIR = map_chr(p50, aus_colours)) %>% 
  st_as_sf() 

# Make choropleth
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm

# Make hexmap
if (!file.exists("data/aus_hexmap.rda")) {

## Check the projection uses long and lat
## Create centroids set
sa2centroids <- sa2 %>% 
  create_centroids(., "sa2_name_2011")
## Create hexagon grid
# For speed, consider if the buffer distance beyond the centroids is necessary
grid <- create_grid(centroids = sa2centroids,
                    hex_size = 0.15,
                    buffer_dist = 4, verbose = TRUE)
## Allocate polygon centroids to hexagon grid points
# This is a time consuming process for a large data set
aus_hexmap <- allocate(
  centroids = sa2centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.15,
  hex_filter = 8,
  focal_points = capital_cities,
  width = 25,
  verbose = TRUE)
save(aus_hexmap, file = "data/aus_hexmap.rda") 
}

load("data/aus_hexmap.rda")

## Prepare to plot
fort_hex <- fortify_hexagon(data = aus_hexmap,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.3) %>% 
            left_join(sa2thyroid_ERP %>%
            select(sa2_name_2011, SIR, p50))
fort_aus <- sa2thyroid_ERP %>%
  fortify_sfc()
## Make a plot
aus_hexmap_plot <- ggplot() +
  geom_polygon(aes(x = long,  y = lat,  
        group = interaction(sa2_name_2011, polygon)),
               fill = "black",  colour = "darkgrey", 
               linewidth = 0.1, data = fort_aus) +
  geom_polygon(aes(x = long, y = lat, group = hex_id, 
                   fill = SIR), data = fort_hex) +
  scale_fill_identity() +
  invthm + coord_equal()


## ----liver, eval=FALSE, fig.height = 4, fig.width = 4, fig.cap = "A choropleth map of the smoothed average of liver cancer diagnoses for Australian males. The diverging colour scheme uses dark blue areas for much lower than average diagnoses, yellow areas with diagnoses around the Australian average, red shows diagnoses much higher than average. The hexagon tile map shows concentrations of higher than expected liver cancer rates in the cities of Melbourne and Sydney, which is not visible from the choropleth.", results = "asis"----
## ggdraw() +
##   draw_plot(rasterGrob(readPNG("figures/aus_liver_m.png")))


## ----liver-hex, eval=FALSE, fig.height = 4, fig.width = 4, fig.cap = "A hexagon tile map of the smoothed average of liver cancer diagnoses for Australian males. The diverging colour scheme uses dark blue areas for much lower than average diagnoses, yellow areas with diagnoses around the Australian average, red shows diagnoses much higher than average. The hexagon tile map shows concentrations of higher than expected liver cancer rates in the cities of Melbourne and Sydney, which is not visible from the choropleth.", results = "asis"----
## 
## ggdraw() +
##   draw_plot(rasterGrob(readPNG("figures/aus_liver_m_hex.png")))


## ----thyroid-choro, out.height = "30%", fig.height = 3, fig.width = 4, fig.align='center', fig.cap = "A choropleth map of thyroid incidence among females across the Statistical Areas of Australia at Level 2. Blue indicates lower than average and red indicates higher than average incidence. A cluster of high incidence is visible on the east coast."----
aus_ggchoro


## ----thyroid-hex, out.height = "30%", fig.height = 3, fig.width = 4, fig.align='center', fig.cap = "A hexagon tile map of female thyroid cancer incidence in Australia, the same data as shown in the choropleth map in Figure 1. The high incidence in several of the metropolitan regions (Brisbane, Sydney and Perth) can now be seen, along with numerous isolated spots."----
aus_hexmap_plot


## ----lineup, fig.cap = "This lineup of twelve hexagon tile map displays contains one map with a real population related structure. The rest are null plots that contain spatial correlation between neighbours.", fig.height = 8, fig.width = 8----
ggdraw() +
  draw_plot(rasterGrob(readPNG("figures/aus_cities_3_hex.png")))


## ----exp-design, results = "asis", fig.width = 8, fig.height = 8, fig.cap = "The experimental design used in the visual inference study."----
ggdraw(xlim = c(0,1), ylim = c(0,1)) + draw_plot(rasterGrob(png::readPNG("figures/experiment_design.png")))


## ----eval=FALSE, echo=FALSE---------------------------------
## var.g.dummy <- gstat(formula = z ~ 1,
##                      locations = ~ longitude + latitude,
##                      dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
##                      nmax = 12)


## ----detect-compare, fig.cap = "The detection rates achieved by participants are contrasted when viewing the four replicates of the three trend models. Each point shows the probability of detection for the lineup display, the facets separate the trend models hidden in the lineup. The points for the same data set shown in a choroleth or hexagon tile map display are linked to show the difference in the detection rate.", fig.height=5----
# Detectability rate for each lineup (image)
d_smry <- d %>% group_by(trend, type, replicate) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) %>%
  ungroup()

# Numerical summary
diffs <- d_smry %>% spread(type, pdetect) %>%
  mutate(dif = `Hex.` - `Choro.`)

# Plot summary
ggplot(d_smry, aes(x = type, y = pdetect, color = trend)) +
  geom_point(size = 2) +  
  geom_line(size = 1, aes(group = replicate)) +  
  facet_wrap(~trend) +
  scale_color_manual(values = trend_colors) +
  xlab("Type of areas visualized") +
  ylab("Detection rate") + 
  ylim(0,1) +
  guides(scale = "none")


## ----desc-stats, results = "asis"---------------------------
types <- c("Choro.", "", "Hex.", "")

d %>% group_by(type, trend) %>%
  summarise(m = as.character(round(mean(detect), 2)),
      std.dev = as.character(round(sd(detect), 2))) %>% 
  mutate(std.dev = ifelse(std.dev == 0.5, "(0.50)", paste0("(", std.dev, ")"))) %>%   mutate(m = ifelse(m == 0.4, "0.40", m)) %>% 
  gather(stat, value, m, std.dev) %>% 
  pivot_wider(names_from = "trend", values_from = "value") %>% 
  arrange(type) %>% ungroup() %>% 
  mutate(Type = types) %>% select(Type, `NW-SE`, `Three Cities`, `All Cities`) %>%
  knitr::kable(., format = "latex", align = "lccc", booktabs = TRUE, 
    linesep = c("", "\\addlinespace"),
    caption = "The mean and standard deviation of the rate of detection for each trend model, calculated for the choropleth and hexagon tile map displays.")


## ----detect-glmer1, results="asis"--------------------------
# Mixed effects models
glmer1 <- glmer(detect ~ type*trend + (1|contributor), 
              family = binomial, data = d)

glmer_terms <- c("Intercept", "Hex.", "Three Cities", "All Cities",
  "Hex:Three Cities", "Hex:All Cities")

detection_rates <- tidy(glmer1) %>%
  mutate(detection_rates = round(exp(estimate)/(1+exp(estimate)),2)) %>%
  select(term, estimate, detection_rates) %>% pull(detection_rates)

tidy(glmer1) %>%
  mutate_at(.vars = c("estimate", "std.error"), round, 2) %>% 
  mutate(p.value = round(p.value, digits=2)) %>% 
  rowwise() %>%
  mutate(sig = case_when(
  p.value <= 0.001 ~ "$^{***}$",
  p.value <= 0.01 ~  "$^{**}$",
  p.value <= 0.05 ~  "$^{*}$",
  p.value <= 0.01 ~  "$^{.}$",
  TRUE ~ "$^{ }$")) %>%
  ungroup() %>% 
  filter(!is.na(std.error)) %>%
  mutate(term = glmer_terms) %>% 
  select(Term = term, 
    Est. = estimate, 
    Sig. = sig,
    `Std. Error` = std.error, 
    `P val` = p.value) %>% 
  knitr::kable(format = "latex", escape = FALSE, align= "rrlrr", 
    booktabs = T, linesep = c("", "\\addlinespace", "", "\\addlinespace", ""), 
    caption = "The model output for the generalised linear mixed effect model for detection rate. This model considers the type of display, the trend model hidden in the data plot, and accounts for contributor performance.")


## ----beeswarm, fig.cap = "The distribution of the time taken (seconds) to submit a response for each combination of trend, whether the data plot was detected, and type of display, shown using horizontally jittered dotplots. The colored point indicates average time taken for each plot type. Although some participants take just a few seconds per evaluation, and some take as much as mcuh as 60 seconds, but there is very little difference in time taken between plot types.", fig.height=8----
# Di playing
s <- d %>% group_by(type, trend, detect_f) %>%
  summarise(m=median(time_taken), 
            q1=quantile(time_taken, 0.25), 
            q3=quantile(time_taken, 0.75))
library(ggbeeswarm)
ggplot() + 
  geom_quasirandom(data=d, aes(x=type, y=time_taken), alpha=0.9) + 
  #geom_hline(data=s, aes(yintercept=m, color=type)) +
  geom_point(data=s, aes(x=type, y=m, color=type), size=5, alpha=0.7) +
  #geom_errorbar(data=s, aes(x=type, ymin=q1, ymax=q3, color=type), width=0.3, size=2) +
  scale_color_brewer("", palette = "Dark2") +
  facet_grid(trend~detect_f) + 
  ylab("Time taken (seconds)") + xlab("") +
  theme(legend.position="bottom")


## ----certainty, fig.cap = "The amount of times each level of certainty was chosen by participants when viewing hexagon tile map or choropleth displays. Participants were more likely to choose a high certainty when considering a Choropleth map. The mid value of 3 was the default certainty, it was chosen most for the Hexagon tile map displays.", fig.height=5----
d <- d %>% 
  mutate(certainty = as_factor(certainty)) %>% 
  mutate(replicate_f = as_factor(replicate)) 
 
d %>% 
  mutate(Detected = factor(detect_f, 
    levels = c("Detected? Yes", "Detected? No"), 
    labels = c("Yes", "No"))) %>% 
ggplot(aes(x = certainty, fill = Detected)) +  
  scale_fill_manual(values = detect_f_colors) +
  geom_bar() + facet_grid(type ~ trend) +
  theme(legend.position = "bottom") + xlab("Level of certainty")


## ----reason, results = "asis"-------------------------------
# Qualitative analysis of reason
d %>% 
  mutate(reason = ifelse(reason =="0.0", "no reason", reason)) %>% 
  mutate(Detected = ifelse(detect_f == "Detected? Yes", "Yes", "No"),
    Trend = trend) %>% 
  group_by(Trend, Detected, type) %>% 
  count(reason) %>% 
  mutate(prop = round(n/sum(n), 2), r_prop = paste0(reason, ":", prop)) %>% 
  top_n(1, n) %>% summarise(reasons = paste(reason, collapse=", ")) %>% 
  pivot_wider(names_from = c("type"), values_from = c("reasons")) %>% 
  ungroup() %>% 
  knitr::kable(., format = "latex", booktabs = TRUE,
    linesep = c("", "\\addlinespace", "", "\\addlinespace",""),
    caption = "The amount of participants that selected each reason for their choice of plot when looking at each trend model shown in Choropleth and Hexagon Tile maps. The facets show whether or not the choice was correct.") %>% 
  #kable_styling(bootstrap_options = "hold_position") %>% 
  collapse_rows(., columns = 1)

