## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE, 
  message = FALSE,
  cache = FALSE,
  dpi = 300,
  out.width = "100%")


## ----libraries-------------------------------------------------------------
# Load Libraries
library(tidyverse)
library(readxl)
library(broom)
library(broom.mixed)
library(cowplot)
library(ggbeeswarm)
library(png)
library(grid)
library(lme4)
library(cartogram)
library(sugarbag)
library(sf)
library(ggstats)
library(emmeans)
library(ggthemes)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(patchwork)

options(knitr.kable.digits = "2")


## ----data------------------------------------------------------------------
trend_colors <- c(
  "NW-SE" = "#B2DF8A",
  "Three Cities" = "#A6CEE3",
  "All Cities" = "#1F78B4")
  
type_colors <- c(
  "Choro." = "#a8c0a8",#"#fcae91",
  "Hex." = "#BD852C")  #"#a50f15")

type2_colors <- c(
  "choropleth" = "#d95f02",#"#fcae91",
  "hexagon tile" = "#e7298a")  #"#a50f15")

detect_f_colors <- c(
  "No" = "#66C2A5",
  "Yes" = "#FC8D62")

detect_colors <- c(
  "Detected? No" = "#66C2A5",
  "Detected? Yes" = "#FC8D62")

  # Downloaded data
d <- read_xlsx("data/experiment-export.xlsx", sheet=2) |>
  filter(!is.na(contributor)) |>
  mutate(contributor = factor(contributor))

n_all_contributors <- d |> count(contributor, sort=TRUE) |> 
  summarise(n_contributors = length(contributor))
  
n_all_evals <- nrow(d)

# Check data set 
# Need to clean multiple entries, 48, 24
# remove duplicated entries due to submit button
d <- d |> group_by(group, contributor, image_name) |>
  slice(1) |> ungroup() |> 
  arrange(group, contributor, plot_order)

# Remove contributors who did not provide answers to most questions
keep <- d |> count(contributor, sort = TRUE) |> filter(n > 10)
d <- d |> 
  filter(contributor %in% keep$contributor) |>
  filter(contributor != "1234567890") # Testing id

# Remove contributors who did not provide any choices
# Or an insufficient amount of responses
bad_contribs <- d |> group_by(contributor) |> 
  #summarise(sum0 = sum(choice)) |> 
  summarise(n_zero = sum(if_else(choice == 0, 1, 0))) |>
  #filter(sum0 < 13) |> 
  filter(n_zero > 8) |>
  pull(contributor)

# Person is in list above
same_answer <- d |> group_by(contributor) |> 
  summarise(v = sd(choice))

# check range of choices
choice_range <- d |> group_by(contributor) |> summarise(m1 = min(choice), m2 = max(choice))

d <- d |> 
  filter(!(contributor %in% bad_contribs))

n_contributors <- d |> count(contributor, sort=TRUE) |> 
  summarise(n_contributors = length(contributor))

n_AB <- d |> count(contributor, group) |> count(group) 
n_gender <- d |> count(contributor, gender) |> count(gender)
n_educ <- d |> count(contributor, education) |> count(education)
n_age <- d |> count(contributor, age) |> count(age)
n_aus <- d |> count(contributor, australia) |> count(australia)

n_evals <- nrow(d)
  
d <- d |> mutate(certainty = factor(as.character(certainty),
  levels = c("1", "2", "3", "4","5"), ordered=TRUE))


## ----reps------------------------------------------------------------------
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
d <- d |> left_join(replicate, by = "image_name")


## ----pdetection_group------------------------------------------------------
# Tidy for analysis
d <- d |> 
  separate(image_name, c("nothing", "trend", "location", "type", "extra"), remove = FALSE) |>
  select(-nothing, -extra) |>
  mutate(location = as.numeric(location), 
    # detect measures the accuracy of the choice
         detect = ifelse(location == choice, 1, 0)) |> 
  mutate(trend = case_when(
    trend == "nwse" ~ "NW-SE",
    trend == "cities" ~ "All Cities",
    trend == "three" ~ "Three Cities")) |> 
  mutate(trend = fct_relevel(trend,"All Cities","Three Cities", "NW-SE")) |> 
  mutate(type = case_when(
    type == "hex" ~"Hex.",
    TRUE~"Choro.")) |> 
    mutate(detect_f = factor(detect, levels = c(1, 0), labels = c("Detected? Yes", "Detected? No")))

plots <- d |> group_by(group, trend, type, location) |>
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) 


## ----makethyroidplots------------------------------------------------------
# Spatial polygons files sourced from: https://github.com/wfmackey/absmapsdata
load("data/sa22011.rda")
load("data/state2011.rda")

invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "black", colour = NA), 
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
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

sa2 <- sa22011 |> 
  filter(!st_is_empty(geometry)) |> 
  filter(!state_name_2011 == "Other Territories") |> 
  filter(!sa2_name_2011 == "Lord Howe Island")

SIR <- read_csv("data/SIR Downloadable Data.csv") |> 
  filter(SA2_name %in% sa2$sa2_name_2011) |> 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) |> 
  filter(Cancer_name == "Thyroid", Sex_name == "Females")
ERP <- read_csv("data/ERP.csv") |>
  filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) |> 
  dplyr::select(Region, Value)
# Alternative maps
# Join with sa2 sf object
sa2thyroid_ERP <- left_join(sa2, SIR, by = c("sa2_name_2011" = "SA2_name")) |>
  left_join(ERP |> 
              dplyr::select(Region, 
              Population = Value), by = c("sa2_name_2011"= "Region")) |> 
  filter(!st_is_empty(geometry))
sa2thyroid_ERP <- sa2thyroid_ERP |> 
  #filter(!is.na(Population)) |> 
  filter(!sa2_name_2011 == "Lord Howe Island") |> 
  mutate(SIR = map_chr(p50, aus_colours)) |> 
  st_as_sf() 

# Make choropleth
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm +
  ggtitle("a. choropleth map")

# Make hexmap
if (!file.exists("data/aus_hexmap.rda")) {

## Check the projection uses long and lat
## Create centroids set
sa2centroids <- create_centroids(sa2, "sa2_name_2011")
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
                            hex_size = 0.3) |> 
            left_join(sa2thyroid_ERP |>
            select(sa2_name_2011, SIR, p50))
fort_aus <- sa2thyroid_ERP |>
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
  invthm + coord_equal() +
  ggtitle("b. hexagon tile map")


## ----liver, eval=FALSE, fig.height = 4, fig.width = 4, fig.cap = "A choropleth map of the smoothed average of liver cancer diagnoses for Australian males. The diverging colour scheme uses dark blue areas for much lower than average diagnoses, yellow areas with diagnoses around the Australian average, red shows diagnoses much higher than average. The hexagon tile map shows concentrations of higher than expected liver cancer rates in the cities of Melbourne and Sydney, which is not visible from the choropleth map.", results = "asis"----
# ggdraw() +
#   draw_plot(rasterGrob(readPNG("figures/aus_liver_m.png")))


## ----liver-hex, eval=FALSE, fig.height = 4, fig.width = 4, fig.cap = "A hexagon tile map of the smoothed average of liver cancer diagnoses for Australian males. The diverging colour scheme uses dark blue areas for much lower than average diagnoses, yellow areas with diagnoses around the Australian average, red shows diagnoses much higher than average. The hexagon tile map shows concentrations of higher than expected liver cancer rates in the cities of Melbourne and Sydney, which is not visible from the choropleth map.", results = "asis"----
# 
# ggdraw() +
#   draw_plot(rasterGrob(readPNG("figures/aus_liver_m_hex.png")))


## --------------------------------------------------------------------------
#| label: fig-thyroid
#| fig-cap: "Thyroid cancer incidence among females across the Statistical Areas of Australia at Level 2, displayed using a choropleth map (a) and a hexagon tile map (b). Blue indicates lower than average, and red indicates higher than average incidence. The choropleth map suggests high incidence is clustered on the east coast but misses the high incidence in Perth and a few locations in inner Melbourne visible in the hexagon tile map."
#| out-width: 100%
#| fig-width: 8
#| fig-height: 3
aus_ggchoro + aus_hexmap_plot + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: fig-lineup
#| fig-cap: "This lineup of twelve hexagon tile map displays contains one map with a real population related structure (location 3). The rest are null plots that contain only spatial dependence."
#| fig-height: 8
#| fig-width: 8
ggdraw() +
  draw_plot(rasterGrob(readPNG("figures/aus_cities_3_hex.png")))


## --------------------------------------------------------------------------
#| label: fig-exp-design
#| results: "asis" 
#| fig-width: 6 
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "The experimental design used in the study. Participants are allocated to group A or B, to evaluate either the choropleth map or hexagon tile map lineup of each simulated data set. The text 'loc' refers to the location of the data plot in the lineup."

expt_design <- d |>
  count(group, trend, location, type, replicate)

ggplot(expt_design, ) +
  geom_tile(aes(x=group, y=1, fill=type)) +
  geom_text(aes(x=group, y=1, label = paste("loc:", location))) + 
  facet_grid(replicate~trend, axes = "all", labeller = labeller(replicate = label_both, trend = label_both)) +
  ylab("") +
  scale_fill_manual("", values = type_colors) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())


## ----eval=FALSE, echo=FALSE------------------------------------------------
# # This is example code illustrating the null data generation
# var.g.dummy <- gstat(formula = z ~ 1,
#                      locations = ~ longitude + latitude,
#                      dummy = T, beta = 1, model = vgm(psill = 1, model = "Gau", range = 0.3),
#                      nmax = 12)
# 
# spatial_smoother <- function(area_number,
#                              values_vector,
#                              area_weight = 0.5,
#                              neighbours_list){
# 
#   stopifnot( area_weight >= 0 && area_weight <= 1)
#   # List of the neighbours
#   neighbours <- as.vector(neighbours_list[[area_number]])
#   # Remove the current area
#   neighbours <- neighbours[!neighbours == area_number]
#   if ((length(values_vector[neighbours]))==0){
#     smoothed_value <- values_vector[area_number]
#   } else {
#     # Weighted value: area value and neighbours
#     ave_of_neighbours <- mean(values_vector[neighbours], na.rm = TRUE)
#     smoothed_value <- (area_weight)*values_vector[area_number] + (1-area_weight)*ave_of_neighbours
#   }
#   return(smoothed_value)
# }
# 
# var.sim1 <- predict(var.g.dummy, newdata = sa3_centroids, nsim = 12) |>
#   left_join(sa3_centroids, ., by=c("longitude", "latitude"))
# 
# sa3_sims1_1 <- as_tibble(var.sim1) |>
#   mutate_at(sims, ~map_dbl(1:nrow(sa3), spatial_smoother,
#                            values_vector = ., area_weight = 0.5, neighbours_list = sa3_neighbours))
# 
# 


## --------------------------------------------------------------------------
#| label: fig-detect-compare
#| fig-cap: "The detection rates achieved by participants are contrasted when viewing the four replicates of the three trend models. Each point shows the probability of detection for the lineup display, the facets separate the trend models hidden in the lineup. The points for the same data set shown in a choroleth or hexagon tile map display are linked to show the difference in the detection rate."
#| fig-height: 4
#| fig-width: 6
#| out-width: 100%
# Detectability rate for each lineup (image)
d_smry <- d |> group_by(trend, type, replicate) |>
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) |>
  ungroup()

# Numerical summary
diffs <- d_smry |> spread(type, pdetect) |>
  mutate(dif = `Hex.` - `Choro.`)

# Plot summary
ggplot(d_smry, aes(x = type, y = pdetect, color = trend)) +
  geom_point(size = 2) +  
  geom_line(size = 1, aes(group = replicate)) +  
  facet_wrap(~trend) +
  scale_color_manual(values = trend_colors) +
  xlab("") +
  ylab("Detection rate") + 
  ylim(0,1) +
  guides(scale = "none") +
  theme(legend.position = "none")


## --------------------------------------------------------------------------
#| eval: false
# # Check detection rate across groups
# d |> group_by(group) |>
#     # pdetect measures the aggregated accuracy of the choices
#     summarise(pdetect = length(detect[detect == 1])/length(detect)) |>
#     ungroup()


## --------------------------------------------------------------------------
#| label: tbl-detect-glmer1
#| tbl-cap: "Parameter estimates of the fitted model fit for  detection rate. All terms are statistically significant  ($^{**}=0.01$, $^{***}=0.001$)."
# Mixed effects models
detect_fit <- glmer(detect ~ type*trend + (1|contributor), 
              family = binomial, data = d)

glmer_terms <- c("Intercept", "Hex.", "Three Cities", "All Cities",
  "Hex:Three Cities", "Hex:All Cities")

detection_rates <- tidy(detect_fit) |>
  mutate(detection_rates = round(exp(estimate)/(1+exp(estimate)),2)) |>
  select(term, estimate, detection_rates) |> pull(detection_rates)

detect_fit_smry <- summary(detect_fit)

tidy(detect_fit) |>
  mutate_at(.vars = c("estimate", "std.error"), round, 2) |> 
  mutate(p.value = round(p.value, digits=2)) |> 
  rowwise() |>
  mutate(sig = case_when(
  p.value <= 0.001 ~ "$^{***}$",
  p.value <= 0.01 ~  "$^{**}$",
  p.value <= 0.05 ~  "$^{*}$",
  p.value <= 0.01 ~  "$^{.}$",
  TRUE ~ "$^{ }$")) |>
  ungroup() |> 
  filter(!is.na(std.error)) |>
  mutate(term = glmer_terms) |> 
  select(Term = term, 
    Est. = estimate,
    `Std. Err.` = std.error, 
    `P-val.` = p.value, 
    Sig. = sig) |> 
  knitr::kable(format = "latex", escape = FALSE, align= "rrlrr", 
    booktabs = T, linesep = c("", "\\addlinespace", "", "\\addlinespace", ""))


## --------------------------------------------------------------------------
#| label: tbl-detect-prop
#| tbl-cap: "Model estimates for the proportion of detection in each of the trend models (standard error). Note that selecting the data plot by chance would produce a detection rate of 0.083, for each lineup."
# Proportions of detections, estimated from model
detect_props <- emmeans(detect_fit, specs = ~ type*trend, type = "response") |>
  as_tibble() |>
  select(type, trend, prob, SE) |>
  pivot_wider(names_from = trend, values_from = c(prob, SE)) |>
  pivot_longer(cols = `prob_All Cities`:`SE_NW-SE`, 
               names_to = "stat", values_to = "value") |>
  separate(stat, into = c("prop", "trend"), sep="_") |>
  pivot_wider(names_from = trend, values_from = value) 
detect_props <- detect_props |>
  mutate(`All Cities` = as.character(round(`All Cities`, 2)),
         `Three Cities` = as.character(round(`Three Cities`, 2)),
         `NW-SE` = as.character(round(`NW-SE`, 2))) |>
  mutate(type = as.character(type))
detect_props[2,1] <- ""
detect_props[4,1] <- ""
detect_props[2, 3] <- paste0("(", detect_props[2, 3], ")")
detect_props[4, 3] <- paste0("(", detect_props[4, 3], ")")
detect_props[2, 4] <- paste0("(", detect_props[2, 4], ")")
detect_props[4, 4] <- paste0("(", detect_props[4, 4], ")")
detect_props[2, 5] <- paste0("(", detect_props[2, 5], ")")
detect_props[4, 5] <- paste0("(", detect_props[4, 5], ")")
detect_props |>
  select(type, `All Cities`, 
                  `Three Cities`, `NW-SE`) |>
  knitr::kable(format = "latex", escape = FALSE, align= "lccc", 
    booktabs = T, 
    col.names = c("Map type", "All Cities", 
                  "Three Cities", "NW-SE"), 
    linesep = c("", "\\addlinespace", "", "\\addlinespace", ""))


## --------------------------------------------------------------------------
#| label: fig-beeswarm
#| fig-cap: "The distribution of the time taken (seconds) to submit a response for each combination of trend, whether the data plot was detected, and type of display, shown using a median value and horizontally jittered dotplots. There are only small differences in time taken between map types. Some participants take under a second per evaluation, and some take as much as 60 seconds, but this occurs with detection and non-detection."
#| fig-height: 4
#| fig-width: 6
#| out-width: 100%
s <- d |> group_by(type, trend, detect_f) |>
  summarise(m=median(time_taken), 
            q1=quantile(time_taken, 0.25), 
            q3=quantile(time_taken, 0.75),
            s=sd(time_taken),
            n=n()) |>
  mutate(lc = m - s*qt(0.975, n-1)/sqrt(n),
         uc = m + s*qt(0.975, n-1)/sqrt(n))
ggplot() + 
  geom_quasirandom(data=d, aes(x=type, y=time_taken, color=type), alpha=0.2) + 
  geom_point(data=s, aes(x=type, y=m, color=type), size=3) +
  scale_color_manual("", values = type_colors) +
  facet_grid(detect_f~trend) + 
  ylab("Time taken (seconds)") + xlab("") +
  theme_bw() +
  theme(legend.position="none")


## --------------------------------------------------------------------------
#| eval: false
# # Check groups and demographics
# d |> group_by(group) |>
#   summarise(m=mean(time_taken),
#             s=sd(time_taken))
# d |> group_by(gender) |>
#   summarise(m=mean(time_taken),
#             s=sd(time_taken))
# d |> group_by(age) |>
#   summarise(m=mean(time_taken),
#             s=sd(time_taken))
# d |> group_by(education) |>
#   summarise(m=mean(time_taken),
#             s=sd(time_taken))
# 


## --------------------------------------------------------------------------
# Check time difference between default choice
d_zeros <- d |> mutate(zeros = if_else(choice == 0, 0, 1)) |>
  group_by(zeros) |>   
  summarise(m=median(time_taken), 
            mn=min(time_taken),
            mx=max(time_taken),
            q1=quantile(time_taken, 0.25), 
            q3=quantile(time_taken, 0.75),
            n=n())

d_zeros_detect <- d |> mutate(zeros = if_else(choice == 0, 0, 1)) |>
  group_by(zeros, detect_f) |>   
  summarise(m=median(time_taken), 
            mn=min(time_taken),
            mx=max(time_taken),
            q1=quantile(time_taken, 0.25), 
            q3=quantile(time_taken, 0.75),
            n=n())
# Summarise across subjects
contributor_smry <- d |> group_by(contributor) |>   
  summarise(m_d=mean(detect), 
            s_d=sd(detect),
            m_t=mean(time_taken), 
            s_t=sd(time_taken)) 


## --------------------------------------------------------------------------
#| eval: false
# ggplot(contributor_smry, aes(m_t, m_d)) +
#   geom_point() +
#   ylab("Detection rate") +
#   xlab("Time taken") +
#   xlim(c(0, 45)) +
#   theme(aspect.ratio = 1)
# ggplot(contributor_smry, aes(m_t, s_t)) +
#   geom_point() +
#   xlab("Average") +
#   ylab("SD") +
#   theme(aspect.ratio = 1)
# ggplot(contributor_smry, aes(m_d, s_d)) +
#   geom_point() +
#   xlab("Average") +
#   ylab("SD") +
#   theme(aspect.ratio = 1)


## --------------------------------------------------------------------------
#| label: fig-certainty 
#| fig-cap: "The distribution of certainty chosen by participants when viewing hexagon tile map or choropleth map displays, shown as centered bar plots, faceted by the trend model and whether the plot was detected or not. Participants tended to choose higher certainty when evaluating a choropleth map, on average, particularly when the data plot was not detected."
#| fig-height: 5
#| fig-width: 10
#| out-width: 100%
d <- d |> 
  mutate(certainty = as_factor(certainty)) |> 
  mutate(replicate_f = as_factor(replicate)) 
 
d_tbl <- d |> 
  mutate(Detected = factor(detect_f, 
    levels = c("Detected? Yes", "Detected? No"), 
    labels = c("Detected? Yes", "Detected? No"))) |>
  mutate(type = fct_recode(type, "choropleth" = "Choro.", "hexagon tile" = "Hex.")) |>
  mutate(certainty_f = fct_recode(certainty, "Very uncertain"="1", 
                                "Uncertain"="2", 
                                "Neutral"="3",
                                "Certain"="4",
                                "Very certain"="5")) |>
  select(contributor, replicate, certainty_f, type, Detected, trend) |>
  pivot_wider(names_from=type, values_from=certainty_f)
gglikert(d_tbl, include=choropleth:`hexagon tile`,
         facet_rows=vars(Detected), facet_cols=vars(trend),
         add_labels=FALSE, add_totals=FALSE) +
  scale_fill_brewer("", palette="PiYG")


## --------------------------------------------------------------------------
#| label: tbl-reason
#| tbl-cap: "Proportion of reasons provided by participants for their plot choice, broken down by Trend, Map Type, and data plot detection. The primary reason when participants were evaluating the choropleth map was 'consistent' or 'trend', but for the hexagon tile map it was 'clusters', when they detected the data plot."
# Qualitative analysis of reason
reasons_smry <- d |> 
  mutate(reason = ifelse(reason =="0.0", "no reason", reason)) |> 
  mutate(Detected = ifelse(detect_f == "Detected? Yes", 
                           "Yes", "No"),
    Trend = trend) |> 
  group_by(Trend, Detected, type) |> 
  count(reason) |>
  ungroup() |>
  group_by(Trend, Detected, type) |>
  mutate(prop = n/sum(n)) |> 
  ungroup() |>
  select(Trend, Detected, type, reason, prop) |>
  mutate(prop = round(prop, 2)) |>
  pivot_wider(names_from = reason, 
              values_from = prop, 
              values_fill = 0) |>
  select(Trend, Detected, type, clusters, trend, consistent, hotspots, `no reason`)
  
reasons_smry |>  
  arrange(desc(Detected), Trend, type) |>
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("Trend", "Detect", "Type", 
                             "clusters", "trend", "consistent",
                             "hotspots", "none"),
                 align = "lllrrrrr",
    linesep = c("", "\\addlinespace", "", "\\addlinespace","", "\\addlinespace","", "\\addlinespace","", "\\addlinespace","")) 

