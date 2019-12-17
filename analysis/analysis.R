###############################################################################
# Analysis of experiment data - by Di
###############################################################################

# Read Data 
#library(googlesheets)
#gs_ls()
#sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")

###############################################################################
invthm <- theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "white", size = 20),
  axis.text = element_text(colour = "white", size = 20)
)

# Load Libraries
library(tidyverse)
library(lubridate)
library(broom)
library(readxl)
library(lme4)
library(ggthemes)
# Downloaded data
d <- read_xlsx("experiment-export.xlsx", sheet=2) %>%
  filter(!is.na(contributor)) %>%
  mutate(contributor = factor(contributor))

###############################################################################
# Check data set 
d %>% count(contributor, sort=TRUE)
# Need to clean multiple entries, 48, 24
# remove duplicated entries due to submit button
d <- d %>% group_by(group, contributor, image_name) %>%
  slice(1) %>% ungroup() %>% 
  arrange(group, contributor, plot_order)
d %>% count(contributor, sort=TRUE)
d %>% count(contributor, sort=TRUE) %>% summarise(n_contributors = length(contributor))

# Now remove contributors who did not provide answers to most questions
keep <- d %>% count(contributor) %>% filter(n > 10)
d <- d %>% 
  filter(contributor %in% keep$contributor) %>%
  filter(contributor != "1234567890")

###############################################################################
# Augment
# add data replication number
d %>% count(image_name) %>% print(n=24)
# image code <--> rep
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
d %>% count(group, image_name, sort=TRUE)

# Double-check numbers
image_smry <- d %>% count(group, contributor, plot_order, sort = TRUE)
image_smry %>% 
  mutate(contributor = factor(contributor)) %>%
  ggplot() + 
  geom_col(aes(x = contributor, y = n, fill = plot_order)) + 
  coord_flip()

###############################################################################
# Tidy for analysis
d <- d %>% 
  separate(image_name, c("nothing", "trend", "location", "type"), remove=FALSE) %>%
  select(-nothing) %>%
  mutate(location = as.numeric(location), 
    # detect measures the accuracy of the choice
         detect = ifelse(location == choice, 1, 0)) %>% 
  mutate(trend = case_when(
    trend == "cities" ~ "all cities",
    trend == "three" ~ "three cities",
    trend == "nwse" ~ "NW-SE")) %>% 
  mutate(trend = fct_relevel(trend, "NW-SE","three cities","all cities")) %>% 
  mutate(type = case_when(
    type == "hex" ~"Hexagons",
    TRUE~"Geography"
  )) 

# Plot results - quick and dirty
# ggplot(d, aes(x=type, y=detect)) + geom_jitter() + facet_wrap(~trend)

# Check contributor performance
contribs <- d %>% group_by(group, contributor) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) 

contribs %>% count(group)
contribs %>% arrange(pdetect)

# Plot performance
contribs %>% ggplot(aes(x = group, y = pdetect, label = contributor)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1) +
  ylim(c(0,1))

# Create one record for each contributor, to examine demographics
dem_contribs <- d %>%
  group_by(contributor) %>%
  slice(1) %>% ungroup()


###############################################################################
# Demographics of contributors
dem_contribs %>% count(gender)
dem_contribs %>% count(age)
dem_contribs %>% count(education)
dem_contribs %>% count(australia)


###############################################################################
# Detectability rate for each lineup (image)
d_smry <- d %>% group_by(trend, type, location, replicate) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) %>%
  ungroup()

# Plot summary
ggplot(d_smry, aes(x=type, y=pdetect, colour = trend)) +
  geom_point(size = 3) +  
  geom_line(size = 1, aes(group = replicate)) +  
  facet_wrap(~trend) +  
  scale_colour_brewer(palette = "Paired") +
  xlab("Type of areas visualised") +
  ylab("Detection rate") + 
  ylim(0,1) + #invthm + 
  guides(colour = FALSE)

#ggsave(filename = "figures/pilot/replicate_change.png", plot = repl_plot, device = "png", dpi = 300, width = 12, height = 8, units = "in", bg = "transparent")
#ggsave(filename = "figures/pilot/replicate_change.png", plot = repl_plot, device = "png", dpi = 300, width = 12, height = 8, units = "in")

# Numerical summary
diffs <- d_smry %>% spread(type, pdetect) %>%
  mutate(dif = Hexagons - Geography)
# Need to do the t-tests for these

ggplot(diffs) + 
  geom_point(aes(Geography, Hexagons, colour = trend)) +
  geom_abline(slope = 1) +
  xlim(0,1) + ylim(0,1) + coord_equal()

# Probability of detection using map types
test_dt <- t.test(pdetect ~ type, data = d_smry, alternative = "less")

test_dt$p.value

# All contributors
contrib_smry <- d %>% 
  group_by(contributor, trend, type) %>%
  summarise(pdetect = length(detect[detect == 1])/length(detect)) %>%
  ungroup()

# Messy plot, but want to see how individuals fare
ggplot(contrib_smry, aes(x=type, y=pdetect, colour = trend)) +
  geom_line(aes(group = contributor), alpha=0.1) +
  facet_wrap(~trend) +  
  scale_colour_brewer(palette = "Paired") +
  geom_point(data=d_smry, aes(x=type, y=pdetect, colour = trend), size = 3) +
  geom_line(data=d_smry, size = 1, aes(group = replicate)) +
  guides(colour = FALSE)

###########################################################

d_time <- d %>% 
  group_by(trend, type, location, replicate) %>%
  summarise(m = mean(time_taken), s = sd(time_taken))
d_time %>% print(n=24)

# Visual summary
ggplot(d_time, aes(x=type, y=m, colour = trend)) +
  geom_point(size = 3) +  
  geom_line(size = 1, aes(group = replicate)) +  
  facet_wrap(~trend) +  
  scale_colour_brewer(palette = "Paired") +
  xlab("Type of areas visualised") +
  ylab("Average time taken") + 
  guides(colour = FALSE)

ggsave(filename = "figures/pilot/replicate_change_time.png", plot = repl_plot_t, device = "png", dpi = 300, width = 12, height = 8, units = "in", bg = "transparent")
#ggsave(filename = "figures/pilot/replicate_change.png", plot = repl_plot, device = "png", dpi = 300, width = 12, height = 8, units = "in")

###########################################################
# Need to check certainty next

ggplot(d, aes(x = trend, y = certainty)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + geom_jitter(height = 0.1, alpha = 0.4)
# certainty wasn't affected by the trend

ggplot(d %>% mutate(detect = as_factor(detect)), aes(x = detect, y = certainty)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + geom_jitter(height = 0.1, alpha = 0.4)
# certainty was slightly higher for correct detections





###########################################################
# Qualitative analysis of reason
d <- d %>% 
  mutate(reason = ifelse(reason =="0.0", "no reason", reason)) 

ggplot(d) + geom_bar(aes(x = reason)) +  
  facet_grid(trend ~ detect) +  
  scale_colour_brewer(palette = "Paired") +
  xlab("Type of areas visualised") + 
  guides(colour = FALSE) + coord_flip()



###########################################################
# Check each contributor performance on each type of plot









###########################################################
# Modelling 
glm1 <- glm(formula = detect ~ type * trend,
  family = binomial(link = "logit"), data = d)
glm1_d <- augment(glm1, d) %>% mutate(fitted = ifelse(.fitted>0.5, 1, 0))

###########################################################
# Mixed effects models
lmer1 <- lmer(detect ~ type*trend + (1|contributor), data = d)
lmer1 
glance(lmer1_d)
tidy(lmer1_d)
# Hexagon maps have better chance of correct detection
# Allowing for contributor effects to vary: 0.12 strong residual
