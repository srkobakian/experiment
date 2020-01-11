## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE, 
  message = FALSE)


## ----libraries------------------------------------------------------------------------------------------------------
# Load Libraries
library(tidyverse)
library(cowplot)
library(png)
library(grid)
library(lubridate)
library(broom)
library(readxl)
library(lme4)
library(ggthemes)
library(RColorBrewer)
library(visreg)
library(emmeans)
library(kableExtra)


## ----liver, fig.height = 9, fig.width = 6, fig.cap = "The smoothed average of liver cancer diagnoses for Australian males. The divering colour scheme shows dark blue areas with much lower than average diagnoses, yellow areas with diagnoses around the Australian average, orange with above average and red shows diagnoses much higher than average."----
ggdraw(ylim = c(0,2), xlim = c(0,1.3), clip = "on") +
  draw_plot(rasterGrob(readPNG("figures/aus_liver_m.png")), -0.15, 0.9, 1.6, 1.2) +
  draw_plot(rasterGrob(readPNG("figures/aus_liver_m_hex.png")), -0.15, -0.1, 1.6, 1.2) +
  draw_plot_label(c("a", "b"), 
    c(0.05, 0.05), c(1.95, 1), size = 20, color = "white") + 
  theme(plot.background = element_rect(fill = "black"))


## ----exp_design, fig.cap = "Experimental design."-------------------------------------------------------------------
count(d, trend, type) %>% mutate(Replicates = 2)

d %>% 
  mutate(Trend = trend) %>% 
  group_by(Trend, type, group) %>% 
  summarise(Reps = paste0(sort(unique(replicate)), collapse = ", ")) %>% 
  mutate(prop = round(n/sum(n), 3), r_prop = paste0(reason, ":", prop)) %>% 
  top_n(1, n) %>% summarise(reasons = paste(reason, collapse=", ")) %>% 
  pivot_wider(names_from = c("type"), values_from = c("reasons")) %>% 
  ungroup() %>% 
  knitr::kable(., format = "latex", booktabs = TRUE) %>% kable_styling("striped") %>% 
  collapse_rows(., columns = 1, custom_latex_hline = c(1:2, 3:4))


## ----cities_lineup, fig.cap = "The lineup of choropleth map displays shows a distribution that affects all capital cities. The values for the inner city areas in the capital cities result in them being coloured red. However, the colour blue dominates the display as the large rural areas are filled."----
#knitr::include_graphics("figures/aus_cities_3_geo.png")


## ----nwse_lineup, fig.cap = "A North-West to South East trend was hidden in this lineup of hexagon tile map displays."----
#knitr::include_graphics("figures/aus_nwse_6_hex.png")


## ----data-----------------------------------------------------------------------------------------------------------
trend_colours <- c(
  "NW-SE" = "#B2DF8A",
  "Three Cities" = "#A6CEE3",
  "All Cities" = "#1F78B4")
  
type_colours <- c(
  "Choro." = "#fcae91",
  "Hex." = "#a50f15")

detect_f_colours <- c(
  "No" = "#66C2A5",
  "Yes" = "#FC8D62")

detect_colours <- c(
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
bad_contribs <- d %>% group_by(contributor) %>% 
  summarise(sum0 = sum(choice)) %>% 
  filter(sum0 == 0) %>% 
  pull(contributor)

d <- d %>% 
  filter(!(contributor %in% bad_contribs))


n_contributors <- d %>% count(contributor, sort=TRUE) %>% 
  summarise(n_contributors = length(contributor))

d <- d %>% mutate(certainty = factor(as.character(certainty),
  levels = c("1", "2", "3", "4","5"), ordered=TRUE))


## ----reps-----------------------------------------------------------------------------------------------------------
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


## ----pdetection_group-----------------------------------------------------------------------------------------------
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
  mutate(trend = fct_relevel(trend, "NW-SE","Three Cities","All Cities")) %>% 
  mutate(type = case_when(
    type == "hex" ~"Hex.",
    TRUE~"Choro.")) %>% 
    mutate(detect_f = factor(detect, levels = c(0,1), labels = c("Detected? No", "Detected? Yes")))

plots <- d %>% group_by(group, trend, type, location) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) 


## ----detection_compare, fig.cap = "The detection rates achieved by participants are contrasted when viewing the four replicates of the three trend models. Each point shows the probability of detection for the lineup display, the facets separate the trend models hidden in the lineup. The points for the same data set shown in a choroleth or haxgon tile map display are linked to show the difference in the detection rate."----
# Detectability rate for each lineup (image)
d_smry <- d %>% group_by(trend, type, replicate) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) %>%
  ungroup()

# Plot summary
ggplot(d_smry, aes(x=type, y=pdetect, colour = trend)) +
  geom_point(size = 3) +  
  geom_line(size = 1, aes(group = replicate)) +  
  facet_wrap(~trend) +  
  scale_colour_manual(values = trend_colours) +
  xlab("Type of areas visualised") +
  ylab("Detection rate") + 
  ylim(0,1) +
  guides(colour = FALSE)


## ----ttest----------------------------------------------------------------------------------------------------------
# Numerical summary
diffs <- d_smry %>% spread(type, pdetect) %>%
  mutate(dif = `Hex.` - `Choro.`)

# Probability of detection using map types
test_dt <- t.test(pdetect ~ type, data = d_smry, alternative = "less")


## ----desc_stats, results = "asis"-----------------------------------------------------------------------------------
types <- c("Choro.", "", "Hex.", "")

d %>% group_by(type, trend) %>%
  summarise(m = round(mean(detect),3),
    std.dev = paste0("(", round(sd(detect),2),")")) %>% 
  gather(stat, value, m, std.dev) %>% 
  pivot_wider(names_from = "trend", values_from = "value") %>% 
  arrange(type) %>% ungroup() %>% 
  mutate(Type = types) %>% select(Type, `NW-SE`, `Three Cities`, `All Cities`) %>%
  knitr::kable(., format = "latex", align = "lccc", booktabs = TRUE)


## ----hist_height,  eval=FALSE, fig.cap = "Six histograms show the distribution of the time taken to submit a response for each combination of trend and type of display. The time taken to evaluate each display is broken into five second windows. The height of the histogram bars show how many evaulations were submitted within each time window. The orange regions of the bars show the amount of correct detections, and the green regions are the amount of incorrect detections."----
## ggplot(d %>% rename(Detected = detect_f),
##   aes(x = time_taken, fill = Detected, group = Detected)) +
##   scale_fill_manual(values = detect_colours) +
##   geom_histogram(binwidth = 5, boundary = 0) +
##   facet_grid(type ~ trend) +
##   theme(legend.position = "bottom") +
##   labs(x = "Time taken (Seconds)", y = "Amount of evaluations")


## ----hist_fill,  eval=FALSE, fig.cap = "The time taken to evaluate each display is broken into five second windows. The height of the histogram bars show the proportion of evaulations that were submitted within each time window."----
## ggplot(d %>% rename(Detected = detect_f),
##   aes(x = time_taken, fill = Detected, group = Detected)) +
##   scale_fill_manual(values = detect_colours) +
##   geom_histogram(binwidth = 5, boundary = 0, position = "fill") +
##   facet_grid(type ~ trend) +
##   labs(x = "Time taken (Seconds)", y = "Proportion of evaluations")
## 
## ggplot(d, aes(x = time_taken, colour = detect_f, group = detect_f)) +
##   scale_fill_manual(values = detect_colours) +
##   geom_freqpoly(binwidth = 3) +
##   facet_grid(type ~ trend) +
##   scale_fill_manual(values = detect_colours) +
##   labs(x = "Time taken (Seconds)", y = "Amount of evaluations") +
##   guides(colour = FALSE)


## ----beeswarm, fig.cap = "The beeswarm plots show the distribution of the time taken to submit a response for each combination of trend and type of display. The time taken shown by the height of the point for each evaluationq1. The height of the histogram bars show how many evaulations were submitted within each time window. The orange regions of the bars show the amount of correct detections, and the green regions are the amount of incorrect detections."----
# Di playing
s <- d %>% group_by(type, trend, detect) %>%
  summarise(m=median(time_taken), 
            q1=quantile(time_taken, 0.25), 
            q3=quantile(time_taken, 0.75))
library(ggbeeswarm)
ggplot() + 
  geom_quasirandom(data=d, aes(x=type, y=time_taken), alpha=0.9) + 
  #geom_hline(data=s, aes(yintercept=m, colour=type)) +
  geom_point(data=s, aes(x=type, y=m, colour=type), size=5, alpha=0.7) +
  #geom_errorbar(data=s, aes(x=type, ymin=q1, ymax=q3, colour=type), width=0.3, size=2) +
  scale_colour_brewer(palette = "Dark2") +
  facet_grid(trend~detect)


## ----certainty, fig.cap = "The amount of times each level of certainty was chosen by participants when viewing hexagon tile map or choropleth displays. Participants were more likely to choose a high certainty when considering a Choropleth map. The mid value of 3 was the default certainty, it was chosen most for the Hexagon tile map displays."----
d <- d %>% 
  mutate(certainty = as_factor(certainty)) %>% 
  mutate(replicate_f = as_factor(replicate)) 

ggplot(d %>% rename(Detected = detect_f), 
   aes(x = certainty, fill = Detected)) +  
  scale_fill_manual(values = detect_colours) +
  geom_bar() + facet_grid(type ~ trend) +
  theme(legend.position = "bottom")


## ----reason, fig.cap = "The amount of participants that selected each reason for their choice of plot when looking at each trend model shown in Choropleth and Hexagon Tile maps. The facets show whether or not the choice was correct."----

# Qualitative analysis of reason
d %>% 
  mutate(reason = ifelse(reason =="0.0", "no reason", reason)) %>% 
  mutate(Detected = ifelse(detect_f == "Detected? Yes", "Yes", "No"),
    Trend = trend) %>% 
  group_by(Trend, Detected, type) %>% 
  count(reason) %>% 
  mutate(prop = round(n/sum(n), 3), r_prop = paste0(reason, ":", prop)) %>% 
  top_n(1, n) %>% summarise(reasons = paste(reason, collapse=", ")) %>% 
  pivot_wider(names_from = c("type"), values_from = c("reasons")) %>% 
  ungroup() %>% 
  knitr::kable(., format = "latex", booktabs = TRUE) %>% kable_styling("striped") %>% 
  collapse_rows(., columns = 1, custom_latex_hline = c(1:2, 3:4))


## ----contributors, fig.cap = "The probablity of detection acheived by the contributors in each group is shown by the points. Group B has a larger range and a smaller inter-quartile range. Group A and  both had 3 people who did not find any of the data maps in the displays."----
# Check contributor performance
contribs <- d %>% group_by(group, contributor) %>% 
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = mean(detect, na.rm = TRUE)) 

# Plot performance
contribs %>% ggplot(aes(x = group, y = pdetect)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, height = 0, alpha = 0.7) + 
  ylab("Detection rate") + 
  ylim(0,1)


## ----choices, fig.cap = "Each facet is associated with one lineup, the height of the points show the proportion of the participants that made each choice when considering each lineup. The points coloured orange show the map which contained a trend model, these are the correct choices. The numbers differentiate the replicates of each trend model and type of map display. Participants were able to select 0 to indicate they did not want to choose a map.", fig.height = 12, fig.width=10----
d %>% 
  count(type, trend, choice, replicate, choice, detect_f) %>% 
  group_by(type, trend, replicate) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(repl = paste("Rep:", replicate, ":\n", type,  sep = "")) %>%
  mutate(bottom = 0) %>% 
  ggplot() + 
  geom_point(aes(x = choice, y = prop, color = detect_f), size = 4) + 
  geom_segment(aes(x = choice, xend = choice,y = bottom, yend = prop, colour = detect_f), size = 1) +
  facet_grid(repl ~ trend, 
    drop = TRUE, as.table = TRUE, scales = "free_y") +
  labs(x = "Choice of plot in lineup", y = "Amount of choices") +
  scale_colour_manual(values = detect_colours) +
  scale_x_continuous(breaks = seq(from = 0, to = 12)) +
  scale_y_continuous(breaks = seq(from = 0.0, to = 1.0, by = 0.1)) +
  theme(legend.position = "bottom") + 
  guides(colour = FALSE, fill = FALSE) +
  theme(strip.text.y = element_text(angle = 0))


## ----modeling_glm1, results="asis"----------------------------------------------------------------------------------
# Modelling 
glm1 <- glm(formula = detect ~ type * trend,
  family = binomial(link = "logit"), data = d)

preds <- predict(glm1, newdata = d, se=T)
pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = tibble(upper = (preds$fit+2*preds$se.fit), lower = (preds$fit-2*preds$se.fit))
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

terms <- c("Intercept", "Hex", "Three cities", "All cities", "Hex : Three cities", "Hex : All cities")

tidy(glm1) %>%
  mutate_if(is.numeric, round, 2) %>% 
  rowwise() %>% 
  mutate(estimate = case_when(
    p.value <= 0.001 ~ paste0(estimate,"$^{***}$"),
    p.value <= 0.01 ~ paste0(estimate, "$^{** }$"), 
    p.value <= 0.05 ~ paste0(estimate, "$^{*  }$"), 
    p.value <= 0.01 ~ paste0(estimate, "$^{.  }$"),
    TRUE ~ paste0(estimate))) %>% 
  ungroup() %>% 
  mutate(term = terms) %>% 
  select(Term = term, 
    Estimate = estimate, 
    Error = std.error, 
    Stat = statistic,
    p.value) %>% knitr::kable(format = "latex", escape = FALSE, align= "rcrrr", booktabs = T)


## ----pred_glm1------------------------------------------------------------------------------------------------------
d_glm <- d %>% select(group:certainty, detect, detect_f) %>%
  bind_cols(as_tibble(se.bands)) %>% 
  mutate(pfit, 
    predicted = ifelse(pfit > 0.5, 1, 0),
    predicted = factor(predicted, levels = c(0,1), 
      labels = c("Predicted: No", "Predicted: Yes")))

table(d_glm$predicted, d_glm$detect_f)


## ----glm2-----------------------------------------------------------------------------------------------------------
glm2 <- glm(formula = detect ~ type * trend + replicate_f,
  family = binomial(link = "logit"), data = d)

emmean2 <- emmeans(glm2, c("trend", "type", "replicate_f"),
                        type = "response")

int_2 <- confint(emmean2, by = c("type", "trend", "replicate_f"), adjust = "bonferroni")
int_2 %>% 
  ggplot(aes(x= replicate_f, y = prob, group = type)) + 
  geom_point(aes(col = type)) + 
  geom_line(alpha = 0.5, lty = "dashed") + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, col = replicate_f), 
                width = 0.2) +
  facet_grid(trend ~ .) +
  ylab("Estimated Marginal Mean")


## ----lmer1, results = "asis"----------------------------------------------------------------------------------------
# Mixed effects models
lmer1 <- lmer(detect ~ type*trend + (1|contributor), data = d)
lmer1_d <- augment(lmer1, d)

preds_lmer <- predict(lmer1, newdata = d)
pfit_lmer <- exp(preds_lmer)/(1+exp(preds_lmer))

stargazer::stargazer(lmer1)

d_lmer <- d %>% select(group:certainty, detect, detect_f) %>%
  bind_cols(as_tibble(se.bands)) %>% 
  mutate(pfit_lmer, 
    predicted = ifelse(pfit_lmer > 0.5, 1, 0),
    predicted = factor(predicted, levels = c(0,1), labels = c("No", "Yes")))
# Hexagon maps have better chance of correct detection
# Allowing for contributor effects to vary: 0.12 strong residual


## ----lmer2, results = "asis"----------------------------------------------------------------------------------------
# Mixed effects models
lmer2 <- lmer(detect ~ (type|trend), data = d)
lmer2_d <- augment(lmer2, d)

preds_lmer2 <- predict(lmer2, newdata = d)
pfit_lmer2 <- exp(preds_lmer2)/(1+exp(preds_lmer2))

stargazer::stargazer(lmer2)

d_lmer2 <- d %>% select(group:certainty, detect, detect_f) %>%
  bind_cols(as_tibble(se.bands)) %>% 
  mutate(pfit_lmer2, 
    predicted = ifelse(pfit_lmer2 > 0.5, 1, 0),
    predicted = factor(predicted, levels = c(0,1), labels = c("No", "Yes")))



## ----lmer1_model, results = "asis"----------------------------------------------------------------------------------
# Mixed effects models
lmer1 <- lmer(detect ~ type*trend + (1|contributor), data = d)
tidy(lmer1) %>% mutate_if(is.numeric, .f = round, 2) %>% stargazer::stargazer(.)

# Hexagon maps have better chance of correct detection
# Allowing for contributor effects to vary: 0.12 strong residual


## ----pdetection_trend, eval = FALSE---------------------------------------------------------------------------------
## plots %>% ggplot(aes(x = group, y = pdetect)) +
##   geom_boxplot() +
##   geom_jitter(width = 0.1) +
##   ylab("Detection rate") +
##   ylim(c(0,1))
## 
## plots %>% ggplot(aes(x = trend, y = pdetect, fill = trend)) +
##   geom_boxplot() +
##   scale_fill_manual(values = trend_colours) +
##   geom_jitter(width = 0.1) +
##   ylab("Detection rate") +
##   ylim(c(0,1))
## 
## plots %>% ggplot(aes(x = type, y = pdetect)) +
##   geom_boxplot() +
##   geom_jitter(width = 0.1) +
##   ylab("Detection rate") +
##   ylim(c(0,1))


## ----demogs---------------------------------------------------------------------------------------------------------
# Create one record for each contributor, to examine demographics
dem_contribs <- d %>%
  group_by(contributor) %>%
  slice(1) %>% ungroup()


# Demographics of contributors
#dem_contribs %>% count(gender)
dem_contribs %>% ggplot() + geom_bar(aes(age))
dem_contribs %>% count(gender) %>% knitr::kable(format = "latex", booktabs = TRUE)
dem_contribs %>% count(education) %>% knitr::kable(format = "latex", booktabs = TRUE)

