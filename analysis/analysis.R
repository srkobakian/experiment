# Analysis of pilot
#library(googlesheets)
#gs_ls()
#sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")

library(tidyverse)
library(lubridate)
library(broom)
library(readxl)

d %>% count(contributor)

# remove test contributors
d <- d %>% filter(!is.na(contributor)) %>%
  filter(!(contributor %in% c("DUMMY", "server_test", "something", "hello")))
#d %>% count(image_name, sort = TRUE)

# add data replication number
replicate <- bind_cols(replicate = sort(rep(1:12, 2)), image_name = (list.files("figures/final"))) %>% 
  mutate(replicate = as.factor(replicate))

d <- d %>% left_join(., replicate, by = "image_name")

d %>% count(group, image_name)

#remove duplilcated entries due to submit button
d <- d %>% group_by(group, contributor, image_name) %>%
  slice(1) %>% ungroup() %>% 
  arrange(group, contributor, order)

image_smry <- d %>% count(contributor, order, sort = TRUE)

image_smry %>% ggplot() + geom_col(aes(x = contributor, y = n, fill = order)) + coord_flip()
# This shows multiple submission of scores by some participants


d <- d %>% 
  separate(image_name, c("nothing", "trend", "location", "type")) %>%
  select(-nothing) %>%
  mutate(location = as.numeric(location), 
    # detect measures the accuracy of the choice
         detect = ifelse(location == choice, 1, 0)) %>% 
  mutate(trend = case_when(
    trend == "cities" ~ "All capital cities",
    trend == "three" ~ "Three capital cities",
    trend == "nwse" ~ "North West to South East")) %>% 
  mutate(type = case_when(
    type == "hex" ~"Hexagons",
    TRUE~"Geography"
  ))

# Contributor performance
contribs <- d %>% group_by(group, contributor) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect)) 

contribs %>% count(group)

contribs %>% ggplot(aes(x = group, y = pdetect, label = contributor)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1)

d_contribs <- d %>%
  group_by(contributor) %>%
  slice(1) %>% ungroup()

# Demographics of contributors

d_contribs %>% count(gender)

d_contribs %>% count(age)

d_contribs %>% count(education)

d_contribs %>% count(australia)

# Average contributor performance
d_smry <- d %>% group_by(trend, type, location, replicate) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect))



library(ggrepel)
repl_plot <- d_smry %>% ungroup() %>% 
  ggplot(aes(x=type, y=pdetect, label = replicate, colour = replicate, group = replicate)) +
  geom_line(size = 2, arrow = arrow()) +  
  facet_wrap(~trend) +  
  scale_colour_brewer(palette = "Paired") +
  geom_hline(aes(yintercept = 0.167)) +
  xlab("Type of areas visualised") +
  ylab("Proportion of participants who selected the true data plot") + ylim(0,1)
repl_plot
ggsave(filename = "figures/pilot/replicate_change.pdf", plot = repl_plot, device = "pdf", dpi = 300, width = 12, height = 8, units = "in")
#ggsave(filename = "figures/pilot/replicate_change.png", plot = repl_plot, device = "png", dpi = 300, width = 12, height = 8, units = "in")


d <- d %>% group_by(group, contributor) %>%
separate(time, into = c("date", "time"), sep = " ") %>%
mutate(start_time = lag(time)) %>%
ungroup()

###########################################################
##############    Time taken to answer   ##################
###########################################################

# Define the amount of second taken to answer
d <- d %>% rowwise() %>% 
  mutate(time_taken = map2_dbl(
  time, start_time, function(t2, t1){
    if (is.na(start_time)){
      return(NA)
    } else {
      as.numeric(hms::as_hms(time) - hms::as_hms(start_time))}
    }
)) %>% ungroup()


# Density plot for time taken
ggplot(d) + geom_density(aes(x= time_taken)) + facet_wrap(~type)

d %>% ggplot() + geom_boxplot(aes(x = replicate, y = time_taken)) + facet_wrap(~type) 

d %>% ggplot() + geom_boxplot(aes(x = type, y = time_taken, fill = trend)) + facet_wrap(~replicate) + ylim(0,130) 

t_smry <- d %>% mutate(time_taken = ifelse(time_taken>200, NA, time_taken)) %>% group_by(trend, type, location, replicate) %>%
  # avg_time measures the average time taken to make a choice
  summarise(avg_time = mean(time_taken, na.rm = TRUE))

library(ggrepel)
repl_plot_t <- t_smry %>% ungroup() %>% 
  ggplot(aes(x=type, y=avg_time, label = replicate, colour = replicate, group = replicate)) +
  geom_line(size = 2, arrow = arrow()) +  
  facet_wrap(~trend) +  
  scale_colour_brewer(palette = "Paired") +
  xlab("Type of areas visualised") +
  ylab("Average time taken to submit responses (seconds)") + ylim(0,45)
repl_plot_t
ggsave(filename = "figures/pilot/replicate_change_time.pdf", plot = repl_plot_t, device = "pdf", dpi = 300, width = 12, height = 8, units = "in")
#ggsave(filename = "figures/pilot/replicate_change.png", plot = repl_plot, device = "png", dpi = 300, width = 12, height = 8, units = "in")


d <- d %>% group_by(group, contributor) %>%
  separate(time, into = c("date", "time"), sep = " ") %>%
  mutate(start_time = lag(time)) %>%
  ungroup()

# compare the time taken and probability of detection for each replicate
repl_plot_c <- left_join(t_smry, d_smry) %>% 
  ggplot() + 
  geom_point(aes(x = avg_time, y = pdetect, colour = trend), 
    size = 5, alpha = 0.8) + xlim(0,45) +
  facet_wrap(~type) + 
  geom_hline(aes(yintercept = 0.167)) +
  xlab("Average time taken to submit responses (seconds) for each type of display") + 
  ylab("Proportion of participants who selected the true data plot")

repl_plot_c
ggsave(filename = "figures/pilot/replicate_timevsdetect.pdf", plot = repl_plot_c, device = "pdf", dpi = 300, width = 12, height = 8, units = "in")




###########################################################
###################### Modelling ##########################
###########################################################

# model using type + trend variables
glm1 <- glm(formula = detect ~ type + trend,
  family = binomial(link = "logit"), data = d)

glm1_d <- augment(glm1, d) %>% mutate(fitted = ifelse(.fitted>0.5, 1, 0))


# classification table
glm1_d %>% 
  count(detect, fitted) %>%
  xtabs(formula = n ~ detect + fitted, data = .)
  

# model using interaction of type + trend variables
glm2 <- glm(formula = detect ~ type + trend + type:trend,
  family = binomial(link = "logit"), data = d)

glm2_d <- augment(glm2, d) %>% 
  mutate(fitted = ifelse(.fitted > 0.5, 1, 0))

# classification table
glm2_d %>% 
  count(detect, fitted) %>%
  xtabs(formula = n ~ detect + fitted, data = .)

###########################################################

# Fixed effects models


lmer1_d <- lmer(detect ~ type + trend + (1 | contributor), data = d)
glance(lmer1_d)
tidy(lmer1_d)
# Hexagon maps have better chance of correct detection
# Allowing for contributor effects to vary 0.147 

lmer2_d <- lmer(detect ~ type*trend + (1 | contributor), data = d)
glance(lmer2_d)
tidy(lmer2_d)


lmer3_d <- lmer(detect ~ type*trend + certainty + (1 | contributor), data = d)
glance(lmer3_d)
tidy(lmer3_d)

