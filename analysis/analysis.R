# Analysis of pilot
#library(googlesheets)
#gs_ls()
#sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")

library(tidyverse)
library(lubridate)
library(broom)
library(readxl)
d <- read_xlsx("data/experiment-export.xlsx") %>% select(-starts_with(".."))
d %>% count(contributor)

# remove test contributors
d <- d %>% filter(!is.na(contributor)) %>%
  filter(!(contributor %in% c("DUMMY", "server_test", "something")))
#d %>% count(group)
#d %>% count(image_name, sort = TRUE)

# add data replication number
replicate <- bind_cols(replicate = sort(rep(1:12, 2)), image_name = (list.files("figures/final"))) %>% 
  mutate(replicate = as.factor(replicate))

d <- d %>% left_join(., replicate, by = "image_name")

image_smry <- d %>% count(group, image_name)

image_smry %>% ggplot() + geom_col(aes(x = image_name, y = n)) + coord_flip() +
  facet_wrap(~group, scales = "free_x")


#remove duplilcated entries due to submit button
d <- d %>% group_by(group, contributor, image_name) %>%
  slice(1) %>% ungroup() %>% 
  arrange(group, contributor, order)

image_smry <- d %>% count(contributor, order, sort = TRUE)

image_smry %>% ggplot() + geom_col(aes(x = contributor, y = n, fill = order)) + coord_flip()


d <- d %>% 
  separate(image_name, c("nothing", "trend", "location", "type")) %>%
  select(-nothing) %>%
  mutate(location = as.numeric(location), 
    # detect measures the accuracy of the choice
         detect = ifelse(location == choice, 1, 0))

d_smry <- d %>% group_by(trend, type, location, replicate) %>%
  # pdetect measures the aggregated accuracy of the choices
  summarise(pdetect = length(detect[detect == 1])/length(detect))

library(ggrepel)
pos <- position_jitter(width = 0.5, height = 0, seed = 1)
ggplot(d_smry, aes(x=type, y=pdetect,label = replicate, colour = replicate)) + 
  geom_text_repel(position = pos, size = 5) +
  geom_point(position = pos, size = 2) +  
  facet_wrap(~trend) + guides(colour = FALSE) + 
  scale_colour_brewer(palette = "Paired")





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
  family = binomial(link = "probit"), data = d)

glm2_d <- augment(glm2, d) %>% mutate(fitted = ifelse(.fitted>0.5, 1, 0))

# classification table
glm2_d %>% 
  count(detect, fitted) %>%
  xtabs(formula = n ~ detect + fitted, data = .)



###########################################################




modelcomp <- bind_cols(detected = glm2_d$detect, glm1 = glm1_d$.fitted, glm2 = glm2_d$.fitted)

modelcomp %>% pivot_longer(., cols = c(glm1,glm2), names_to = "models") %>% 
  ggplot() + geom_density(aes(x = value, colour = models))


