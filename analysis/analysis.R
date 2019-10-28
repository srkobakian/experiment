# Analysis of pilot
#library(googlesheets)
#gs_ls()
#sheet <- gs_key("1PcKMmsojzljDZl4bftOu5lbG51AVYuiPIJFq2t7kiGg")

library(tidyverse)
library(readxl)
d <- read_xlsx("experiment-export.xlsx")
#d %>% count(contributor)
d <- d %>% filter(!is.na(contributor)) %>%
  filter(!(contributor %in% c("DUMMY", "server_test", "something")))
#d %>% count(group)
#d %>% count(image_name, sort = TRUE)

d <- d %>% 
  separate(image_name, c("nothing", "trend", "location", "type")) %>%
  select(-nothing) %>%
  mutate(location = as.numeric(location), 
         detect = ifelse(location == choice, 1, 0))

d_smry <- d %>% group_by(trend, type, location) %>%
  summarise(pdetect = length(detect[detect == 1])/length(detect))

ggplot(d_smry, aes(x=type, y=pdetect)) + 
  geom_jitter(width=0.1) + 
  facet_wrap(~trend)
