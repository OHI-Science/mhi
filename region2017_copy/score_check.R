## load tidyverse library and scores variable
library(tidyverse)
scores <- read_csv('https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/scores.csv')
scores %>%
  filter(goal == "FIS") %>% as.data.frame()
fis2 <- scores %>%
  filter(goal == "FIS", region_id == 2) %>%
  spread(dimension, score) %>%
  mutate(status     = status     /100,
         resilience = resilience /100,
         pressures  = pressures  /100)
