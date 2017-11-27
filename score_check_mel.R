## Score checking with Mel

library(tidyverse)
scores <- read_csv('https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/scores.csv')


scores %>%
  filter(goal == "CP") %>% as.data.frame()
## likely future state = [1 + 0.67*trend + (1 - 0.67)*(resilience - pressures)]*status

## Question: do the future scores for FIS seem correct?

## Let's look at the FIS scores. First glance: there is a super small trend and resilience is twice as high as pressures, so high future scores look right.  so this looks right

## let's walk through for just one region. How about region_id 2, and spread it so that each value has its own column header.
fis2 <- scores %>%
  filter(goal == "FIS", region_id == 2) %>% # as.data.frame() %>% ## to see all rows here
  spread(dimension, score)


## confirm likely future state (lfs) score.
fis2 %>%
  ## multiply to bring status, resilience, pressures onto same scale with trend
  mutate(status     = status     /100,
         resilience = resilience /100,
         pressures  = pressures  /100) %>%
  ## likely future state equation
  mutate(lfs =
           (1 +
              (0.67*trend) +
              ((1 - 0.67)*(resilience - pressures))) *
           status * 100)
## lfs score will be capped at 100.


## confirm overall score.
fis2 %>%
  mutate(score_check = (future + status)/2)


### now check coastal protection
## let's walk through for just one region. How about region_id 1, and spread it so that each value has its own column header.
(cp1 <- scores %>%
  filter(goal == "CP", region_id == 1) %>%
  as.data.frame() %>%
  spread(dimension, score))


## confirm likely future state (lfs) score.
cp1 %>%
  ## multiply to bring status, resilience, pressures onto same scale with trend
  mutate(status     = status     /100,
         resilience = resilience /100,
         pressures  = pressures  /100) %>%
  ## likely future state equation
  mutate(lfs =
           (1 +
              (0.67*trend) +
              ((1 - 0.67)*(resilience - pressures))) *
           status * 100)
## lfs score will be capped at 100.

## here it is by hand:
status <- 0.7857046
pressure <- 0.3004
resil <- 0.8143
trend <- -0.223814

(lfs =
  (1 +
     (0.67*trend) +
     ((1 - 0.67)*(resil - pressure))) *
  status * 100)

##

## load tidyverse library and scores variable
library(tidyverse)
scores <- read_csv('https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/scores.csv')

(fis2 <- scores %>%
    filter(goal == "FIS", region_id == 2) %>%
    spread(dimension, score) %>%
    mutate(status     = status     /100,
           resilience = resilience /100,
           pressures  = pressures  /100))
fis2 %>%
  mutate(lfs =
           (1 +
              (0.67*trend) +
              ((1 - 0.67)*(resilience - pressures))) *
           status * 100)

