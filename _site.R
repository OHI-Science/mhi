## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)
})

## brewed vars
study_area      = "Main Hawaiian Islands"
key             = "mhi"
dir_scenario_gh = "https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017"


## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

## read in variables
scores <- readr::read_csv(file.path(dir_scenario_gh, 'scores.csv'))
layers <- readr::read_csv(file.path(dir_scenario_gh, 'layers.csv'))
weight <- readr::read_csv(file.path(dir_scenario_gh, 'conf/goals.csv')) %>%
  select(goal, weight)

