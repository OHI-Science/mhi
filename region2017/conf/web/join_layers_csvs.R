## Join region2017/layers.csv with prep/data_layers.csv


library(tidyverse)

## url setup
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017"
layers_web <- 'http://ohi-science.org/mhi/layers.html'
short_goal_web <- file.path(dir_scenario_gh, "conf/web/goals")
prep_web <- "https://raw.githubusercontent.com/OHI-Science/mhi/master/prep"

## read in information from layers.csv
layers_conf  <- readr::read_csv(file.path(dir_scenario_gh, 'layers.csv')) %>%
  select(targets, layer, filename_conf = filename)


## read in Eva's excel sheet from github.com (so make sure that you push any updates)
layers_prep <- readr::read_csv(file.path(prep_web, "data_layers.csv")) %>%
  dplyr::select(name = `Data Layer`, # previously renamed to `header_layer`
                layer   = Name,      # previously renamed to `layer_name`
                description  = `Brief Description`,
                reference    = Reference,
                #targets    = Goal, # previously renamed to `Dimension`
                filename_prep  = File, # previously renamed to `filename`
                bib,
                bib2,
                url)

## join two layers dataframes
layers_join <- layers_conf %>%
  left_join(layers_prep, by = "layer")

## rename for rendering below
# layers_csv <- layers_join
