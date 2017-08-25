## configure_toolbox.r

## configure_toolbox.r ensures all files in your repo are properly configured.
## It must be sourced before calculating OHI scores with ohicore::CalculateAll();
## it can be sourced here or is also sourced from calculate_scores.r.

## You are encouraged to use this script when developing individual goal models. A good workflow is:
  ## 1. prepare data layers in the /prep folders (script as much as possible in R)
  ## 2. register data layers in layers.csv and save them in /layers folder
  ## 3. source configure_repo.r to ensure proper configuration
  ## 4. develop goal models in functions.r, running individual goal models line by line

## load required packages after checking whether they are already installed
pkgs_required <- c('ohicore', 'tidyverse', 'stringr', 'zoo')
pkgs_check <- pkgs_required[!pkgs_required %in% (.packages())]
pkgs_installed <- sapply(pkgs_check, FUN = function(x) library(x, character.only = TRUE))

## set working directory
setwd("~eschemmel/Documents/github/mhi/region2017")


## load scenario configuration
conf = ohicore::Conf('conf')

## check that files in the \layers folder match layers.csv registration.
ohicore::CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

## load layers for ohicore to access.
layers = ohicore::Layers('layers.csv', 'layers')
