## To update the layers information displayed on the website:

## 1. Update prep/data_layers.csv

## 2. Recreate any layer .Rmd files if necessary (to be saved in conf/web/layers_all/)
source("conf/web/create_layers_rmds.R")

## 3. Combine layer .Rmd files into layers_all.Rmd
source("conf/web/combine_layers_rmds.R")
# This will check if there are any mismatches between layers listed in the `layers_join` object and the layer .Rmd files

##3.5 need to push to get layers to github

## 4. Inspect layers_all.Rmd and knit

## 5. Commit and push

## 6. Switch to gh-pages branch, pull, reknit layers page


