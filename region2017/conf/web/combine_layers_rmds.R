#####################################################
## This file takes all the layer Rmd files and
## writes them into a single Rmd file
## By M. Frazier, adapted by J. Lowndes
## https://github.com/OHI-Science/ohi-global/blob/draft/eez/conf/web/CombineLayers.R
#####################################################

## setwd if not already there
# setwd('region2017')

## load relevant libraries
library(tidyverse)

######################################################
### Rmd file header information
#######################################################
tmp <- capture.output(cat("---",
                          "\ntitle: Layers descriptions",
                          "\noutput:",
                          "\n  html_document:",
                          "\n    toc: true",
                          "\n    toc_depth: 1",
                          "\n    number_sections: false",
                          "\n    toc_float: yes",
                          "\n---"))

write(tmp, "conf/web/layers_all.Rmd")

######################################################
### Load libraries in Rmd
### and get master list of layers
#######################################################

tmp <- capture.output( cat(paste0("\n```{r, message=FALSE, echo=FALSE, warning=FALSE, error=FALSE}"),
                           "\n",
                           "library(tidyverse)",
                           "\n",
                           "library(knitr)",
                           "\n",
                           "\n",
                           "layer_meta <- readr::read_csv('https://raw.githubusercontent.com/OHI-Science/mhi/master/prep/data_layers.csv')",
                           "\n",
                           "layer_path <- 'https://github.com/OHI-Science/mhi/tree/master/region2017/layers'",
                           "\n",
                           "\n",
                           "\n```"))

write(tmp, "conf/web/layers_all.Rmd", append=TRUE)


######################################################
### Cycle through each layer and add to file
#######################################################

layer_path <- 'https://github.com/OHI-Science/mhi/tree/master/region2017/layers'

## make sure all the Rmd files are in there and no typos!
layers_Rmd <- list.files("conf/web/layers_all")
layers_Rmd <- layers_Rmd[grep(".Rmd", layers_Rmd)]
layers_Rmd <- gsub(".Rmd", "", layers_Rmd)
layers <- readr::read_csv("../prep/data_layers.csv") %>%
  dplyr::select(header_layer = `Data Layer`,
                layer_name   = Name,
                description  = `Brief Description`,
                reference    = Reference,
                url) %>%
  arrange(layer_name)

## extra Rmd file (or is mislabeled)
## can ignore the "layers_all" file, but there should be no others:
setdiff(layers_Rmd, layers$layer_name)

## a layer that is missing an Rmd file
## Should be none:
setdiff(layers$layer_name, layers_Rmd)

### Grab each layer description and add to master Rmd file!

data <- layers %>%
  select(header_layer, layer_name, description) %>%
  arrange(header_layer)

for (h in data$header_layer){ # h="access"

  layer_name <-  data$layer_name[data$header_layer == h]
  layer_path <- 'https://github.com/OHI-Science/mhi/tree/master/region2017/layers'

  tmp <- capture.output( cat("\n",
                             paste0("\n# ", h),

                             paste0("\n####[", layer_name, "]", "(", file.path(layer_path, layer_name), ".csv) {-}"),

                             paste0("\n```{r, echo=FALSE, results='hide'}\n
                                    x <- tempfile(fileext = 'Rmd')\n
                                    on.exit(unlink(x))\n
                                    download.file(", "\"",
                                    sprintf('https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/conf/web/layers_all/%s.Rmd',

                                            layer_name), "\", x)\n```\n"),

                             paste0("\n```{r, child = x, echo=FALSE, results='asis'}"),
                             "\n",
                             "\n```",
                             "\n"
                             ))

  write(tmp, "conf/web/layers_all.Rmd", append=TRUE)
}
