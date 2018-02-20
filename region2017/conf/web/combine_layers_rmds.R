#####################################################
## This file takes all the layer Rmd files and
## writes them into a single Rmd file: layers_all.Rmd.
## By M. Frazier, adapted by J. Lowndes
## https://github.com/OHI-Science/ohi-global/blob/draft/eez/conf/web/CombineLayers.R
#####################################################

## setwd if not already there
# setwd('region2017')

## load relevant libraries
library(tidyverse)

## delete layers_all.Rmd to start fresh
unlink('conf/web/layers_all.Rmd')

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

## join region2017/layers.csv with prep/data_layers.csv
source(
  "https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/conf/web/join_layers_csvs.R")


## extra Rmd file (or is mislabeled)
## can ignore the "layers_all" file, but there should be no others:
setdiff(layers_Rmd, layers_join$layer)
## Feb 19 2018:
# [1] "ao_access"               "ao_need"                 "ao_residents"
# [4] "cc_slr"                  "cc_sst"                  "cp_hab_condition"
# [7] "fis_sus_score"           "fp_habitat"              "fp_mpa_eez"
# [10] "gdp_usd"                 "hd_mpa_eez"              "li_gci"
# [13] "lsp_coastal_condist"     "NA"                      "r_participation_mhi2017"
# [16] "spp_status"              "wetlands_coastal_1km"


## a layer that is missing an Rmd file
## Should be none:
setdiff(layers_join$layer, layers_Rmd)
## Feb 19 2018:
# [1] "element_wts_cp_km2_x_protection" "element_wts_cs_km2_x_storage"
# [3] "species_diversity_eez"           "rgn_area"
# [5] "ao_access_mhi2017"               "con_participation"
# [7] "cp_hab_condition_mhi2017"        "cw_po_lbsp_nosds_bil"
# [9] "cw_po_lbsp_sed"                  "cw_po_lbspaggolfrunoff"
# [11] "cw_po_lbspurbanrunoff"           "cw_po_marinedebris"
# [13] "cw_po_shipbased_shipp"           "fis_sus"
# [15] "fp_wildcaught_weight_mhi"        "le_sector_weight"
# [17] "le_wage"                         "liv_a_wage"
# [19] "lsp_coastal_conservation"        "lsp_historic_sites"
# [21] "lsp_mpa_3nm"                     "lsp_mpa_nearshore"
# [23] "t_boarding"                      "t_kayaking"
# [25] "t_snorkel_scuba"                 "t_swimming"
# [27] "t_thrill_craft"                  "t_whale_watching"
# [29] "fp_MPA_eez"                      "hd_MPA_eez"
# [31] "rgn_georegions"                  "rgn_global"
# [33] "rgn_labels"                      "spp_status_mhi"
# [35] "t_visitor_gdp"

### Grab each layer description and add to master Rmd file!

data <- layers_join %>%
  filter(!is.na(name)) %>% ## when above setdiffs are fixed, won't need this anymore
  select(name, layer, filename_prep, description) %>%
  arrange(name)

for (h in data$name){ # h="aquarium fishing"

  layer      <-  data$layer[data$name == h]
  filename   <-  data$filename_prep[data$name == h]
  layer_path <- 'https://github.com/OHI-Science/mhi/tree/master/region2017/layers'

  tmp <- capture.output( cat("\n",
                             paste0("\n# ", h),

                             paste0("\n####[", layer, "]", "(", file.path(layer_path, filename), ") {-}"),

                             paste0("\n```{r, echo=FALSE, results='hide'}\n
                                    x <- tempfile(fileext = 'Rmd')\n
                                    on.exit(unlink(x))\n
                                    download.file(", "\"",
                                    sprintf('https://raw.githubusercontent.com/OHI-Science/mhi/master/region2017/conf/web/layers_all/%s.Rmd',

                                            layer), "\", x)\n```\n"),

                             paste0("\n```{r, child = x, echo=FALSE, results='asis'}"),
                             "\n",
                             "\n```",
                             "\n"
                             ))

  write(tmp, "conf/web/layers_all.Rmd", append=TRUE)
}
