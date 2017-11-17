## Create Individual Rmd layers from an EXCEL file for MHI

library(tidyverse)
library(stringr)

## excel filepath relative to region2017
dir_excel <- "../prep"

## layers directories locally relative to region2017/conf/web and on github
dir_layers_all <- "conf/web/layers_all/"
dir_layers_gh <- "https://github.com/OHI-Science/mhi/blob/master/region2017/layers/"


## read in excel sheet
layers_excel <- readxl::read_excel(path = file.path(dir_excel, "OHI_state assessment_framework.xlsx"), sheet = "Data_Layers")
names(layers_excel)

## rename a few columns
layers_info <- layers_excel %>%
  dplyr::select(header_layer = `Data Layer`,
                layer_name   = Name,
                description  = `Brief Description`,
                reference    = Reference,
                url) %>%
  arrange(layer_name)


## Loop through all columns and format for web

layers_list <- layers_info$header_layer

for (i in layers_list) { # i = "ao_access"

  st <- layers_info %>%
    ## filter for this layer
    filter(header_layer == i) %>%
    ## all together now
    mutate(info = paste0(#"# ", header_layer, "\n\n",
                         #sprintf("[%s](%s)\n\n", layer_name, dir_layers_gh),
                         description, "\n\n",
                         "#### Reference\n\n",
                         sprintf("[%s](%s)", reference, url)))

  ## save rmd
  write_file(st$info, paste0(dir_layers_all, st$layer_name, ".Rmd"))

}

readr::write_csv(layers_info, 'conf/web/layers_info_copied_from_prep_excel.csv')
