## March 21, 2018

## Hi Eva, when I run ohicore::CheckLayers() in configure_toolbox.R, it says that there are 2 layers with duplicates. I think you should check these out because they are likely errantly affecting the calcs (minorly):

# Rows duplicated...
# fis_sus: 88
# spp_status: 476

library(tidyverse)

## 1. check out fis_sus (.csv registered in layers.csv):
x <- read_csv('layers/fis_sus_updated_mhi2017.csv')
head(x)

x %>%
  group_by(rgn_id, year, key_sp) %>%
  filter(n()>1)

# A tibble: 136 x 4
# # Groups:   rgn_id, year, key_sp [48]
# rgn_id  year key_sp    score
# <int> <int> <chr>     <dbl>
#   1      1  2012 Acan_Kala 0.470
# 2      1  2012 Acan_Kala 0.100
# 3      1  2013 Acan_Kala 0.470
# 4      1  2013 Acan_Kala 0.100
# 5      1  2014 Acan_Kala 0.470
# 6      1  2014 Acan_Kala 0.100
# 7      1  2015 Acan_Kala 0.470
# 8      1  2015 Acan_Kala 0.100
# 9      1  2016 Acan_Kala 0.470
# 10      1  2016 Acan_Kala 0.100


## 2. check spp_status (.csv registered in layers.csv):
y <- read_csv('layers/spp_status_mhi2017.csv')
head(y)


## in functions.r you select just rgn_id and score so maybe this isn't a problem:
y %>%
  group_by(rgn_id, score) %>%
  filter(n()>1)

# # A tibble: 492 x 7
# # Groups:   rgn_id, score [16]
# rgn_id Type          `Common Name` `Hawaiian Name` `Species name` `ESA Status` score
# <int> <chr>         <chr>         <chr>           <chr>          <chr>        <dbl>
#   1      1 Coastal Plant Beach spurge  Koko, _akoko    Chamaesyce de… NA           1.00
# 2      1 Coastal Plant NA            __kia, kauhi    Wikstroemia s… NA           1.00
# 3      1 Coastal Plant NA            Ahuawa          Mariscus java… NA           1.00
# 4      1 Coastal Plant NA            Akaakai         Schoenoplecte… NA           1.00
# 5      1 Coastal Plant NA            Alena           Boerhavia rep… NA           1.00
# 6      1 Coastal Plant NA            Degener_s _ako… Euphorbia deg… V            0.700
# 7      1 Coastal Plant Dwarf naupaka Dwarf naupaka   Scaevola cori… E            0.250
# 8      1 Coastal Plant Pandanus      hala/Pu hala    Pandanus tect… NA           1.00
# 9      1 Coastal Plant NA            Hau             Talipariti ti… NA           1.00
# 10      1 Coastal Plant NA            Hinahina        Heliotropium … NA           1.00
