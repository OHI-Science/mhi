#FIS prep
#last modified March 28 2017
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/github/mhi/prep/FIS')


## import dummy 'local data' that is already formatted nicely. Note the naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'fis_catch_dar.csv')
d <- readr::read_csv(data_file)

str(d)

#combine pelagic catch summarized by island into one catch estimate for all of Hawaii - pelagics and bottom fish only
library(plyr)#install.packages('plyr')
d_pel_bot<-subset(d, mus=="PMUS"|mus=="BMUS")
d_reef<-subset(d, mus=="CHCRT"|mus=="PHCRT")
d_pel_bot<-ddply(d_pel_bot, .(species,year), summarise, total=sum(lbs), licenses=sum(licenses))#
d_reef<-ddply(d_reef, .(species,year,rgn_id, half), summarise, total=sum(lbs), licenses=sum(licenses))#summarize the reef fish landings by rgn also
d_reef_2_3<-subset()
d_reef_2_3<-ddply(d_reef, .(species,year,rgn_id), summarise, total2=sum(total+half))#summarize the reef fish landings by rgn also


}
d_reef<-ddply(d_reef, .(species), mutate, total=(total+d_reef_pg$half))#summarize the reef fish landings by rgn also


## save this local data layer in "layers" folder with the same naming convention as above format
dir_layers <- file.path('~/github/mhi/region2017/layers')
readr::write_csv(d_pel_bot, file.path(dir_layers, "fis_pelagic&bottom_catch_mhi2017.csv"))
readr::write_csv(d_reef, file.path(dir_layers, "fis_reef_catch_mhi2017.csv"))


## You will see a notification in the "Git" window in RStudio once the layer is saved successfully.
#dont forgt to register the layer in layers.csv
