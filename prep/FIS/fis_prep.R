#FIS prep
#last modified March 28 2017
###ISSUE###We are missing catch data that is not associated with islands - Must have along given us data from catch blocks around island (including cross seamount)


## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/github/mhi/prep/FIS')


## import dummy 'local data' that is already formatted nicely. Note the naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'fis_catch_dar.csv')
d <- readr::read_csv(data_file)

str(d)

#combine pelagic catch summarized by island into one catch estimate for all of Hawaii - pelagics and bottom fish only
library(plyr)#install.packages('plyr')
d_pel<-subset(d, mus=="PMUS")

d_pel2<-ddply(d_pel, .(year,rgn_id), mutate, total=sum(catch))
d_pel2<-ddply(d_pel2, .(rgn_id), summarise, mean=mean(total), licenses=mean(licenses))#
d_pel_islands<-subset(d_pel2, rgn_id=="1"|rgn_id=="2"|rgn_id=="3"|rgn_id=="4")#subset for only island level data (remove offshore fishing catch)
d_pel_islands$total<-sum(d_pel_islands$mean)#calculate the mean total catch
d_pel_islands$prop<-d_pel_islands$mean/d_pel_islands$total*100#get island group proportion of total mean catch

d_reef<-subset(d, mus=="CHCRT"|mus=="PHCRT")#subset data for reef fish
d_reef<-ddply(d_reef, .(species,year,rgn_id), summarise, total=sum(catch), licenses=sum(licenses))#summarize the reef fish landings by rgn also

d_deep7<-subset(d, mus=="BMUS")
## save this local data layer in "layers" folder with the same naming convention as above format
dir_layers <- file.path('~/github/mhi/region2017/layers')
readr::write_csv(d_pel, file.path(dir_layers, "fis_pelagic_catch_mhi2017.csv"))
readr::write_csv(d_reef, file.path(dir_layers, "fis_reef_catch_mhi2017.csv"))
readr::write_csv(d_deep7, file.path(dir_layers, "fis_deep7_catch_mhi2017.csv"))
readr::write_csv(d_pel_islands, file.path(dir_layers, "fis_pel_propcatch_mhi2017.csv"))


## You will see a notification in the "Git" window in RStudio once the layer is saved successfully.
#dont forgt to register the layer in layers.csv

#####all catch data in one file
data_file  <- file.path(dir_layers, 'fis_catch_mhi2017.csv')
d <- readr::read_csv(data_file)

str(d)
d<-ddply(d, .(species, year, rgn_id), numcolwise(sum) )
readr::write_csv(d, file.path(dir_layers, "fis_catch_island_mhi2017.csv"))

