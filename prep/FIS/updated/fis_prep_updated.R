#FIS prep
#last modified Sept 21, 2017
###ISSUE###We are missing catch data that is not associated with islands - waiting on updated pelagic catch data from DAR

## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/documents/github/mhi/prep/FIS/updated')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'fis_reef_catch_mhi2017.csv')
reef <- readr::read_csv('fis_reef_catch_mhi2017.csv')


data_file  <- file.path(dir_layers, 'fis_deep7_catch_mhi2017.csv')
deep<- readr::read_csv('fis_deep7_catch_mhi2017.csv')


data_file  <- file.path(dir_layers, 'fis_pelagic_catch_mhi2017.csv')
pelagic<- readr::read_csv('fis_pelagic_catch_mhi2017.csv')

data_file  <- file.path(dir_layers, 'fis_reef_catch_multiplier_mhi2017.csv')
mult<- readr::read_csv('fis_reef_catch_multiplier_mhi2017.csv')

#separate out coastal pelagics
coast<-subset(deep, code=="C") %>%
  select(rgn_id,year, family, species, catch)

rcoast<-subset(reef, code=="C") %>%
  select(year, rgn_id,family, species, catch)
coast<-join(rcoast, coast)
coast_pelagic<-ddply(coast, .(rgn_id, family,species,year), summarise, catch=sum(catch))

#add in region multiplier for non-commerical reef catch
reef<-join(reef, mult, by="rgn_id") %>%
  mutate(reef, catch_mult=catch*value)
reef<-ddply(reef, .(rgn_id,family,species,year), summarise, catch=sum(catch_mult))

#combine catch summarized by island into one catch estimate for all of Hawaii - pelagics and bottom fish only
library(plyr)#install.packages('plyr')
str(pelagic)
pelagic<-ddply(pelagic, .(rgn_id,family,species,year), summarise, catch=sum(lbs))

deep<-ddply(deep, .(family,rgn_id,species,year), summarise, catch=sum(catch))

#create_key to be able to apply sustainability scores and median scores for each family for missing species sustainability scores
deep<-mutate(deep,Fam=str_sub(family, start=1L, +4)) %>% #trim Family name to first 4 letters to use as family code
    mutate(key_sp=paste(Fam, species, sep = "_")) #add key to species name to get into toolbox as category
coast_pelagic<-mutate(coast_pelagic,Fam=str_sub(family, start=1L, +4)) %>% #trim Family name to first 4 letters to use as family code
  mutate(key_sp=paste(Fam, species, sep = "_")) #add key to species name to get into toolbox as category
reef<-mutate(reef,Fam=str_sub(family, start=1L, +4)) %>% #trim Family name to first 4 letters to use as family code
  mutate(key_sp=paste(Fam, species, sep = "_")) #add key to species name to get into toolbox as category
pelagic<-mutate(pelagic,Fam=str_sub(family, start=1L, +4)) %>% #trim Family name to first 4 letters to use as family code
  mutate(key_sp=paste(Fam, species, sep = "_")) #add key to species name to get into toolbox as category
deep<-select(deep, year, rgn_id,key_sp,catch)
coast_pelagic<-select(coast_pelagic,rgn_id, year,key_sp,catch)
reef<-select(reef,rgn_id, rgn_id,year,key_sp,catch)
pelagic<-select(pelagic, rgn_id,year,key_sp,catch)


#set up sus scores with same key_sp
data_file  <- file.path(dir_layers, 'fis_sus_scores_updated_mhi2017.csv')
d <- readr::read_csv('fis_sus_scores_updated_mhi2017.csv')
str(d)
d<-mutate(d,Fam=str_sub(Family, start=1L, +4)) %>% #trim Family name to first 4 letters to use as family code
  mutate(key_sp=paste(Fam, species, sep = "_")) #add key to species name to get into toolbox as category
d<-select(d, rgn_id,year,key_sp,score)

## save this local data layer in "layers" folder with the same naming convention as above format
dir_layers <- file.path('~/documents/github/mhi/region2017/layers')
readr::write_csv(pelagic, file.path(dir_layers, "fis_pelagic_catch_mhi2017.csv"))
readr::write_csv(deep, file.path(dir_layers, "fis_deep_catch_mhi2017.csv"))
readr::write_csv(coast_pelagic, file.path(dir_layers, "fis_coast_pelagic_catch_mhi2017.csv"))
readr::write_csv(reef, file.path(dir_layers, "fis_reef_catch_mhi2017.csv"))
readr::write_csv(d, file.path(dir_layers, "fis_sus_mhi2017.csv"))

## You will see a notification in the "Git" window in RStudio once the layer is saved successfully.
#dont forgt to register the layer in layers.csv

