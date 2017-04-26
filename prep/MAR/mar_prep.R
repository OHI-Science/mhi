#mariculture prep
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
library(dplyr)
library(plyr)
library(reshape2)
library(plyr)

dir_layers <- file.path('~/github/mhi/prep/mar')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'mar_fishponds.csv') #fishponds from 1994 layer used to get historical extent
fp <- readr::read_csv(data_file)

fp<-subset(fp, select=c("rgn_id", "Area_acres"))
fp$rgn_id<-as.factor(fp$rgn_id)

fp<-as.data.frame(fp)
fp<-ddply(fp, .(rgn_id), summarize, area_acres=sum(Area_acres))
fp<-na.omit(fp)

data_file  <- file.path(dir_layers, 'mar_fishponds_current.csv')#remaining fishponds
fpc <- readr::read_csv(data_file)
fpc$rgn_id<-as.factor(fpc$rgn_id)

fpc<-ddply(fpc, .(rgn_id), summarize, area_acres=sum(Area_acres))
fpc<-na.omit(fpc)



dir_layers <- file.path('~/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(fp, file.path(dir_layers, "mar_fishpond_historic_mhi2017.csv"))
readr::write_csv(fpc, file.path(dir_layers, "mar_fishpond_current_mhi2017.csv"))
