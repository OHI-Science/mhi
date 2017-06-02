#HAB prerp
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/github/mhi/prep/HAB')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'hab_wetlands_mhi2017.csv')
d <- readr::read_csv(data_file)
d <- ddply(d, .(year,rgn_id), summarize, km2=sum(km2))
d$habitat<-"wetlands"
d <- ddply(d, .(year,rgn_id), summarize, km2=sum(km2))


#d$trend<-ddply(d, .(rgn_id, year), mutate, trend=c(diff(km2)))


d_2010<-subset(d, year=="2010") #current wetland extent

readr::write_csv(d_2010, file.path(dir_layers, "wetlands_2010_extent.csv")) #wetland 2010 habitat extent

readr::write_csv(d, file.path(dir_layers, "wetlands_trend.csv")) #wetland 2010 habitat extent

#need condition and trend data for wetlands


data_file  <- file.path(dir_layers, 'hab_extent.csv') #combined habtat layer
all_hab <- readr::read_csv(data_file)

library(plyr)#install.packages('plyr')
hab<-subset(all_hab, habitat=="reef"| habitat=="beach")


cp_hab<-merge(d_2010,hab, by="rgn_id")

cp_hab <- select(cp_hab, rgn_id,habitat.y,km2.y,reference)


dir_layers <- file.path('~/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(cp_hab, file.path(dir_layers, "cp_hab.csv"))
