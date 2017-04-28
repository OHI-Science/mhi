#Livelihoods and economies prep
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
library(dplyr)
library(plyr)
library(reshape2)
library(plyr)

dir_layers <- file.path('~/github/mhi/prep/LIV')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'le_ENOW_self.csv')
jobs_self_county <- readr::read_csv(data_file)


data_file  <- file.path(dir_layers, 'le_ENOW.csv') #note double underscore between ENOW and state
jobs_county <- readr::read_csv(data_file)


data_file  <- file.path(dir_layers, 'LE_ENOW__state.csv') #note double underscore between ENOW and state
jobs_state <- readr::read_csv(data_file)


jobs_county_all<-join(jobs_county, jobs_self)#joins data sets for self employed and state employed

#formatting
jobs_county_all$Employment[jobs_county_all$Employment==-9999]=NA
jobs_county_all$Wages[jobs_county_all$Wages==-9999]=NA
jobs_county_all$GDP[jobs_county_all$GDP==-9999]=NA
jobs_county_all$wage_per_employ[jobs_county_all$wage_per_employ==1]=NA
jobs_county_all$wage_per_employ[jobs_county_all$wage_per_employ=="#DIV/0!"]=NA
GDP<-ddply(jobs_county_all, .(rgn_id, Year, OceanSectorId, OceanSector), summarize, gdp_usd=sum(GDP))
jobs<-ddply(jobs_county_all, .(rgn_id, Year, OceanSectorId, OceanSector), summarize, jobs=sum(Employment))
wages<-ddply(jobs_county_all, .(rgn_id, Year, OceanSectorId, OceanSector), summarize, wages=sum(Wages))
per_person_wages<-ddply(jobs_county_all, .(rgn_id, Year, OceanSectorId, OceanSector), summarize, wages=sum(Wages), jobs=sum(Employment))
per_person_wages<-ddply(per_person_wages, .(rgn_id, Year, OceanSectorId, OceanSector), mutate, value=wages/jobs)
allsectors<-subset(jobs_county_all, OceanSectorId==9)
sector_weight<-ddply(jobs_county_all, .(rgn_id, Year), summarize, totaljobs=sum(Employment))#proportion jobs within each sector
#need to get proportion of sectors jobs by allsector jobs

dir_layers <- file.path('~/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(fp, file.path(dir_layers, ".csv"))



