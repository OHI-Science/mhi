#CW prep
#updated Feb 2018
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/documents/github/mhi/prep/CW')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
#old data file # skip to below for updated data
data_file  <- file.path(dir_layers, 'cw_full_data.csv')
d <- readr::read_csv(data_file)

str(d)
d$sea_water_quality_alert<-as.integer(d$sea_water_quality_alert)


library(plyr)#install.packages('plyr')
d_alerts<-ddply(d, .(rgn_id, Year),summarize,  alerts=sum(sea_water_quality_alert,  na.rm=TRUE))

d_alert<-na.omit(d_alerts)
dir_layers <- file.path('~/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(d_alert, file.path(dir_layers, "cw_alerts_mhi2017.csv"))

#updated CW alerts data# FEb 2018
dir_layers <- file.path('~/documents/github/mhi/prep/CW')
data_file  <- file.path(dir_layers, 'cw_alerts.csv')
d <- readr::read_csv('cw_alerts.csv')
d<-select(d, rgn_id, year, Date, date_value,sea_water_quality_alert)
str(d)
d$sea_water_quality_alert<-as.integer(d$sea_water_quality_alert)


library(plyr)#install.packages('plyr')
d_alerts<-ddply(d, .(rgn_id, year, Date),summarize,  alerts=sum(sea_water_quality_alert,  na.rm=TRUE))
d_alerts$alerts<-d_alerts$alerts==0|is.na(d_alerts$alerts)
d_alerts<-subset(d_alerts, alerts==FALSE)
d_alerts<-d_alerts %>%
  group_by(rgn_id, year) %>%
  summarize(alerts=length(alerts)) #data file is now the # of days per region that had at least one
#seawater quality alert (can be multiple beaches)

dir_layers <- file.path('~/documents/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(d_alerts, file.path(dir_layers, "cw_alerts_mhi2017.csv"))
