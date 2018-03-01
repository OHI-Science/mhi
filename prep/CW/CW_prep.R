#CW prerp
## setup: libraries, file paths ----
library(tidyverse) # install.packages('tidyverse')
dir_layers <- file.path('~/github/mhi/prep/CW')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
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

#update CW alerts
dir_layers <- file.path('~/documents/github/mhi/prep/CW')
data_file  <- file.path(dir_layers, 'cw_alerts.csv')
d <- readr::read_csv('cw_alerts.csv')
d<-select(d, rgn_id, year, Date, date_value,sea_water_quality_alert)
str(d)
d$sea_water_quality_alert<-as.integer(d$sea_water_quality_alert)


library(plyr)#install.packages('plyr')
d_alerts<-ddply(d, .(rgn_id, Date),summarize,  alerts=sum(sea_water_quality_alert,  na.rm=TRUE))
d_alerts<-count(d_alerts, )

