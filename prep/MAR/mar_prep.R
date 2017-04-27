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

data_file  <- file.path(dir_layers, 'mar_operations_census_mhi2017.csv')
op <- readr::read_csv(data_file)
str(op)
#need to gap fill for missing years
op_shellfish<-subset(op, commodity!="FOOD FISH")
op_shellfish<-ddply(op_shellfish, .(year,rgn_id), summarize, value=sum(value))
op_shellfish$commodity<-"shellfish"
op_fish<-subset(op, commodity=="FOOD FISH")
op_fish<-ddply(op_fish, .(year,rgn_id), summarize, value=sum(value))
op_fish$commodity<-"finfish"
op<-rbind(op_fish, op_shellfish)


dir_layers <- file.path('~/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(fp, file.path(dir_layers, "mar_fishpond_historic_mhi2017.csv"))
readr::write_csv(fpc, file.path(dir_layers, "mar_fishpond_current_mhi2017.csv"))
readr::write_csv(op, file.path(dir_layers, "mar_operations_census_mhi2017.csv"))

dir_layers <- file.path('~/github/mhi/region2017/layers')
data_file  <- file.path(dir_layers, 'mar_operations_census_mhi2017.csv') #
op <- readr::read_csv(data_file)
str(op)


#assign harvest estimate by regression of porudciton ops per island weight of total harvest per commodity
#rgn_1
harv_1f<-subset(op, rgn_id==1 & commodity=="finfish")
m1<-lm(value~year, harv_1f)
plot(value~year, harv_1f)
harv_1f$est_value<-m1$coeff[1]+harv_1f$year*m1$coeff[2]

harv_1s<-subset(op, rgn_id==1 & commodity=="shellfish")
m1s<-lm(value~year, harv_1s)
plot(value~year, harv_1s)
harv_1s$est_value<-m1s$coeff[1]+harv_1s$year*m1s$coeff[2]

#rgn2
harv_2f<-subset(op, rgn_id==2 & commodity=="finfish")
m2<-lm(value~year, harv_2f)
plot(value~year, harv_2f)
harv_2f$est_value<-m2$coeff[1]+harv_2f$year*m2$coeff[2]

harv_2s<-subset(op, rgn_id==2 & commodity=="shellfish")
m2s<-lm(value~year, harv_2s)
plot(value~year, harv_2s)
harv_2s$est_value<-m2s$coeff[1]+harv_2s$year*m2s$coeff[2]

#rgn3
harv_3f<-subset(op, rgn_id==3 & commodity=="finfish")
m3<-lm(value~year, harv_3f)
plot(value~year, harv_3f)
harv_3f$est_value<-m3$coeff[1]+harv_3f$year*m3$coeff[2]

harv_3s<-subset(op, rgn_id==3 & commodity=="shellfish")
m3s<-lm(value~year, harv_3s)
plot(value~year, harv_3s)
harv_3s$est_value<-m3s$coeff[1]+harv_3s$year*m3s$coeff[2]

#rgn4
harv_4f<-subset(op, rgn_id==4 & commodity=="finfish")
m4<-lm(value~year, harv_4f)
plot(value~year, harv_4f)
harv_4f$est_value<-m4$coeff[1]+harv_4f$year*m4$coeff[2]

harv_4s<-subset(op, rgn_id==4 & commodity=="shellfish")
m4s<-lm(value~year, harv_4s)
plot(value~year, harv_4s)
harv_4s$est_value<-m4s$coeff[1]+harv_4s$year*m4s$coeff[2]

op_est<-rbind(harv_1f, harv_1s, harv_2f, harv_2s, harv_3f, harv_3s, harv_4f, harv_4s)


op_est <- op_est %>%
  mutate(value = ifelse(is.na(value),est_value,value)) # fill in data gaps for missing years

readr::write_csv(op_est, file.path(dir_layers, "mar_operations_census_mhi2017.csv"))
