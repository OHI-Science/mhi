#T prep
#updated March 2018 #eva

## setup: libraries, file paths ----

library(tidyverse) # install.packages('tidyverse')
library(plyr) # install.packages('plyr')
dir_layers <- file.path('~/documents/github/mhi/prep/TR')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 't_visitor_gdp_mhi2017.csv') #older file

data_file  <- file.path(dir_layers, 't_visitor_expenditures_mhi2017.csv') #updated to real dollar visitor expenditures

d <- readr::read_csv(data_file)

data_file  <- file.path(dir_layers, 't_average_visitors_mhi2017.csv')
v <- readr::read_csv(data_file)

library(plyr)#install.packages('plyr')
t_economic<-join(d,v, by=c("year"))
t_economic$county_gdp<-t_economic$percent/100*t_economic$real_dollars #the visitor created gdp for Hawaii weighted to county estimates by the average number of visitor by county by day
growth<-subset(t_economic, select=c("county_gdp","year","rgn_id"))


dir_layers <- file.path('~/documents/github/mhi/region2017/layers')
#create the data layer
readr::write_csv(growth, file.path(dir_layers, "t_growth_mhi2017.csv"))
