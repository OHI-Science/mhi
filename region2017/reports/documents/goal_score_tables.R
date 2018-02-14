#score output tables for report documentation
#Feb 12, 2018
#Eva
setwd("~/Documents/github/mhi/region2017")
scores <- readr::read_csv('scores.csv')

#reshape table
library(reshape)
md_scores<-cast(scores, goal+region_id~dimension)

#reorder table
md_scores<-md_scores[, c(1,2,7,8,4,5,3,6)]

#captialize column names
library(data.table)
md_scores<-setnames(md_scores, old=c("goal", "region_id", "status","trend","pressures","resilience", "future", "score"),
                      new=c("Goal", "Region", "Status","Trend","Pressures","Resilience", "Future", "Score"))
str(md_scores)

#change rgn_id to region names
md_scores$Region<-as.factor(md_scores$Region)
levels(md_scores$Region)<-list("Index"=0,"Hawaii"=1, "Maui Nui"=2, "Oahu"=3, "Kauai & Niihau"=4)

setwd("~/Documents/github/mhi/region2017/reports/documents")
readr::write_csv(md_scores, 'scores_table.csv')
