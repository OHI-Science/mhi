#score output tables for report documentation
#Feb 12, 2018
#Eva

scores <- readr::read_csv('scores.csv')
library(reshape)
md_scores<-cast(scores, goal+region_id~dimension)
md_scores<-md_scores[, c(1,2,7,8,4,5,3,6)]
