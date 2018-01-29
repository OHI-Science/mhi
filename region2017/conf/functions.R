FIS = function(layers, status_year=2016){
  #catch data
  reef = SelectLayersData(layers, layers='fis_reef_catch', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      key_sp = category,
      year,
      catch          = val_num)

 deep = SelectLayersData(layers, layers='fis_deep_catch', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      key_sp = category,
      year,
      catch          = val_num)

  coast_pelagic = SelectLayersData(layers, layers='fis_coast_pelagic_catch', narrow = TRUE) %>%
   select(
     rgn_id    = id_num,
     key_sp = category,
     year,
     catch          = val_num)

  pelagic = SelectLayersData(layers, layers='fis_pelagic_catch', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      key_sp = category,
      year,
      catch          = val_num)

  # stock assessment score data
  b = SelectLayersData(layers, layer='fis_sus', narrow = TRUE) %>% #has rgn_id (will also need to use fish_sus for fisheries that are not separated out into regions)
    select(
     rgn_id         = id_num,
      key_sp      = category,
      year,
      value           = val_num)

  reef <- reef %>%
    mutate(key_sp=as.character(key_sp))%>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(rgn_id, year, key_sp, catch)


  pelagic <-pelagic %>%
    mutate(key_sp=as.character(key_sp))%>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(rgn_id, year, key_sp, catch)

  coast_pelagic<- coast_pelagic %>%
    mutate(key_sp=as.character(key_sp))%>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(rgn_id, year, key_sp, catch)

  deep <- deep %>%
    mutate(key_sp=as.character(key_sp))%>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(rgn_id, year, key_sp, catch)


  # general formatting:
  b <- b %>%
    mutate(key_sp=as.character(key_sp))%>%
    mutate(value = as.numeric(value)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(rgn_id, year, key_sp, value)


#separate Family 4 letter code back out to use to apply median family sustainability scores for species with no formal stock assessment
  reef<-reef %>%
    mutate(Fam=str_sub(key_sp, start=1L, +4))%>%
    mutate(code="reef")
  pelagic<-pelagic %>%
    mutate(Fam=str_sub(key_sp, start=1L, +4))%>%
    mutate(code="pelagic")
  deep<-deep %>%
    mutate(Fam=str_sub(key_sp, start=1L, +4))%>%
    mutate(code="deef")
  coast_pelagic<-coast_pelagic %>%
    mutate(Fam=str_sub(key_sp, start=1L, +4))%>%
    mutate(code="coast_pelagic")
  fis_data<-coast_pelagic %>%
    rbind(pelagic)
  fis_data<-fis_data %>%
   rbind(deep)
  fis_data<-fis_data %>%
     rbind(reef)

  # ---
  ## Calculate scores for Bbmsy values
  # ---
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(b$value < lowerBuffer, b$value,
                   ifelse (b$value >= lowerBuffer & b$value <= upperBuffer, 1, NA))

  #removed unnderharvesting penalty
  # b$score = ifelse(!is.na(b$score), b$score,
  #                   ifelse(1 - alpha*(b$value - upperBuffer) > beta,
  #                          1 - alpha*(b$value - upperBuffer),
  #                          beta))
  b$score = ifelse(!is.na(b$score), b$score,
                   ifelse(1 - alpha*(b$value - upperBuffer) > beta,
                          1,b$score))
  # ---
  # STEP 1. Merge the b/bmsy data with catch data #

  fis_data <- fis_data %>%
    left_join(b) %>%
    select(rgn_id, year, code, catch, key_sp, Fam, score)


  sum(is.na(fis_data$score))
  # ---
  #Estimate scores for taxa without stock assessment values
  # Median score of other fish in the taxon group ("Fam"and if no stock assessments for the Fam then to code (group) bottom, pelagic, or reef fish) is an estimate


  ## this takes the median score within each family and applies it for species that did not have stock assessment score
  data_fis_gf <- fis_data %>%
    dplyr::group_by(year, Fam) %>% #Fam is the code for family and code is the taxon key to separate out bottom, pelagics, coastal pelagics and reef fish
    dplyr::mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    dplyr::mutate(Mean_score = mean((score), na.rm=TRUE)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(Median_score = ifelse(is.na(score), Median_score, score))%>%
    dplyr::mutate(Mean_score = ifelse(is.na(score), Mean_score, score))

  ## this takes the median score for each fishery (bottom, pelagic, coastal pelagic, reef) and applies it for Families that did not have a median stock assessment score
  data_fis_gf <- data_fis_gf %>%
    dplyr::group_by(year, code) %>% #Fam is the code for family and code is the taxon key to separate out bottom, pelagics, coastal pelagics and reef fish
    dplyr::mutate(code_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    dplyr::mutate(Mean_code_score = mean((score), na.rm=TRUE)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(score_gf = ifelse(is.na(Median_score), code_score, Median_score)) %>%
    dplyr::mutate(score_gf = ifelse(is.na(score_gf), 1, score_gf))%>%
    dplyr::mutate(Mean_score = ifelse(is.na(Mean_score), Mean_code_score, Mean_score))

    #note no assessments have been done for species that we classified as coastal pelagics
# could apply reef fish median scores


  #didn't apply a penalty to taxon reported to family since there are only two - misc parrots and misc ulua
  #instead used the lowest stock assessment value reported for the family and applied to the category

  #documentation for scores that were gap filled
  data_fis_gf <- data_fis_gf %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "formal stock assessment"))


  #select data needed for status
  #use this code for median gap filled scores
  #data_fis_gf <- data_fis_gf %>%
  #  select(rgn_id, key_sp, code, year, catch, score=score_gf, score_gapfilled)

  #use this code for mean gap filled scores
  data_fis_gf <- data_fis_gf %>%
    select(rgn_id, key_sp, code, year, catch, score=Mean_score, score_gapfilled)

  str(data_fis_gf) #check data
  length(unique(data_fis_gf$code)) #check that there are no typos in the code should be 4 levels

  ##Need to separate back out each fishery and get status and score separately
  reef<-data_fis_gf %>%
    subset(code=="reef")

  bottom<-data_fis_gf %>%
    subset(code=="deef")

  pelagic<-data_fis_gf %>%
    subset(code=="pelagic")

  coastal_pelagic<-data_fis_gf %>%
    subset(code=="coast_pelagic")


## get fishery total catch tables and proportion of total catch
 #reef <- reef %>%
#    group_by(rgn_id, year) %>% #summarize catch data
#    dplyr::summarize(mean_score=mean(score), catch_total = sum(catch)) %>%
#    ungroup()
#  reef <- reef %>%
#    group_by(rgn_id) %>% #summarize catch data
#    dplyr::summarize(catch_total = mean(catch_total), mean_score=mean(mean_score))%>%
#    dplyr::mutate(fishery="reef")

#  bottomfish <- bottom %>%
#    group_by(rgn_id, year) %>% #summarize catch data
#    dplyr::summarize(mean_score=mean(score), catch_total = sum(catch)) %>%
#    ungroup()
#  bottomfish <- bottomfish %>%
#    group_by(rgn_id) %>% #summarize catch data
#    dplyr::summarize(catch_total = mean(catch_total), mean_score=mean(mean_score))%>%
#    dplyr::mutate(fishery="bottomfish")

#  coastal_pelagic <- coastal_pelagic %>%
#    group_by(rgn_id, year) %>% #summarize catch data
#    dplyr::summarize(catch_total = sum(catch)) %>%
#    ungroup()
#  coastal_pelagic <- coastal_pelagic %>%

#    group_by(rgn_id) %>% #summarize catch data
#    dplyr::summarize(catch_total = mean(catch_total), mean_score=NA)%>%
#    dplyr::mutate(fishery="coastal_pelagic")

#pelagic <- pelagic %>%
#    group_by(year) %>% #summarize catch data
#    dplyr::summarize(mean_score=mean(score), catch_total = sum(catch))
#  pelagic <- pelagic %>%
#    dplyr::summarize(mean_score=mean(mean_score), catch_total = mean(catch_total)) %>%
#    dplyr::mutate(fishery="pelagic", rgn_id=1)

#  summary<-rbind(reef,bottomfish,coastal_pelagic,pelagic)
# readr::write_csv(summary, file.path("~/documents/github/mhi/region2017/reports/documents", "fishery_summary_table.csv"))
##end fishery summary section


##use code below to get summary tables for species catch and scores
#  reef<-data_fis_gf %>%
#    subset(code=="reef")

#   reef_species_average_scores <- reef %>%
#    group_by(year,key_sp) %>% #summarize catch data
#    dplyr::summarize(catch = sum(catch), mean_score=mean(score))%>%
#    dplyr::ungroup()

#  reef_species_average_scores <- reef_species_average_scores %>%
#     group_by(key_sp) %>% #summarize catch data
#     dplyr::summarize(mean_catch = mean(catch), mean_score=mean(mean_score))

#     str(reef_species_average_scores)
#     readr::write_csv(reef_species_average_scores, file.path("~/documents/github/mhi/region2017/reports/documents", "reef_species_average_scores.csv"))


#  coastal_pelagic<-data_fis_gf %>%
#        subset(code=="coast_pelagic")
#
#       coastal_pelagic_species_average_scores <- coastal_pelagic %>%
#        group_by(year,key_sp) %>% #summarize catch data
#        dplyr::summarize(catch = sum(catch))%>%
#        dplyr::ungroup()
#
#      coastal_pelagic_species_average_scores <- coastal_pelagic_species_average_scores %>%
#         group_by(key_sp) %>% #summarize catch data
#         dplyr::summarize(mean_catch = mean(catch), mean_score=NA)
#
#       str(coastal_pelagic_species_average_scores)
#       readr::write_csv(coastal_pelagic_species_average_scores, file.path("~/documents/github/mhi/region2017/reports/documents", "coastal_pelagic_species_average_scores.csv"))

  ##end summary code section


  # ---
  #Calculate status for each region for the reef fishery only (all other fisheries get one score for the entire assessment area)
  # ---
  #  To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year


   #code to get wieght of wild caught fisheries to compare to mariculture to weight final fp score
  #total_fisheries_c<-status_data %>%
  #dplyr::group_by(rgn_id) %>%
  #summarize(Total=sum(SumCatch))


#reef
  # sum of mean catch of all species in region/year
  reef <- reef %>%
    group_by(year, rgn_id, key_sp) %>% #summarize catch data
    dplyr::summarize(catch = sum(catch), mean_score=mean(score))


  reef <- reef %>%
    #dplyr::mutate(catch_w=ifelse(key.x=="CHCR", catch*value, catch))%>%
    group_by(rgn_id,year)%>%
    dplyr::mutate(SumCatch = sum(catch))%>%
    dplyr::mutate(wprop = catch/SumCatch)


  #to get sus scores for reef by region
  reef <- reef %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::summarize(status = prod(mean_score^wprop)) %>%
    ungroup()




#bottom

  bottom <- bottom %>%
    group_by(year,  key_sp) %>% #summarize catch data
    dplyr::summarize(catch = sum(catch), mean_score=mean(score))

  bottom <- bottom %>%
    group_by(year)%>%
    dplyr::mutate(SumCatch = sum(catch))%>%
    dplyr::mutate(wprop = catch/SumCatch)
  bottom_status <- bottom %>%
    dplyr::group_by( year) %>%
    dplyr::summarize(status = prod(mean_score^wprop)) %>%
    ungroup()

  ##use code below to get summary tables for species catch and scores
  #bottom<-data_fis_gf %>%
  #  subset(code=="deef")

  # bottom_species_average_scores <- bottom %>%
  #  group_by(year,key_sp) %>% #summarize catch data
  #  dplyr::summarize(catch = sum(catch), mean_score=mean(score))%>%
  #  dplyr::ungroup()

  # bottom_species_average_scores <- bottom_species_average_scores %>%
  #   group_by(key_sp) %>% #summarize catch data
  #   dplyr::summarize(mean_catch = mean(catch), mean_score=mean(mean_score))

  #   str(bottom_species_average_scores)
  #   readr::write_csv(bottom_species_average_scores, file.path("~/documents/github/mhi/region2017/reports/documents", "bottom_species_average_scores.csv"))
  ##end summary code section

  #pelagic
  pelagic <- pelagic %>%
    group_by(year,  key_sp) %>% #summarize catch data
    dplyr::summarize(catch = sum(catch), mean_score=mean(score))

  pelagic <- pelagic %>%
    group_by(year)%>%
    dplyr::mutate(SumCatch = sum(catch))%>%
    dplyr::mutate(wprop = catch/SumCatch)
  pelagic_status <- pelagic%>%
    dplyr::group_by( year) %>%
    dplyr::summarize(status = prod(mean_score^wprop)) %>%
    ungroup()

  ##use code below to get summary tables for species catch and scores
  #pelagic<-data_fis_gf %>%
  #  subset(code=="pelagic")

  # pelagic_species_average_scores <- pelagic %>%
  #  group_by(year,key_sp) %>% #summarize catch data
  #  dplyr::summarize(catch = sum(catch), mean_score=mean(score))%>%
  #  dplyr::ungroup()

  #pelagic_species_average_scores <- pelagic_species_average_scores %>%
  #   group_by(key_sp) %>% #summarize catch data
  #   dplyr::summarize(mean_catch = mean(catch), mean_score=mean(mean_score))

  #  str(pelagic_species_average_scores)
  #   readr::write_csv(pelagic_species_average_scores, file.path("~/documents/github/mhi/region2017/reports/documents", "pelagic_species_average_scores.csv"))
  ##end summary code section


  #no assessments for coastal pelagics so can not score them

  # ---
  # Join fishies back together to get region yearly status and trend
  # ---
  region_1<-pelagic_status %>%
    rbind(bottom_status) %>%
    mutate(rgn_id=1)

  region_2<-pelagic_status %>%
    rbind(bottom_status) %>%
    mutate(rgn_id=2)

  region_3<-pelagic_status %>%
    rbind(bottom_status) %>%
    mutate(rgn_id=3)

  region_4<-pelagic_status %>%
    rbind(bottom_status) %>%
    mutate(rgn_id=4)

  status_data<- region_1 %>%
    rbind(region_2) %>%
    rbind(region_3) %>%
    rbind(region_4) %>%
    rbind(reef)

  str(status_data)
  status_data<-as.data.frame(status_data)

  status_data<-status_data %>%
    dplyr::group_by(year, rgn_id) %>%
    dplyr::summarize(status=mean(status))%>%
    dplyr::ungroup()

  status <-  status_data %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    select(region_id=rgn_id, score, dimension)



    trend_years <- (status_year-4):(status_year)###need to set status year or adjust this year value for each assessment

  first_trend_year <- min(trend_years)

  status_data<-as.data.frame(status_data)

  trend <- status_data %>%
    filter(year %in% trend_years) %>%
    dplyr::group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    dplyr::summarize(region_id = rgn_id,
                     score = round(coef(mdl)['year']/adjust_trend * 5, 4),
                     dimension = 'trend') %>%
    ungroup() %>%
    dplyr::mutate(score = ifelse(score > 1, 1, score)) %>%
    dplyr::mutate(score = ifelse(score < (-1), (-1), score))

  # assemble dimensions
  scores <- rbind(status, trend) %>%
    mutate(goal='FIS') %>%
    filter(region_id != 255)
  scores <- data.frame(scores)

  return(scores)
}

MAR = function(layers){
#mariculture operators and yield
  mar_harv <- SelectLayersData(layers, layers='mar_harvest', narrow = TRUE) %>%
    select(rgn_id=id_num, commodity=category, year, lbs=val_num) #data for each rgn_id is acctually sums for entire state - this is how it is reported - need to weight by # of operators to get rgn level data


  mar_operations <- SelectLayersData(layers, layers='mar_operations', narrow = TRUE) %>%
    select(rgn_id=id_num, year, commodity=category, value=val_num)

#fishpond number and area
  mar_fp_current <- SelectLayersData(layers, layers='mar_fishpond_current', narrow = TRUE) %>%
    select(rgn_id=id_num, Area_acres=val_num)


  mar_fp_number<- SelectLayersData(layers, layers='mar_fishpond_numbers', narrow = TRUE) %>%
    select(rgn_id=id_num, current=val_num)


  mar_fp_health<- SelectLayersData(layers, layers='mar_fishpond_health', narrow = TRUE) %>% #potential of fishponds/mariculture
    select(rgn_id=id_num, status=val_num)


  #join area and fp potential data
  mar_fp<-mar_fp_current %>%
    left_join(mar_fp_number, by=c('rgn_id'))

  mar_fp<-mar_fp %>%
    left_join(mar_fp_health, by=c('rgn_id'))

  mar_fp<-mar_fp%>%
    dplyr::mutate(total_area=sum(Area_acres))

  mar_fp<-mar_fp%>%
    dplyr::mutate(value=status/.30)%>%
    dplyr::mutate(value= ifelse(value>1, 1, value))

  #join operator and harvest data
#determine % of production by rgn as an estimate based on #operators per island/#state operators

  mar_d<-mar_harv %>%
    left_join(mar_operations, by=c('year','commodity','rgn_id'))

  # mar_d<- ddply(mar_d, .(commodity, year), mutate, sum_value=sum(value)) # @jules32 commenting out in favor of dplyr
  mar_d<- mar_d %>%
    arrange(commodity, year) %>%
    mutate(sum_value = sum(value))


  mar_d$prop<-(mar_d$value/mar_d$sum_value)
  mar_d$est_harv<-mar_d$prop*mar_d$lbs

   #to get total lbs for mariculture weight of pf
 # total_mar<-as.data.frame(mar_d)
  #total_mar<- total_mar%>%
  #  group_by(rgn_id) %>%
  #  summarize(sum_value = sum(est_harv))



  #  mariculture harvest * sustainability coefficient (i.e risk)
  risk=0.67 #allcultured species 0.67
  risks=0.5 # average of Trujilo scores with added scores for biosecurity threat for Hawaii for shellfish
  riskf=0.44 # average of Trujilo scores with added scores for biosecurity threat for Hawaii for finfish (includes tilapia because can not separate out tilapia farms given data)
#mar_ds<-subset(mar_d, commodity=="shellfish")
#mar_df<-subset(mar_d, commodity=="finfish")
#mar_ds$estimate_s<-mar_ds$est_harv*risks
#mar_df$estimate_f<-mar_df$est_harv*riskf
#mar_data<-mar_df%>%
#    left_join(mar_ds, by=c('rgn_id', 'year','commodity'))


  mar_d<-mar_d%>%
    dplyr::group_by(rgn_id, year)%>%
    dplyr::mutate(total_harvest=sum(lbs))
    #dplyr::summarize(score = est_harv*risk)
    #dplyr::mutate(score=est_harv/total_harvest)#spatial reference score
    #dplyr::ungroup()
  mar_d<-mar_d%>%
    dplyr::group_by(rgn_id,commodity)%>%
    dplyr::mutate(ref=max(est_harv))%>%
    dplyr::ungroup()

  mar_d$score<-(mar_d$est_harv/mar_d$ref)
  mar_d<-mar_d %>%
    dplyr::group_by(rgn_id,year) %>%
    dplyr::summarize(score=mean(score))
  mar_d$score<-mar_d$score*risk

  #if want to separate out finfish and shellfish risk scores
  #mar_ds<-subset(mar_d, commodity=="shellfish")
  #mar_df<-subset(mar_d, commodity=="finfish")
  #mar_ds$score<-mar_ds$score*risks
  #mar_df$score<-mar_df$score*riskf
  #mar_data<-mar_df%>%
  #    left_join(mar_ds, by=c('rgn_id', 'year','commodity'))

# aggregate all weighted timeseries per region,
 #ry<-ddply(mar_harv, .(rgn_id, year), summarize, sust_tones_sum=sum(estimate))
#  max_harv<-max(ry$sust_tones_sum)
 #need to deside if including population as part of the model
# ry = mar_harv %>%
#    group_by(rgn_id, year) %>%
#    summarize(sust_tonnes_sum = sum(estimate)) %>%  #na.rm = TRUE assumes that NA values are 0
   # left_join(popn_inland25mi, by = c('rgn_id','year')) %>%
   # mutate(mar_pop = sust_tonnes_sum / popsum) %>%
#    ungroup()


  # get reference quantile based on argument years
#  ref_95pct_data <- ry %>%
#    filter(year <= status_year)

#  ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)

  # identify reference rgn_id
#  ry_ref = ref_95pct_data %>%
#    arrange(mar_pop) %>%
#    filter(mar_pop >= ref_95pct)
#  message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct)) # rgn_id 25 = Thailand
#  message(sprintf('95th percentile rgn_id for MAR ref pt is: %s\n', ry_ref$rgn_id[1])) # rgn_id 25 = Thailand

#  rp <- read.csv(file.path( 'temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
#    rbind(data.frame(goal = "MAR", method = "temporal maximum per region times risk score",
#                     reference_point = paste0("region id: ", ry_ref$rgn_id[1], ' value: ', ref_95pct)))
#  write.csv(rp, file.path( 'temp/referencePoints.csv'), row.names=FALSE)


 # ry = ry %>%
#    mutate(status = ifelse(mar_pop / ref_95pct > 1,
 #                          1,
#                           mar_pop / ref_95pct))



  status_year=max(mar_d$year) #@eschemmel: double-check that this is where you want to set the status year and not at the function call

  #merge fp and opperater data - mariculture score is an average of fp and opperators
  mar_data<-mar_d %>%
    left_join(mar_fp)%>%
    dplyr::mutate(score=(score+value)/2)%>%
    select(rgn_id, year, score)

  status <- mar_data %>%
    filter(year == status_year) %>%
    select(rgn_id, score) %>%
    mutate(status = round(score*100, 2))
 status <- data.frame(status)


  trend_years <- (status_year-4):(status_year)
  first_trend_year <- min(trend_years)

  # get MAR trend
  trend = mar_data %>%
    dplyr::group_by(rgn_id) %>%
    filter(year %in% trend_years) %>%
    do(mdl = lm(score ~ year, data=.),
       adjust_trend = .$score[.$year == first_trend_year]) %>%
    dplyr::summarize(rgn_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup()

  trend <- trend %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id = rgn_id, score = trend) %>%
    mutate(dimension = "trend")
  trend <- data.frame(trend)
  # return scores
  status = status %>%
    select(region_id = rgn_id,
           score     = status) %>%
    mutate(dimension='status')

  #str(status)
  #str(trend)

   scores<-
   rbind(trend,status) %>%
  mutate(goal='MAR')
  scores <- data.frame(scores)
  return(scores)
}


FP = function(layers, scores){

  # weights
  w <-  SelectLayersData(layers, layers='fp_wildcaught_weight_mhi', narrow = TRUE) %>%
    select(region_id = id_num,  w_FIS = val_num); head(w)

  # scores
 # s <- scores %>%
#    filter(goal %in% c('FIS', 'MAR')) %>%
#    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
#    left_join(w, by="region_id")  %>%
#    mutate(w_MAR = 1 - w_FIS) %>%
#    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))

  #to give equal weight to mar and fis
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - 0.5) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages due to potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  s <- s  %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO = function(layers){


  r <- SelectLayersData(layers, layers = 'ao_access_mhi2017', narrow=TRUE) %>%   ##shoreline access weights (removes MPAs and inaccessable coastlines)
    select(region_id=id_num, access=val_num)
  r <- na.omit(r)

  #ry <- SelectLayersData(layers, layers = 'ao_need', narrow=TRUE) %>% ##households that fish
  #  select(region_id = id_num, year, need=val_num) %>%
  #  left_join(r, by="region_id")

  ao_data <- SelectLayersData(layers, layers = 'ao_resource', narrow=TRUE) %>% ##resource measured as biomass of resource fish
    select(region_id=id_num,bio=val_num,trend=category)%>%
    left_join(r, by="region_id")

  ao_data$bio<-ao_data$bio/100

  #ao_data <- SelectLayersData(layers, layers = 'ao_residents', narrow=TRUE) %>% ##resident population to weight the need by rgn
  #  select(region_id=id_num, year,res=val_num)%>%
  #  left_join(ao_data, by = c("region_id", "year"))

  #ao_data <- na.omit(ao_data)
<<<<<<< HEAD

  #ao_data$fishers<-(ao_data$need/100)*(ao_data$res) #resident population * the percent of households that fish to estimate # of fishers

=======

  #ao_data$fishers<-(ao_data$need/100)*(ao_data$res) #resident population * the percent of households that fish to estimate # of fishers
>>>>>>> d04b0e3af3a2da164fcf5f0c17d67c0fd9a7ddb0

  # ao_data<-ddply(ao_data, .(year), summarize, total_fishers=sum(fishers))%>% #total fishers in Hawaii
  # left_join(ao_data, by="year")
  # ao_data<-ddply(ao_data, .(year), summarize, total_res=sum(res))%>% #total fishers in Hawaii
  #   left_join(ao_data, by="year")

  ## calculate total fishers and total res for each year
 # ao_data <- ao_data %>%
#    group_by(year) %>%
#    mutate(total_fishers = sum(fishers), # total fishers  in Hawaii
#          total_res = sum(res)) %>%     # total res
#    ungroup()

#  ao_data$need_score<-ao_data$fishers/ao_data$total_fishers # need standardized to number of fishers

 # ao_data$need_score<-ao_data$res*.106/ao_data$total_res
  #need standardized to poverty level


  # Hawaii model

  #ao_data <- ao_data %>%
  #  mutate(Du = (1-need/100) * (1-access)) %>%  #need based on # of fishers
  #           mutate(status = (1-Du) * bio)

  #ao_data <- ao_data %>%
  #  mutate(Du = (1-.106) * (1-access)) %>% #need based on poverty level
  #  mutate(status = (1-Du) * bio)


  #ao_sum<-ao_data %>%
  #  group_by(region_id) %>%
  #  summarize(bio_m=mean(bio), access_m=mean(access))
 #ao_data <-ao_data %>%
#    mutate(status=(access+bio)/2)

  #global model
# ry <- ry %>%
#    mutate(Du = (1 - need) * (1 - access)) %>%
#   mutate(status = (1 - Du) * Sustainability)

<<<<<<< HEAD
    # status
=======
  # status_year=2013 ## @eschemmel: this is overwriting the variable set in conf/goals.csv from 2014. @jules32 removed it from goals.csv and moves this 2013 value up to the function call <-  doublecheck to make sure this is what you want!
  # status
>>>>>>> d04b0e3af3a2da164fcf5f0c17d67c0fd9a7ddb0
  ao_data<-ao_data %>%
    dplyr::mutate(status=(bio+access)/2)

  r.status <- ao_data %>%
    #filter(year==status_year) %>%
    select(region_id, status) %>%
    mutate(status=status*100)

  # trend
  r.trend=ao_data %>%
    select(region_id, category=trend)


#trend is based on change in %fishers per year and % change in fish biomass #

  #r.trend<-SelectLayersData(layers, layers = 'ao_resource', narrow=TRUE)
  # str(r.trend)
 #r.trend$region_id=r.trend$id_num
  #r.trend$category<-r.trend$category+0.007 #access increased by 0.7% per year

  #trend_years <- (status_year-4):(status_year)
  #adj_trend_year <- min(trend_years)

  #r.trend = ao_data %>%
   # group_by(region_id) %>%
    #do(mdl = lm(status ~ year, data=., subset=year %in% trend_years),
    #   adjust_trend = .$status[.$year == adj_trend_year]) %>%
    #dplyr::summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    #ungroup() %>%
    #mutate(trend = ifelse(trend>1, 1, trend)) %>%
    #mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    #mutate(trend = round(trend, 4))



  ## reference points
  rp <- read.csv(file.path( 'temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "AO", method = "shoreline access, biomass/pristine biomass",
                     reference_point = NA))
  write.csv(rp, file.path('temp/referencePoints.csv'), row.names=FALSE)


  # return scores
  scores = r.status %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        select(region_id, score=category) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO') %>% # dlply(scores, .(dimension), summary)
    select(goal, dimension, region_id, score)


  # return all scores
  return(scores)
}


CS <- function(layers){

  ## read in layers
  extent <- layers$data[['cp_hab_extent_mhi2017']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  health <-  layers$data[['cp_hab_condition_mhi2017']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['cp_hab_trend_mhi2017']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))

  ## set ranks for each habitat
  #habitat.rank <- c('wetland' = 210)#if using more than 1 habitat assing carbon storage potential ranks
  habitat.rank <- c('wetland' = 1) #set to 1 because only one habitat being assessed

  ## limit to CS habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, file.path( 'temp/cs_hab_contributions.csv'), row.names=FALSE)

  ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  if (sum(d$km2, na.rm=TRUE) > 0){
    # status
    scores_CS <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(
        score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
        dimension = 'status') %>%
      ungroup()

    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    if (nrow(d_trend) > 0 ){
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend')) %>%
        ungroup()
    } else { # if no trend score, assign NA
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }

    ### output data file for checking and data review
    scores_check <- spread(scores_CS, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")

    ### end: output...

    scores_CS <- scores_CS %>%
      mutate(
        goal = 'CS') %>%
      select(region_id=rgn_id, goal, dimension, score)

    scores_CS$score[scores_CS$goal == 'CS']

  } else { ## else -- if sum(d$km2) is not greater than 0

     ## set status and trend to NA for all regions
     # message('CS status and trend are NA, consider removing goal if no CS habitats in assessment area')

     # rgns <-layers$data[['rgn_labels']]
     # scores_CS <- bind_rows(
      #  rgns %>%
      #  mutate(goal      = 'CS',
      #         dimension = 'status',
      #         score     = NA),
      #rgns %>%
      #  mutate(goal      = 'CS',
      #         dimension = 'trend',
      #         score     = NA)) %>%
      #  select(goal, dimension, region_id = rgn_id, score)




  } ## end -- if (sum(d$km2) > 0)

  ## reference points
  rp <- read.csv(file.path( 'temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, file.path( 'temp/referencePoints.csv'), row.names=FALSE)



  # return scores
  return(scores_CS)
}



CP <- function(layers){

  ## read in layers
  extent <- layers$data[['cp_hab_extent_mhi2017']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  health <-  layers$data[['cp_hab_condition_mhi2017']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['cp_hab_trend_mhi2017']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))


  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))

  ## set ranks for each habitat # they are all equally important in Hawaii MHI assessment so all = 1

  habitat.rank <- c('reef'            = 1,
                    'wetland'        = 1,
                    'beach' = 1) #need to look up reference for Hawaii coastal protection to justify weighting

  ## limit to CP habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
     extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  #this section needs to be updated - weight all habitats equally - each have a role in coastal protection
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    dplyr::group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, file.path('temp/cp_hab_contributions.csv'), row.names=FALSE)

   #status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  #if (sum(d$km2, na.rm=TRUE) > 0){
    # status

  #remove extent from model to set all habitats equal in weight = to add back in habitat weights then multiply score* extent
   scores_CP <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
     group_by(rgn_id) %>%
     #dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) /
      #                         (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
     dplyr::summarize(score = pmin(1, sum(rank * health, na.rm=TRUE) /
                              (sum(rank, na.rm=TRUE)) ) * 100) %>%

     mutate(dimension = 'status') %>%
      ungroup()


   #code was added to work around if else for scores_CP
   # scores_CP <- ddply(d, .(rgn_id),mutate, score = pmin(1, sum(rank * health * extent, na.rm=TRUE) /
    #                           (sum(extent * rank, na.rm=TRUE)) ) * 100)
    #  scores_CP$dimension <-"status"


    # trend
    d$trend<-d$trend*5#need to multiple trend per year times 5 to get future status?




     d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

    if (nrow(d_trend) > 0 ){
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d_trend %>%
          group_by(rgn_id) %>%
          dplyr::summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend'))
    } else { # if no trend score, assign NA
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d %>%
          group_by(rgn_id) %>%
          dplyr::summarize(
            score = NA,
            dimension = 'trend'))
    }



    ### output data file for checking and data review
    scores_check <- spread(scores_CP, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")


    ## finalize scores_CP
    scores<- scores_CP %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)

 { ## else -- if sum(d$km2) is not greater than 0

    ## set status and trend to NA for all regions
 #   message('CP status and trend are NA, consider removing goal if no CP habitats in assessment area')

  #  rgns <-layers$data[['rgn_labels']]
  #  scores_CP <- bind_rows(
   #   rgns %>%
      #  mutate(goal      = 'CP',
       #        dimension = 'status',
        #       score     = NA),
    #  rgns %>%
     #   mutate(goal      = 'CP',
      #         dimension = 'trend',
       #        score     = NA)) %>%
      #select(goal, dimension, region_id = rgn_id, score)

  } ## end -- if (sum(d$km2) > 0)

  ## reference points
  rp <- read.csv(file.path('temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, file.path( 'temp/referencePoints.csv'), row.names=FALSE)

  return(scores)
}


TR = function(layers, status_year=2015) {
#formula
  #t_sentiment is average score our of 100 of three consistent HTA questions on resident feelings towards tourism
  #t_sentiment reference from HTA report goal of 80% of Hawaii residents that agree that tourism has brought more benefits than problems

  #reference for inflation adjusted GDP spending is estimated at 2.5% increase per year (calculated as annual rate from 2020 goal of $13,280 mil compared to 2015 11,796 mil)

     ## formula:
  ##  E   = Eg                         # Eg: % growth of visitor contribution to Hawaii's gdp. t_visitor_gdp_mhi2017.csv reference value comes from HTA's goal for 2020 and coresponds to a 2.5% annual growth
  ##  S   = resident sentiment        # resident feelings toward tourism
  ##  env = environmental sustainability # score based on 30% protected area reference for effectively managing nearshore waters
  ##Xtr = (E + S + env)/3

  ## read in layers

  sent = SelectLayersData(layers, layers='t_sentiment', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      year,
      value          = val_num)
  growth = SelectLayersData(layers, layers='t_growth', narrow = TRUE) %>%
    select(
      year,
      rgn_id    = id_num,
      value      = category,
      county_gdp         = val_num)
  env = SelectLayersData(layers, layers='t_env_sus', narrow = TRUE) %>% #update to include %of priority watersheds protected=freshwater capacity
    select(
      year,
      rgn_id    = id_num,
      percent         = val_num)

 # need to scale the t_sentiment score to a reference level of 80 or 80%
sent$score<-sent$value/80*100

  #need to scale estimated county growth to a reference of 2.5% increase per year

growth<-growth %>% group_by(rgn_id) %>%
  arrange(value)%>%
  mutate(growth_rate=((county_gdp-lag(county_gdp,1))/lag(county_gdp,1))) #takes the difference between county gdp each year


#growth rate calucaltion not accurate for 2010 - remove to get last 5 years
growth<-subset(growth, year!=2010)
#need to normalize score between 0 and 1.
#if greater than 2.5% than score =1

r=0.025
#growth$n_score<-ifelse(growth$growth_rate>=r, 1,ifelse(growth$growth_rate<=0, 0, growth$growth_rate/r))
growth$n_score<-ifelse(growth$growth_rate>=r, 1,ifelse(growth$growth_rate<=0.0125 & growth$growth_rate>=-0.03, .5, growth$growth_rate/r))#if growth rate is >2.5% than perfect score = 1
#if growth is 1.5% or less gets score of 0.5 or 50%. If growth rate falls considerable <1.5 score is 0

growth$n_score<-growth$n_score*100
#need to score environmental data to reference - use 30%
env$env_score<-ifelse(env$percent>30, 100,(env$percent/30)*100)
env<-env %>%
  dplyr::group_by(rgn_id, year)%>%
  dplyr::summarize(env_score=mean(env_score), percent=mean(percent)) #####need to FIX### otherwise duplicates in env scores
env<-env[order(env$year),]
#str(env)

#basic formatting of data
env$rgn_id<-as.numeric(env$rgn_id)
sent$rgn_id<-as.numeric(sent$rgn_id)
growth$rgn_id<-as.numeric(growth$rgn_id)
env$year<-as.numeric(env$year)
sent$year<-as.numeric(sent$year)
growth$year<-as.numeric(growth$year)
sent<-subset(sent, year!=2009&year!=2010)
tr_data <- sent %>%
  left_join(env,by=c('rgn_id', 'year'))%>%
  select(rgn_id, year, score, env_score)
tr_data<-tr_data %>%
  left_join(growth,by=c('rgn_id', 'year'))%>%
  select(rgn_id, year, score, env_score,n_score)

#Tourism goal is an average of the enivronmental protection, resident sentiment about tourism, and growth rate
tr_data$status<-rowMeans(tr_data[,c("score","env_score","n_score")])
tr_data_sum<-tr_data


tr_data_sum <- tr_data %>%
  group_by(rgn_id) %>%
  summarize(status=mean(status))

# ------------------------------------------------------------------------
#Get yearly status and trend
# -----------------------------------------------------------------------

status <-  tr_data_sum %>%
  mutate(
    score     = round(status, 1),
    dimension = 'status') %>%
  select(region_id=rgn_id, score, dimension)

#year=as.data.frame(tr_data$year)#work around for not finding status_year
##status_year=2015 @eschemmel can delete; set at the function call above
trend_years <- status_year:(status_year-4)
first_trend_year <- min(tr_data$year)

status_data=tr_data
#str(status_data)

trend <- status_data %>%
  #filter(year) %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ year, data=.),
   adjust_trend = .$status[.$year == first_trend_year]) %>%
  dplyr::summarize(region_id = rgn_id,
          score = round(coef(mdl)['year']/adjust_trend * 5, 4),
          dimension = 'trend') %>%
  ungroup() %>%
  mutate(score = ifelse(score > 1, 1, score)) %>%
  mutate(score = ifelse(score < (-1), (-1), score))

# assemble dimensions
scores <- rbind(status, trend) %>%
  mutate(goal='TR')
scores <- data.frame(scores)

return(scores)

}




LIV_ECO = function(layers, subgoal){ # LIV_ECO(layers, subgoal='LIV')

  ## read in all data: gdp, wages, jobs and workforce_size data
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year,sector = category, gdp_usd = val_num)

  le_wages = SelectLayersData(layers, layers='le_wage') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_wkforce') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)

  le_a_wage = SelectLayersData(layers, layers='liv_a_wage') %>%
    dplyr::select(rgn_id = id_num, year, a_wage = val_num)


  # local multipliers from DBEDT 2007 report
  multipliers_jobs = data.frame('sector' = c('Offshore Mineral Extraction','All Ocean Sectors', 'Tourism and Recreation','Living Resources', 'Marine Construction', 'Ship and Boat Building','Marine Transportation'),
                                'multiplier' = c(1,1,1.27, 1.76, 1, 1, 1.69)) # no multiplers for marine construction, and boat building (1)
  multipliers_rev  = data.frame('sector' = c('Offshore Mineral Extraction','All Ocean Sectors','Tourism and Recreation','Living Resources', 'Marine Construction', 'Ship and Boat Building','Marine Transportation'),
                                'multiplier' = c(1,1,1.32,1.58,1,1, 1.63)) # Ship and Boat building given a value of one becuase no multiplier available

  #multiple options for reference points for wages
  #Average wage # used this for the reference point for wages by sector
  le_wages = le_wages %>%
    left_join(le_a_wage, by = c('rgn_id', 'year'))

  #sss
  sss<-data.frame(rgn_id=c('1','2','3','4'),
      sss=c('24435','31435','31675','38472')) #self suficency standard for one adult family- DBEDT 2014 databook for 2013 year
    sss$rgn_id<-as.integer(sss$rgn_id)
    sss$sss=as.integer(c('24435','31435','31675','38472'))

  #median wage
  median_wage<-data.frame(rgn_id=c('1','2','3','4'),
                          m_wage=c('25601','36791','31869','31473')) #median wage- DBEDT 2014 databook for 2013 year
  median_wage$rgn_id<-as.integer(median_wage$rgn_id)
  median_wage$m_wage=as.integer(c('25601','36791','31869','31473'))


    # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors.
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>% # if using multipliers run this code
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed #assumes unemployment rate is equal to county unemployment rate and equal accross sectors
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    left_join(sss, by=('rgn_id'))%>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd),
           sector!="All Ocean Sectors",
           sector!="Offshore Mineral Extraction") ###need to remove mineral extraction as not
            #considered a sustainable ocean sector, remove all ocean sectors since this is the sum and
            #would be double counting


  liv_status = liv_status %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    arrange(rgn_id, year) %>%
    # summarize across sectors
    dplyr::group_by(rgn_id, year) %>%
    dplyr::mutate(
      # across sectors, wages are averaged
      #wages_avg = mean(wage_usd, na.rm=T)) #does not take into account higher number of poverty level jobs in tourism industry - does not account for wieght of job sector
      jobs_sum=sum(jobs_adj, na.rm=T))%>%
    #ungroup() %>%
    #change mean wage to reflect the total number of jobs per sector * sector wage/ total jobs
    ungroup()%>%
    dplyr::group_by(rgn_id) %>%
    dplyr::mutate(jobs_rgn=mean(jobs_sum))%>%
    dplyr::ungroup() %>%
    dplyr::group_by(rgn_id,sector) %>%
    dplyr::mutate(wages_weight = (mean(jobs_adj)/jobs_rgn)) %>%
    dplyr::ungroup() %>%
    #dplyr::group_by(rgn_id,sector, year) %>%
    # dplyr::mutate(
    # wages_avg=sum(wages_total/jobs_sum))%>% #removed to calculate sector score in reference to median wage and weighted by proportion of total jobs
    #dplyr::ungroup() %>%
    dplyr::group_by(rgn_id,year,sector) %>%
    #summarize(wages_avg=mean(wages_avg), jobs_sum=mean(jobs_sum))%>%
    arrange(rgn_id, year) %>%
    dplyr::mutate(
      # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
      # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      wages_avg_first = a_wage) %>% ##average_wage by county from 2009-2013 DBEDT
    #calculate final scores
    ungroup() %>%
    mutate(
      x_jobs  = pmin(1,  jobs_sum / jobs_sum_first),
      x_wages = pmin(1, wage_usd / wages_avg_first)) %>%
    dplyr::group_by(rgn_id,sector, year) %>%
    dplyr::mutate(x_wages_w = (x_wages*wages_weight))%>% ##scores are weighted by proportion of jobs in each sector
    ungroup() %>%

    mutate(x_jobs  = pmin(1,  jobs_sum / jobs_sum_first)) %>%#compare the current jobs (2015) to 5 years prior
    dplyr::group_by(rgn_id,year) %>%
    summarize(x_wages=sum(x_wages_w), x_jobs=mean(x_jobs))%>%
    ungroup() %>%
    mutate(score = rowMeans(.[,c('x_jobs', 'x_wages')]) * 100) %>%
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'LIV',
      dimension = 'status') %>%
    dplyr::select(
      goal,
      dimension,
      region_id = rgn_id,
      score)

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend =
      liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd),
             sector!="All Ocean Sectors",
             sector!="Offshore Mineral Extraction")%>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(rgn_id, year, sector) %>%
      dplyr::group_by(rgn_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      dplyr::group_by(metric, rgn_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      dplyr::summarize(
        metric = metric,
        weight = weight,
        rgn_id = rgn_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(rgn_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      dplyr::group_by(metric, rgn_id) %>%
      dplyr::summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      dplyr::group_by(rgn_id) %>%
      dplyr::summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id = rgn_id,
        score)


  # ECO calculations ----
  le_gdp<-le_gdp %>%
      filter(
    sector!="All Ocean Sectors",
    sector!="Offshore Mineral Extraction")

    #to add revenue multipliers (from DBEDT 2007)
    eco =
      # adjust revenue with multipliers
      le_gdp %>%
      left_join(multipliers_rev, by = 'sector') %>% # if using multipliers run this code
      mutate(rev_mult = gdp_usd * multiplier)

    eco = eco %>%
    mutate(
      rev_adj = rev_mult,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
      dplyr::select(rgn_id, year, sector, rev_adj)



  # ECO status
    #str(eco)
    eco$rev_adj<-as.numeric(eco$rev_adj)#summarize needs data to be numeric

  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    filter(rev_adj>0) %>%
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; are these be normalized?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores.LE = scores %>%
    dplyr::filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)

  # return scores
  return(scores)
}

#this is the "Connection to Place" subgoal of sense of place
CON = function(layers, status_year=2015){
#ocean use data from ocean use atlas (# of acitivites that take place in or near the ocean including cultural activities)


  #access is important
  #county and state beach park area
  #national_parks

  parks = SelectLayersData(layers, layers='r_parks', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      value          = val_num)

  res <- SelectLayersData(layers, layers = 'r_residents', narrow=TRUE) %>% ##resident population to weight the need by rgn
    select(region_id=id_num, year,res=val_num)


  shore = SelectLayersData(layers, layers='r_shoreline', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      length          = val_num)

  access = SelectLayersData(layers, layers='ao_access_mhi2017', narrow = TRUE) %>% #score is # shoreline access points/shoreline length in miles
    select(
      rgn_id    = id_num,
      value          = val_num)


  #get score for beach parks based on resident population
  #rec_d<-res %>%
  #  mutate(rgn_id=region_id) %>%
  #  left_join(parks)%>%
  #  mutate(park_density=res/value)%>%

  rec_d<-res %>%
    mutate(rgn_id=region_id) %>%
    left_join(shore) %>%
    mutate(density=res/length) %>% ## of people per km of shoreline
    select(rgn_id, year, density)

  #weighting parks score as number of parks weighted by the desity of residents per km of coastline
  rec_d<-rec_d %>%
    left_join(parks) %>%
    mutate(value=value/density) %>%
    group_by(year)  %>%
    mutate(score = value/max(value)) %>%
    select(rgn_id, year,score)

    #join shoreline access data and park scores
  rec_d<-rec_d %>%
    left_join(access)%>%
    mutate(status=(score+value)/2*100) %>%
    select(rgn_id, year, status)

  #weigthing park score as # of parks per 5 km
  #rec_d<-parks %>%
  #  left_join(shore) %>%
  #  mutate(value=value/(length/5))


  # ------------------------------------------------------------------------
  #Get yearly status and trend
  # -----------------------------------------------------------------------

  status <-  rec_d %>%
    filter(year==status_year)
  status<-as.data.frame(status)

  status<-status %>%
    mutate(score     = round(status, 1),
           dimension = 'status') %>%
    select(region_id=rgn_id, score, dimension)

  trend_years <- status_year:(status_year-4)
  first_trend_year <- min(rec_d$year)
  status_data=rec_d
  #str(status_data)

  rec_d<-as.data.frame(rec_d)

  trend <- rec_d %>%
    #filter(year) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    dplyr::summarize(region_id = rgn_id,
                     score = round(coef(mdl)['year']/adjust_trend * 5, 4),
                     dimension = 'trend') %>%
    ungroup() %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    mutate(score = ifelse(score < (-1), (-1), score))

  # assemble dimensions
  scores <- rbind(status, trend) %>%
    mutate(goal='CON')
  scores <- data.frame(scores)

  return(scores)


}


LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year=2015){

      trend_years = (status_year-4):status_year
  #updated with regional protected areas offshore and inshore and number of preserved historic sites/sacred places
  # select data ----

  #listed historic places (dbedt data section 7.46)
  layers_data = SelectLayersData(layers, layers=c('lsp_historic_sites'))#inland protected conservation districts

   hs<- layers_data %>%
     select(region_id = id_num,  year, sites=val_num) ##need to get support on choosing a reference point

 #managed marine areas
  r = SelectLayersData(layers, layers=c('lsp_area_3nm_mhi2017', 'lsp_area_1km_coast'))  #total offshore/inland areas
  #ry = SelectLayersData(layers, layers=c('lsp_mpa_3nm', 'lsp_coastal_conservation')) #total protected areas

  layers_data = SelectLayersData(layers, layers=c('lsp_mma_mhi2017'))#marine managed areas

  mpa<- layers_data %>%
    select(rgn_id = id_num, mpa=val_num)


  #layers_data = SelectLayersData(layers, layers=c('lsp_mpa_nearshore'))#nearshore protected areas

  #mpa<- layers_data %>%
  #  select(region_id = id_num, type=category, mpa=val_num)

  layers_data = SelectLayersData(layers, layers=c('lsp_coastal_conservation'))#inland protected conservation districts

  #use if wanting to weight MPAʻs by protective ability - we did not use because wanted score
  #to be consistent with the states 30 by 30 management goals
  #rank <- c('zoned_w_no_take'            = 1,
  #          'multiple_use'            = .5)

  #mpa <- mpa%>%
  #  filter(type %in% names(rank)) %>%
  #  mutate(
  #    rank = rank[type],
  #    mpa = ifelse(mpa==0, NA, mpa))%>%
  #  mutate(mpa_weighted = rank*mpa)%>%
  #  filter(!is.na(mpa_weighted))%>%
  #  group_by(region_id)%>%
  #  summarize(mpa_weighted=sum(mpa_weighted))%>%
  #  select(rgn_id=region_id, mpa=mpa_weighted)

  #mpa<-as.data.frame(mpa)

  ry <- layers_data %>%
    select(region_id = id_num, condist = category, km2=val_num)





  ## set ranks for each conservation district protective ability
  rank <- c('P'            = 1,
            'L'            = .9,
            'R'            = .8,
            'G'            =.7,
            'SS'           =.6)

  ## limit to conservation districts R, L, R, G, and SS, and add rank
  ry <- ry %>%
    filter(condist %in% names(rank)) %>%
    mutate(
      rank = rank[condist],
      extent = ifelse(km2==0, NA, km2))%>%
    mutate(weighted_cont = rank*extent)%>%
    filter(!is.na(weighted_cont))

   ry <- ry %>%
    group_by(region_id) %>%
    mutate(cp=sum(weighted_cont)) %>%
    select(rgn_id=region_id, cp)

  ry<-ry[1:4,]

  r <- r %>%
    dplyr::select(region_id = id_num, val_num, layer) %>%
    spread(layer, val_num) %>%
    select(rgn_id=region_id, area_inland1km = lsp_area_1km_coast,
           area_offshore3nm = lsp_area_3nm_mhi2017)


   ry <- ry %>%
    left_join(mpa)

  # fill in time series for all regions

r.yrs <-r %>%
  left_join(ry, by=c('rgn_id'))

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year (note currently do not have time series data for protection)
  # and calculate status score
r.yrs = r.yrs %>%
  mutate(pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
         pct_cmpa  = pmin(mpa / area_offshore3nm * 100, 100),
         prop_protected    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
  filter(!is.na(prop_protected))

# extract status based on specified year
  r.status = r.yrs %>%
    #filter(year==status_year) %>%
    select(region_id=rgn_id, status=prop_protected) %>%
    mutate(status=status*100) %>%
    select(region_id, score = status) %>%
    mutate(dimension = "status")

  # calculate trend

  #adj_trend_year <- min(trend_years)

  #r.trend =   r.yrs %>%
  #  group_by(region_id) %>%
  #  do(mdl = lm(prop_protected ~ year, data=., subset=year %in% trend_years),
  #     adjust_trend = .$prop_protected[.$year == adj_trend_year]) %>%
  #  summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
  #  ungroup() %>%
  #  mutate(trend = ifelse(trend>1, 1, trend)) %>%
  #  mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  #  mutate(trend = round(trend, 4)) %>%
  #  select(region_id, score = trend) %>%
  #  mutate(dimension = "trend")


 #currently do not have year or time series data so setting trend to 0
    r.trend<-r.yrs %>%
    select(region_id=rgn_id) %>%
    mutate(score=0.31) %>% #comes from state marine managed area dashboard data estimates percent of marine managed areas per year from 1972-2009 in relation to 30% goal
    mutate(dimension = "trend")

  ## reference points
  rp <- read.csv(file.path( 'temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                                   ref_pct_cp, "% coastal protected area"),
                     reference_point = "varies by area of region's eez and 1 km inland"))
  write.csv(rp, file.path( 'temp/referencePoints.csv'), row.names=FALSE)


  # return scores
  scores = bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")
  return(scores[,c('region_id','goal','dimension','score')])
}

SP = function(scores){

  ## to calculate the four SP dimesions, average those dimensions for CON and LSP
  s <- scores %>%
    filter(goal %in% c('CON','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}



HAB = function(layers){
   ## get the data:
  health <-  layers$data[['hab_health_mhi']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-  layers$data[['hab_trend_mhi']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  extent <- layers$data[['hab_extent_mhi']] %>%
    select(rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('rgn_id', 'habitat')) %>%
    full_join(extent, by = c('rgn_id', 'habitat')) %>%
   # filter(habitat %in% c('reef','wetlands-estuary','beach','soft','priority-watersheds')) %>%
    #mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA))%>%
    filter(!is.na(rgn_id))

  #if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
  #  warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  #}

  #if(sum(d$w %in% 1 & is.na(d$health)) > 0){
  #  warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  #}

  #d <- d %>%
  #  dplyr::group_by(rgn_id)%>%
  #  dplyr::mutate(
  #    total_extent=sum(extent) )

  ## calculate scores
  status <- d %>%
    dplyr::group_by(rgn_id, habitat) %>%
    dplyr::mutate(
      score = (health*extent)/extent*100)%>% #health already as proportion protected or historical extent so multiply times current extent to get score
    dplyr::ungroup()%>%
    dplyr::group_by(rgn_id) %>%
    dplyr::summarize(
      score=mean(score),
    dimension = 'status')%>%
    dplyr::mutate(region_id=rgn_id)%>%
    select(region_id, score, dimension) %>%
    ungroup()

  trend <- d %>%
    dplyr::group_by(rgn_id) %>%
    filter(!is.na(trend)) %>%
    dplyr::summarize(
      score = mean(trend),
      dimension = 'trend')  %>%
    dplyr::mutate(region_id=rgn_id) %>%
    select(region_id, score, dimension)%>%
    ungroup()

  trend<-as.data.frame(trend)

  #str(trend)
  #str(status)

  scores_HAB<- rbind(status, trend) %>%
    mutate(goal = "HAB")

  ## reference points
  rp <- read.csv(file.path( 'temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "HAB", method = "Health/condition variable based on current vs. historic extent and area protected for watersheds",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, file.path('temp/referencePoints.csv'), row.names=FALSE)

  # return scores
  return(scores_HAB)
}


SPP = function(layers){

   fish_score<-  layers$data[['spp_fish']]#score for fish populations from NOAA report card - fish biomass/pristine fish biomass

    #ESA scores for marine mammals, turtles, marine birds, and coastal plants
   scores <-  layers$data[['spp_status_mhi']] %>%
    select(rgn_id, score)  %>%
    dplyr::group_by(rgn_id)%>%
    summarize(
    score=mean(score*100))%>%
    mutate(goal = 'SPP')%>%
   mutate(dimension="status")

   scores <- scores %>%
     full_join(fish_score, by = c('rgn_id'))
    scores$score=
      (scores$score+scores$value)/2
  scores<-scores%>%
    select(rgn_id, score, goal, dimension)

  #trend from change in fish biomass from last NOAA CREP surveys 2010-2011 to 2014-2015
  trend<-fish_score %>%
    select(rgn_id, trend)%>%
    mutate(dimension="trend")%>%
    mutate(goal="SPP")%>%
    mutate(score=trend)%>%
    select(rgn_id, score, dimension, goal)

  scores<-scores%>%
    full_join(trend)%>%
    select(goal, dimension, rgn_id, score)%>%
    mutate(region_id=rgn_id)

    ## reference points
  rp <- read.csv(file.path('temp/referencePoints.csv'), stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "SPP", method = "Average of ESA status and fish biomass scores",
                     reference_point = NA))
  write.csv(rp, file.path('temp/referencePoints.csv'), row.names=FALSE)


  return(scores)
}

BD = function(scores){
  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
