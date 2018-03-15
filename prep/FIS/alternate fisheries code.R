#Alternate nearshore fisheries code

reef = SelectLayersData(layers, layers='fis_reef_catch', narrow = TRUE) %>%
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

fis_data<-reef

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


# ---
#Estimate scores for taxa without stock assessment values
# Median score of other fish in the taxon group ("Fam"and if no stock assessments for the Fam then to code (group) bottom, pelagic, or reef fish) is an estimate


## this takes the median score within each family and applies it for species that did not have stock assessment score
# data_fis_gf <- fis_data %>%
#   dplyr::group_by(year, Fam) %>% #Fam is the code for family and code is the taxon key to separate out bottom, pelagics, coastal pelagics and reef fish
#   dplyr::mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
#   dplyr::mutate(Mean_score = mean((score), na.rm=TRUE)) %>%
#   dplyr::ungroup()%>%
#   dplyr::mutate(Median_score = ifelse(is.na(score), Median_score, score))%>%
#   dplyr::mutate(Mean_score = ifelse(is.na(score), Mean_score, score))

## this takes the median score for each fishery (bottom, pelagic, coastal pelagic, reef) and applies it for Families that did not have a median stock assessment score
# data_fis_gf <- data_fis_gf %>%
#   dplyr::group_by(year, code) %>% #Fam is the code for family and code is the taxon key to separate out bottom, pelagics, coastal pelagics and reef fish
#   dplyr::mutate(code_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
#   dplyr::mutate(Mean_code_score = mean((score), na.rm=TRUE)) %>%
#   dplyr::ungroup()%>%
#   dplyr::mutate(score_gf = ifelse(is.na(Median_score), code_score, Median_score)) %>%
#   dplyr::mutate(score_gf = ifelse(is.na(score_gf), 1, score_gf))%>%
#   dplyr::mutate(Mean_score = ifelse(is.na(Mean_score), Mean_code_score, Mean_score))


#documentation for scores that were gap filled
# data_fis_gf <- data_fis_gf %>%
#   mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "formal stock assessment"))
#

#select data needed for status
#use this code for median gap filled scores
#data_fis_gf <- data_fis_gf %>%
#  select(rgn_id, key_sp, code, year, catch, score=score_gf, score_gapfilled)

#use this code for mean gap filled scores
# data_fis_gf <- data_fis_gf %>%
#   select(rgn_id, key_sp, code, year, catch, score=Mean_score, score_gapfilled)
#
# str(data_fis_gf) #check data
# length(unique(data_fis_gf$code)) #check that there are no typos in the code should be 4 levels
#

#remove NAs - only use scores for stock assessed species (remove invasives first)
fis_data<-na.omit(fis_data)
fis_data = fis_data %>%
  filter(key_sp!="Lutj_Toau")


# sum of mean catch of all species in region/year
fis_data <- fis_data %>%
  group_by(year, rgn_id, key_sp) %>% #summarize catch data
  dplyr::summarize(catch = sum(catch), mean_score=mean(score))


fis_data <- fis_data %>%
  #dplyr::mutate(catch_w=ifelse(key.x=="CHCR", catch*value, catch))%>%
  group_by(rgn_id,year)%>%
  dplyr::mutate(SumCatch = sum(catch))%>%
  dplyr::mutate(wprop = catch/SumCatch)


#to get sus scores for reef by region
fis_data <- fis_data %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::summarize(status = prod(mean_score^wprop)) %>%
  ungroup()





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
