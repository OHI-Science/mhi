## calculate_scores.R

## calculate_scores.R ensures all files are properly configured and calculates OHI scores.
  ## - configure_toolbox.r ensures your files are properly configured. It is a script in your repository.
  ## - CalculateAll() calculates OHI scores. It is a function in the `ohicore` R package
  ##   (this can be written in R as `ohicore::CalculateAll()`).

## When you begin, configure_toolbox.r and CalculateAll() will calculate scores using
## the 'templated' data and goal models provided. We suggest you work
## goal-by-goal as you prepare data in the prep folder and develop goal models
## in functions.r. Running configure_toolbox.r and a specific goal model line-by-line
## in functions.R is a good workflow.

## run the configure_toolbox.r script to check configuration
setwd("~/github/mhi/region2017")

source('configure_toolbox.r')

## calculate scenario scores
scores = ohicore::CalculateAll(conf, layers)

## save scores as scores.csv
write.csv(scores, 'scores.csv', na='', row.names=FALSE)



## visualize scores ----


## source from ohibc until added to ohicore, see https://github.com/OHI-Science/ohibc/blob/master/regionHoweSound/ohibc_howesound_2016.Rmd
ohibc_url <- 'https://raw.githubusercontent.com/OHI-Science/ohibc/'
source(paste0(ohibc_url, 'master/src/R/common.R'))
source(paste0(ohibc_url, 'master/regionHoweSound/plot_flower.R'))

## goal info
goal_names <- readr::read_csv('conf/goals.csv') %>% select(goal, name)
weight     <- readr::read_csv('conf/goals.csv') %>% select(goal, weight)

## regions info
regions <- bind_rows(
  data_frame(                # order regions to start with whole study_area
    region_id   = 0,
    region_name = 'Main Hawaiian Islands'),
  read_csv('spatial/regions_list.csv') %>%
    select(region_id   = rgn_id,
           region_name = rgn_name)) %>%
  mutate(
    flower_png = sprintf('figures/flower_%s.png', str_replace_all(region_name, ' ', '_')))

## cycle through each region
for (i in regions$region_id) { # i = 0

  ## set up for plotting
  score_df <- scores %>%
    filter(dimension == 'score') %>%
    filter(region_id == i) %>%
    select(-region_id) %>%
    distinct() %>%
    inner_join(weight, by = 'goal')

  goal_labels <- score_df %>%
    left_join(goal_names, by = 'goal') %>%
    mutate(goal_label = str_replace(name, ' ', '\n'),
           goal_label = paste(goal_label, round(score), sep = '\n')) %>%
    select(goal, goal_label)


  ## Casey's new flower plot
  plot_obj <- plot_flower(score_df,
                          goal_labels = goal_labels,
                          filename    = regions$flower_png[regions$region_id == i],
                          incl_legend = TRUE)

}


