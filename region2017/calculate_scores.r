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

setwd("~/Documents/github/mhi/region2017")

source('configure_toolbox.r')

## calculate scenario scores
scores = ohicore::CalculateAll(conf, layers)

## save scores as scores.csv
write.csv(scores, 'scores.csv', na='', row.names=FALSE)



## visualize scores ----

## Flower plots for each region ----
source('plot_flower_local.R') # source local MHI copy, not ARC copy

## to create flower plots with equal weighting for FIS/MAR: since this relies on the `fp_wildcaught_weigth*` csv that is also used for calculating it, we'll rename it temporarily
fp_real <- 'layers/fp_wildcaught_weight_mhi2017.csv'
fp_temp <- 'layers/fp_TEMP_COPY_wildcaught.csv'

## temporarily rename fp file and delete original
readr::read_csv(fp_real) %>%
  write_csv(fp_temp)
unlink(fp_real)

## temporarily change CON subgoal scores to NA (to reset, rerun CalculateAll and rewrite scores.csv)
scores$score[scores$goal == "CON"] <- NA
write.csv(scores, 'scores.csv', na='', row.names=FALSE)

## now plot
PlotFlower(assessment_name = "Main Hawaiian Islands",
           display_region_title = FALSE,
           display_supra_title  = FALSE)

## now reinstate original file
readr::read_csv(fp_temp) %>%
  write_csv(fp_real)
unlink(fp_temp)
## check to make sure neither fp_real nor fp_tmp is in the Git tab!!


