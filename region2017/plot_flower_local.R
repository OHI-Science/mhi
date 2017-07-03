# This generates the flower plots:

## weights for FIS vs. MAR
weights <- read.csv(sprintf("../../eez%s/layers/fp_wildcaught_weight.csv", scenario), stringsAsFactors=FALSE)
weights_global <- data.frame(rgn_id=0, w_fis=mean(weights$w_fis))
weights <- rbind(weights_global, weights)

# getting the goals that will be plotted:
conf <-  read.csv(sprintf("../../eez%s/conf/goals.csv", scenario), stringsAsFactors=FALSE)

goals_supra = na.omit(unique(conf$parent)) # goals comprised of subgoals, not included in plot

conf <- conf %>%
  filter(!(goal %in% goals_supra)) %>%
  select(goal, order_color, order_hierarchy, weight, name_flower) %>%
  mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
  arrange(order_hierarchy)


data <- read.csv(sprintf('../radical_%s.csv', radicalFile), stringsAsFactors=FALSE)
data <- data[data$scenario == scenario, ]

data <- data %>%
  filter(dimension == "score") %>%   # focus only on score data
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  mutate(region_id = ifelse(region_id==300, 0, region_id)) %>%   #convert the 300 (i.e., only eez's averaged to zero)
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213)

# region names, ordered by GLOBAL and alphabetical
rgn_names2 = rbind(
  data.frame(
    region_id=0,
    country='GLOBAL'),
  rgn_names) %>%
    arrange(country) %>%
  filter(region_id != 213)

# loop through regions to plot flowers
for (rgn_id in unique(rgn_names2$region_id)){  #rgn_id=0

  # header md
  rgn_name = subset(rgn_names2, region_id==rgn_id, country, drop=T)
  message(sprintf('\n## %s (%d)\n\n', rgn_name, rgn_id))

    # region scores
  g_x <- subset(data, region_id==rgn_id) %>%
         inner_join(conf, by="goal") %>%
         arrange(order_color)
  x <-  subset(data, region_id==rgn_id & goal == 'Index', value, drop=T)

    # get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
if(colorScheme == "new"){
 reds <-  colorRampPalette(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090"), space="Lab")(65)
 blues <-  colorRampPalette(c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))(35)
 colors <-   c(reds, blues)

  g_x$cols.goals.all = cut(g_x$value, breaks=seq(0, 100, by=1), include.lowest=TRUE,
                       labels = colors) } else {
    g_x$cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'), space='Lab')(length(goals.all))
   }

       #weights after correcting for fisheries/mariculture contributions
  g_x$weight[g_x$goal == "FIS"] <-   weights$w_fis[weights$rgn_id == rgn_id]
  g_x$weight[g_x$goal == "MAR"] <- 1 - weights$w_fis[weights$rgn_id == rgn_id]


  # res=72

  res=150
   ## start plot
   png(sprintf('figures/FlowerPlots/flower_%s_%s.png', rgn_name, scenario),
          width=res*6, height=res*6, bg = "transparent")
#par(oma=c(0,0,3,0), mar=c(6, 4, 0, 2) + 0.1)
   PlotFlower(main = ifelse(rgn_name=="GLOBAL", "", as.character(rgn_name)),
                 lengths=ifelse(
                   is.na(g_x$value),
                   100,
                   g_x$value),
                 widths=g_x$weight,
                 fill.col=ifelse(
                   is.na(g_x$cols.goals.all),
                   'grey80',
                   as.character(g_x$cols.goals.all)),
                 labels  =ifelse(
                   is.na(g_x$value),
                   paste(g_x$name_flower, '-', sep='\n'),
                   paste(as.character(g_x$name_flower), round(g_x$value), sep='\n')),
                 center=round(x),
               #  max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
           max.length = 100, disk=0.3, label.cex=1.5, label.offset=0.15, cex=3, cex.main=3)

      dev.off()
      #system(sprintf('convert -density 150x150 %s %s', fig_pdf, fig_png)) # imagemagick's convert

  }




plot_flower <- function(score_df,
                        score_ref   = 100,    ### scale from 0-1 or 0-100? default is 0-100
                        outline     = TRUE,   ### show the outer borders; default is yes indeedy
                        filename    = NULL,   ### give it a file name to save the plot; default is no save
                        center_text = NULL,   ### pass it a number or label; default is blank
                        center_score = TRUE,  ### overridden if center_text != NULL
                        goal_labels = TRUE,   ### show goal labels? TRUE goes with generics; FALSE hides the goal labels;
                          ### a data frame with cols 'goal' and 'goal_label' will replace generic goal labels
                        goals_csv    = NULL,   ### filepath for configuration info: goals.csv  # goals_csv = 'conf/goals.csv'
                        incl_legend = FALSE,  ### show the legend? FALSE hides the legend
                        show_plot   = TRUE) {

  ### set up positions for the bar centers:
  ### cumulative sum of weights (incl current) minus half the current weight
  score_df <- score_df %>%
    mutate(score   = score * 100/score_ref,   ### if 0-1, turn into 0-100; otherwise leave as is
           pos     = cumsum(weight) - 0.5 * weight,
           pos_end = last(pos) + 0.5 * last(weight)) %>%
    filter(weight != 0)

  if(is.data.frame(goal_labels)) {
    message('appending goal labels')
    score_df <- left_join(score_df, goal_labels, by = 'goal')
  } else {
    score_df <- score_df %>%
      mutate(goal_labels = goal)
  }


  ## goals.csv configuration information, if available
  if(!is.null(goals_csv)) {

    conf <-  read.csv(goals_csv, stringsAsFactors=FALSE)

    conf <- conf %>%
      # filter(!(goal %in% goals_supra)) %>%
      select(goal, order_color, order_hierarchy, weight, name_flower) %>%
      mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
      arrange(order_hierarchy)

    # region scores
    score_df <- score_df %>%
      inner_join(conf, by="goal") %>%
      arrange(order_color)
    # x <-  subset(data, region_id==rgn_id & goal == 'Index', value, drop=T)
  }


  ### set up for displaying NAs
  score_df_na <- score_df %>%
    mutate(score = ifelse(is.na(score), 100, NA))



  p_labels <- score_df$goal
  p_breaks <- score_df$pos
  p_limits <- c(0, score_df$pos_end[1])
  p_score  <- round(weighted.mean(score_df$score, score_df$weight, na.rm = TRUE), 0)

  ### some parameters for the plot:
  blank_circle_rad <- 42
  light_line <- 'grey90'
  white_fill <- 'white'
  light_fill <- 'grey80'
  med_line   <- 'grey50'
  med_fill   <- 'grey52'
  dark_line  <- 'grey20'
  dark_fill  <- 'grey22'

  ### set up basic plot parameters
  plot_obj <- ggplot(data = score_df,
                     aes(x = pos, y = score, fill = score, width = weight))

  if(outline) {
    plot_obj <- plot_obj +
      ### sets up the background/borders to the external boundary (100%) of plot:
      geom_bar(aes(y = 100),
               stat = 'identity', color = light_line, fill = white_fill, size = .2) +
      geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                    size = 0.5, color = light_line, show.legend = NA)
      ### lays any NA bars on top of background, with darker grey:
    if(any(!is.na(score_df_na$score)))
      plot_obj <- plot_obj +
        geom_bar(data = score_df_na, aes(x = pos, y = score),
                 stat = 'identity', color = light_line, fill = light_fill, size = .2)
  }

  ### establish the basics of the flower plot...
  plot_obj <- plot_obj +
    ### plots the actual scores on top of background/borders:
      geom_bar(stat = 'identity', color = dark_line, size = .2) +
      geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                    size = 0.5, color = dark_line, show.legend = NA) +
    ### plot zero as a baseline:
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                    size = 0.5, color = dark_line, show.legend = NA) +
    ### turns linear bar chart into polar coordinates:
      coord_polar(start = - score_df$pos[1]/score_df$pos_end[1] * 2 * pi) +
    ### sets petal colors to the red-yellow-blue color scale:
      scale_fill_gradientn(colors = brewer.pal(n = 11, name = 'RdYlBu'),
                           limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = seq(0, 100, 20)) +
      # scale_fill_gradientn(colours = c('#F27259', '#FFCF5C', '#686FB2'), # med blue: #70B8E2, Dark Blue: #455A66 or #686FB2
      #                      limits = c(0, 100),
      #                      breaks = seq(0, 100, 20),
      #                      labels = seq(0, 100, 20)) +
    ### uses weights to assign widths to petals:
      scale_x_continuous(labels = p_labels, breaks = p_breaks, limits = p_limits) +
      scale_y_continuous(limits = c(-blank_circle_rad,
                                    ifelse(first(goal_labels == TRUE) | is.data.frame(goal_labels), 150, 100)))

  ### fill the center?
  ###   if center text is available use it; if not, see if center_score is desired
  if(!is.null(center_text)) {
    plot_obj <- plot_obj +
      geom_text(aes(label = center_text),
                x = 0, y = -blank_circle_rad,
                hjust = .5, vjust = .5,
                size = 8,
                fontface = 'bold.italic',
                color = dark_line)
  } else if(center_score) {
    plot_obj <- plot_obj +
      geom_text(aes(label = p_score),
                x = 0, y = -blank_circle_rad,
                hjust = .5, vjust = .5,
                size = 12,
                # fontface = 'italic',
                color = dark_line)
  }

  ### clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot() +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())


  ### include or exclude goal labels; dynamic if no border
  if(first(goal_labels == TRUE) | is.data.frame(goal_labels)) {
    ### if no outline, labels go near bars; otherwise place near outer edge
    plot_obj <- plot_obj +
      geom_text(aes(label = goal_label, x = pos, y = 130),
                hjust = .5, vjust = .5,
                size = 3,
                fontface = 'italic',
                color = dark_line)
  }

  ### include or exclude the legend
  if(!incl_legend) {
    plot_obj <- plot_obj +
      theme(legend.position = 'none')
  }


  ### display/save options: print to graphics, save to file
  if(show_plot) {
    print(plot_obj)
  }

  if(!is.null(filename)) {
    ggsave(filename = filename,
           height = 6, width = 8, units = 'in', dpi = 300,
           plot = plot_obj)
  }

  ### ...then return the plot object for further use
  return(invisible(plot_obj))
}
