#'
#'HW1, R stacked barplot visualization for Cancer by Gender faceted by race.
#'======
#'CDC cancer dataset 1999-2012, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view
#'

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lazyeval)
library(geofacet)
library(ggthemes)

SCALING_FACTOR = 1

draw_stacked_barplot_facet_vis <- function(fname) {
  # Visualizes cancer types (used interchangeably with cancer sites)
  # as stacked bar plot (stacked by gender) count per cancer per Race. 
  # Vis is created as a stacked bar plot for 
  # count per cancer and then "faceted" by Race to visualize the same
  # relationship for every Race. The addition of Gender in the bar plot
  # to make it stacked adds another dimension to the vis.
  # The generated visualization is stored as a png file in the same
  # directory as the input file.
  #
  # Args:
  #   fname: filename of the CSV file containing the CDC data.
  # Returns:
  #   vis_file: filename of the saved visualization
  usa_cancer_df = read.csv(fname)
  
  usa_cancer_groupby_gender_df = usa_cancer_df %>%
                                 group_by(Race, Sex, LeadingCancerSites) %>%
                                 summarise(Count = (sum(Count)/n())/SCALING_FACTOR)
  
  # X Axis Breaks and Labels 
  brks <- seq(0, 10000, 5000)
  lbls = paste0(as.character(c(0,5,10)), "th")
  
  # Plot
  p = usa_cancer_groupby_gender_df %>%
    ggplot(aes(x = LeadingCancerSites, y = Count, fill = Sex)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    #scale_y_continuous(breaks = brks)+   # Breaks
                       #labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="Cancer counts by cancer types by gender", x="Cancer", y="Count per 100,000") +
    facet_wrap(~ Race, ncol=3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5), 
          axis.ticks = element_blank()) +   # Centre plot title
    scale_fill_brewer(palette = "Dark2")  # Color palette
  print(p)
  vis_file = "usa_cancer_race_sex_cancersites_facet.png"
  ggsave(vis_file, p, units="in", width=10, height=6)
  print(p)
  return(vis_file)
}

vis_file = draw_stacked_barplot_facet_vis('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')
cat(sprintf("%s created..\n", vis_file))
