#'
#'HW1, R ECDF visualization for Cancer counts by gender across states.
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

draw_ecdf_vis <- function(fname) {
  # Visualizes cancer types (used interchangeably with cancer sites)
  # counts as ECDF plot for both males and females across states.
  # Data is summarized as counts by state and gender and then plotted
  # as an empirical cumulative distribution function (ECDF). The idea
  # is to be able to make a statement like X% of U.S. states have cancer
  # counts of Y thousands or more amongst each gender.
  # The generated visualization is stored as a png file in the same
  # directory as the input file.
  #
  # Args:
  #   fname: filename of the CSV file containing the CDC data.
  # Returns:
  #   vis_file: filename of the saved visualization
  usa_cancer_df = read.csv(fname)
  
  #ecdf by state
  usa_cancer_by_state_df = usa_cancer_df %>%
                           group_by(State, Sex) %>%
                           summarise(state_wise_count = (sum(Count)/n())/SCALING_FACTOR) %>%
                           arrange(desc(state_wise_count))
  
  p = usa_cancer_by_state_df %>%
      ggplot(aes(state_wise_count, col=Sex)) +
      stat_ecdf(geom = "step") +
      labs(title="Cancer counts by State (ECDF plot)", 
           x="Count per 100,000", y="Quantile") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5), 
            axis.ticks = element_blank()) +   # Centre plot title
      scale_fill_brewer(palette = "Dark2")  # Color palette
  print(p)
  vis_file = "usa_cancer_counts_by_state_ecdf.png"
  ggsave(vis_file, p, units="in", width=10, height=6)
  return(vis_file)
}

vis_file = draw_ecdf_vis('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')
cat(sprintf("%s created..\n", vis_file))
