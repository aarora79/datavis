#'
#'HW1, R barplot visualization for AgeGroup Vs Cancer faceted by race.
#'======
#'CDC cancer dataset 1999-2012, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view
#'

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lazyeval)
library(geofacet)

SCALING_FACTOR = 1

draw_barplot_facet_vis <- function(fname) {
  # Visualizes cancer types (used interchangeably with cancer sites)
  # as count per age group per Race. Vis is created as a bar plot for 
  # count per age group and then "faceted" by Race to visualize the same
  # relationship for every Race.
  # The generated visualization is stored as a png file in the same
  # directory as the input file.
  #
  # Args:
  #   fname: filename of the CSV file containing the CDC data.
  # Returns:
  #   vis_file: filename of the saved visualization
  usa_cancer_df = read.csv(fname)
  usa_cancer_agegroup_race_df = usa_cancer_df %>%
                                group_by(AgeGroupCode, Race) %>%
                                summarise(Count = (sum(Count)/n())/SCALING_FACTOR)
  
  p = usa_cancer_agegroup_race_df %>% 
      #mutate(Count = Count/10000) %>%
      ggplot(aes(AgeGroupCode, Count)) +
      geom_col() +
      facet_wrap(~ Race, ncol=3) +
      labs(x= "Age Group", y = "Count per 100,000") +
      #theme(axis.text.x=element_text(angle=45, hjust=1))
      theme(plot.title = element_text(hjust = .5), 
            axis.ticks = element_blank()) +   # Centre plot title
      scale_fill_brewer(palette = "Dark2")  # Color palette
  
  print(p)
  vis_file = "usa_cancer_agegroup_race_facet.png"
  ggsave(vis_file, p, units="in", width=10, height=6)
  return(vis_file)
}

vis_file = draw_barplot_facet_vis('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')
cat(sprintf("%s created..\n", vis_file))