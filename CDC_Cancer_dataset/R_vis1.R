#'
#'HW1, R geofacet'ing visualization for CDC cancer dataset.
#'======
#'CDC cancer dataset 1999-2012, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view
#'

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lazyeval)
library(geofacet)

SCALING_FACTOR = 1000

draw_geofacet_vis <- function(fname) {
  # Visualizes cancer types (used interchangeably with cancer sites)
  # as count per U.S. state. Vis is created as a bar plot for count per
  # cancer site and then "geofaceted" to visualize by U.S. states.
  # The generated visualization is stored as a png file in the same
  # directory as the input file.
  #
  # Args:
  #   fname: filename of the CSV file containing the CDC data.
  # Returns:
  #   vis_file: filename of the saved visualization
  #fname = 'USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv'
  usa_cancer_df = read.csv(fname)
  
  p = usa_cancer_df %>% 
      group_by(State, LeadingCancerSites) %>%
      summarize(Count = (sum(Count)/n())/SCALING_FACTOR) %>%
      ggplot(aes(LeadingCancerSites, Count, fill = LeadingCancerSites)) +
      geom_col() +
      coord_flip() +
      facet_geo(~ State) +
      labs(x= "Leading Cancer Sites", y = "Count (in thousands) per 100,000") +
      theme_bw() + 
      theme(text = element_text(size=8))
  
  print(p)
  vis_file = "usa_cancer_geo_facet.png"
  ggsave(vis_file, p, units="in", width=10, height=6)
  return(vis_file)
}

vis_file = draw_geofacet_vis('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')
cat(sprintf("%s created..\n", vis_file))