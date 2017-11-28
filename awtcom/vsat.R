#'
#'vsat.R, read the CSV files containing VSAT data from two different sources
#'======
#'Dataset is the vsat.csv file which contains information about all VSATs in the network'
#'

#install any missing packages
list.of.packages <- c("ggplot2", "dplyr", "ggthemes", "tidyr",
                      "tidyverse", "lazyeval", "futile.logger",
                      "zipcode", "rgdal", "ggfortify", 
                      "lubridate", "GGally", "gridExtra",
                      "ggmap", "grid", "RJSONIO", 
                      "ini", "randomForest", "officer",
                      "rvg", "png")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tidyverse)
library(lazyeval)
library(futile.logger)
library(zipcode)
library(rgdal)
library(ggfortify)
library(lubridate)
library(GGally)
library(gridExtra)
library(ggmap)
library(grid)
library(RJSONIO)
library(ini)
library(randomForest)
library(officer)
library(rvg)

#globals
APP_CFG_FILE_NAME = "app.cfg"
INPUT_FOLDER_PATH = ".//input"
DATA_FOLDER_PATH  = ".//data"

#bins for spectral efficiency
SPEC_EFFI_LOW    = "[0.5 to 2)"
SPEC_EFFI_MEDIUM = "[2 to 3)"
SPEC_EFFI_HIGH   = "[3 and above)"

#bins for volume usage
USAGE_LOW    = "< 100GB"
USAGE_MEDIUM = "100GB to 200GB"
USAGE_HIGH   = "200GB and higher"

#google map objects so that we dont have to query for the same map many times
google_map_objs = list()

#handy not-in function
'%!in%' <- function(x,y)!('%in%'(x,y))

STACK_TRACE <- function()
{
  # Gets the name of the calling function and logs it
  # Args:
  #   
  # Returns:
  #   None   
  #
  flog.info("%s, entry", as.character(deparse(sys.calls()[[sys.nframe()-1]])))
}

save_plot <- function(config, p, name, width=6, height=4, subdir="")
{
  # Saves the plot as a png file
  # Args:
  #   config (list): configuration parameters
  #   p (gg, ggplot): plot object
  #   name (string): filename for png file
  #   width (numeric, default 6): width in inches 
  #   height (numeric, default 4): height in inches
  #   subdir (string): optional sub directory for beam level plots
  # Returns:
  #   None   
  #
  dir.create(file.path("output"), showWarnings = FALSE)
  dir.create(file.path("output", config$network$short_name), showWarnings = FALSE)
  dir.create(file.path("output", config$network$short_name, config$gateway$gateway1.short_name), showWarnings = FALSE)
  if(subdir != "")
  {
    dir.create(file.path("output", config$network$short_name, config$gateway$gateway1.short_name, subdir), showWarnings = FALSE)
    vis_file = file.path("output", config$network$short_name, config$gateway$gateway1.short_name, subdir, name)
  } else {
    vis_file = file.path("output", config$network$short_name, config$gateway$gateway1.short_name, name)
  }
  ggsave(vis_file, p, units="in", width=width, height=height)
  flog.info("saved plot to %s", vis_file)
}

save_csv <- function(config, df, name)
{
  # Saves the dataframe as a CSV file
  # Args:
  #   config (list): configuration parameters
  #   df (data frame): plot object
  #   name (string): filename for csv file
  # Returns:
  #   None   
  #
  dir.create(file.path("output"), showWarnings = FALSE)
  dir.create(file.path("output", config$network$short_name), showWarnings = FALSE)
  dir.create(file.path("output", config$network$short_name, config$gateway$gateway1.short_name), showWarnings = FALSE)
  dir.create(file.path("output", config$network$short_name, config$gateway$gateway1.short_name, "csv"), showWarnings = FALSE)
  
  csv_file = file.path("output", config$network$short_name, config$gateway$gateway1.short_name, "csv", name)
  write.csv(df, csv_file, row.names = FALSE)
  flog.info("saved dataframe to %s", csv_file)
}

get_map_wrapper <- function (location, zoom, maptype, source)
{
  # google maps has a query limit so we want to save the maps we downloaded
  # otherwise we keep on hiting the limit. Saving the map and reusing it makes it faster
  # anyways. Each map is uniquely identified by combining all four characterstics  used to
  # create the map. so first we check if the unique key for identifying a map exists, if it 
  # does then the map already exists and we simply return the stored map object, if it does not
  # then we invoke the get_map function to get the map and then store it in the global data structure
  # Args:
  #   location
  #   zoom
  #   maptype
  #   source
  # Returns:
  #   map: ggmap object 
  #
  map_key_name = paste0(location, "_", zoom)
  if(map_key_name %in% names(google_map_objs))
  {
    flog.info("%s map at zoom %d FOUND, going to return stored map object", location, zoom)
    map <- google_map_objs[[map_key_name]]
  } else {
    #check if this map is stored locally already
    flog.info("%s map at zoom %d NOT FOUND, going to invoke get_map", location, zoom)
    map <- get_map(location=location, zoom=zoom, maptype=maptype, source=source)
    
    #store it in the global map object so that we can find it next time in this run of the program
    google_map_objs[[map_key_name]] <<- map
    flog.info("google_map_objs names->")
    flog.info(names(google_map_objs))
  }
  return(map)  
}

get_bits_per_symbol <- function(modcod_string)
{
  # Maps modcod string to spectral efficiency or bits per symbol
  # Args:
  #   modcod_string (string): modod string of the format "16APSK - four-fifth" 
  # Returns:
  #   effi (numeric): efficiency corresponding to the modcod string.
  #                   Return -1 in case no matching modcod is found and effi cannot be provided
  
  if(modcod_string == "16APSK - two-third") return(2.66);
  if(modcod_string == "16APSK - nine-tenth") return(3.6);
  if(modcod_string == "16APSK - four-fifth") return(3.2);
  if(modcod_string == "16APSK - twenty five-thirty sixth") return(2.77);
  if(modcod_string == "16APSK - twenty six- fourty fifth") return(2.311);
  if(modcod_string == "16APSK - seven-ninth") return(3.111);
  if(modcod_string == "16APSK - three-fourth") return(3);
  if(modcod_string == "16APSK - eight-ninth") return(3.555);
  if(modcod_string == "32APSK - two-third") return(3.333);
  if(modcod_string == "8PSK - eight-ninth") return(2.666);
  if(modcod_string == "8PSK - fifth-sixth") return(2.5);
  if(modcod_string == "8PSK - three-fourth") return(2.25);
  if(modcod_string == "8PSK - two-third") return(2);
  if(modcod_string == "QPSK - ninth-tenth") return(1.8);
  if(modcod_string == "8PSK - twenty five-thirty sixth") return(2.083);
  if(modcod_string == "16APSK - twenty eight-fourty fifth") return(2.488);
  if(modcod_string == "8PSK - three-fifth") return(1.8);
  if(modcod_string == "8PSK - twenty three-thirty sixth") return(1.916);
  if(modcod_string == "QPSK - fifth-sixth") return(1.666);
  if(modcod_string == "32APSK - twenty five-thirty sixth") return(3.472);
  if(modcod_string == "32APSK - seven-ninth") return(3.888);
  if(modcod_string == "32APSK - three-fourth") return(3.75);
  if(modcod_string == "16APSK - five-sixth") return(3.333);
  if(modcod_string == "QPSK - fourth-fifth") return(1.6);
  if(modcod_string == "QPSK - three-fourth") return(1.5);
  if(modcod_string == "QPSK - three-fifth") return(1.2);
  if(modcod_string == "QPSK - one-half") return(1);
  if(modcod_string == "QPSK - two-third") return(1.333);
  if(modcod_string == "QPSK - eleven-twenty") return(1.1);
  if(modcod_string == "QPSK - one-fourth") return(0.5);
  
  return (-1)
}

bin_spectral_efficiency <- function(effi)
{
  # Bin the spectral efficiency to convert continous to categorical
  # this is to make it easier to visualize when plotted on a map so that
  # we can use the binned spectral efficiency as a "color" parameter
  # Args:
  #   effi (numeric): Spectral efficiency, anything from [1, 4)
  # Returns:
  #   binned_effi (string): "[1 to 2)", "[2 to 3)", "[3 and above)", "invalid"
  
  if(effi >= 0.5 && effi < 2) return(SPEC_EFFI_LOW)
  if(effi >= 2 && effi < 3) return(SPEC_EFFI_MEDIUM)
  if(effi >= 3) return(SPEC_EFFI_HIGH)
  return("invalid")
}



bin_total_usage <- function(usage)
{
  # Bin the total usage in GB to convert continous to categorical
  # this is to make it easier to visualize when plotted on a map so that
  # we can use the binned usage as a "color" parameter
  # Args:
  #   usage (numeric): total volume usage for the user in this billing cycle
  # Returns:
  #   binned_usage (string): "<= 50GB", "51GB to 100GB", "101GB to 200GB", "201GB and higher", "invalid"
  
  if(usage < 100) return(USAGE_LOW)
  if(usage >= 100 && usage < 200) return(USAGE_MEDIUM)
  if(usage >= 200) return(USAGE_HIGH)
  
  flog.error("returning invalid for usage %f", usage)
  return("invalid")
}

geocode_address <- function(address) {
  # Uses google maps api to get lat long of a location
  # Args:
  #   address: address to be looked up
  # Returns:
  #   list object containing lat/long
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", "&key=AIzaSyArLKF6RjIx2vpViGyg742i6lhra_g8UFI", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
    flog.info("found lat/long for location %s", address)
    flog.info(out)
  } else {
    flog.info("got error response from Google API when trying to lookup lat/long")
    flog.info(x)
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  return(out)
}


read_terminal_data_from_nms <- function(config) {
  # Reads a locally stored (in the same directory as this script) CSV file
  # Args:
  #   
  # Returns:
  #   tibble for the dataframe
  STACK_TRACE()
  fname = file.path(DATA_FOLDER_PATH, config$network$short_name,
                    config$gateway$gateway1.short_name,
                    config$general$csv_file_name_for_vsat_info_from_nms)
  flog.info("going to read dataset %s", fname)
  df <- read.csv(fname) %>% as.tibble()
  return(df)
}

read_terminal_data_from_ipgw <- function(config) {
  # Reads a locally stored (in the same directory as this script) CSV file
  # containing ATL command output from all the IPGWs in a GW
  # Args:
  #   
  # Returns:
  #   tibble for the dataframe
  STACK_TRACE()
  
  fname = file.path(DATA_FOLDER_PATH, config$network$short_name,
                    config$gateway$gateway1.short_name,
                    config$general$csv_file_name_for_vsat_info_from_ipgw)
  flog.info("going to read dataset %s", fname)
  df_vsat_data_from_ipgw <- read.csv(fname) %>% as.tibble()
  return(df_vsat_data_from_ipgw)
}

get_subtitle <- function(df)
{
  # creates a subtitle string by taking the first and last date from the dataframe
  # 
  # Args: 
  #   df: data frame
  # Returns:
  #   subtitle: string of the format "Timespan: 1/1/2015 - 11/15/2015"
  timespan = paste0(df$Date[1], " - ", df$Date[NROW(df)])
  subtitle = paste0("Timespan: ", timespan)
  return(subtitle)
}

convert_ISO6709  <- function(latlon)
{
  # Converts the lat/long values from ISO6709 to degrees so that it is easier
  # to plot.
  #
  # Args: 
  #   latlon: Latitude or Longitude in ISO 6709
  # Returns:
  #   out_latlong: Latitude/Longitude in degrees
  
  # will just do lat and lon together, as the process is the same for both
  # It's simpler to do the arithmetic on positive numbers, we'll add the signs
  #  back in at the end.
  sgns   <- sign(latlon)
  latlon <- abs(latlon)
  
  # grab the MM.MMMM bit, which is always <100. '%%' is modular arithmetic.
  mm <- latlon %% 100
  
  # grab the DD bit. Divide by 100 because of the MM.MMMM bit.
  dd <- (latlon - mm)/100
  
  # convert to decimal degrees, don't forget to add the signs back!
  out_latlon <- (dd+mm/60) * sgns
  return(out_latlon)
}

get_beam_centers <- function(config)
{
  # Get beam centers as a dataframe. Takes the beam centers location (as a string)
  # as a list and geocodes it, returns the result in a dataframe
  #
  # Args: 
  #   beam_centers: list, list of strings representing the beam centers, 
  #                 for eg. "Madrid, Spain"
  # Returns:
  #   beam_centers_df: dataframe, dataframe with fields "lat", "lng" and "loc"
  
  STACK_TRACE()
  
  keys = names(config$gateway)
  flog.info("key names under config$gateway")
  flog.info(keys)
  beam_ids = config$gateway[unlist(keys[grep(".id", keys)])]
  beam_center_country = config$gateway[unlist(keys[grep(".country", keys)])]
  
  beam_centers = config$gateway[unlist(keys[grep(".beam_center", keys)])]
  flog.info("beam centers ->")
  flog.info(beam_centers)
  num_loc = length(beam_centers)
  flog.info("there are %d beams", num_loc)
  lat = vector(mode="numeric", num_loc)
  lng = vector(mode="numeric", num_loc)
  locs = vector(mode="character", num_loc)
  beam_id_vector = vector(mode="character", num_loc)
  country_vector = vector(mode="character", num_loc)
  i = 1
  for(location in beam_centers)
  {
    flog.info("going to lookup lat/long for %s", location)
    lat_long = geocode_address(location)
    lat[[i]] = lat_long[1]
    lng[[i]] = lat_long[2]
    locs[[i]] = location
    beam_id_vector[[i]] = paste0("Beam ", beam_ids[i])
    country_vector[[i]] = beam_center_country[[i]]
    i = i + 1
  }
  
  beam_centers_df = data.frame(lat=lat, lng=lng, loc=locs, ABSOLUTE_BEAM_ID=beam_id_vector,
                               country=country_vector)
  return(beam_centers_df)
}

plot_terminal_locations_binned <- function(config, beam_centers_df, df)
{
  # Plots a stat_bin_2d plot for Terminals in all beams. The idea is to take care
  # of overplotting by using binn'ing. Look at
  # https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf for reference.
  #
  # Args: 
  #   beam_centers_df: dataframe, dataframe containing lat/lng/loc for beam centers
  #   df: dataframe, dataframe containing lat/long and other information for each terminal.
  # Returns:
  #   
  #begin plotting things on map
  STACK_TRACE()
  #df = df %>%
  #     filter(ABSOLUTE_BEAM_ID == "Beam 20")
  map<-get_map_wrapper(location=config$gateway$gateway1.location_for_map, zoom=config$gateway$gateway1.zoom_level_for_map, maptype='roadmap', 
                       source='google')
  
  
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
    stat_bin2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
      size = .5, bins = 60, alpha = 0.3,
      data = df
    ) + 
    #facet_wrap(~ ABSOLUTE_BEAM_ID) +
    labs(title=paste0("Distribution of Terminals ", config$gateway$gateway1.nw_name),
         x="Longitude", y="Latitude", subtitle=config$gateway$beam_centers_location_text) +
    theme_tufte() + 
    theme(legend.position="bottom") +
    theme(strip.text.x = element_text(size = 7)) + 
    scale_colour_discrete(guide = FALSE) +
    guides(fill=guide_legend(title="")) +
    theme(plot.title = element_text(size=10)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(p)

  #save_plot(config, p, "terminal_map_binned.png", width=4.5, height=6)
  
  #now make the same plot at a per country level
  countries = unique(df$country)
  for(c in countries)
  {
    #filter by each country, one at a time
    flog.info("going to plot terminal density for beams in %s", c)
    df_country = df %>%
                 filter(country == c) %>%
                 group_by(ABSOLUTE_BEAM_ID) %>%
                 mutate(count = n()) %>%
                 filter(count != 0)
    save_csv(config, df_country, paste0("terminals_in_", c, ".csv"))
    #do this little trick to remove unused factors after filtering
    df_country$ABSOLUTE_BEAM_ID = factor(df_country$ABSOLUTE_BEAM_ID)
    
    flog.info("following beams are present in %s", c)
    flog.info(unique(df_country$ABSOLUTE_BEAM_ID))
    map<-get_map_wrapper(location=c, zoom=config$gateway$gateway1.zoom_level_for_map_beam, maptype='roadmap', 
                 source='google')
    subtitle = paste0(c, " beams, ", config$gateway$gateway1.short_name, " Gateway")
    p = ggmap(map) +
       geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
       stat_bin2d(
        aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
        size = .5, bins = 60, alpha = 0.3,
        data = df_country
      ) + 
      labs(title=paste0("Distribution of Terminals ", config$gateway$gateway1.nw_name),
           x="Longitude", y="Latitude", subtitle=subtitle) +
      theme_tufte() + 
      theme(legend.position="bottom") +
      scale_colour_discrete(guide = FALSE) +
      guides(fill=guide_legend(title="")) +
      theme(plot.title = element_text(size=10)) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
      
    
    print(p)

    fname = paste0(c, "_", "terminal_map_binned.png")
    save_plot(config, p, fname, subdir="maps", width=4.5, height=6)
  }
}

#binned terminal map with modcod info
plot_terminal_locations_w_modcod_info_binned <- function (config, beam_centers_df, df_merged)
{
  # Plots a stat_bin_2d plot for Terminals in all beams. The idea is to take care
  # of overplotting by using binn'ing. Look at
  # https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf for reference.
  #modcod information is overlayed on this by coloring the bins based on spectral efficiency
  #
  # Args: 
  #   beam_centers_df: dataframe, dataframe containing lat/lng/loc for beam centers
  #   df: dataframe, dataframe containing lat/long and other information for each terminal.
  # Returns:
  #   
  #begin plotting things on map
  STACK_TRACE()
  
  map<-get_map_wrapper(location=config$gateway$gateway1.location_for_map, zoom=config$gateway$gateway1.zoom_level_for_map, maptype='roadmap', 
               source='google')
  
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
    stat_bin2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = SPECTRAL_EFFICIENCY_BINNED, fill = SPECTRAL_EFFICIENCY_BINNED),
      size = .5, bins = 60, alpha = 0.3,
      data = df_merged
    ) + 
    facet_wrap(~ ABSOLUTE_BEAM_ID) +
    labs(title=paste0("Spectral efficiency in ", config$gateway$gateway1.nw_name),
         x="Longitude", y="Latitude", subtitle=config$gateway$beam_centers_location_text) +
    theme_tufte() + 
    theme(legend.position="bottom") +
    theme(strip.text.x = element_text(size = 7)) + 
    scale_colour_discrete(guide = FALSE) +
    guides(fill=guide_legend(title="")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(p)
  save_plot(config, p, "terminal_map_w_modcod_binned.png", subdir="maps", width=4.5, height=6)
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
    stat_bin2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
      size = .5, bins = 60, alpha = 0.3,
      data = df_merged
    ) + 
    facet_wrap(~ SPECTRAL_EFFICIENCY_BINNED) +
    labs(title=paste0("Spectral efficiency in ", config$gateway$gateway1.nw_name),
         x="Longitude", y="Latitude", subtitle=config$gateway$beam_centers_location_text) +
    theme_tufte() + 
    theme(legend.position="bottom") +
    theme(plot.title = element_text(size=10))+
    #theme(strip.text.x = element_text(size = 7)) + 
    scale_colour_discrete(guide = FALSE) +
    guides(fill=guide_legend(title="")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(p)
  #save_plot(config, p, "terminal_map_w_modcod_binned_2.png", width=4.5, height=6)
  
  #now make the same plot at a per country level
  countries = unique(df_merged$country)
  for(c in countries)
  {
    #filter by each country, one at a time
    flog.info("going to plot terminal density for beams in %s", c)
    df_country = df_merged %>%
                 filter(country == c) 
    
    flog.info("following beams are present in %s", c)
    flog.info(unique(df_country$ABSOLUTE_BEAM_ID))
    map<-get_map_wrapper(location=c, zoom=config$gateway$gateway1.zoom_level_for_map_beam, maptype='roadmap', 
                 source='google')
    subtitle = paste0(c, " beams, ", config$gateway$gateway1.short_name, " Gateway")
    p = ggmap(map) +
      geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
      stat_bin2d(
        aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
        size = .5, bins = 60, alpha = 0.3,
        data = df_country
      ) + 
      facet_wrap(~ SPECTRAL_EFFICIENCY_BINNED) +
      labs(title=paste0("Spectral efficiency in ", config$gateway$gateway1.nw_name),
           x="Longitude", y="Latitude", subtitle=subtitle) +
      theme_tufte() + 
      theme(legend.position="bottom") +
      theme(plot.title = element_text(size=12))+
      #theme(strip.text.x = element_text(size = 7)) + 
      scale_colour_discrete(guide = FALSE) +
      guides(fill=guide_legend(title="")) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    print(p)
    fname = paste0(c, "_", "terminal_map_w_modcod_binned_2.png")
    save_plot(config, p, fname, subdir="maps", width=4.5, height=6)
  }
}


#binned terminal map with usage info
plot_terminal_locations_w_usage_info_binned <- function (config, beam_centers_df, df_merged)
{
  # Plots a stat_bin_2d plot for Terminals in all beams. The idea is to take care
  # of overplotting by using binn'ing. Look at
  # https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf for reference.
  #modcod information is overlayed on this by coloring the bins based on spectral efficiency
  #
  # Args: 
  #   beam_centers_df: dataframe, dataframe containing lat/lng/loc for beam centers
  #   df: dataframe, dataframe containing lat/long and other information for each terminal.
  # Returns:
  #   
  #begin plotting things on map
  STACK_TRACE()
  
  cols = c("<= 50GB" = "pink", "51GB to 100GB"="yellow", "101GB to 200GB"="orange",
           "201GB and higher"="red", "invalid"="black")
  
  map<-get_map_wrapper(location=config$gateway$gateway1.location_for_map, zoom=config$gateway$gateway1.zoom_level_for_map, maptype='roadmap', 
               source='google')
  Sys.sleep(0.5) #sleep for 200ms otherwise we end up querying google api too fast
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=2, shape=23) +
    stat_bin2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, color = TOTAL_USAGE_BINNED, fill = TOTAL_USAGE_BINNED),
      size = .5, bins = 60, alpha = 0.3,
      data = df_merged
    ) + 
    facet_wrap(~ ABSOLUTE_BEAM_ID) +
    labs(title=paste0("Distribution of Terminals ", config$gateway$gateway1.nw_name),
         x="Longitude", y="Latitude", subtitle=config$gateway$beam_centers_location_text) +
    theme_tufte() + 
    theme(legend.position="bottom") +
    theme(strip.text.x = element_text(size = 7)) + 
    scale_colour_discrete(guide=FALSE) +
    guides(fill=guide_legend(title="")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(p)
  #save_plot(config, p, "terminal_map_w_usage_binned.png", width=4.5, height=6)
  
  #now make the same plot at a per country level
  countries = unique(df_merged$country)
  for(c in countries)
  {
    #filter by each country, one at a time
    flog.info("going to plot terminal density for beams in %s", c)
    df_country = df_merged %>%
                 filter(country == c) 
    
    flog.info("following beams are present in %s", c)
    flog.info(unique(df_country$ABSOLUTE_BEAM_ID))
    map<-get_map_wrapper(location=c, zoom=config$gateway$gateway1.zoom_level_for_map_beam, maptype='roadmap', 
                 source='google')
    subtitle = paste0(c, " beams, ", config$gateway$gateway1.short_name, " Gateway")
    p = ggmap(map) +
        geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
        stat_bin2d(
         aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, color = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
         size = .5, bins = 60, alpha = 0.3,
         data = df_country
        ) + 
        facet_wrap(~ TOTAL_USAGE_BINNED) +
        labs(title=paste0("Usage based distribution of Terminals\n", config$gateway$gateway1.nw_name),
             x="Longitude", y="Latitude", subtitle=subtitle) +
        theme_tufte() + 
        theme(legend.position="bottom") +
        theme(strip.text.x = element_text(size = 10)) + 
        theme(plot.title = element_text(size=12))+
        scale_colour_discrete(guide=FALSE) +
        guides(fill=guide_legend(title="")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) + 
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    
    print(p)
    fname = paste0(c, "_", "terminal_map_w_usage_binned.png")
    save_plot(config, p, fname, subdir="maps", width=4.5, height=6)
  }
}

plot_terminal_growth <- function(config, beam_centers_df, df)
{
  # Plots a stat_bin_2d plot for Terminals in all beams. The idea is to take care
  # of overplotting by using binn'ing. Look at
  # https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf for reference.
  #
  # Args: 
  #   beam_centers_df: dataframe, dataframe containing lat/lng/loc for beam centers
  #   df: dataframe, dataframe containing lat/long and other information for each terminal.
  # Returns:
  #   
  STACK_TRACE()
  df = df_merged
  df = df %>%
       mutate(year = year(ymd_hms(ACTIVATION_DATE))) %>%
       mutate(month = substr(months(ymd_hms(ACTIVATION_DATE)),1,3)) %>% #months gets month as string
       arrange(desc(year, month)) %>%
       mutate(month_year = paste0(month, ", ", year))
  
  #MONTHS = c("January", "February", "March", "April", "May", "June",
  #           "July", "August", "September", "October", "November", "December")
  MONTHS = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_year_list = c()
  for (y in sort(unique(df$year)))
  {
    for(m in MONTHS)
    {
      m_y = paste0(m, ", ", y)
      if(m_y %in% df$month_year)
      {
        month_year_list = append(month_year_list, m_y)
      }
    }
  }
  df = within(df, month_year <- factor(month_year, levels = month_year_list))
  
  #begin plotting things on map
  map<-get_map_wrapper(location=config$gateway$gateway1.location_for_map, zoom=config$gateway$gateway1.zoom_level_for_map, maptype='roadmap', 
               source='google')
  
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=2, shape=23) +
    stat_bin2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
      size = .5, bins = 60, alpha = 0.3,
      data = df
    ) + 
    facet_wrap(~ month_year) +
    labs(title=paste0("How has the ", config$gateway$gateway1.nw_name, " grown?"),
         x="Longitude", y="Latitude", subtitle="Each year/month combination shows new activations in that month") +
    theme_tufte() + 
    theme(legend.position="bottom") +
    theme(strip.text.x = element_text(size = 7)) + 
    scale_colour_discrete(guide = FALSE) +
    guides(fill=guide_legend(title="")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(p)
  #save_plot(config, p, "terminal_growth_binned.png", width=4.5, height=6)
  
  #now make the same plot at a per country level
  countries = unique(df_merged$country)
  for(c in countries)
  {
    #filter by each country, one at a time
    flog.info("going to plot terminal density for beams in %s", c)
    df_country = df %>%
                 filter(country == c) 
    
    flog.info("following beams are present in %s", c)
    flog.info(unique(df_country$ABSOLUTE_BEAM_ID))
    map<-get_map_wrapper(location=c, zoom=config$gateway$gateway1.zoom_level_for_map_beam_terminal_binned, maptype='roadmap', 
                 source='google')
    subtitle = paste0(c, " beams, ", config$gateway$gateway1.short_name, " Gateway")
    
    p = ggmap(map, extent = "device") +
        geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
        stat_bin2d(
          aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, colour = ABSOLUTE_BEAM_ID, fill = ABSOLUTE_BEAM_ID),
          size = .5, bins = 60, alpha = 0.3,
          data = df_country
        ) + 
        facet_wrap(~ month_year) +
        labs(title=paste0("How has the ", config$gateway$gateway1.nw_name, " grown?"),
             x="", y="", subtitle=subtitle, caption="Each year/month combination shows new activations in that month") +
        theme_tufte() + 
        theme(legend.position="bottom") +
        theme(strip.text.x = element_text(size = 7)) + 
        scale_colour_discrete(guide = FALSE) +
        guides(fill=guide_legend(title="")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) + 
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    print(p)
    fname = paste0(c, "_", "terminal_growth_binned.png")
    save_plot(config, p, fname, subdir="maps", width=4.5, height=6)
  }
  
  #bar chart for month on month growth
  p = df %>%
      group_by(ABSOLUTE_BEAM_ID, year, month, month_year) %>%
      summarise(count = n()) %>%
      ggplot(aes(x=month_year, y=count, fill=ABSOLUTE_BEAM_ID)) +   
      geom_col(position="dodge", width = .6) + 
      labs(title="New Terminal activations", 
           x="", y="Count", subtitle=config$gateway$gateway1.nw_name, caption="") +
      theme_bw() +
      theme(legend.position="bottom", 
            legend.background = element_rect(fill = "transparent", size = 1), 
            axis.text.x = element_text(angle = 90, vjust=1, hjust=1)) +
      guides(fill=guide_legend(title=""))
  #if there are more than 2 beams then facet the chart by beam
  if(NROW(beam_centers_df) > 2)
  {
    p = p + facet_wrap(~ABSOLUTE_BEAM_ID, ncol = 3) + theme(legend.position="none")
  }
  if(NROW(beam_centers_df) <= 8)
  {
    p = p + scale_fill_brewer(palette = "Dark2")  # Color palette
    width = 6
    height = 4
  } else {
    width = 8
    height = 6
  }
  print(p)
  save_plot(config, p, "terminal_activations_bar_graph.png", width, height, subdir="plots")
}

plot_terminal_locations_density <- function(config, beam_centers_df, df)
{
  # Plots a stat_density_2d plot for Terminals in all beams. The idea is to take care
  # of overplotting by using kernel density plot. Look at
  # https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf for reference.
  #
  # Args: 
  #   beam_centers_df: dataframe, dataframe containing lat/lng/loc for beam centers
  #   df: dataframe, dataframe containing lat/long and other information for each terminal.
  # Returns:
  #   
  #begin plotting things on map
  STACK_TRACE()
  
  map<-get_map_wrapper(location=config$gateway$gateway1.location_for_map, zoom=config$gateway$gateway1.zoom_level_for_map_beam_terminal_density, maptype='roadmap', 
               source='google')
  
  p = ggmap(map) +
    geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=2, shape=23) +
    stat_density2d(
      aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, fill = ..level.., alpha = ..level..),
      size = 1, bins = 8, data = df,
      geom = "polygon"
    ) +
    facet_wrap(~ ABSOLUTE_BEAM_ID) +
    labs(title=paste0("Density plot of Terminals in ", config$gateway$gateway1.nw_name),
         x="Longitude", y="Latitude", subtitle=config$gateway$beam_centers_location_text) +
    theme_tufte()  +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size = 9)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    #guides(fill=guide_legend(title=""), col=guide_legend(title=""))
  
  print(p)
  #save_plot(config, p, "terminal_map_density.png", 4.5, 6)
  
  #now make the same plot at a per country level
  countries = unique(df$country)
  for(c in countries)
  {
    #filter by each country, one at a time
    flog.info("going to plot terminal density for beams in %s", c)
    #we have to do this funny trick of creating a new column for beams
    #because there seems to be no way to make ggplot2 forget about filtered
    #out beams while doing facet_wrap
    df_country = df_merged %>%
                 filter(country == c) %>%
                 mutate(NEW_ABSOLUTE_BEAM_ID = ABSOLUTE_BEAM_ID)
    flog.info("following beams are present in %s", c)
    flog.info(unique(df_country$ABSOLUTE_BEAM_ID))
    map<-get_map_wrapper(location=c, zoom=config$gateway$gateway1.zoom_level_for_map_beam_terminal_density, maptype='roadmap', 
                 source='google')
    subtitle = paste0(c, " beams, ", config$gateway$gateway1.short_name, " Gateway")
    p = ggmap(map) +
      geom_point(data=beam_centers_df, aes(x=lat, y=lng), fill="blue", size=config$gateway$icon_size, shape=23) +
      stat_density2d(
        aes(x = LATITUDE_DEGREES, y = LONGITUDE_DEGREES, fill = ..level.., alpha = ..level..),
        size = 1, bins = 8, data = df_country,
        geom = "polygon"
      ) +
      facet_wrap(~ NEW_ABSOLUTE_BEAM_ID) +
      labs(title=paste0("Terminal density plot, ", config$gateway$gateway1.nw_name),
           x="Longitude", y="Latitude", subtitle=subtitle) +
      theme_tufte()  +
      theme(legend.position="none") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    print(p)
    fname = paste0(c, "_", "terminal_map_density.png")
    save_plot(config, p, fname, subdir="maps", width=4.5, height=6)
  }
}

plot_service_plan_distribution <- function(config, df)
{
  # Plots a bar plot for the number of terminals per service plan
  # Args: 
  #   
  #   df: dataframe, dataframe containing lat/long, service plan and other information for each terminal.
  # Returns:
  #   
  STACK_TRACE()
  df_plans = df %>%
             group_by(ABSOLUTE_BEAM_ID, SERVICE_PLAN_NAME) %>%
             summarize(count = n())
  save_csv(config, df_plans, "service_plans.csv")
  
  p = df_plans %>%
      filter(count > 10) %>%
      ggplot(aes(x=reorder(SERVICE_PLAN_NAME, -count), y=count)) +   
      geom_col(position="dodge", width = .6) + 
      labs(title="Service plan distribution", 
           x="", y="Terminal Count", subtitle=config$gateway$gateway1.nw_name, caption="Plans with less than 10 Terminals not shown.") +
      facet_wrap(~ABSOLUTE_BEAM_ID) +
      theme_bw() + 
      theme(legend.position=c(0.8, 0.7), 
            legend.background = element_rect(fill = "transparent", size = 1), 
            axis.text.x = element_text(angle = 90, vjust=1, hjust=1, size=7)) 
  print(p)
  
  save_plot(config, p, "service_plan_distribution.png", 6, 4, subdir="plots")
  
  
  df_plans = df %>%
    filter(SPECTRAL_EFFICIENCY_BINNED == SPEC_EFFI_LOW) %>%
    group_by(ABSOLUTE_BEAM_ID, SERVICE_PLAN_NAME) %>%
    summarize(count = n())
  
  p = df_plans %>%
    filter(count > 10) %>%
    ggplot(aes(x=reorder(SERVICE_PLAN_NAME, -count), y=count)) +   
    geom_col(position="dodge", width = .6) + 
    labs(title="Service plan distribution for low spectral efficiency Terminals", 
         x="", y="Terminal Count", subtitle=config$gateway$gateway1.nw_name, caption="Plans with less than 10 Terminals not shown.") +
    facet_wrap(~ABSOLUTE_BEAM_ID) +
    theme_bw() + 
    theme(legend.position=c(0.8, 0.7), 
          legend.background = element_rect(fill = "transparent", size = 1), 
          axis.text.x = element_text(angle = 0, vjust=1, hjust=1)) 
  print(p)
  save_plot(config, p, "service_plan_distribution_for_low_spectral_efficiency_terminals.png", subdir="plots")
}


#main program starts here

#read configuration
#first read the nw.cfg file to see which network do we want to analyze
nw_Cfg_file = file.path(".//input", "nw.cfg")
#if files does not exists then dont continue
stopifnot(file.exists(nw_Cfg_file))

nw_config = read.ini(nw_Cfg_file)

#read config files from input folder. All config files are named
#like "app.cfg.<nw name>.<gw name"
app_cfg_file = nw_config$general$cfg_file_name

app_cfg_file = file.path(".//input", app_cfg_file)
config = read.ini(app_cfg_file)
config$gateway$gateway1.zoom_level_for_map = as.numeric(config$gateway$gateway1.zoom_level_for_map)
config$gateway$gateway1.zoom_level_for_map_beam = as.numeric(config$gateway$gateway1.zoom_level_for_map_beam)
config$gateway$icon_size = as.numeric(config$gateway$icon_size)
config$gateway$gateway1.zoom_level_for_map_beam_terminal_binned = as.numeric(config$gateway$gateway1.zoom_level_for_map_beam_terminal_binned)
config$gateway$gateway1.zoom_level_for_map_beam_terminal_density = as.numeric(config$gateway$gateway1.zoom_level_for_map_beam_terminal_density)

flog.info("Starting the %s module, config read from %s",
          config$general$module_name, app_cfg_file)

#load any stored maps from the previous run
map_RData = file.path("data", config$network$short_name, config$gateway$gateway1.short_name, "tmp", "map_data.RData")
if(file.exists(map_RData)) {
  flog.info("going to read map data from %s", map_RData)
  load(map_RData)
  flog.info("found the following maps in the stored map_RData")
  flog.info(names(google_map_objs))
} else {
  flog.info("did not find %s, all needed maps will be downloaded...", map_RData)
}


#lets start with reading the data
df = read_terminal_data_from_nms(config)

flog.info("here is the data we have ---->")
#print it out, since this is a tibble, only 5 rows would be printed
df

#convert the latlong to degrees, the data has lat/long values swapped
#so we need to take care of that as well
if ("LATITUDE_DEGREES" %!in% names(df) &&
    "LATITUDE_DEGREES" %!in% names(df))
{
  df = df %>%
       mutate(LATITUDE_DEGREES = sapply(LATITUDE, convert_ISO6709),
              LONGITUDE_DEGREES = sapply(LONGITUDE, convert_ISO6709))
} 
df = df %>%
     mutate(ABSOLUTE_BEAM_ID = paste0("Beam ", ABSOLUTE_BEAM_ID)) %>%
     rename(DEVICEID=DEVICE_ID)

#get beam centers
beam_centers_df = get_beam_centers(config)

#now read IPGW data also, this is the ATL command data from the IPGW
#exclude rows with empty device id (why does this happen at all?)
#add new column to get spectral efficiency from modcod and another one
#to bin the spectral efficiency
df_atl = read_terminal_data_from_ipgw(config)
df_atl = df_atl %>% 
  filter(DEVICEID != "") %>%
  mutate(SPECTRAL_EFFICIENCY = sapply(CurrentModCodValue, get_bits_per_symbol)) %>%
  mutate(SPECTRAL_EFFICIENCY_BINNED = sapply(SPECTRAL_EFFICIENCY, bin_spectral_efficiency)) %>%
  mutate(TOTAL_USAGE = round(CAPOVERALL - USGOVERALL)/1000) %>%
  mutate(TOTAL_USAGE_BINNED = sapply(TOTAL_USAGE, bin_total_usage))

df_error= df_atl %>% filter(SPECTRAL_EFFICIENCY == -1)
save_csv(config, df_error, "error.csv")

#join the two datasets, so that we can combine geolocation with say modcod information
df_merged = merge(df, df_atl, by="DEVICEID")
#also join beam center information
df_merged = merge(df_merged, beam_centers_df, by="ABSOLUTE_BEAM_ID")
save_csv(config, df_merged, "vsat_merged_df.csv")

#binned terminal map
Sys.sleep(1)  # API only allows 5 requests per second
plot_terminal_locations_binned(config, beam_centers_df, df_merged)

#binned terminal map with modcod overlayed
Sys.sleep(1)  # API only allows 5 requests per second
plot_terminal_locations_w_modcod_info_binned(config, beam_centers_df, df_merged)

#binned terminal map with usage overlayed
Sys.sleep(1)  # API only allows 5 requests per second
plot_terminal_locations_w_usage_info_binned(config, beam_centers_df, df_merged)

#density plot for terminals
Sys.sleep(1)  # API only allows 5 requests per second
plot_terminal_locations_density(config, beam_centers_df, df_merged)

#service plan information
Sys.sleep(1)  # API only allows 5 requests per second
plot_service_plan_distribution(config, df)

#new terminal activation by month
Sys.sleep(1)  # API only allows 5 requests per second
plot_terminal_growth(config, beam_centers_df, df_merged)

#finally save the map data in the .RData file, so that any new maps we downloaded
#in this run are also now saved
#save the map for next time this program is run
map_fname = file.path("data", config$network$short_name, config$gateway$gateway1.short_name, "tmp", "map_data.RData")
flog.info("saving map data locally to %s", map_fname)
save(google_map_objs, file = map_fname)

