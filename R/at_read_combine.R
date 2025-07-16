#' Script for initial steps of reading in and combining the acoustic data from
#' multiple files. Code is adapted from Cole Monnahan's initial work.

library(here)
library(dplyr)
library(purrr)
library(readxl)

# Loop through files & pull them into R for processing 
yrs <- c(2007:2010, 2012, 2014, 2016, 2018)

read_folder <- function(yr) {
  print(yr)
  files <- list.files(path = here("data", "at", yr), 
                      pattern = "\\.xlsx$", full.names = TRUE)
  
  map_df(files, read_excel) %>%
    mutate(year = yr)
}
df <- map_df(yrs, read_folder)

# Clean up columns
df$start_lon[which(df$start_lon > 100)] <- df$start_lon[which(df$start_lon > 100)] - 360
df$end_lon[which(df$end_lon > 100)] <- df$end_lon[which(df$end_lon > 100)] - 360

dat0 <- df %>% mutate(
  density = biomass_nm2,
  year = as.numeric(as.character(year)),
  bottom = -1 * layer_bottom_height,
  top = -1 * layer_top_height,
  ## Distance of the intervals in KM
  dist=raster::pointDistance(cbind(start_lon, start_lat),
                             cbind(end_lon, end_lat),
                             lonlat=TRUE)/1000,
  lon = start_lon,  #(start_lon+end_lon)/2,
  lat = start_lat,  #(start_lat+end_lat)/2,
  ## Classify the class as pollock or not. First strip out some random quotation marks in the class names
  class = gsub('\"',  "", x = class),
  ## There are some in FSHGRP_2 but Nate says to ignore them
  pollock = class %in% c('PK1', 'PK2', "PK3", "PK1_FILTERED"),
  duration = as.numeric(end_time - start_time, 'mins'),
  ## Now we should have a unique key with this combination
  key = paste(year, interval, class, as.numeric(factor(paste0(bottom, "-", top))), sep = '_'),
  ## These are useful below
  key2 = paste(year, interval, class, sep = '_'),
  key3 = paste(year, transect, interval, sep = '_')
)

## Calculate the surface height which is the top of the highest bin at each
## unique interval (across all layers and classes which is key3) and the
## ground depth which is the smallest of the lower bin
dat <- dat0 %>% group_by(key3) %>%
  mutate(surface = max(top), ground = min(bottom)) %>%
  ungroup() %>%
  select(-start_lon, -end_lon, -start_lat, -end_lat, -start_time,
         -start_date, -end_time, -end_date, -class, -start_log, -end_log,
         -NASC, -numbers_nm2, -biomass_nm2, -numbers, -biomass, -layer,
         -layer_bottom_height, -layer_top_height,
         -interval, -transect) 

saveRDS(dat, file = here("data", "at", "at_combined.rds"))
