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

# -----------------------------------------------------------------------------
## Now everything <3m comes from a difference source

## These have different column names and aren't consistent so just reading
## them in manually
x2007 <- cbind(year=2007, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2007.xlsx")))[,1:6])
x2008 <- cbind(year=2008, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2008.xlsx")))[,1:6])
x2009 <- cbind(year=2009, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2009.xlsx")))[,1:6])
x2010 <- cbind(year=2010, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2010.xlsx")))[,1:6])
x2012 <- cbind(year=2012, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2012.xlsx")))[,1:6])
x2014 <- cbind(year=2014, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2014.xlsx")))[,1:6])
x2016 <- cbind(year=2016, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2016.xlsx")))[,1:6])
x2018 <- cbind(year=2018, data.frame(read_excel(here("data", "at", "Below3m", "Results_below_3_m2018.xlsx")))[,1:6])

## Process this below3 data a bit. The files are not the same so
## **** BE VERY CAREFUL IF THESE CHANGE ****
### From Nate: you can assume 10 nm^2 before 2016.  There were
### some variety from 10 in 2016 & 2018, and I think that's why I
### explicitly made a column for area.
warning("!! below3 files are handled specially, if changed you must update the code !!")

x1 <- rbind(x2007, x2008, x2009, x2010, x2012, x2014)
names(x1)  <-  c('year', 'vessel', 'NASC', 'biomass', 'numbers', 'lat', 'lon')
x1$area <- 10; x1$vessel <- NULL
names(x2016) <- names(x2018)  <-
  c('year', 'NASC', 'biomass', 'numbers', 'lat', 'lon', 'area')
x2 <- rbind(x2016, x2018)
below3 <- rbind(x1,x2)

saveRDS(below3, file = here("data", "at", "below3.rds"))
