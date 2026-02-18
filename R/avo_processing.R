#' STEP 2: Script for disaggregating the acoustic vessel of opportunity (AVO) 
#' index by depth layer: 0.5m above the bottom, 0.5m above the bottom to 16m 
#' from the bottom, and 16m from the surface and above

library(here)
library(dplyr)
library(RODBC)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Set up output folder and processing inputs ----------------------------------
# year <- format(Sys.Date(), "%Y")
year <- 2025
wd <- here("data", year)

# Whether to make the plots when running the code (set to FALSE to save time)
plotting <- TRUE

# Year set for AVO
avo_years <- c(2009, 2010, 2012, 2014:2019, 2021:2025)

# Read in & update haul information
hauls <- read.csv(here(wd, "hauls.csv")) %>%
  filter(year %in% avo_years) %>%
  rename(station = stationid)

# Read in AVO data & disaggregate by depth/height -----------------------------
avo_original <- data.frame()
for(i in avo_years) {
  df <- read.csv(here("data", "avo", paste0(i, "-AVO-1m-grid-cell.csv")))
  df$year <- i
  avo_original <- rbind.data.frame(avo_original, df)
}

# Join with depth information from haul dataframe
avo_joined <- avo_original %>% 
  left_join(hauls, by = c("year", "station")) %>%
  mutate(from_surface = bottom_depth - height)

# Disaggregate 
AVO2 <- avo_joined %>% 
  filter(height > 0.5 & height < 16) %>%  # 0.5m off bottom to 16m off bottom
  mutate(gear = "AVO2")
AVO3 <- avo_joined %>% 
  filter(height >= 16 & from_surface >= 16) %>%  # 16m off bottom to 16m from the surface
  mutate(gear = "AVO3")

# Combine w/gear label & export
avo_processed <- rbind.data.frame(AVO2, AVO3) %>%
  select(year, latitude, longitude, station, sA, height, from_surface, gear)

write.csv(avo_processed, file = here(wd, "avo_processed.csv"), row.names = FALSE)

# Bin and export
avo_out <- avo_processed %>% 
  group_by(latitude, longitude, year, gear) %>%
  summarize(total_sA = sum(sA)) %>%  # get abundance for each survey point
  ungroup() %>%
  # filter(gear == "AVO3") %>%  # Only above 16m from the bottom for now 
  select(Lat = latitude, 
         Lon = longitude, 
         Year = year, 
         Abundance = total_sA, 
         Gear = gear)

write.csv(avo_out, file = here(wd, "avo_binned.csv"), row.names = FALSE)
  
# Calculate total backscatter for proportions
total_sA <- avo_original %>%
  group_by(year, station) %>%
  summarize(total_sA = sum(sA))

# Calculate proportions
AVO2_prop <- AVO2 %>%
  group_by(year, station, latitude, longitude, gear)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = c("year", "station")) %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion)) 

# AVO3 (16m off bottom to 16m from the surface)
AVO3_prop <- AVO3 %>%
  group_by(year, station, latitude, longitude, gear)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = c("year", "station")) %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion)) 

# Set up maping & plot --------------------------------------------------------
if(plotting == TRUE) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  sf_use_s2(FALSE)  # turn off spherical geometry
  
  avo_prop <- ggplot(data = world) +
    geom_sf() +
    geom_tile(data = rbind.data.frame(AVO2_prop, AVO3_prop), 
              aes(x = longitude, y = latitude, fill = proportion),
              width = 0.55, height = 0.3) +
    coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
    scale_fill_viridis(option = "mako", direction = -1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "bottom") +
    ggforce::facet_grid_paginate(gear ~ year, ncol = 7, nrow = 2, page = 1) 
  
  avo_prop2 <- avo_prop +
    ggforce::facet_grid_paginate(gear ~ year, ncol = 7, nrow = 2, page = 2)

  ggsave(avo_prop, filename = here("Results", "avo exploration", "avo_prop1.png"),
         width = 250, height = 100, units = "mm", dpi = 300)
  ggsave(avo_prop2, filename = here("Results", "avo exploration", "avo_prop2.png"),
         width = 250, height = 100, units = "mm", dpi = 300)
}
