#' STEP 4: Combine AT, BT, and AVO data into a single file.

library(here)
library(dplyr)
library(ggplot2)

# year <- format(Sys.Date(), "%Y")
year <- 2025

# Read in and combine ---------------------------------------------------------
at <- read.csv(here("data", "at", "ats_16.csv"))
bt <- read.csv(here("data", year, "bt_noddc.csv"))  # made in bt_processing.R
avo <- read.csv(here("data", year, "avo_binned.csv"))  # made in avo_processing.R

at_new <- at %>%
  select(-surface, -X) %>%
  rename(AT1 = strata1, AT2 = strata2, AT3 = strata3) %>%
  reshape2::melt(id.vars = c("lat", "lon", "year"), 
                 variable.name = "Gear", 
                 value.name = "Abundance") %>%
  rename(Lat = lat, Lon = lon, Year = year) %>%
  select(Lat, Lon, Year, Abundance, Gear)

bt_new <- bt %>% filter(Year >= 2007)

dat_new <- rbind.data.frame(at_new, bt_new, avo)
  
write.csv(dat_new, here("data", year, "dat_all_noddc.csv"), row.names = FALSE)

# Check if raw data looks ok --------------------------------------------------
# library(ggsidekick)
# theme_set(theme_sleek())
# 
# test <- rbind.data.frame(at_new %>% mutate(Gear = "AT"), 
#                          bt_new %>% mutate(Gear = "BT"),
#                          avo %>% mutate(Gear = "AVO")) %>% 
#   group_by(Year, Gear) %>%
#   summarize(Mean = mean(Abundance)) %>%
#   ggplot(.) +
#   geom_bar(aes(x = Year, y = Mean, fill = Gear), 
#            position = "dodge", stat = "identity")
# test
