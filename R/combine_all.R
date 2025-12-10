#' STEP 4: Combine AT, BT, and AVO data into a single file.

library(here)
library(dplyr)

year <- format(Sys.Date(), "%Y")

# Read in and combine ---------------------------------------------------------
at <- read.csv(here("data", "at", "ats_16.csv"))
bt <- read.csv(here("data", year, "bt_processed.csv"))
avo <- read.csv(here("data", year, "avo_binned.csv")) 

at_new <- at %>%
  select(-surface, -X) %>%
  rename(AT1 = strata1, AT2 = strata2, AT3 = strata3) %>%
  reshape2::melt(id.vars = c("lat", "lon", "year"), 
                 variable.name = "Gear", 
                 value.name = "Abundance") %>%
  rename(Lat = lat, Lon = lon, Year = year) %>%
  select(Lat, Lon, Year, Abundance, Gear)

bt_new <- bt %>%
  select(-depth, height) %>%
  mutate(Gear = "BT") %>%
  select(Lat, Lon, Year, Abundance, Gear)


dat_new <- rbind.data.frame(at_new, bt_new, avo)
write.csv(dat_new, here("data", year, "dat_all_at.csv"), row.names = FALSE)
