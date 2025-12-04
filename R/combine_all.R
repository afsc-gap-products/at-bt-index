#' STEP 4: Combine AT, BT, and AVO data into a single file.

library(here)
library(dplyr)

year <- format(Sys.Date(), "%Y")

# Read in and combine ---------------------------------------------------------
at <- read.csv(here("data", "at", "ats_16.csv"))
bt_avo <- read.csv(here("data", year, "at_bt_avo_binned_all.csv")) %>%
  filter(!Gear %in% c("AT2", "AT3"))  # remove AT

at_new <- at %>%
  select(-surface, -X) %>%
  rename(AT1 = strata1, AT2 = strata2, AT3 = strata3) %>%
  reshape2::melt(id.vars = c("lat", "lon", "year"), 
                 variable.name = "Gear", 
                 value.name = "Abundance") %>%
  rename(Lat = lat, Lon = lon, Year = year) %>%
  select(Lat, Lon, Year, Abundance, Gear)

dat_new <- rbind.data.frame(bt_avo, at_new)
write.csv(dat_new, here("data", year, "dat_all_at.csv"), row.names = FALSE)
