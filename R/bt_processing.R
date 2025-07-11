# Script for processing BTS data for inclusion in the model.

library(here)
library(dplyr)
library(RODBC)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Connect to Oracle & pull haul information -----------------------------------
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
} else {
  # For those without a ConnectToOracle file
  channel_products <- odbcConnect(dsn = "AFSC", 
                                  uid = rstudioapi::showPrompt(title = "Username", 
                                                               message = "Oracle Username", 
                                                               default = ""), 
                                  pwd = rstudioapi::askForPassword("Enter Password"),
                                  believeNRows = FALSE)
}

odbcGetInfo(channel_products)  # check connection

# Get haul info
query_command <- paste0("select a.REGION, a.CRUISE, a.HAUL_TYPE, a.PERFORMANCE, 
                            a.STATIONID, a.GEAR_DEPTH, a.BOTTOM_DEPTH, a.HAULJOIN,
                            floor(a.CRUISE/100) year
                            from racebase.haul a
                            where a.PERFORMANCE >=0 and a.HAUL_TYPE = 3 and a.REGION = 'BS'
                            order by a.CRUISE;")
# remove restriction and correct for missing stratum in 2022
# and a.stratum is not null and a.stationid is not null

hauls <- sqlQuery(channel_products, query_command) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(year %in% 1982:as.numeric(format(Sys.Date(), "%Y")))  # standard years

write.csv(hauls, file = here("data", "hauls.csv"))

# Read in pollock CPUE info & combine with haul info --------------------------
ddc_cpue <- read.csv(here("data", "VAST_ddc_all_2024.csv"))  # density-dependence corrected

cpue_depth <- ddc_cpue %>%
  left_join(hauls, by = "hauljoin") %>%
  mutate(height = bottom_depth - gear_depth)  # calculate height-off-bottom



