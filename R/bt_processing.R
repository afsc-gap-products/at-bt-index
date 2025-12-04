#' STEP 1: Script for processing BTS data for inclusion in the model. This 
#' script pulls a record of all hauls from the BTS (also used in processing the
#' AVO data), and combines this with the estimates of pollock CPUE calculated
#' using the density-dependent correction in the pollock-ddc repository.

library(here)
library(dplyr)
library(RODBC)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Create data output folder ---------------------------------------------------
year <- format(Sys.Date(), "%Y")
wd <- here("data", year)
dir.create(wd, showWarnings = FALSE, recursive = TRUE)

# Connect to Oracle & pull haul information -----------------------------------
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
} else {
  # For those without a ConnectToOracle file
  channel <- odbcConnect(dsn = "AFSC", 
                         uid = rstudioapi::showPrompt(title = "Username", 
                                                      message = "Oracle Username", 
                                                      default = ""), 
                         pwd = rstudioapi::askForPassword("Enter Password"),
                         believeNRows = FALSE)
}

odbcGetInfo(channel)  # check connection

# Get haul info
query_command <- paste0("select a.REGION, a.CRUISE, a.HAUL_TYPE, a.PERFORMANCE, 
                            a.STATIONID, a.GEAR_DEPTH, a.BOTTOM_DEPTH, a.HAULJOIN,
                            floor(a.CRUISE/100) year
                            from racebase.haul a
                            where a.PERFORMANCE >=0 and a.HAUL_TYPE = 3 and a.REGION = 'BS'
                            order by a.CRUISE;")
# remove restriction and correct for missing stratum in 2022
# and a.stratum is not null and a.stationid is not null

hauls <- sqlQuery(channel, query_command) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(year %in% 1982:as.numeric(format(Sys.Date(), "%Y")))  # standard years

write.csv(hauls, file = here(wd, "hauls.csv"), row.names = FALSE)

# Read in pollock CPUE info & combine with haul info --------------------------
ddc_cpue <- read.csv(here("data", "bt", paste0("VAST_ddc_all_", year, ".csv")))  # density dependence corrected

cpue_depth <- ddc_cpue %>%
  left_join(hauls, by = "hauljoin") %>%
  mutate(height = bottom_depth - gear_depth) %>%  # calculate height off bottom 
  select(year = year.x, 
         latitude = start_latitude, 
         longitude = start_longitude,
         ddc_cpue_kg_ha, 
         depth = gear_depth, 
         height)
  
write.csv(cpue_depth, file = here(wd, "bt_processed.csv"), row.names = FALSE)
