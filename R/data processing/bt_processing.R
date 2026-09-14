#' STEP 1: Script for processing BTS data for inclusion in the model. This 
#' script pulls a record of all hauls from the BTS (also used in processing the
#' AVO data), and combines this with the estimates of pollock CPUE calculated
#' using the density-dependent correction in the pollock-ddc repository.

library(here)
library(dplyr)
library(RODBC)
library(ggplot2)
library(sf)

if (!requireNamespace("gapindex", quietly = TRUE)) {
  pak::pkg_install("afsc-gap-products/gapindex")
}
library(gapindex)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  pak::pkg_install("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Create data output folder ---------------------------------------------------
# year <- format(Sys.Date(), "%Y")
year <- 2025
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

# Read in density-dependent corrected pollock & combine with haul info --------
ddc_cpue <- read.csv(here("data", "bt", paste0("VAST_ddc_EBSonly_", year, ".csv")))  # density dependence corrected

ddc_cpue_out <- ddc_cpue %>%
  left_join(hauls, by = "hauljoin") %>%
  # mutate(height = bottom_depth - gear_depth) %>%  # calculate height off bottom 
  select(Lat = start_latitude, 
         Lon = start_longitude,
         Year = year.x,
         Abundance = ddc_cpue_kg_ha) %>%
  mutate(Abundance = Abundance * 100) %>%  # convert from kg/ha to kg/km2
  mutate(Gear = "BT") 

write.csv(ddc_cpue_out, file = here(wd, "bt_processed.csv"), row.names = FALSE)

# Constrain to extent of the AT survey ----------------------------------------
# Read in shapefile for AT survey
Sys.setenv(SHAPE_RESTORE_SHX = "YES")  # create missing .shx file
at_shp <- st_read(here("shapefiles", "EBS_bounds.shp"))
st_crs(at_shp) <- 4326  # set CRS to WGS84

# Transform dataframe to sf object
ddc_sf <- st_as_sf(ddc_cpue_out, coords = c("Lon", "Lat"), crs = st_crs(at_shp))

# Filter points to those within the AT polygon
ddc_filtered <- st_filter(ddc_sf, at_shp) 

ddc_filtered_out <- ddc_filtered %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude  = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(
    Lat = latitude,
    Lon = longitude,
    Year = Year,
    Abundance = Abundance,
    Gear = Gear
  )

# Double check and save output
ggplot() +
  geom_sf(data = at_shp, fill = "lightblue", alpha = 0.3, color = "darkblue", linewidth = 0.5) +
  geom_sf(data = ddc_sf, aes(color = "Excluded"), size = 1.5, alpha = 0.5) +
  geom_sf(data = ddc_filtered, aes(color = "Kept"), size = 2, alpha = 0.8) +
  scale_color_manual(
    name = "",
    values = c("Excluded" = "grey60", "Kept" = "darkmagenta")
  ) +
  xlab("") + ylab("")
ggsave(filename = here("output", "bt_filtering.png"), width = 7, height = 6, units = "in", dpi = 300)

write.csv(ddc_filtered_out, here("data", "2025", "bt_filtered.csv"), row.names = FALSE)

# Compare with original dataset -----------------------------------------------
# bt_old <- read.csv(here("data", "dat_all_at.csv")) %>%
#   filter(Gear == "BT") %>%
#   select(-X)
# 
# compare <- rbind.data.frame(cpue_depth %>% mutate(data = "new"),
#                             bt_old %>% mutate(data = "old")) %>%
#   group_by(Year, data) %>%
#   summarize(mean = mean(Abundance)) %>%
#   ggplot(.) +
#   geom_bar(aes(x = Year, y = mean, fill = data),
#            position = "dodge", stat = "identity")
# compare

# # Pull uncorrected pollock CPUE (biomass) -------------------------------------
# # Pull catch and effort data -
# species_code <- c(21740, 21741)

# # First, pull data from the standard EBS stations
# ebs_standard_data <- get_data(year_set = 1982:as.integer(format(Sys.Date(), "%Y")),
#                               survey_set = "EBS",
#                               spp_codes = species_code,
#                               pull_lengths = FALSE, 
#                               haul_type = 3, 
#                               abundance_haul = "Y",
#                               channel = channel,
#                               remove_na_strata = TRUE)

# #' Next, pull data from hauls that are not included in the design-based index
# #' production (abundance_haul == "N") but are included in VAST. By default, the 
# #' gapindex::get_data() function will filter out hauls with negative performance 
# #' codes (i.e., poor-performing hauls).
# ebs_other_data <- get_data(year_set = c(1994, 2001, 2005, 2006),
#                            survey_set = "EBS",
#                            spp_codes = species_code,
#                            pull_lengths = FALSE, 
#                            haul_type = 3, 
#                            abundance_haul = "N",
#                            channel = channel, 
#                            remove_na_strata = TRUE)

# # Combine the EBS standard and EBS other data into one list. 
# ebs_data <- list(survey = ebs_standard_data$survey,
#                  survey_design = ebs_standard_data$survey_design,
#                  #' Some cruises are shared between the standard and other EBS cruises, so the 
#                  #' unique() wrapper is there to remove duplicate cruise records. 
#                  cruise = unique(rbind(ebs_standard_data$cruise, ebs_other_data$cruise)),
#                  haul = rbind(ebs_standard_data$haul, ebs_other_data$haul),
#                  catch = rbind(ebs_standard_data$catch, ebs_other_data$catch),
#                  species = ebs_standard_data$species,
#                  strata = ebs_standard_data$strata)

# # Calculate CPUE and export 
# ebs_cpue <- calc_cpue(gapdata = ebs_data) %>%
#   select("YEAR", "LATITUDE_DD_START",
#          "LONGITUDE_DD_START", "CPUE_KGKM2") %>%
#   transmute(Lat = LATITUDE_DD_START,
#             Lon = LONGITUDE_DD_START,
#             Year = as.integer(YEAR),
#             Abundance =  CPUE_KGKM2, 
#             Gear = "BT") 

# write.csv(ebs_cpue, file = here(wd, "bt_noddc.csv"), row.names = FALSE)

# Compare DDC and uncorrected CPUE --------------------------------------------
# annual_ddc <- ddc_cpue_out %>%
#   group_by(Year) %>%
#   summarize(CPUE = sum(Abundance)) %>%
#   mutate(data = "DDC")
# 
# annual_ebs <- ebs_cpue %>%
#   group_by(Year) %>%
#   summarize(CPUE = sum(Abundance)) %>%
#   mutate(data = "base")
# 
# ggplot(bind_rows(annual_ddc, annual_ebs), aes(x = Year, y = CPUE, color = data)) +
#   geom_line() +
#   geom_point()
