# Generic plots for this project

library(here)
library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)

# Set ggplot theme
# if (!requireNamespace("ggsidekick", quietly = TRUE)) {
#   devtools::install_github("seananderson/ggsidekick")
# }
# library(ggsidekick)
# theme_set(theme_sleek())

source("~/GAP/gap-random/sleek_dark.R")
theme_set(theme_sleek_transparent())

# Pollock survey overview -----------------------------------------------------
survey_years <- bind_rows(
  data.frame(survey = rep("EBS bottom trawl (BT)"), 
             year = c(1982:2019, 2021:2024)),
  data.frame(survey = rep("Acoustic trawl survey (AT)"), 
             year = c(1994, 1996, 1997, 1999, 2000, 2002, 2004, 2006:2010, 
                      seq(2012, 2024, by = 2))),
  data.frame(survey = "Acoustic vessels of opportunity (AVO)",
             year = c(2006:2019, 2021:2024))) %>%
  mutate(occurred = 1) %>%
  mutate(survey = factor(survey, levels = c("EBS bottom trawl (BT)", 
                                            "Acoustic trawl survey (AT)", 
                                            "Acoustic vessels of opportunity (AVO)"))) %>%
  ggplot(.) +
  geom_point(aes(x = year, y = occurred, color = survey)) +
  scale_color_viridis(discrete = TRUE, begin = 0.3) +
  facet_wrap(~survey, ncol = 1) +
  theme(legend.position = "none",
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(" ") + xlab("")
survey_years

ggsave(survey_years, filename = here("Results", "survey_years.png"),
       width = 6, height = 3, units = "in", dpi = 300, bg = "transparent")

# Pollock depth (AVO) ---------------------------------------------------------
avo_processed <- read.csv(here("data", "2025", "avo_processed.csv")) 

weighted_depth <- avo_processed %>%
  group_by(year, station) %>%
  summarise(weighted_depth = weighted.mean(from_surface,
                                           w = sA,
                                           na.rm = TRUE),
            .groups = "drop")
avo_processed <- avo_processed %>%
  left_join(weighted_depth, by = c("year", "station"))

# Convert AVO points to sf
avo_sf <- st_as_sf(avo_processed, coords = c("longitude", "latitude"), crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry
avo_depth <- ggplot(avo_sf) +
  geom_sf(data = world) +
  geom_sf(data = avo_sf, aes(color = weighted_depth), shape = "square") +
  scale_color_viridis(na.value = NA, option = "mako", direction = -1, 
                      limits = c(0, NA),
                      guide = guide_colourbar(title.position = "bottom",
                                              title.hjust = 0.5)) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  facet_wrap(~ year, ncol = 7) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color = "Backscatter-weighted depth")
avo_depth

ggsave(avo_depth, filename = here("Results", "avo_weighted_depth.png"),
       width = 8, height = 3.5, units = "in", dpi = 300, bg = "transparent")

# Survey location overlap -----------------------------------------------------
dat <- read.csv(here("data", "data_real.csv"))[, -9]
dat_avo <- cbind.data.frame(Lat = avo_processed$latitude,
                            Lon = avo_processed$longitude,
                            Year = avo_processed$year,
                            sA = avo_processed$sA,
                            Gear = avo_processed$gear,
                            AreaSwept_km2 = 1,
                            Vessel = "none",
                            depth = avo_processed$height)

all_dat <- rbind.data.frame(dat[, c(1:3, 5, 8)],
                            dat_avo[, c(1:3, 5, 8)]) %>%
  filter(Gear %in% c("AT2", "BT", "AVO2")) %>%
  mutate(Gear = factor(Gear, levels = c("BT", "AT2", "AVO2"), labels = c("BT", "AT", "AVO"))) %>%
  filter(Year == 2016)

survey_locations <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = all_dat, 
             aes(x = Lon, y = Lat, color = Gear)) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  scale_color_viridis(discrete = TRUE, begin = 0.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ Gear)
survey_locations

ggsave(survey_locations, filename = here("Results", "survey_locations.png"),
       width = 7, height = 3, units = "in", dpi = 300, bg = "transparent")
