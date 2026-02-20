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

dir <- here("Results", "wkuser")

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
  scale_color_manual(values = c("#834beb", "#2FB47C", "#FDE725")) +
  facet_wrap(~survey, ncol = 1) +
  theme(legend.position = "none",
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(" ") + xlab("")
survey_years

ggsave(survey_years, filename = here(dir, "survey_years.png"),
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

ggsave(avo_depth, filename = here(dir, "avo_weighted_depth.png"),
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
  scale_color_manual(values = c("#834beb", "#2FB47C", "#FDE725")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ Gear)
survey_locations

ggsave(survey_locations, filename = here(dir, "survey_locations.png"),
       width = 7, height = 3, units = "in", dpi = 300, bg = "transparent")

# Original model results ------------------------------------------------------
avail_depth <- read.csv(here("Results", "archive", "availability_depth_noAVO.csv"))[, -1] %>%
  mutate(Height = factor(Height, levels = c(">16m", "0.5-16m", "<0.5m")))
avail_gear <- read.csv(here("Results", "archive", "availability_gear_noAVO.csv"))[, -1] %>%
  mutate(Gear = factor(Gear, levels = c("BT", "AT"))) 

at_years <- c(2007:2010, 2012, 2014, 2016, 2018)
bt_years <- 2007:2018

survey_yr_points <- avail_gear %>% 
  filter((Gear == "AT" & Year %in% at_years) | 
           (Gear == "BT" & Year %in% bt_years)) %>%
  mutate(Gear = factor(Gear, levels = c("BT", "AT")))

gear_plot <- ggplot() +
  geom_line(data = avail_gear, 
            aes(x = Year, y = Proportion, color = Gear)) +
  geom_point(data = survey_yr_points,
             aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = avail_gear, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD), fill = Gear), alpha = 0.4) +
  scale_color_manual(values = c("#834beb", "#2FB47C")) +
  scale_fill_manual(values = c("#834beb", "#2FB47C")) +
  ylim(0, NA) +
  xlab("")
gear_plot

ggsave(gear_plot, filename = here(dir, "avail_gear_plot_original.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Bar plot of availability by depth
depth_plot <- ggplot(avail_depth) +
  geom_bar(aes(x = Year, y = Proportion, fill = Height), 
           position = "fill", stat = "identity") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.3) +
  xlab("")
depth_plot

ggsave(depth_plot, filename = here(dir, "avail_depth_plot_original.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Both plots together
avail_both <- cowplot::plot_grid(depth_plot, gear_plot, ncol = 1)
avail_both

ggsave(avail_both, filename = here(dir, "avail_both_original.png"),
       width = 150, height = 150, units = "mm", dpi = 300)

# Model results with AVO & 4 layers -------------------------------------------
avail_depth <- read.csv(here("Results","4 layers", "availability_depth.csv")) %>%
  filter(Year <= 2018) %>%
  mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m")))
avail_gear <- read.csv(here("Results","4 layers", "availability_gear.csv")) %>%
  mutate(Gear = factor(Gear, levels = c("BT", "AT"))) %>%
  filter(Year <= 2018)

# Time series of proportion available
# Get years where there was a survey
at_years <- c(2007:2010, 2012, 2014, 2016, 2018)
bt_years <- 2007:2018

survey_yr_points <- avail_gear %>% 
  filter((Gear == "AT" & Year %in% at_years) | 
           (Gear == "BT" & Year %in% bt_years))
survey_yr_points <- rbind.data.frame(survey_yr_points,
                                     cbind.data.frame(Year = c(2009, 2010, 2012, 2014, 2015, 2016, 2017, 2018),
                                                      Proportion = 0,
                                                      SD = 0,
                                                      Gear = "AVO")) %>%
  mutate(Gear = factor(Gear, levels = c("BT", "AT", "AVO")))

gear_plot <- ggplot() +
  geom_line(data = avail_gear, 
            aes(x = Year, y = Proportion, color = Gear)) +
  geom_point(data = survey_yr_points,
             aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = avail_gear, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD), fill = Gear), alpha = 0.4) +
  scale_color_manual(values = c("#834beb", "#2FB47C", "#FDE725")) +
  scale_fill_manual(values = c("#834beb", "#2FB47C")) +
  xlab("")
gear_plot

ggsave(gear_plot, filename = here(dir, "avail_gear_plot_avo.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Bar plot of availability by depth
depth_plot <- ggplot(avail_depth) +
  geom_bar(aes(x = Year, y = Proportion, fill = Height), 
           position = "fill", stat = "identity") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.3) +
  xlab("")
depth_plot

ggsave(depth_plot, filename = here(dir, "avail_depth_plot_avo.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Both plots together
avail_both <- cowplot::plot_grid(depth_plot, gear_plot, ncol = 1)
avail_both

ggsave(avail_both, filename = here(dir, "avail_both_avo.png"),
       width = 150, height = 150, units = "mm", dpi = 300)

# Index by depth
ind_depth <- read.csv(here("Results", "4_layers", "index_depth.csv")) %>%
  mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m")))

ind_depth_plot <- ggplot() +
  geom_line(data = ind_depth, 
            aes(x = Year, y = Estimate, color = Height)) +
  # geom_point(data = survey_yr_points,
  #            aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = ind_depth, 
              aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD), fill = Height), alpha = 0.4) +
  scale_color_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.3) +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.3) +
  ylab("Index of Abundance (Mt)") + xlab("")
ind_depth_plot

ggsave(ind_depth_plot, filename = here(dir, "index_depth_plot.png"),
       width = 120, height = 70, units = "mm", dpi = 300)

# Comparison plots ------------------------------------------------------------
# Function for reading in model results 
read_model <- function(wd, filetype) {
  df <- read.csv(here("results", wd, filetype))
  df$model <- wd
  return(df)
}

# Availability to gear across models 
gear_results <- bind_rows(read_model("4 layers", "availability_gear.csv"),
                          read_model("no AVO 3-16", "availability_gear.csv"),
                          read_model("no AVO 16", "availability_gear.csv"),
                          read_model("no AVO 4-layer", "availability_gear.csv"),
                          read_model("no AT 3-16", "availability_gear.csv"),
                          read_model("no AT 16", "availability_gear.csv"))
gear_results$model <- factor(gear_results$model, 
                             levels = c("4 layers", "no AVO 4-layer", 
                                        "no AVO 3-16", "no AVO 16",
                                        "no AT 3-16", "no AT 16"))

ggplot() +
  geom_line(data = gear_results, 
            aes(x = Year, y = Proportion), color = "#2FB47C") +
  geom_ribbon(data = gear_results, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD)), fill = "#2FB47C", alpha = 0.4) +
  ylab("Proportion available") + xlab("") +
  facet_grid(Gear ~ model)

ggsave(filename = here("Results", "wkuser", "model_compare_gear.png"), 
       width = 220, height = 90, units = "mm", dpi = 300)

# Index by depth across models
index_results <- bind_rows(read_model("4 layers", "index_depth.csv"),
                           read_model("no AVO 3-16", "index_depth.csv"),
                           read_model("no AVO 16", "index_depth.csv"),
                           read_model("no AVO 4-layer", "index_depth.csv"),
                           read_model("no AT 3-16", "index_depth.csv"),
                           read_model("no AT 16", "index_depth.csv"))
index_results$model <- factor(index_results$model,
                              levels = c("4 layers", "no AVO 4-layer",
                                         "no AVO 3-16", "no AVO 16",
                                         "no AT 3-16", "no AT 16"))
# index_results$Height <- factor(index_results$Height, 
#                                levels = c("<0.5m", "0.5-3m", "3-16m", ">16m"))
# 
# ggplot() +
#   geom_line(data = index_results, 
#             aes(x = Year, y = Estimate), color = "#2FB47C") +
#   geom_ribbon(data = index_results, 
#               aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD)), 
#               fill = "#2FB47C", alpha = 0.4) +
#   ylab("Index of Abundance (Mt)") + xlab("") +
#   facet_grid(model ~ Height)
# 
# ggsave(filename = here("Results", "wkuser", "model_compare_depth.png"), 
#        width = 240, height = 180, units = "mm", dpi = 300)

# Total index of abundance across models 
total_index <- index_results %>% 
  filter(model %in% c("4 layers", "no AVO 4-layer")) %>%
  group_by(model, Year) %>%
  summarize(Estimate = sum(Estimate),
            SD = sum(SD)) 

# Read in sdmTMB index, select EBS only, at-bt model date range, convert to Mt
index <- readRDS(here("data", "indices.RDS")) %>% 
  filter(stratum == "EBS") %>%
  filter(year %in% min(index_results$Year):max(index_results$Year)) %>%
  mutate(est = est / 1e9, 
         lwr = lwr / 1e9,
         upr = upr / 1e9)

ggplot() +
  geom_ribbon(data = total_index, 
              aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD), fill = model), 
              alpha = 0.2) +
  geom_line(data = total_index, 
            aes(x = Year, y = Estimate, color = model)) +
  geom_ribbon(data = index,
              aes(x = year, ymin = lwr, ymax = upr), fill = "#c107f5", alpha = 0.2) +
  geom_line(data = index,
            aes(x = year, y = est), color = "#c107f5") +
  scale_color_viridis(discrete = TRUE, begin = 0.6) +
  scale_fill_viridis(discrete = TRUE, begin = 0.6) +
  ylab("Index of Abundance (Mt)") + xlab("") 

ggsave(filename = here("Results", "wkuser", "total_index_compare.png"),
       width = 150, height = 90, units = "mm", dpi = 300)
