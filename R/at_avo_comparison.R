# Exploration of AT & AVO data as included in the AT/BT overlap model.

library(here)
library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(sf)
library(rnaturalearth)
library(tidyr)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Set up mapping
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry

year <- format(Sys.Date(), "%Y")

# Combine data ----------------------------------------------------------------
dat <- read.csv(here("data", "data_real.csv"))[, -9]
avo_processed <- read.csv(here("data", year, "avo_processed.csv")) %>%
  filter(year <= 2018)  # filter years until we have processed AT data after 2018
dat_avo <- cbind.data.frame(Lat = avo_processed$latitude,
                            Lon = avo_processed$longitude,
                            Year = avo_processed$year,
                            sA = avo_processed$sA,
                            Gear = avo_processed$gear,
                            AreaSwept_km2 = 1,
                            Vessel = "none",
                            depth = avo_processed$height)

# Set up maping ---------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry
  
# Annual plot of survey data points -------------------------------------------
all_dat <- rbind.data.frame(dat[, c(1:3, 5, 8)],
                            dat_avo[, c(1:3, 5, 8)])
at_avo_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = all_dat %>% filter(Gear %in% c("AT2", "BT", "AVO2")), 
             aes(x = Lon, y = Lat, color = Gear, shape = Gear), alpha = 0.7) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  scale_color_manual(values = c("#93329E", "#FDA94F", "#A4C400")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~Year)
at_avo_map

# Proportion by depth layer for acoustic data ---------------------------------
AT2 <- dat %>% filter(Gear == "AT2") 
AT3 <- dat %>% filter(Gear == "AT3")

total_AT <- AT2$Catch_KG + AT3$Catch_KG 

AT2$proportion <- AT2$Catch_KG / total_AT
AT3$proportion <- AT3$Catch_KG / total_AT

AT_prop <- avo_prop <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data = rbind.data.frame(AT2, AT3), 
            aes(x = Lon, y = Lat, fill = proportion),
            width = 0.55, height = 0.2) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  scale_fill_viridis(option = "mako", direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = NULL, y = NULL) +
  facet_grid(Gear ~ Year) +
  theme(legend.position = "bottom") 
AT_prop

# Proportion by depth layer for AVO data --------------------------------------
# Calculate total backscatter for proportions
total_sA <- avo_processed %>%
  group_by(year, station) %>%
  summarize(total_sA = sum(sA))

# Calculate proportions
AVO2_prop <- avo_processed %>%
  filter(gear == "AVO2") %>%
  group_by(year, station, latitude, longitude, gear)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = c("year", "station")) %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion)) 

# AVO3 (16m off bottom to 16m from the surface)
AVO3_prop <- avo_processed %>%
  filter(gear == "AVO3") %>%
  group_by(year, station, latitude, longitude, gear)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = c("year", "station")) %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion)) 

# Map of AVO
# avo_map <- ggplot(data = world) +
#   geom_sf() +
#   geom_tile(data = avo_processed,
#             aes(x = longitude, y = latitude, fill = sA),
#             width = 0.55, height = 0.3) +
#   coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
#   scale_fill_viridis(option = "mako", direction = -1) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   labs(x = NULL, y = NULL) +
#   facet_grid(gear ~ year) +
#   theme(legend.position = "bottom") 
# avo_map

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
  facet_grid(gear ~ year) +
  theme(legend.position = "bottom") 
avo_prop

# Comparison of proportion by depth layer for AT & AVO ------------------------
avo_total <- rbind.data.frame(AVO2_prop, AVO3_prop) %>%
  group_by(year, gear) %>%
  summarize(proportion = mean(proportion)) %>%
  mutate(interval = if_else(gear == "AVO2", "2", "3")) %>%
  mutate(gear = "AVO") %>%
  mutate(interval = factor(interval, levels = c("3", "2")))

at_total <- rbind.data.frame(AT2, AT3) %>%
  na.omit() %>%
  group_by(Year, Gear) %>%
  summarize(proportion = mean(proportion)) %>%
  mutate(interval = if_else(Gear == "AT2", "2", "3")) %>%
  mutate(Gear = "AT") %>%
  mutate(interval = factor(interval, levels = c("3", "2"))) %>%
  rename(year = Year, gear = Gear)

mean_prop <- ggplot(data = rbind.data.frame(avo_total, at_total),
                    aes(x = gear, y = proportion, fill = interval)) +
  geom_col(position = "stack") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.3, end = 0.7) +
  facet_wrap(~ year, ncol = 5) 
mean_prop

# Breakdown of AT data differentiating 0.5-3m ---------------------------------
# Using fully filtered & subsampled dataset
new_at <- read.csv(here("data", "at", "ats_16.csv"))[, -1]

# Compare new AT dataset to original one & plot
dat_total <- dat %>% 
  filter(Gear %in% c("AT2", "AT3")) %>%
  pivot_wider(id_cols = c(Lat, Lon, Year), 
              names_from = Gear, 
              values_from = Catch_KG) %>%
  mutate(total = AT2 + AT3) %>%
  select(Lat, Lon, Year, total)

new_at_total <- new_at %>%
  select(-surface) %>%
  mutate(total = strata1 + strata2 + strata3) %>%
  rename(Lat = lat, Lon = lon, Year = year) %>%
  select(Lat, Lon, Year, total)

total_compare <- dat_total %>% 
  inner_join(new_at_total, by = c("Lat", "Lon", "Year"), suffix = c("_old", "_new")) %>%
  mutate(total_dif = total_old - total_new)

format(max(abs(total_compare$total_dif)), scientific = FALSE)  # Check max difference
  
# Map of the proportion in each strata in each year
at_3strata <- new_at %>%  
  select(year, lat, lon, strata1, strata2, strata3) %>%
  reshape2::melt(id.vars = c("year", "lat", "lon"), 
                 variable.name = "interval", 
                 value.name = "catch") %>%
  mutate(interval = case_when(
    interval == "strata1" ~ "0.5-3m",
    interval == "strata2" ~ "3-16m",
    interval == "strata3" ~ ">16m"
  )) %>%
  mutate(interval = factor(interval, levels = c(">16m", "3-16m", "0.5-3m"))) %>%
  group_by(year, lat, lon) %>%
  mutate(total_catch = sum(catch)) %>%
  ungroup() %>%
  filter(total_catch > 0) %>%
  mutate(proportion = catch / total_catch) 

map_3strata <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data = at_3strata, 
            aes(x = lon, y = lat, fill = proportion),
            width = 0.55, height = 0.15) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  scale_fill_viridis(option = "mako", direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = NULL, y = NULL) +
  facet_grid(interval ~ year) +
  theme(legend.position = "bottom") 
map_3strata

# Annual plot of proportions
annual_3strata <- at_3strata %>%
  group_by(year, interval) %>%
  summarize(proportion = mean(proportion)) %>%
  ggplot(., aes(x = year, y = proportion, fill = interval)) +
  geom_col(position = "stack") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, end = 0.7) +
  ylab("mean proportion")
annual_3strata

# Export plots ----------------------------------------------------------------
ggsave(at_avo_map, filename = here("Results", "avo exploration", "at_avo_bt_map.png"),
       width = 225, height = 150, units = "mm", dpi = 300)
ggsave(mean_abundance, filename = here("Results", "avo exploration", "mean_abundance.png"),
       width = 150, height = 150, units = "mm", dpi = 300, bg = "white")
ggsave(abund_depth_plot, filename = here("Results", "avo exploration", "abundance_depth.png"),
       width = 170, height = 150, units = "mm", dpi = 300)
ggsave(AT_prop, filename = here("Results", "avo exploration", "AT proportion.png"),
       width = 250, height = 80, units = "mm", dpi = 300)
ggsave(avo_prop, filename = here("Results", "avo exploration", "avo_proportion.png"),
       width = 250, height = 80, units = "mm", dpi = 300)
ggsave(mean_prop, filename = here("Results", "avo exploration", "mean_proportion.png"),
       width = 250, height = 100, units = "mm", dpi = 300)
ggsave(map_3strata, filename = here("Results", "avo exploration", "at_proportion_map.png"),
       width = 250, height = 100, units = "mm", dpi = 300)
ggsave(annual_3strata, filename = here("Results", "avo exploration", "at_annual_proportion.png"),
       width = 200, height = 110, units = "mm", dpi = 300)
