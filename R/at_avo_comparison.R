# Exploration of AT & AVO data as included in the AT/BT overlap model.

library(here)
library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(sf)
library(rnaturalearth)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

dat <- read.csv(here("data", "data_real.csv"))[, -9]
dat_avo <- read.csv(here("data", "avo", "2018-AVO-1m-grid-cell.csv"))
dat_avo <- cbind.data.frame(Lat = dat_avo$latitude,
                            Lon = dat_avo$longitude,
                            Year = 2018,
                            sA = dat_avo$sA,
                            Gear = "AVO",
                            AreaSwept_km2 = 1,
                            Vessel = "none",
                            depth = dat_avo$height)
  
# Annual plot of AT sampling location (as recorded here)
lat_lon <- rbind.data.frame(cbind.data.frame())
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry
map_at <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = dat %>% filter(Gear != "BT"), aes(x = Lon, y = Lat, color = Gear, shape = Gear), alpha = 0.3) +
  coord_sf(xlim = c(-179, -157), ylim = c(54, 65), expand = FALSE) +
  scale_x_continuous(breaks = c(-178, -158)) +
  scale_y_continuous(breaks = c(55, 64)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = "transparent")) +
  facet_wrap(~ Year)
map_at

# Combined plot of locations for 2018
at_avo <- rbind.data.frame(dat[, c(1:3, 5, 8)] %>% filter(Gear == "AT2" & Year == 2018),
                           dat_avo[, c(1:3, 5, 8)])
at_avo_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = at_avo, aes(x = Lon, y = Lat, color = Gear, shape = Gear), alpha = 0.7) +
  coord_sf(xlim = c(-179, -157), ylim = c(54, 65), expand = FALSE) +
  scale_x_continuous(breaks = c(-178, -158)) +
  scale_y_continuous(breaks = c(55, 64)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = "transparent")) 
at_avo_map

# Mean depth (comparing AT2 & AT3 gear labels) - they're the same?
mean_depth <- dat %>% 
  group_by(Gear, Year) %>%
  summarize(mean_depth = mean(depth))

# Plot of mean depth by survey
mean_depth_loc <- at_avo %>%
  group_by(Gear, Lat, Lon) %>%
  summarize(mean_depth = mean(depth))

at_avo_mean_depth <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = mean_depth_loc, aes(x = Lon, y = Lat, color = mean_depth), shape = "square", size = 2) +
  coord_sf(xlim = c(-179, -157), ylim = c(54, 65), expand = FALSE) +
  scale_x_continuous(breaks = c(-178, -158)) +
  scale_y_continuous(breaks = c(55, 64)) +
  scale_color_viridis(option = "mako", direction = -1, guide = guide_colorbar(reverse = TRUE)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = "transparent")) +
  facet_wrap(~ Gear)
at_avo_mean_depth

# Abundance-at-depth by survey
abundance_depth <- function(df, gear, label) {
  colnames(df)[4] <- "abundance"  # rename column for simpler plotting
  
  plot <- df %>% 
    filter(Gear == gear & Year == 2018) %>%
    mutate(depth_bin = cut(depth, breaks = seq(0, 370, by = 10))) %>%
    group_by(Gear, depth_bin) %>%
    summarize(abundance = mean(abundance)) %>%
    ggplot(data = ., aes(x = Gear, y = depth_bin, fill = abundance)) +
    geom_tile() +
    scale_fill_viridis(name = label) +
    ylab("Depth Bin")
  
  return(plot)
}

at_abund <- abundance_depth(dat, gear = "AT2", label = "Abundance (kg)")
at_abund

avo_abund <- abundance_depth(dat_avo, gear = "AVO", label = "Abundance (sA)")
avo_abund

bt_abund <- abundance_depth(dat, gear = "BT", label = "Abundance (kg)")
bt_abund

abund_depth_plot <- plot_grid(at_abund, avo_abund, bt_abund, ncol = 3)
abund_depth_plot

# Depth-weighted abundance (biomass or backscatter) estimates for both surveys

# Z-score datasets for basic comparison
