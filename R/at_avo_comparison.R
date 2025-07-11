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

# Combine data ----------------------------------------------------------------
dat <- read.csv(here("data", "data_real.csv"))[, -9]
avo_original <- read.csv(here("data", "avo", "2018-AVO-1m-grid-cell.csv"))
dat_avo <- cbind.data.frame(Lat = avo_original$latitude,
                            Lon = avo_original$longitude,
                            Year = 2018,
                            sA = avo_original$sA,
                            Gear = "AVO",
                            AreaSwept_km2 = 1,
                            Vessel = "none",
                            depth = avo_original$height)

# Set up maping ---------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry
  
# Annual plot of AT & BT stations ---------------------------------------------
stations <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = dat %>% filter(Gear != "AT3"), aes(x = Lon, y = Lat, color = Gear, shape = Gear), alpha = 0.5) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = c("#93329E", "#A4C400")) +
  facet_wrap(~ Year)
stations

# Combined plot of locations for 2018 -----------------------------------------
all_dat <- rbind.data.frame(dat[, c(1:3, 5, 8)] %>% filter(Year == 2018),
                            dat_avo[, c(1:3, 5, 8)])
at_avo_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = all_dat %>% filter(Gear != "AT3"), 
             aes(x = Lon, y = Lat, color = Gear, shape = Gear), alpha = 0.7) +
  coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
  scale_color_manual(values = c("#93329E", "#FDA94F", "#A4C400")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = NULL, y = NULL) 
at_avo_map

# Plot of mean abundance by survey --------------------------------------------
abundance <- function(df, gear) {
  colnames(df)[4] <- "abundance"  # rename column for simpler manipulation
  
  df <- df %>%
    filter(Gear == gear & Year == 2018) %>%
    group_by(Gear, Lat, Lon) %>%
    summarize(mean_abundance = mean(abundance))
  
  plot <- ggplot(data = world) +
    geom_sf() +
    geom_point(data = df, aes(x = Lon, y = Lat, color = mean_abundance), shape = "square", size = 2) +
    coord_sf(xlim = c(-179, -157), ylim = c(53.8, 63.5), expand = FALSE) +
    scale_color_viridis(option = "mako", direction = -1, name = paste0("Abundance (", gear, ")")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "bottom") +
    guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5))
  
  return(plot)
}

mean_abundance <- plot_grid(
  abundance(dat, gear = "AT2"), 
  abundance(dat, gear = "AT3"), 
  abundance(dat_avo, gear = "AVO"), 
  abundance(dat, gear = "BT"), 
  ncol = 2)
mean_abundance

# Abundance-at-depth by survey ------------------------------------------------
abundance_depth <- function(df, gear, label) {
  colnames(df)[4] <- "abundance"  # rename column for simpler manipulation
  
  plot <- df %>% 
    filter(Gear == gear & Year == 2018 & depth <= 170) %>%
    mutate(depth_bin = cut(depth, breaks = seq(0, 370, by = 10))) %>%
    group_by(Gear, depth_bin) %>%
    summarize(abundance = mean(abundance)) %>%
    ggplot(data = ., aes(x = Gear, y = depth_bin, fill = abundance)) +
    geom_tile() +
    scale_fill_viridis(option = "mako", direction = -1, name = label) +
    xlab("") + ylab("Depth/Height Bin")
  
  return(plot)
}

abund_depth_plot <- plot_grid(
  abundance_depth(dat, gear = "AT2", label = "kg"), 
  abundance_depth(dat, gear = "AT3", label = "kg"), 
  abundance_depth(dat_avo, gear = "AVO", label = "sA"), 
  abundance_depth(dat, gear = "BT", label = "kg"), 
  ncol = 2)
abund_depth_plot

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
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5))
AT_prop

# Export plots ----------------------------------------------------------------
ggsave(stations, filename = here("Results", "avo exploration", "at_bt_stations.png"),
       width = 225, height = 150, units = "mm", dpi = 300)
ggsave(at_avo_map, filename = here("Results", "avo exploration", "map2018.png"),
       width = 150, height = 110, units = "mm", dpi = 300)
ggsave(mean_abundance, filename = here("Results", "avo exploration", "mean_abundance.png"),
       width = 150, height = 150, units = "mm", dpi = 300, bg = "white")
ggsave(abund_depth_plot, filename = here("Results", "avo exploration", "abundance_depth.png"),
       width = 170, height = 150, units = "mm", dpi = 300)
ggsave(AT_prop, filename = here("Results", "avo exploration", "AT proportion.png"),
       width = 250, height = 80, units = "mm", dpi = 300)
