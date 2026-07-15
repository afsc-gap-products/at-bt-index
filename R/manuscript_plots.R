# Manuscript plots

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)


# Set ggplot theme
# if (!requireNamespace("ggsidekick", quietly = TRUE)) {
#   devtools::install_github("seananderson/ggsidekick")
# }
library(ggsidekick)
theme_set(theme_sleek())

# Get land polygons for maps
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry

# Data availability in each depth layer in each year --------------------------
at <- data.frame(year = 2007:2025, 
                 gear = "AT",
                 l1 = rep(0, 19),
                 l2 = c(1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0),
                 l3 = c(1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0),
                 l4 = c(1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0))

bt <- data.frame(year = 2007:2025,
                 gear = "BT",
                 l1 = c(rep(1, 13), 0, rep(1, 5)),
                 l2 = c(rep(1, 13), 0, rep(1, 5)),
                 l3 = c(rep(1, 13), 0, rep(1, 5)),
                 l4 = c(rep(0, 19)))

avo <- data.frame(year = 2007:2025,
                  gear = "AVO",
                  l1 = rep(0, 19),
                  l2 = rep(0, 19),
                  l3 = c(0, 0, rep(1, 11), 0, rep(1, 5)),
                  l4 = c(0, 0, rep(1, 11), 0, rep(1, 5)))

dat_avail <- bind_rows(at, bt, avo) %>%
  pivot_longer(cols = c("l1", "l2", "l3", "l4"), 
               names_to = "depth_layer", values_to = "available") %>%
  mutate(depth_layer = factor(depth_layer, levels = c("l4", "l3", "l2", "l1"),
                              labels = c(">16m", "3-16m", "0.5-3m", "<0.5m")),
         gear = factor(gear, levels = c("BT", "AT", "AVO")),
         Available = factor(available, levels = c(0, 1), labels = c("No", "Yes")))

ggplot(dat_avail) +
  geom_tile(aes(x = year, y = gear, fill = Available), color = "gray") +
  facet_wrap(~ depth_layer, ncol = 1) +
  scale_fill_manual(values = c("transparent", "#4c126b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("") +
  theme_sleek()

ggsave(filename = here("output", "figures", "survey_availability.png"), 
       width = 5.5, height = 5, units = "in", dpi = 300)


# Spatial density -------------------------------------------------------------
labels = c("0.5", "0.5-3", "3-16", "16") # , "AT", "BT")  # select which to read in

# Load in and plot spatial density results
spatial_results <- function(interval) {
  den_map <- readRDS(here::here(
    "Results", 
    "new_avo_years", 
    paste0("Densities", "_", interval, ".rds")
  )) %>%
    mutate(year = as.integer(year))

  plot <- ggplot(den_map) +
    geom_sf(aes(fill = value, color = value)) +
    scale_fill_viridis(na.value = NA) +
    scale_color_viridis(na.value = NA) +
    facet_wrap(~year) +
    labs(fill = "Density", color = "Density") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  return(list(df = den_map, plot = plot))
}

# Apply function to each layer
spatial_df <- lapply(labels, function(i) {
  df_out <- spatial_results(i) 
  
  ggsave(
    filename = here::here("output", "figures", paste0("density_", i, ".png")),
    plot = df_out$plot,
    width = 9, height = 6, units = "in", dpi = 300
  )
  
  return(df_out$df) 
})

names(spatial_df) <- labels

combined_df <- bind_rows(spatial_df, .id = "interval") %>%
  mutate(interval = factor(
    interval, 
    levels = c("16", "3-16", "0.5-3", "0.5"), 
    labels = c(">16m", "3-16m", "0.5-3m", "<0.5m")
  )) %>%
  filter(year %in% c(2007, 2010, 2013, 2017, 2021, 2024))

# Single, faceted plot of density
ggplot(combined_df) +
  geom_sf(aes(fill = value, color = value)) +
  scale_fill_viridis_c(na.value = NA, option = "inferno") +
  scale_color_viridis_c(na.value = NA, option = "inferno") +
  facet_grid(interval ~ year) + 
  labs(fill = "log(Density)", color = "log(Density)") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
    filename = here::here("output", "figures", "combined_density.png"),
    width = 11, height = 5, units = "in", dpi = 300
  )
  
