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
  scale_fill_manual(values = c("transparent", "#3d5297")) +
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
    filename = here("output", "figures", paste0("density_", i, ".png")),
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
  filename = here("output", "figures", "combined_density.png"),
  width = 11, height = 5, units = "in", dpi = 300
)
 
# Comparison plots of the model with and without AVO --------------------------
# Proportion available by depth
depth_prop <- bind_rows(
  bind_cols(
    read.csv(here("Results", "new_avo_years", "availability_depth.csv")), 
    Model = "All Surveys"),
  bind_cols(
    read.csv(here("Results", "no AVO updated", "availability_depth.csv")),
    Model = "No AVO"
  )
) %>%
  mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m"))) %>%
  ggplot(.) +
  geom_bar(aes(x = Year, y = Proportion, fill = Height), 
           position = "fill", stat = "identity") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.1, end = 0.9) +
  facet_wrap(~ Model, ncol = 1)
depth_prop

ggsave(
  depth_prop,
  filename = here("output", "figures", "depth_proportion.png"),
  width = 5, height = 5, units = "in", dpi = 300
)

# Proportion available by gear type
gear_prop <- bind_rows(
  bind_cols(
    read.csv(here("Results", "new_avo_years", "availability_gear.csv")), 
    Model = "All Surveys"),
  bind_cols(
    read.csv(here("Results", "no AVO updated", "availability_gear.csv")),
    Model = "No AVO"
  )
) %>%
  ggplot(.) +
    geom_line(aes(x = Year, y = Proportion, color = Model)) +
    geom_ribbon(aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD), fill = Model), alpha = 0.4) +
    scale_fill_viridis(na.value = NA, option = "inferno", discrete = TRUE, begin = 0.2, end = 0.7) +
    scale_color_viridis(na.value = NA, option = "inferno", discrete = TRUE, begin = 0.2, end = 0.7) +
    facet_wrap(~ Gear, ncol = 1) +
    xlab("")
gear_prop

ggsave(
  gear_prop,
  filename = here("output", "figures", "gear_proportion.png"),
  width = 5, height = 5, units = "in", dpi = 300
)

# Biomass available by depth
ind_depth_compare <- bind_rows(
  bind_cols(
    read.csv(here("Results", "new_avo_years", "index_depth.csv")), 
    Model = "All Surveys"),
  bind_cols(
    read.csv(here("Results", "no AVO updated", "index_depth.csv")),
    Model = "No AVO"
  )
) %>%
  mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m"))) %>%
  ggplot(.) +
  geom_line(aes(x = Year, y = Estimate, color = Model)) +
  geom_ribbon(aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD), fill = Model), alpha = 0.4) +
  scale_fill_viridis(na.value = NA, option = "inferno", discrete = TRUE, begin = 0.2, end = 0.7) +
  scale_color_viridis(na.value = NA, option = "inferno", discrete = TRUE, begin = 0.2, end = 0.7) +
  ylab("Abundance (Mt)") + xlab("") +
  facet_wrap(~ Height)
ind_depth_compare

ggsave(
  ind_depth_compare,
  filename = here("output", "figures", "index_depth_compare.png"),
  width = 8, height = 5, units = "in", dpi = 300
)

# Total biomass by survey (compared to assessment index) ----------------------
index_gear <- bind_rows(
  read.csv(here("Results", "new_avo_years", "index_depth.csv")) %>%
    mutate(Gear = case_when(
      Height %in% c("<0.5m", "0.5-3m", "3-16m") ~ "BT",
      Height %in% c("0.5-3m", "3-16m", ">16m") ~ "AT"
    )) %>%
    mutate(Model = "All Surveys"),
  read.csv(here("Results", "no AVO updated", "index_depth.csv")) %>%
    mutate(Gear = case_when(
      Height %in% c("<0.5m", "0.5-3m", "3-16m") ~ "BT",
      Height %in% c("0.5-3m", "3-16m", ">16m") ~ "AT"
    )) %>% 
    mutate(Model = "No AVO")
) %>%
  summarize(
    .by = c(Year, Gear, Model),
    Estimate = sum(Estimate),
    SD = sum(SD)
  ) %>%
  mutate(
    lwr = Estimate - 2 * SD,
    upr = Estimate + 2 * SD
  ) %>%
  select(Year, Gear, Model, Estimate, lwr, upr)

bt_index <- readRDS(here("data", "indices.RDS")) %>% 
  filter(stratum == "EBS") %>%
  filter(year %in% min(index_gear$Year):max(index_gear$Year)) %>%
  mutate(
    Estimate = est / 1e9, 
    lwr = lwr / 1e9,
    upr = upr / 1e9
  ) %>%
  mutate(
    Year = year,
    Gear = "BT",
    Model = "Estimate / Index"
  ) %>%
  select(Year, Gear, Model, Estimate, lwr, upr)

at_index <- read.csv(here("data", "2025", "at_estimate.csv")) %>%
  filter(Year %in% min(index_gear$Year):max(index_gear$Year)) %>%
  mutate(
    lwr = Estimate - Estimate * Error,
    upr = Estimate + Estimate * Error,
    Gear = "AT",
    Model = "Estimate / Index"
  ) %>%
  select(Year, Gear, Model, Estimate, lwr, upr)

all_indices <- bind_rows(index_gear, bt_index, at_index) %>%
  mutate(lwr = pmax(0, lwr)) %>% # Clamp negative lower bounds to 0
  ggplot() + 
    # geom_line(aes(x = Year, y = Estimate, color = Model), alpha = 0.2) +
    # geom_errorbar(aes(x = Year, y = Estimate, ymin = lwr, ymax = upr, color = Model), width = 0.2) +
    geom_pointrange(aes(x = Year, y = Estimate, ymin = lwr, ymax = upr, color = Model), position = position_dodge(width = 0.4)) +
    scale_color_viridis(na.value = NA, option = "inferno", discrete = TRUE, begin = 0.2, end = 0.7) +
    ylab("Abundance (Mt)") + xlab("") +
    coord_cartesian(ylim = c(0, NA)) +
    facet_wrap(~ Gear, ncol = 1)
all_indices

ggsave(
  all_indices,
  filename = here("output", "figures", "all_indices.png"),
  width = 7, height = 7, units = "in", dpi = 300
)

# Proportion by gear type and total index value vs. real life
cowplot::plot_grid(gear_prop, all_indices, ncol = 2, labels = c("A", "B"))

ggsave(
  filename = here("output", "figures", "compare_models.png"),
  width = 11, height = 6, units = "in", dpi = 300
)

