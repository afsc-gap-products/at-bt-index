#' Script for disaggregating the acoustic vessel of opportunity (AVO) index by
#' depth layer: 0.5m above the bottom, 0.5m above the bottom to 16m from the
#' surface, and 16m from the surface and above

library(here)
library(dplyr)
library(RODBC)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Read in & update haul information
hauls <- read.csv(here("data", "hauls.csv")) %>%
  filter(year == 2018) %>%
  rename(station = stationid)

# Read in AVO data & disaggregate by depth/height -----------------------------
avo_original <- read.csv(here("data", "avo", "2018-AVO-1m-grid-cell.csv"))

# Join with depth information from haul dataframe
avo_joined <- avo_original %>% 
  left_join(hauls, by = "station") %>%
  mutate(from_surface = bottom_depth - height)

# Calculate total backscatter for proportions
total_sA <- avo_original %>%
  group_by(station) %>%
  summarize(total_sA = sum(sA))

# Disaggregate AVO2 (0.5m off bottom to 16m below the surface)
AVO2 <- avo_joined %>% 
  filter(height > 0.5 & from_surface > 16) %>%
  group_by(station, latitude, longitude)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = "station") %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion))
AVO2$gear <- "AVO2"

AVO3 <- avo_joined %>% 
  filter(from_surface <= 16) %>%
  group_by(station, latitude, longitude)  %>%
  summarise(sA = sum(sA)) %>%
  ungroup() %>%
  left_join(total_sA, by = "station") %>%
  mutate(proportion = sA / total_sA) %>%
  filter(!is.na(proportion))
AVO3$gear <- "AVO3"

# Set up maping & plot --------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)  # turn off spherical geometry
avo_prop <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data = rbind.data.frame(AVO2, AVO3), 
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
  facet_wrap(~gear)
avo_prop

ggsave(avo_prop, filename = here("Results", "avo exploration", "avo_proportion.png"),
       width = 200, height = 80, units = "mm", dpi = 300)
  