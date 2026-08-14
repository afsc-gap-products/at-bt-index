# Combine AT, BT, and AVO data into a single file.

library(here)
library(dplyr)
library(ggplot2)

# year <- format(Sys.Date(), "%Y")
year <- 2025

# Read in and combine ---------------------------------------------------------
at <- read.csv(here("data", "at", "ats_16.csv"))  # made in at_processing.R
bt <- read.csv(here("data", year, "bt_processed.csv"))  # made in bt_processing.R
avo <- read.csv(here("data", year, "avo_binned.csv"))  # made in avo_processing.R

# Reconfigure AT dataframe to match BT and AVO dataframes
at_new <- at %>%
  select(-surface) %>%
  rename(AT1 = strata1, AT2 = strata2, AT3 = strata3) %>%
  reshape2::melt(id.vars = c("lat", "lon", "year"), 
                 variable.name = "Gear", 
                 value.name = "Abundance") %>%
  rename(Lat = lat, Lon = lon, Year = year) %>%
  select(Lat, Lon, Year, Abundance, Gear)

# Remove years before 2007 (when the AT time series starts)
bt_new <- bt %>% filter(Year >= 2007)

dat_new <- rbind.data.frame(at_new, bt_new, avo)
  
write.csv(dat_new, here("data", year, "dat_all.csv"), row.names = FALSE)

# Constrain BT data to the same range as AT -----------------------------------
# Create a concave hull polygon from AT points
at_proj <- dat_new |> 
  filter(Gear %in% c("AT1", "AT2", "AT3")) |> 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |> 
  st_transform(3338)

bt_proj <- dat_new |> 
  filter(Gear == "BT") |> 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
  st_transform(3338)

# install.packages("concaveman") 
library(concaveman)
at_concave <- concaveman(at_proj)  

# Keep only BT points within the concave hull
bt_in_at <- bt_proj[st_within(bt_proj, at_concave, sparse = FALSE), ]

# Extract coordinates for plotting
bt_in_at_ll <- bt_in_at |> st_transform(4326)
coords <- st_coordinates(bt_in_at_ll)
bt_final <- bt_in_at_ll |>
  mutate(
    Lon = coords[, 1],
    Lat = coords[, 2],
    Gear = "BT",
    Year = Year
  ) |>
  st_drop_geometry()

dat_constrained <- rbind.data.frame(at_new, bt_final, avo)
write.csv(dat_constrained, here("data", year, "dat_bt_constrained.csv"), row.names = FALSE)

# Plot data for each layer and survey -----------------------------------------
dir.create(here("output", "data_by_gear")) 

plot_by_gear <- function(dat2, gear) {
  p <- ggplot() +
    geom_sf(data = world) +
    geom_point(data = dat2, aes(Lon, Lat, color = log(Abundance)), size = 0.7) +
    scale_color_viridis_c() +
    coord_sf(xlim = range(dat2$Lon), 
             ylim = range(dat2$Lat), 
             expand = TRUE) +  # ggplot adds a buffer
    xlab("") + ylab("") +
    facet_wrap(~ Year)
  ggsave(p, filename = here("output", "data_by_gear", paste0("data_", gear, ".png")),
         width = 9, height = 7, units = "in", dpi = 300)
}

lapply(unique(dat_new$Gear), function(i) {
  dat2 <- dat_new %>% 
    filter(Gear == i & Abundance > 0)  # filter out zeroes for plotting
  plot_by_gear(dat2, i)
})

# Plot the survey years -------------------------------------------------------
survey_years <- bind_rows(
  data.frame(survey = rep("EBS bottom trawl (BT)"), 
             year = unique(bt_new$Year)),
  data.frame(survey = rep("Acoustic trawl survey (AT)"), 
             year = unique(at_new$Year)),
  data.frame(survey = "Acoustic vessels of opportunity (AVO)",
             year = unique(avo$Year))) %>%
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
