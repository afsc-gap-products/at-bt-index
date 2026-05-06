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

# Check if raw data looks ok --------------------------------------------------
library(ggsidekick)
theme_set(theme_sleek())

test <- rbind.data.frame(at_new %>% mutate(Gear = "AT"),
                         bt_new %>% mutate(Gear = "BT"),
                         avo %>% mutate(Gear = "AVO")) %>%
  group_by(Year, Gear) %>%
  summarize(Mean = mean(Abundance)) %>%
  ggplot(.) +
  geom_bar(aes(x = Year, y = Mean, fill = Gear),
           position = "dodge", stat = "identity")
test

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
