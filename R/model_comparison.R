# Comparison between model runs

library(here)
library(dplyr)
library(ggplot2)
library(viridis)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Function for reading in model results ---------------------------------------
read_model <- function(wd, filetype) {
  df <- read.csv(here("results", wd, filetype))
  df$model <- wd
  return(df)
}

# Availability to gear across models ------------------------------------------
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
            aes(x = Year, y = Proportion)) +
  geom_ribbon(data = gear_results, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD)), alpha = 0.4) +
  ylab("Proportion available") + xlab("") +
  facet_grid(model ~ Gear)

ggsave(filename = here("Results", "model_compare_gear.png"), 
       width = 180, height = 200, units = "mm", dpi = 300)

# Index by depth across models ------------------------------------------------
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
index_results$Height <- factor(index_results$Height, 
                               levels = c("<0.5m", "0.5-3m", "3-16m", ">16m"))

ggplot() +
  geom_line(data = index_results, 
            aes(x = Year, y = Estimate)) +
  geom_ribbon(data = index_results, 
              aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD)), alpha = 0.4) +
  ylab("Index of Abundance (Mt)") + xlab("") +
  facet_grid(model ~ Height)

ggsave(filename = here("Results", "model_compare_depth.png"), 
       width = 240, height = 180, units = "mm", dpi = 300)

# Total index of abundance across models --------------------------------------
total_index <- index_results %>% 
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
              aes(x = year, ymin = lwr, ymax = upr), fill = "magenta", alpha = 0.2) +
  geom_line(data = index,
            aes(x = year, y = est), color = "magenta") +
  scale_color_viridis(discrete = TRUE, end = 0.9) +
  scale_fill_viridis(discrete = TRUE, end = 0.9) +
  ylab("Index of Abundance (Mt)") + xlab("") 

ggsave(filename = here("Results", "total_index_compare.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Comparison across survey years ----------------------------------------------
gear_results <- gear_results %>%
  mutate(surveys = case_when(Year %in% c(2009, 2010, 2012, 2014, 2016, 2018) ~ "all surveys",
                             Year %in% c(2007, 2008) ~ "no AVO",
                             Year %in% c(2015, 2017) ~ "no AT",
                             Year %in% c(2011, 2013) ~ "only BT"))
ggplot(gear_results) +
  geom_pointrange(aes(x = Gear, y = Proportion, 
                      ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD),
                      color = model),
                  position = position_dodge(width = 0.2), alpha = 0.8) +
  scale_color_viridis(discrete = TRUE, end = 0.9) +
  facet_wrap(~ surveys)

ggsave(filename = here("Results", "point_gear_compare.png"),
       width = 170, height = 120, units = "mm", dpi = 300)
