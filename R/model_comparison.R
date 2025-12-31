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
                          read_model("no AVO 4-layer", "availability_gear.csv"))
gear_results$model <- factor(gear_results$model, 
                             levels = c("4 layers", "no AVO 4-layer", "no AVO 3-16", "no AVO 16"))

ggplot() +
  geom_line(data = gear_results, 
            aes(x = Year, y = Proportion)) +
  geom_ribbon(data = gear_results, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD)), alpha = 0.4) +
  ylab("Proportion available") + xlab("") +
  facet_grid(model ~ Gear)

ggsave(filename = here("Results", "model_compare_gear.png"), 
       width = 200, height = 200, units = "mm", dpi = 300)

# Index by depth across models ------------------------------------------------
index_results <- bind_rows(read_model("4 layers", "index_depth.csv"),
                           read_model("no AVO 3-16", "index_depth.csv"),
                           read_model("no AVO 16", "index_depth.csv"),
                           read_model("no AVO 4-layer", "index_depth.csv"))
index_results$model <- factor(index_results$model, 
                             levels = c("4 layers", "no AVO 4-layer", "no AVO 3-16", "no AVO 16"))
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

ggplot(total_index) +
  geom_ribbon(aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD), fill = model), alpha = 0.2) +
  geom_line(aes(x = Year, y = Estimate, color = model)) +
  scale_color_viridis(discrete = TRUE, end = 0.9) +
  scale_fill_viridis(discrete = TRUE, end = 0.9) +
  ylab("Index of Abundance (Mt)") + xlab("") 

ggsave(filename = here("Results", "total_index_compare.png"),
       width = 150, height = 90, units = "mm", dpi = 300)
