# Generic plots for this project

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

# Pollock data availability
          
          