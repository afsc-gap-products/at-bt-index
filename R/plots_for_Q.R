# Code to explore Q
# Plots for spatial maps of model output of Q
# Plots for empirically derived log(AVO/AT) versus modeled Q


# ==============================================================================
# Plot AVO Q from model
# ==============================================================================

xi_gct <- rep$epsilon_q_gct[, 1, , drop = FALSE]  # Dimensions: [n_grid_cells, 2_layers, n_years]

# Convert extrap matrix to data frame and sf spatial object
extrap_df <- as.data.frame(extrap)
extrap_sf <- st_as_sf(extrap_df, coords = c("Lon", "Lat"), crs = 4326)

sf_use_s2(FALSE)

# 2. Extract ONLY unique station coordinates across all survey years
dat_bt_unique <- dat %>%
  filter(Gear == "AVO2") %>%
  distinct(Lon, Lat) %>% # Drops 5,000+ redundant yearly points down to ~376 unique stations
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

# 3. Buffer unique stations and union them instantly
bt_boundary <- dat_bt_unique %>%
  st_buffer(dist = 0.35) %>%  # ~20 nmi radius around unique stations
  st_union()

# 4. Re-enable S2 geometry engine for downstream operations
sf_use_s2(TRUE)

# Identify grid cells in extrap that fall within the refined BT footprint
in_bt_domain <- st_intersects(extrap_sf, bt_boundary, sparse = FALSE)[, 1]

# Crop both the extrapolation grid and xi_gct spatial array
extrapolation_grid <- extrap_df[in_bt_domain, ]
xi_gct_bt            <- xi_gct[in_bt_domain, , , drop = FALSE]

cat("Original Grid Cells:", nrow(extrap_df), "\n")
cat("Refined Footprint Grid Cells:", nrow(extrapolation_grid), "\n")

# Full sequence of model years corresponding to the 3rd array dimension of xi_gct
full_years <- min(dat$Year):max(dat$Year)

# Dynamically extract unique years where AVO sampling actually occurred
avo_years <- sort(unique(dat$Year[grepl("AVO", dat$Gear)]))

n_years_avo  <- length(avo_years)

# Compute optimal grid layout for years (e.g., 9 years -> 3x3; 12 years -> 4x3)
n_cols_years <- ceiling(sqrt(n_years_avo))
n_rows_years <- ceiling(n_years_avo / n_cols_years)

cat("AVO Years Count:", n_years_avo, "\n")
cat("Grid Layout:", n_rows_years, "year-rows x", n_cols_years, "year-columns\n")

# Get array dimensions
n_grid   <- dim(xi_gct_bt)[1]
n_layers <- 1  # Exactly 1 conversion layer
n_years  <- dim(xi_gct_bt)[3]

# Set layer labels and year vector (update starting year to match your dataset)
layer_names <- c("Midwater (Layers 3 & 4)")
years_vec <- sort(unique(dat$Year))

# Assumes 'extrapolation_grid' contains Lon and Lat for each grid cell
plot_list <- list()

for (t in 1:n_years) {
  plot_list[[length(plot_list) + 1]] <- data.frame(
    Lon   = extrapolation_grid$Lon,
    Lat   = extrapolation_grid$Lat,
    xi    = xi_gct_bt[, 1, t],
    Layer = layer_names[1],
    Year  = full_years[t]
  )
}

plot_df <- bind_rows(plot_list)
plot_df_avo <- plot_df %>% filter(Year %in% avo_years)


plot_single_layer_grid <- function(target_layer_name, df = plot_df_avo) {
  df_layer <- df %>% filter(Layer == target_layer_name)
  
  p <- ggplot(df_layer, aes(x = Lon, y = Lat, fill = xi)) +
    geom_tile(width = 0.35, height = 0.25) +
    scale_fill_viridis_c(
      option   = "plasma",
      name = " "
    ) +
    facet_wrap(~ Year, ncol = n_cols_years) +
    coord_quickmap() +
    theme_bw(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(face = "bold", size = 11),
      axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 8),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 11)
    ) +
    labs(
      title    = paste0("Q Fields (", target_layer_name, ")"),
      subtitle = expression("AVO backscatter-to-biomass conversion across survey years"),
      x        = "Longitude",
      y        = "Latitude"
    )
  
  return(p)
}

calc_width  <- n_cols_years * 3.5
calc_height <- n_rows_years * 3.2

p_layer3 <- plot_single_layer_grid("Midwater (Layers 3 & 4)")
print(p_layer3)
ggsave("EBS_STVC_Q_AVO.png", p_layer3, width = calc_width, height = calc_height, dpi = 300)


# ==============================================================================
# Compute empirical Q: log(AVO/AT)
# ==============================================================================

at_years      <- sort(unique(dat$Year[dat$Gear %in% c("AT2", "AT3")]))
avo_years     <- sort(unique(dat$Year[grepl("^AVO", dat$Gear)]))
overlap_years <- sort(intersect(at_years, avo_years))


# Observation columns in dat
obs_col <- if ("Abundance" %in% names(dat)) {
  "Abundance"
} else if ("b_i" %in% names(dat)) {
  "b_i"
} else {
  "CPUE"
}

# Define spatial bin size in degrees (0.5° ~ 30 km grid)
bin_size <- 0.5

# Assign spatial grid cell IDs to all observations
dat_binned <- dat %>%
  filter(Year %in% overlap_years) %>%
  mutate(
    Lon_bin = floor(Lon / bin_size) * bin_size + bin_size / 2,
    Lat_bin = floor(Lat / bin_size) * bin_size + bin_size / 2
  )


# Round coordinates to 2 decimal places to handle minor floating-point station differences
at_grid <- dat_binned %>%
  filter(Gear %in% c("AT2", "AT3")) %>%  # <--- Restricts to AT Layers 2 & 3
  group_by(Year, Lon_bin, Lat_bin, Lon, Lat) %>%
  summarize(AT_loc = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop_last") %>%
  summarize(
    AT_density = mean(AT_loc, na.rm = TRUE),
    .groups    = "drop"
  )

# B. AVO: Sum AVO2 + AVO3 per station location, then average within grid cell
avo_grid <- dat_binned %>%
  filter(grepl("^AVO", Gear)) %>%
  group_by(Year, Lon_bin, Lat_bin, Lon, Lat) %>%
  summarize(AVO_loc = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop_last") %>%
  summarize(
    AVO_sA  = mean(AVO_loc, na.rm = TRUE),
    .groups = "drop"
  )

# C. Join on Grid Cell (Year, Lon_bin, Lat_bin) & Calculate log(q)
emp_q_df <- inner_join(
  at_grid, 
  avo_grid, 
  by = c("Year", "Lon_bin", "Lat_bin")
) %>%
  mutate(
    Lon = Lon_bin,
    Lat = Lat_bin
  ) %>%
  filter(AT_density > 0, AVO_sA > 0) %>%
  mutate(log_q_empirical = log(AVO_sA / AT_density))

# ==============================================================================
# Plot maps of empirically derived Q after binning in space
# ==============================================================================


n_years_overlap <- length(overlap_years)
n_cols_years    <- ceiling(sqrt(n_years_overlap))
n_rows_years    <- ceiling(n_years_overlap / n_cols_years)

p_emp_q <- ggplot(emp_q_df, aes(x = Lon, y = Lat)) +
  geom_tile(aes(fill = log_q_empirical), width = bin_size, height = bin_size * 0.7) +
  scale_fill_viridis_c(
    option = "plasma",
    name   = expression(log(q[empirical]))
  ) +
  facet_wrap(~ Year, ncol = n_cols_years) +
  coord_quickmap() +
  theme_bw(base_size = 11) +
  theme(
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11)
  ) +
  labs(
    title    = expression("Binned Empirical Conversion Ratio: " * log(D[AT] / s[A, AVO])),
    subtitle = paste0("Spatially binned at ", bin_size, "° × ", bin_size, "° across overlapping AT & AVO survey years"),
    x        = "Longitude",
    y        = "Latitude"
  )

print(p_emp_q)

calc_width  <- n_cols_years * 3.5
calc_height <- n_rows_years * 3.2
ggsave("EBS_empirical_binned_log_q_grid.png", p_emp_q, width = calc_width, height = calc_height, dpi = 300)


# ==============================================================================
# Compare the distributions empirical and model based Q
# ==============================================================================


# 1. Overall empirical Q Distribution (Histogram + Overlaid Density Curve)

# Compute reference quantiles for annotation
med_val <- median(emp_q_df$log_q_empirical, na.rm = TRUE)
q025    <- quantile(emp_q_df$log_q_empirical, probs = 0.025, na.rm = TRUE)
q975    <- quantile(emp_q_df$log_q_empirical, probs = 0.975, na.rm = TRUE)

p_hist <- ggplot(emp_q_df, aes(x = log_q_empirical)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins  = 35, 
    fill  = "#2b83ba", 
    color = "white", 
    alpha = 0.75
  ) +
  geom_density(color = "#d7191c", linewidth = 1) +
  geom_vline(xintercept = med_val, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = c(q025, q975), linetype = "dotted", color = "#d7191c", linewidth = 0.8) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  ) +
  labs(
    title    = expression("Distribution of Empirical Conversion Factors: " * log(s[A, AVO] / D[AT] )),
    subtitle = paste0("Black dashed = Median (", round(med_val, 2), "); Red dotted = 95% Central Range [", 
                      round(q025, 2), ", ", round(q975, 2), "]"),
    x        = expression(log(q[empirical])),
    y        = "Density"
  )

print(p_hist)
ggsave("EBS_empirical_log_q_histogram.png", p_hist, width = 8, height = 5, dpi = 300)


# 2. Interannual empirical Q distribution (Boxplots across Overlapping Years)

p_box <- ggplot(emp_q_df, aes(x = factor(Year), y = log_q_empirical, fill = factor(Year))) +
  geom_boxplot(alpha = 0.75, outlier.size = 1.8, outlier.color = "red") +
  scale_fill_viridis_d(option = "plasma") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(face = "bold", size = 14)
  ) +
  labs(
    title    = expression("Interannual Variability in Binned " * log(q[empirical])),
    subtitle = "Boxplots showing median, IQR, and outliers per overlapping survey year",
    x        = "Year",
    y        = expression(log(q[empirical]))
  )

print(p_box)
ggsave("EBS_empirical_log_q_annual_boxplots.png", p_box, width = 8, height = 5, dpi = 300)


# 3. Direct Scale Comparison: Empirical vs. Unconstrained Model Sum

# Combine empirical values with model's reported sum (epsilon_q_gct)
comp_df <- bind_rows(
  data.frame(Value = emp_q_df$log_q_empirical, Source = "Empirical Data: log(AVO / AT)"),
  data.frame(Value = as.vector(rep$epsilon_q_gct), Source = "Model Sum: mu + beta + omega + epsilon")
) %>%
  filter(!is.na(Value) & is.finite(Value) & abs(Value) > 1e-6)  # Removes zeros and shrinkage artifacts


p_comp <- ggplot(comp_df, aes(x = Value, fill = Source, color = Source)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values  = c("Empirical Data: log(AVO / AT)" = "#2b83ba", 
                                "Model Sum: mu + beta + omega + epsilon" = "#d7191c")) +
  scale_color_manual(values = c("Empirical Data: log(AVO / AT)" = "#2b83ba", 
                                "Model Sum: mu + beta + omega + epsilon" = "#d7191c")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11)
  ) +
  labs(
    title    = "Empirical Data vs. Model Conversion Field",
    x        = "Log Conversion Factor Value",
    y        = "Density"
  )

print(p_comp)
ggsave("EBS_empirical_vs_model_scale_comparison.png", p_comp, width = 9, height = 5.5, dpi = 300)



full_years <- min(dat$Year):max(dat$Year)
xi_model   <- rep$epsilon_q_gct  # Dimensions: [n_grid, 1, n_years]

model_list <- list()
for (t in 1:dim(xi_model)[3]) {
  yr <- full_years[t]
  if (yr %in% overlap_years) {
    vals <- as.vector(xi_model[, 1, t])
    vals <- vals[!is.na(vals) & is.finite(vals)]
    model_list[[length(model_list) + 1]] <- data.frame(
      Year   = yr,
      Value  = vals,
      Source = "Model Summed Q (mu + beta + omega + epsilon)"
    )
  }
}
model_df <- bind_rows(model_list)


# 4. Create Interannual Side-by-Side Boxplot Comparison

at_grid_midwater <- dat_binned %>%
  filter(Gear %in% c("AT2", "AT3")) %>%  # <--- Restricts to AT Layers 2 & 3
  group_by(Year, Lon_bin, Lat_bin, Lon, Lat) %>%
  summarize(AT_loc = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop_last") %>%
  summarize(AT_density = mean(AT_loc, na.rm = TRUE), .groups = "drop")

# B. AVO: Sum AVO2 + AVO3 per station location, then average within grid cell
avo_grid_midwater <- dat_binned %>%
  filter(grepl("^AVO", Gear)) %>%
  group_by(Year, Lon_bin, Lat_bin, Lon, Lat) %>%
  summarize(AVO_loc = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop_last") %>%
  summarize(AVO_sA = mean(AVO_loc, na.rm = TRUE), .groups = "drop")

emp_df <- inner_join(
  at_grid_midwater, 
  avo_grid_midwater, 
  by = c("Year", "Lon_bin", "Lat_bin")
) %>%
  filter(AT_density > 0, AVO_sA > 0) %>%
  mutate(
    Value  = log(AVO_sA / AT_density),
    Source = "Empirical Data: log(AVO / AT2+3)"
  ) %>%
  select(Year, Value, Source)

combined_df <- bind_rows(emp_df, model_df) %>%
  mutate(Year = factor(Year))

p_annual_comp <- ggplot(combined_df, aes(x = Year, y = Value, fill = Source)) +
  geom_boxplot(
    outlier.size  = 0.8, 
    outlier.alpha = 0.3, 
    position      = position_dodge(width = 0.8), 
    alpha         = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Empirical Data: log(AVO / AT2+3)"             = "#2b83ba", # <--- Updated to match emp_df
      "Model Summed Q (mu + beta + omega + epsilon)" = "#d7191c"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title    = expression("Interannual Scale Comparison: Empirical " * log(AVO / AT) * " vs. Model Summed Q"),
    subtitle = "Side-by-side annual boxplots illustrating year-by-year scale dispersion and interannual trends",
    x        = "Survey Year",
    y        = "Log Conversion Factor Value"
  )

print(p_annual_comp)
ggsave("EBS_interannual_empirical_vs_model_Q.png", p_annual_comp, width = 10, height = 6, dpi = 300)



