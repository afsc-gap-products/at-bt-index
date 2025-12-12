#' Index of abundance for Eastern Bering Sea walleye pollock combining biomass 
#' estimates from the MACE acoustic trawl and GAP bottom trawl surveys. The 
#' original model was published by Monnahan et al. (2020) in the ICES journal:
#' https://academic.oup.com/icesjms/article/78/5/1826/6278035
#' 
#' This script is based on an updated version of the model by Jim Thorson. The
#' model was moved to RTMB, which allowed a ridge correction approach to be 
#' implemented.
#' 
#' Code updated and maintained by Sophia Wassermann

library(RTMB)
library(fmesher)
library(Matrix)
library(sf)
library(viridis)
library(here)
library(ggplot2)
library(dplyr)
library(remotes)
library(reshape2)
library(tidyr)

if (!requireNamespace("akgfmaps", quietly = TRUE)) {
  install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
}

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Read in data and set up model inputs ----------------------------------------
year <- format(Sys.Date(), "%Y")
dat <- read.csv(here("data", year, "dat_all.csv"))

# # Thin AVO3 samples
# which_AVO3 <- which(dat$Gear == "AVO3")
# which_drop <- sample(which_AVO3, replace = FALSE, size = floor(length(which_AVO3) * 0.5))
# dat <- dat[-which_drop, ]

# Set up grid 
dat_sf <- st_as_sf(dat, coords = c("Lon", "Lat"))
year_set <- min(dat$Year):max(dat$Year)

# Get extrapolation grid from akgfmaps package
ebs <- akgfmaps::get_base_layers(select.region = "sebs")$survey.area
ebs <- st_geometry(ebs)
ebs <- st_transform(ebs, crs = st_crs("EPSG:4326"))
grid <- st_make_grid(ebs, cellsize = c(0.25,0.25))
grid <- st_intersection(grid, ebs)
grid <- st_make_valid(grid)
extrap <- st_coordinates(st_centroid(grid))  # Warning: st_centroid does not give correct centroids for longitude/latitude data
extrap <- cbind("Lon" = extrap[, 1], 
                "Lat" = extrap[, 2],
                "Area_in_survey_km2" = units::drop_units(st_area(grid)) / 1e6)

# Unpack data
b_i <- dat$Abundance
Gear <- dat$Gear
t_i <- dat$Year - min(dat$Year) + 1

# Construct mesh
mesh <- fm_mesh_2d(dat[, c("Lon", "Lat")], cutoff = 0.5)
spde <- fm_fem(mesh, order = 2)
A_is <- fm_evaluator(mesh, loc = as.matrix(dat[, c("Lon", "Lat")]))$proj$A
A_gs <- fm_evaluator(mesh, loc = as.matrix(extrap[, c("Lon", "Lat")]))$proj$A
area_g <- extrap[,"Area_in_survey_km2"]

# Extract
M0 <- spde$c0
M1 <- spde$g1
M2 <- spde$g2

parlist <- list(
  mu_c = rep(0, 4),
  beta_ct = array(0, dim = c(4, max(t_i))),
  epsilon_sct = array(0, dim = c(mesh$n, 4, max(t_i))),
  omega_sc = array(0, dim = c(mesh$n, 4)),
  log_catchability = c(0),  # Q = E( backscatter / biomass )
  ln_kappa = log(1),
  ln_tau_omega = log(1),
  ln_tau_epsilon = log(1),
  ln_q = log(1),
  ln_phi = log(1),
  invf_p = 0,
  invf_rho = 1,
  ln_sd = log(0.1)
)

# Model construction ----------------------------------------------------------
jnll_spde <- function(parlist, what = "jnll") {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  getAll(parlist)
  phi <- exp(ln_phi)
  p <- plogis(invf_p) + 1
  Q_omega <- (exp(4 * ln_kappa) * M0 + 2 * exp(2 * ln_kappa) * M1 + M2) * exp(2 * ln_tau_omega)
  Q_epsilon <- (exp(4 * ln_kappa) * M0 + 2 * exp(2 * ln_kappa) * M1 + M2) * exp(2 * ln_tau_epsilon)
  rho <- invf_rho # plogis()
  sd <- exp(ln_sd)
  omega_ic <- A_is %*% omega_sc
  
  # Likelihood terms
  # For the following lines: 1 = <0.5m, 2 = 0.5-3m, 3 = 3-16m, 4 = >16m
  nll_prior = nll_beta = nll_data = nll_epsilon = nll_omega = 0
  for(i in seq_along(b_i)) {
    # BT covers all intervals from <0.5 to the effective fishing height (16m)
    if(Gear[i] == "BT") {
      yhat <- exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 1, t_i[i]]) + beta_ct[1, t_i[i]] + mu_c[1] + omega_ic[i, 1]) + 
        exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2]) +
        exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3])
    }
    # AT disaggregated into 0.5-3, 3-16, and >16
    if(Gear[i] == "AT1") yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2])
    if(Gear[i] == "AT2") yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3]) 
    if(Gear[i] == "AT3") yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 4,t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4])
    # AVO only available for 3-16 and >16
    if(Gear[i] == "AVO2") yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3] + log_catchability)
    if(Gear[i] == "AVO3") yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 4, t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4] + log_catchability)
    nll_data <- nll_data - RTMB:::Term(dtweedie(x = b_i[i], 
                                                mu = yhat, 
                                                phi = phi,
                                                p = p, 
                                                log = TRUE))
  }
  
  for(t_index in 1:max(t_i)) {
    for(c_index in 1:4) {
      if(t_index == 1) {
        nll_epsilon <- nll_epsilon - dgmrf(epsilon_sct[, c_index, t_index], 
                                           Q = Q_epsilon,
                                           log = TRUE)
      } else {
        nll_epsilon <- nll_epsilon - dgmrf(epsilon_sct[, c_index, t_index], 
                                           mu = rho * epsilon_sct[, c_index, t_index - 1], 
                                           Q = Q_epsilon,
                                           log = TRUE)
      }
    }}
  
  for(c_index in 1:4) {
    nll_omega <- nll_omega - dgmrf(omega_sc[, c_index], 
                                   Q = Q_omega, 
                                   log = TRUE)
  }
  
  for(t_index in 1:max(t_i)) {
    for(c_index in 1:4) {
      if(t_index == 1) {
        nll_beta <- nll_beta - dnorm(beta_ct[c_index, t_index], 
                                     mean = 0, 
                                     sd = sd, 
                                     log = TRUE)
      } else {
        nll_beta <- nll_beta - dnorm(beta_ct[c_index, t_index], 
                                     mean = rho * beta_ct[c_index, t_index - 1], 
                                     sd = sd, 
                                     log = TRUE)
      }
    }}
  
  nll_prior <- -1 * dnorm(ln_q, mean = 0, sd = 0.15, log = TRUE)
  if(what == "jnll") out <- nll_data + nll_epsilon + nll_beta + nll_omega + nll_prior
  if(what == "diag") {
    out <- list(nll_data = nll_data,
                nll_epsilon = nll_epsilon,
                nll_beta = nll_beta,
                nll_omega = nll_omega,
                nll_prior = nll_prior)
  }
  
  # Make index
  index_ct <- matrix(0, nrow = 4, ncol = max(t_i))
  omega_gc <- A_gs %*% omega_sc
  epsilon_gct = D_gct = array(0, dim = c(length(area_g), 4, max(t_i)))
  
  for(t_index in 1:max(t_i)) {
    for(c_index in 1:4) {
      epsilon_gct[, c_index, t_index] <- (A_gs %*% epsilon_sct[, c_index, t_index])[, 1]
      D_gct[, c_index, t_index] <- area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index])[, 1]
      index_ct[c_index, t_index] <- sum(area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index]))
    }}
  
  # Only producing an index for the BT & AT surveys (for their respective intervals)
  Btrawl_t <- colSums(index_ct[1:3, ])
  Baccoustic_t <- colSums(index_ct[2:4, ])
  Btotal_t <- colSums(index_ct)
  Ptrawl_t <- Btrawl_t / Btotal_t
  Paccoustic_t <- Baccoustic_t / Btotal_t
  
  # reports
  REPORT(index_ct)
  REPORT(D_gct)
  REPORT(epsilon_gct)
  REPORT(Ptrawl_t)
  REPORT(Paccoustic_t)
  REPORT(Btrawl_t)
  REPORT(Baccoustic_t)
  REPORT(Btotal_t)
  # bias-correction and SEs (be parsimonious to avoid memory issue)
  # ADREPORT(Btrawl_t)
  # ADREPORT(Baccoustic_t)
  # ADREPORT(Btotal_t)
  if(isTRUE(extra_adreport)) {
    ADREPORT(Ptrawl_t)
    ADREPORT(Paccoustic_t)
  }
  ADREPORT(index_ct)
  return(out)
}

extra_adreport <- FALSE
jnll_spde(parlist)

# 
map <- list()
map$invf_rho <- factor(NA)
#map$ln_sd = factor(NA)
map$ln_q <- factor(NA)
  
build_obj <- function() {
  MakeADFun( 
    func = jnll_spde,
    par = parlist,
    random = c("epsilon_sct", "beta_ct", "omega_sc"),
    silent = TRUE,
    #profile = "mu_c",
    map = map,
    ridge.correct = TRUE
  )
}

# Run model -------------------------------------------------------------------
obj <- build_obj()
opt <- nlminb(obj$par, 
              obj$fn, 
              obj$gr, 
              control = list(iter.max = 1e4, eval.max = 1e4, trace = 1))

parlist <- obj$env$parList()  # parameter estimates
Hess <- optimHess(opt$par, obj$fn, obj$gr)

# Get epsilon estimator (bias correction)
biascor <- sdreport(obj, 
                    par.fixed = opt$par,
                    hessian.fixed = Hess,
                    getReportCovariance = FALSE,
                    # bias.correct.control = list(sd = FALSE, split = NULL, nsplit = 10),
                    # skip.delta.method = FALSE, 
                    bias.correct = TRUE)

# Get SEs
extra_adreport <- TRUE
obj <- build_obj()
sdrep <- sdreport(obj,
                  par.fixed = opt$par,
                  hessian.fixed = Hess, 
                  bias.correct = FALSE,
                  getReportCovariance = FALSE)
rep <- obj$report()

# Extract index and proportion ------------------------------------------------
# Extract index
SD_report <- as.list(sdrep, report = TRUE, what = "Std. Error")
index_ct <- as.list(biascor, report = TRUE, what = "Est. (bias.correct)")$index_ct
index_se_ct <- SD_report$index_ct
#index_ct = rep$index_ct
epshat_sct <- as.list(sdrep, report = FALSE, what = "Std. Error")$epsilon_sct
epshat_gct <- rep$epsilon_gct
Dhat_gct <- rep$D_gct

# Proportions
prop_ct <- sweep(index_ct, MARGIN = 2, STAT = colSums(index_ct), FUN = "/")

prop_bt <- colSums(index_ct[1:3, ]) / colSums(index_ct)
prop_at <- colSums(index_ct[2:4, ]) / colSums(index_ct)

# Plot densities & spatiotemporal term ----------------------------------------
plot_spatial_data <- function(grid, data_array, year_set, interval_labels, output_prefix, log_transform = TRUE) {
  n_intervals <- dim(data_array)[2]  # [g, c_index, t]
  
  for(c_index in 1:n_intervals) {
    slice <- data_array[, c_index, , drop = FALSE]
    
    # Apply log + cutoff only if needed
    if(log_transform && c_index %in% 1:2) {
      slice <- log(slice)
      cutoff <- max(slice, na.rm = TRUE) - log(1000)
      slice[slice < cutoff] <- NA
    }
    
    # Convert to long data frame
    df <- as.data.frame(slice)
    colnames(df) <- year_set
    df$id <- 1:nrow(df)
    
    plotgrid <- st_sf(geometry = grid, crs = st_crs(grid))
    plotgrid$id <- 1:nrow(plotgrid)
    
    plotgrid_long <- left_join(plotgrid, df, by = "id") %>%
      pivot_longer(cols = all_of(as.character(year_set)),
                   names_to = "year",
                   values_to = "value")
    
    ggplot(plotgrid_long) +
      geom_sf(aes(fill = value, color = value)) +
      scale_fill_viridis(na.value = NA) +
      scale_color_viridis(na.value = NA) +
      facet_wrap(~year) +
      labs(fill = "value", color = "value") +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    # Save
    ggsave(filename = here("output", paste0(output_prefix, "_", interval_labels[c_index], ".png")),
           width = 7.5, height = 5, units = "in", dpi = 300)
  }
}

# Log density plot
plot_spatial_data(grid, Dhat_gct, year_set, interval_labels, "Densities", log_transform = TRUE)

# Spatio-temporal term (eps) plots
plot_spatial_data(grid, epshat_gct, year_set, interval_labels, "eps", log_transform = FALSE)

# Density/proportion by survey plots
# Compute sums and proportions
D_bt_gt <- apply(Dhat_gct[, 1:3, ], c(1,3), sum)        # BT
D_at_gt <- apply(Dhat_gct[, 2:4, ], c(1,3), sum)        # AT
prop_bt_gt <- D_bt_gt / apply(Dhat_gct, c(1,3), sum)    # BT proportion
prop_at_gt <- D_at_gt / apply(Dhat_gct, c(1,3), sum)    # AT proportion

# Combine into one array [g, c_index, t]
D_gzt <- array(NA, dim = c(nrow(D_bt_gt), 4, ncol(D_bt_gt)))
D_gzt[, 1, ] <- D_bt_gt
D_gzt[, 2, ] <- D_at_gt
D_gzt[, 3, ] <- prop_bt_gt
D_gzt[, 4, ] <- prop_at_gt

types <- c("BT", "AT", "BTprop", "ATprop")  # labels

# Call the function (ppply log-transform only to the first two intervals (BT and AT))
plot_spatial_data(grid, D_gzt, year_set, types, output_prefix = "Densities", log_transform = TRUE)

# Export results and new plots ------------------------------------------------
# Intercepts and data availability
cbind( 
  t(parlist$beta_ct),
  tapply(dat$Abundance, INDEX = list(factor(dat$Year, levels = year_set), dat$Gear), FUN = length)
)

indices <- data.frame(Year = year_set,
                      BT = colSums(index_ct[1:3, ]),
                      AT = colSums(index_ct[2:4, ])) 

avail_gear <- rbind(
  cbind.data.frame(Year = year_set, 
        Proportion = prop_at, 
        SD = SD_report$Paccoustic,
        Gear = "AT"),
  cbind.data.frame(Year = year_set, 
        Proportion = prop_bt, 
        SD = SD_report$Ptrawl,
        Gear = "BT")) 

write.csv(avail_gear, here("Results", "availability_gear_4layers.csv"), row.names = FALSE)

# Time series of proportion available
# Get years where there was a survey
at_years <- c(2007:2010, 2012, 2014, 2016, 2018)
bt_years <- unique(dat[Gear == "BT", ]$Year)

survey_yr_points <- avail_gear %>% 
  filter((Gear == "AT" & Year %in% at_years) | 
           (Gear == "BT" & Year %in% bt_years))
survey_yr_points <- rbind.data.frame(survey_yr_points,
                                     cbind.data.frame(Year = unique(dat[Gear == "AVO2", ]$Year),
                                                      Proportion = 0,
                                                      SD = 0,
                                                      Gear = "AVO")) %>%
  mutate(Gear = factor(Gear, levels = c("AT", "BT", "AVO")))

gear_plot <- ggplot() +
  geom_line(data = avail_gear, 
            aes(x = Year, y = Proportion, color = Gear)) +
  geom_point(data = survey_yr_points,
             aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = avail_gear, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD), fill = Gear), alpha = 0.4) +
  scale_color_manual(values = c("#93329E", "#A4C400", "black")) +
  scale_fill_manual(values = c("#93329E", "#A4C400", "black"))
gear_plot

ggsave(gear_plot, filename = here("Results", "avail_gear_plot_4layers.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Bar plot of availability by depth
avail_depth <- data.frame(t(prop_ct))
colnames(avail_depth) <- c("<0.5m", "0.5-3m", "3-16m", ">16m")
avail_depth$Year <- year_set
avail_depth <- reshape2::melt(avail_depth, 
                              id.vars = "Year",
                              variable.name = "Height",
                              value.name = "Proportion") %>%
  dplyr::mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m")))

write.csv(avail_depth, here("Results", "availability_depth_4layers.csv"), row.names = FALSE)

depth_plot <- ggplot(avail_depth) +
  geom_bar(aes(x = Year, y = Proportion, fill = Height), 
           position = "fill", stat = "identity") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.1, end = 0.9)
depth_plot

ggsave(depth_plot, filename = here("Results", "avail_depth_plot_4layers.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Both plots together
avail_both <- cowplot::plot_grid(depth_plot, gear_plot, ncol = 1)
avail_both

ggsave(avail_both, filename = here("Results", "avail_both_4layers.png"),
       width = 150, height = 150, units = "mm", dpi = 300)
