# Simulation testing the combined AT/BT model

library(here)
library(sf)
library(fmesher)
library(tweedie)
library(RTMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

library(ggsidekick)
theme_set(theme_sleek())

# Setup & load model results --------------------------------------------------
results_dir <- here("Results", "simulation_test")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

load(here("Results", "base", "model.RData"))  # Loads obj, opt, parlist, Hess, biascor, sdrep, rep, year_set

# Extract true values & set up simulation parameters --------------------------
true_fixed_pars <- opt$par
param_names     <- names(true_fixed_pars)
true_yhat       <- rep$yhat
true_p          <- plogis(true_fixed_pars["invf_p"]) + 1
true_phi        <- exp(true_fixed_pars["ln_phi"])

N_SIMS <- 50  # number of simulations
set.seed(12345)  # Reproducibility seed for simulation suite

# Read in original observations from data
year <- 2025  # static for now (but set up for updating annually)
dat <- read.csv(here("data", year, "dat_all.csv")) 
original_b_i <- dat$Abundance

# Set up grid 
dat_sf <- st_as_sf(dat, coords = c("Lon", "Lat"))
year_set <- min(dat$Year):max(dat$Year)

# Get EBS area from akgfmaps
if(!file.exists(here("data", "ebs_grid.Rdata"))) {
  ebs <- akgfmaps::get_base_layers(select.region = "sebs")$survey.area
  ebs <- st_geometry(ebs)
  ebs <- st_transform(ebs, 4326)  # keep in lon/lat for grid creation
  
  grid <- st_make_grid(ebs, cellsize = 0.25)
  grid <- st_intersection(grid, ebs)
  grid <- st_make_valid(grid)
  save(grid, file = here("data", "ebs_grid.Rdata"))
} else {
  load(here("data", "ebs_grid.Rdata"))
}

grid_proj <- st_transform(grid, 3338)  # Reproject grid for accurate centroids
centroids <- st_centroid(grid_proj)
centroids <- st_transform(centroids, 4326) # Transform centroids back to lon/lat
extrap <- st_coordinates(centroids)
extrap <- cbind(Lon = extrap[, 1], 
                Lat = extrap[, 2],
                Area_in_survey_km2 = units::drop_units(st_area(grid)) / 1e6)

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
M0 <- spde$c0  # mass matrix
M1 <- spde$g1  # gradient matrix (first derivative)
M2 <- spde$g2  # stiffness matrix (second derivative / Laplacian)

# Storage arrays
sim_convergence <- logical(N_SIMS)
sim_hess_pd     <- logical(N_SIMS)
sim_max_grad    <- numeric(N_SIMS)
sim_times       <- numeric(N_SIMS)

estimates_matrix <- matrix(NA, nrow = N_SIMS, ncol = length(true_fixed_pars))
colnames(estimates_matrix) <- param_names

se_matrix <- matrix(NA, nrow = N_SIMS, ncol = length(true_fixed_pars))
colnames(se_matrix) <- param_names

# Set up model ----------------------------------------------------------------
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
  yhat <- numeric(length(b_i))  # <--- Initialize vector here

  for(i in seq_along(b_i)) {
    # BT covers all intervals from <0.5 to the effective fishing height (16m)
    if(Gear[i] == "BT") {
      yhat[i] <- exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 1, t_i[i]]) + beta_ct[1, t_i[i]] + mu_c[1] + omega_ic[i, 1]) + 
                exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2]) +
                exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3])
    }
    # AT disaggregated into 0.5-3, 3-16, and >16
    if(Gear[i] == "AT1") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2])
    if(Gear[i] == "AT2") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3]) 
    if(Gear[i] == "AT3") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 4, t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4])
    # AVO only available for 3-16 and >16
    if(Gear[i] == "AVO2") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3] + log_catchability)
    if(Gear[i] == "AVO3") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 4, t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4] + log_catchability)
    
    nll_data <- nll_data - RTMB:::Term(
      RTMB::dtweedie(
        x = b_i[i], 
        mu = yhat[i], 
        phi = phi,
        p = p, 
        log = TRUE
        )
      )
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
  # REPORT(D_gct)
  # REPORT(epsilon_gct)
  REPORT(Ptrawl_t)
  REPORT(Paccoustic_t)
  REPORT(Btrawl_t)
  REPORT(Baccoustic_t)
  REPORT(Btotal_t)
  return(out)
}

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

# Simulation loop -------------------------------------------------------------
start_total_time <- Sys.time()

for (s in seq_len(N_SIMS)) {
  cat(sprintf("[%s] Simulation Run %d / %d ... ", format(Sys.time(), "%H:%M:%S"), s, N_SIMS))
  
  # Simulate dataset from Tweedie process
  b_i <<- tweedie::rtweedie(
    n     = length(true_yhat),
    mu    = true_yhat,
    phi   = true_phi,
    power = true_p
  )
  
  # Build AD function with simulated observations
  sim_obj <- build_obj()
  
  # Optimize model with crash protection
  start_sim <- Sys.time()
  sim_opt <- tryCatch({
    nlminb(
      sim_obj$par,
      sim_obj$fn,
      sim_obj$gr,
      control = list(iter.max = 1e4, eval.max = 1e4)
    )
  }, error = function(e) NULL)
  
  elapsed <- as.numeric(difftime(Sys.time(), start_sim, units = "mins"))
  sim_times[s] <- elapsed
  
  if (is.null(sim_opt)) {
    cat("FAILED (Optimization crash)\n")
    sim_convergence[s] <- FALSE
    next
  }
  
  sim_convergence[s] <- (sim_opt$convergence == 0)
  sim_max_grad[s]    <- max(abs(sim_obj$gr(sim_opt$par)))
  
  # Evaluate Hessian & standard errors
  if (sim_convergence[s]) {
    sim_hess <- tryCatch({
      optimHess(sim_opt$par, sim_obj$fn, sim_obj$gr)
    }, error = function(e) NULL)
    
    if (!is.null(sim_hess)) {
      eigen_vals <- eigen(sim_hess)$values
      sim_hess_pd[s] <- all(eigen_vals > 0)
      
      if (sim_hess_pd[s]) {
        estimates_matrix[s, ] <- sim_opt$par
        
        sim_sdrep <- tryCatch({
          sdreport(sim_obj, par.fixed = sim_opt$par, hessian.fixed = sim_hess, 
                   bias.correct = FALSE, getReportCovariance = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(sim_sdrep)) {
          se_matrix[s, ] <- summary(sim_sdrep, "fixed")[, "Std. Error"]
        }
      }
    }
  }

  # Clean up memory after each simulation
  rm(sim_obj, sim_opt, sim_hess, sim_sdrep)
  gc(verbose = FALSE)
  
  cat(sprintf("Done (Conv: %s, Hessian PD: %s, MaxGrad: %.2e, Time: %.1fm)\n",
              sim_convergence[s], sim_hess_pd[s], sim_max_grad[s], elapsed))
}

# Restore original observations to workspace
b_i <<- original_b_i

# Diagnostics -----------------------------------------------------------------
# Stability Metrics
conv_rate <- mean(sim_convergence) * 100
pd_rate   <- mean(sim_hess_pd & sim_convergence) * 100

cat(sprintf("Total Run Time             : %.2f mins\n", as.numeric(difftime(Sys.time(), start_total_time, units = "mins"))))
cat(sprintf("Optimizer Convergence Rate : %.1f%%\n", conv_rate))
cat(sprintf("Positive-Definite Hessian  : %.1f%%\n", pd_rate))
cat(sprintf("Average Run Time per Fit   : %.2f mins\n\n", mean(sim_times)))

# Performance & Recovery Metrics Dataframe
valid_runs <- which(sim_convergence & sim_hess_pd)

performance_df <- data.frame(
  Parameter = param_names,
  True_Value = true_fixed_pars,
  Mean_Estimate = colMeans(estimates_matrix[valid_runs, , drop = FALSE], na.rm = TRUE),
  SD_Estimate = apply(estimates_matrix[valid_runs, , drop = FALSE], 2, sd, na.rm = TRUE),
  Mean_SE = colMeans(se_matrix[valid_runs, , drop = FALSE], na.rm = TRUE)
) %>%
  mutate(
    Raw_Bias = Mean_Estimate - True_Value,
    Rel_Bias_Pct = (Raw_Bias / abs(True_Value)) * 100,
    RMSE = sqrt(colMeans((estimates_matrix[valid_runs, , drop = FALSE] - 
                           matrix(true_fixed_pars, nrow = length(valid_runs), 
                                  ncol = length(true_fixed_pars), byrow = TRUE))^2, na.rm = TRUE))
  )

# Calculate 95% Wald CI Coverage Rate
coverage_vec <- numeric(length(param_names))
for (j in seq_along(param_names)) {
  lower <- estimates_matrix[valid_runs, j] - 1.96 * se_matrix[valid_runs, j]
  upper <- estimates_matrix[valid_runs, j] + 1.96 * se_matrix[valid_runs, j]
  coverage_vec[j] <- mean(true_fixed_pars[j] >= lower & true_fixed_pars[j] <= upper, na.rm = TRUE) * 100
}

performance_df$Coverage_Pct <- coverage_vec

print(performance_df, digits = 3)

# Save tabular outputs
write.csv(performance_df, here(results_dir, "simulation_performance.csv"), row.names = FALSE)

# Visualizations --------------------------------------------------------------
# Reshape estimates for plotting
plot_df <- as.data.frame(estimates_matrix[valid_runs, , drop = FALSE]) %>%
  pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Estimate") %>%
  left_join(data.frame(Parameter = param_names, True_Value = true_fixed_pars), by = "Parameter")

# Plot parameter recovery boxplots
ggplot(plot_df, aes(x = Parameter, y = Estimate)) +
  geom_boxplot(fill = "#35a1ab", alpha = 0.6, outlier.color = "red", outlier.shape = 1) +
  geom_point(aes(y = True_Value), color = "black", shape = 18, size = 4) +
  facet_wrap(~Parameter, scales = "free") +
  labs(
    title = "Parameter Recovery Across Simulation Testing",
    subtitle = "Black diamonds indicate true baseline values; Red circles indicate outliers",
    x = "Parameter",
    y = "Estimated Value"
  )

ggsave(filename = here(results_dir, "parameter_recovery_plot.png"),
       width = 10, height = 7, dpi = 300)
