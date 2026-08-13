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

load(here("Results", "test", "model.RData"))  # Loads obj, opt, parlist, Hess, biascor, sdrep, rep, year_set

# Extract true values & set up simulation parameters --------------------------
true_fixed_pars <- opt$par
param_names     <- names(true_fixed_pars)
true_yhat       <- rep$yhat
true_p          <- plogis(true_fixed_pars["invf_p"]) + 1
true_phi        <- exp(true_fixed_pars["ln_phi"])

N_SIMS <- 50  # number of simulations
set.seed(12345)  # Reproducibility seed for simulation suite

# Backup original observations
original_b_i <- b_i  

# Storage arrays
sim_convergence <- logical(N_SIMS)
sim_hess_pd     <- logical(N_SIMS)
sim_max_grad    <- numeric(N_SIMS)
sim_times       <- numeric(N_SIMS)

estimates_matrix <- matrix(NA, nrow = N_SIMS, ncol = length(true_fixed_pars))
colnames(estimates_matrix) <- param_names

se_matrix <- matrix(NA, nrow = N_SIMS, ncol = length(true_fixed_pars))
colnames(se_matrix) <- param_names

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
          sdreport(sim_obj, par.fixed = sim_opt$par, hessian.fixed = sim_hess, bias.correct = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(sim_sdrep)) {
          se_matrix[s, ] <- summary(sim_sdrep, "fixed")[, "Std. Error"]
        }
      }
    }
  }
  
  cat(sprintf("Done (Conv: %s, Hessian PD: %s, MaxGrad: %.2e, Time: %.1fs)\n",
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
cat(sprintf("Average Run Time per Fit   : %.2f secs\n\n", mean(sim_times)))

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
