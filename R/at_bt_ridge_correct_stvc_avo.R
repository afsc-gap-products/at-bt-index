#' Index of abundance for Eastern Bering Sea walleye pollock combining biomass 
#' estimates from the MACE acoustic trawl and GAP bottom trawl surveys. The 
#' original model was published by Monnahan et al. (2020) in the ICES journal:
#' https://academic.oup.com/icesjms/article/78/5/1826/6278035
#' 
#' This script is based on an updated version of the model by Jim Thorson. The
#' model was moved to RTMB, which allowed a ridge correction approach to be 
#' implemented.
#' 
#' At the moment, the model is indexed by depth intervals (<0.5m, 0.5-3m, 3-16m,
#' and >16m from the bottom) and returns the biomass (and SE) available for the 
#' two main surveys: bottom trawl and acoustic trawl, depending on the depth 
#' intervals sampled by those surveys, by combining the calculated index for a
#' subset of depth layers. The backscatter from the AVO survey is included in 
#' the model, but no biomass associated with AVO availability. 
#' 
#' Code updated and maintained by Sophia Wassermann

install <- "full"
source("R/requirements.R")

results_dir <- here("Results", "new STVC")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Read in data and set up model inputs ----------------------------------------
year <- 2025  # static for now (but set up for updating annually)
dat <- read.csv(here("data", year, "dat_all.csv")) 
# dat <- read.csv(here("data", year, "dat_bt_constrained.csv")) 

# Remove AVO in Bristol Bay
dat <- dat %>%
  filter(!(Year == 2010 & Gear %in% c("AVO2", "AVO3") & Lon > -160))

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

parlist <- list(
  mu_c = rep(0, 4),
  ln_mu_q = 0,
  beta_ct = array(0, dim = c(4, max(t_i))),
  epsilon_sct = array(0, dim = c(mesh$n, 4, max(t_i))),
  omega_sc = array(0, dim = c(mesh$n, 4)),
  beta_q_ct = array(0, dim = c(1, max(t_i))),
  epsilon_q_sct = array(0, dim = c(mesh$n, 1, max(t_i))),
  omega_q_sc = array(0, dim = c(mesh$n, 1)),
  ln_kappa = log(1), # log-spatial scale parameter
  ln_tau_omega = log(1),
  ln_tau_epsilon = log(1),
  ln_q = log(1),
  ln_phi = log(1),
  invf_p = 0,
  invf_rho = 1,
  ln_sd = log(0.1),
  ln_tau_epsilon_q = log(1),
  ln_tau_omega_q = log(1),
  ln_kappa_q = log(1),  
  invf_rho_q = 1,
  ln_sd_q = log(0.2)
)

# Model construction ----------------------------------------------------------
jnll_spde <- function(parlist, what = "jnll") {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  getAll(parlist)
  phi <- exp(ln_phi) # tweedie dispersion param
  p <- plogis(invf_p) + 1 # tweedie power param
  Q_omega <- (exp(4 * ln_kappa) * M0 + 2 * exp(2 * ln_kappa) * M1 + M2) * exp(2 * ln_tau_omega)
  Q_epsilon <- (exp(4 * ln_kappa) * M0 + 2 * exp(2 * ln_kappa) * M1 + M2) * exp(2 * ln_tau_epsilon)
  Q_epsilon_q <- (exp(4 * ln_kappa_q) * M0 + 2 * exp(2 * ln_kappa_q) * M1 + M2) * exp(2 * ln_tau_epsilon_q)
  Q_omega_q <- (exp(4 * ln_kappa_q) * M0 + 2 * exp(2 * ln_kappa_q) * M1 + M2) * exp(2 * ln_tau_omega_q)
  rho <- invf_rho # plogis(), controls how strongly spatiotemporal anomalies are linked from year to year
  rho_q <- invf_rho_q
  sd <- exp(ln_sd)
  sd_q <- exp(ln_sd_q)
  omega_ic <- A_is %*% omega_sc
  omega_q_ic <- A_is %*% omega_q_sc
  
  # Likelihood terms
  # For the following lines: 1 = <0.5m, 2 = 0.5-3m, 3 = 3-16m, 4 = >16m
  nll_prior = nll_beta = nll_data = nll_epsilon = nll_omega = nll_epsilon_q = nll_beta_q = nll_omega_q = nll_prior_q = 0
  yhat <- numeric(length(b_i))  # initial data vector for residual calculations

  for(i in seq_along(b_i)) {
    # BT covers all intervals from <0.5 to the effective fishing height (16m)
    # yhat is expected density
    if(Gear[i] == "BT") {
      yhat[i] <- exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 1, t_i[i]]) + beta_ct[1, t_i[i]] + mu_c[1] + omega_ic[i, 1]) + 
        exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2]) +
        exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3])
    }
    # AT disaggregated into 0.5-3, 3-16, and >16
    if(Gear[i] == "AT1") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2])
    if(Gear[i] == "AT2") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3]) 
    if(Gear[i] == "AT3") yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 4,t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4])
    
    # AVO only available for 3-16 and >16 - do svtc here for AVO
    if(Gear[i] == "AVO2") {
      stvc_term <- sum(A_is[i, ] * epsilon_q_sct[, 1, t_i[i]]) + beta_q_ct[1, t_i[i]] + omega_q_ic[i, 1] + ln_mu_q
      yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3] + stvc_term)
    }
    
    if(Gear[i] == "AVO3") {
      stvc_term <- sum(A_is[i, ] * epsilon_q_sct[, 1, t_i[i]]) + beta_q_ct[1, t_i[i]] + omega_q_ic[i, 1] + ln_mu_q
      yhat[i] <- exp(sum(A_is[i, ] * epsilon_sct[, 4, t_i[i]]) + beta_ct[4, t_i[i]] + mu_c[4] + omega_ic[i, 4] + stvc_term)
    }
    
    nll_data <- nll_data - RTMB:::Term(dtweedie(x = b_i[i], 
                                                mu = yhat[i], 
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
  

  
  for(t_index in 1:max(t_i)) {
      if(t_index == 1) {
        # Year 1: Baseline Matérn GMRF field across spatial knots
        nll_epsilon_q <- nll_epsilon_q - dgmrf(epsilon_q_sct[, 1, t_index], 
                                 Q = Q_epsilon_q, 
                                 log = TRUE)
      } else {
        # Years 2+: AR(1) temporal transition over years
        nll_epsilon_q <- nll_epsilon_q - dgmrf(epsilon_q_sct[, 1, t_index], 
                                 mu = rho_q * epsilon_q_sct[, 1, t_index - 1], 
                                 Q = Q_epsilon_q, 
                                 log = TRUE)
      }
    }
  
  
    
    # 1. Year 1 initial prior (anchors Year 1 near 0)
    nll_beta_q <- nll_beta_q - dnorm(beta_q_ct[1, 1], mean = 0, sd = 1, log = TRUE)
    
    # 2. Years 2+ random walk transition
    for(t_index in 2:max(t_i)) {
      nll_beta_q <- nll_beta_q - dnorm(
        x    = beta_q_ct[1, t_index],
        mean = beta_q_ct[1, t_index - 1],
        sd   = sd_q,
        log  = TRUE
      )
    }
  
    nll_omega_q <- nll_omega_q - dgmrf(omega_q_sc[, 1], 
                                   Q = Q_omega_q, 
                                   log = TRUE)

  nll_prior_q <- -1 * sum(dnorm(epsilon_q_sct, mean = 0, sd = 0.35, log = TRUE))
  nll_prior <- -1 * dnorm(ln_q, mean = 0, sd = 0.15, log = TRUE)
  if(what == "jnll") out <- nll_data + nll_epsilon + nll_beta + nll_omega + nll_prior + nll_prior_q + nll_epsilon_q + nll_beta_q + nll_omega_q
  if(what == "diag") {
    out <- list(nll_data = nll_data,
                nll_epsilon = nll_epsilon,
                nll_beta = nll_beta,
                nll_omega = nll_omega,
                nll_prior = nll_prior,
                nll_beta_q = nll_beta_q,
                nll_epsilon_q = nll_epsilon_q,
                nll_omega_q = nll_omega_q,
                nll_prior_q   = nll_prior_q)
  }
  if(what == "cond") out <- nll_data  # for cAIC
  
  # Make index
  index_ct <- matrix(0, nrow = 4, ncol = max(t_i))
  omega_gc <- A_gs %*% omega_sc
  omega_q_gc <- A_gs %*% omega_q_sc
  epsilon_gct = D_gct = epsilon_q_gct = array(0, dim = c(length(area_g), 4, max(t_i)))
  
  for(t_index in 1:max(t_i)) {
    for(c_index in 1:4) {
      epsilon_gct[, c_index, t_index] <- (A_gs %*% epsilon_sct[, c_index, t_index])[, 1]
      D_gct[, c_index, t_index] <- area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index])[, 1]
      index_ct[c_index, t_index] <- sum(area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index]))
    }}
  
  for(t_index in 1:max(t_i)) {
      epsilon_q_gct[, 1, t_index] <- (A_gs %*% epsilon_q_sct[, 1, t_index] + beta_q_ct[1,t_index] + ln_mu_q + omega_q_gc)[, 1]
  }
  
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
  REPORT(yhat)  # for residuals
  REPORT(Ptrawl_t)
  REPORT(Paccoustic_t)
  REPORT(Btrawl_t)
  REPORT(Baccoustic_t)
  REPORT(Btotal_t)
  REPORT(nll_data)  # for cAIC
  REPORT(epsilon_q_gct)
  # bias-correction and SEs (be parsimonious to avoid memory issue)
  # ADREPORT(Btrawl_t)
  # ADREPORT(Baccoustic_t)
  # ADREPORT(Btotal_t)
  if(isTRUE(extra_adreport)) {
    ADREPORT(Ptrawl_t)
    ADREPORT(Paccoustic_t)
  }
  ADREPORT(index_ct)
  # ADREPORT(D_gct)  # too computationally expensive (CHOLMOD error 'problem too large')
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
    random = c("epsilon_sct", "beta_ct", "omega_sc", "epsilon_q_sct", "beta_q_ct", "omega_q_sc"),
    silent = FALSE,
    #profile = "mu_c",
    map = map,
    ridge.correct = TRUE
  )
}

# Run model -------------------------------------------------------------------
start <- Sys.time()
obj <- build_obj()

init_fn <- obj$fn()
cat("Initial JNLL value:", init_fn, "\n")

if (is.na(init_fn) || is.nan(init_fn) || is.infinite(init_fn)) {
  stop("CRITICAL ERROR: Initial function value is NA, NaN, or Inf. Check initial values in parlist.")
}

init_gr <- obj$gr()
names(init_gr) <- names(obj$par)
if (any(is.na(init_gr)) || any(is.nan(init_gr)) || any(is.infinite(init_gr))) {
  cat("\n--- Parameters with NA/NaN/Inf gradients ---\n")
  print(init_gr[is.na(init_gr) | is.nan(init_gr) | is.infinite(init_gr)])
  stop("CRITICAL ERROR: Found invalid gradients. Do not proceed to nlminb().")
} else {
  cat("\nSUCCESS: All gradient components are numeric and valid!\n")
}
cat("\n--- Maximum absolute gradient by parameter group ---\n")
max_gr_by_par <- tapply(abs(init_gr), names(init_gr), max)
print(round(max_gr_by_par, 4))


opt <- nlminb(obj$par, 
              obj$fn, 
              obj$gr, 
              control = list(iter.max = 1e4, eval.max = 1e4, trace = 1))

cat("\nOptimizer Convergence Code:", opt$convergence, "\n")
cat("Message:", opt$message, "\n")
final_max_gr <- max(abs(obj$gr(opt$par)))
cat("Final Max Absolute Gradient:", round(final_max_gr, 6), "\n")


parlist <- obj$env$parList()  # parameter estimates
Hess <- optimHess(opt$par, obj$fn, obj$gr)

# Check if Hessian is positive-definite
is_pd <- all(eigen(Hess)$values > 0)
cat("Is Hessian positive definite?", is_pd, "\n")

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
                  getReportCovariance = TRUE)
rep <- obj$report()
end <- Sys.time()
runtime <- end - start
cat("Model took", round(runtime, 2), attr(runtime, "units"), "\n")

save(obj, opt, parlist, Hess, biascor, sdrep, rep, year_set, file = here(results_dir, "model.RData"))

# Table of standard errors, etc -----------------------------------------------
if (!exists("obj")) {load(here(results_dir, "model.RData"))}

# Extract parameter estimates and standard errors from sdreport, calculate CI
param_table <- as.data.frame(summary(sdrep, "fixed")) %>%
  mutate(Lower = Estimate - 1.96 * `Std. Error`,
         Upper = Estimate + 1.96 * `Std. Error`,
         CI = sprintf("%.2f (%.2f–%.2f)", Estimate, Lower, Upper)) 

# Set parameter names
param_table$parameter <- rownames(param_table)
rownames(param_table) <- NULL
param_table$description <- case_when(
  grepl("mu_c", param_table$parameter) ~ "Median for layer",
  grepl("beta_ct", param_table$parameter) ~ "Depth interval year effect",
  grepl("log_catchability", param_table$parameter) ~ "Log catchability for AVO",
  grepl("ln_kappa", param_table$parameter) ~ "Spatial decorrelation rate",
  grepl("ln_tau_omega", param_table$parameter) ~ "Spatial variance per distance",
  grepl("ln_tau_epsilon", param_table$parameter) ~ "Spatio-temporal variance per distance",
  grepl("ln_phi", param_table$parameter) ~ "Tweedie dispersion parameter",
  grepl("invf_p", param_table$parameter) ~ "Tweedie power parameter",
  grepl("ln_sd", param_table$parameter) ~ "Log SD of AR(1) process for beta_ct"
)

# Select final columns
param_table <- param_table %>%
  select(parameter, description, Estimate, `Std. Error`, CI)
param_table

write.csv(param_table, here(results_dir, "parameter_estimates.csv"), row.names = FALSE)

# Extract index and proportion ------------------------------------------------
# Extract index
SD_report <- as.list(sdrep, report = TRUE, what = "Std. Error")
cov <- as.list(sdrep, report = TRUE, what = "")
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

# Residuals -------------------------------------------------------------------
# Get model outputs and fitted values
yhat <- rep$yhat

# Extract Tweedie parameters
p <- plogis(opt$par["invf_p"]) + 1  # transform back to (1, 2)
phi <- exp(opt$par["ln_phi"])  # dispersion

# Simulate from Tweedie distribution using yhat
n_sims <- 250
simulated_data <- matrix(NA, nrow = length(b_i), ncol = n_sims)

for (i in seq_len(n_sims)) {
  # rtweedie accepts a vector for mu
  simulated_data[, i] <- tweedie::rtweedie(
    n = length(b_i),
    mu = yhat,
    phi = phi,
    power = p
  )
}

# Create DHARMa residuals
simulated_residuals <- createDHARMa(
  simulatedResponse = simulated_data,
  observedResponse = b_i,
  fittedPredictedResponse = yhat,
  integerResponse = TRUE  # prevent artificial stacking from exact 0s
)

# Standard DHARMa diagnostic plots (Q-Q plot, residuals vs. fitted)
plot(simulated_residuals)

# # Tests for 'bad' q-q plot
# # Run robust bootstrap outlier test, looking for p > 0.05
# testOutliers(simulated_residuals, type = "bootstrap")

# # Try KS test on a random subset of 300 points, looking for p close to 0.05
# sub_idx <- sample(1:length(b_i), 300)
# ks.test(dharma_residuals[sub_idx], "punif")

# Create dataframe with spatial info
residuals_df <- data.frame(
  Lon = dat$Lon,
  Lat = dat$Lat,
  Year = dat$Year,
  Gear = dat$Gear,
  Residual = residuals(simulated_residuals, quantileFunction = qnorm)
)

# Convert residuals to sf points
residuals_sf <- st_as_sf(residuals_df, coords = c("Lon", "Lat"), crs = 4326)

# Prepare grid polygon sf object
plotgrid <- st_sf(geometry = grid, crs = st_crs(grid))
plotgrid$id <- 1:nrow(plotgrid)

# Ensure CRS match for spatial join
residuals_sf <- st_transform(residuals_sf, st_crs(plotgrid))

# Spatial join: assign grid cell 'id' directly to each point
grid_residuals <- st_join(residuals_sf, plotgrid["id"]) %>%
  st_drop_geometry() %>%
  summarise(
    .by = c(id, Year, Gear),
    Residual = mean(Residual, na.rm = TRUE)
  )

# Merge mean residuals back to the grid for plotting & save file
plotgrid_residuals <- left_join(plotgrid, grid_residuals, by = "id") |>
  filter(!is.na(Residual))

saveRDS(plotgrid_residuals, file = here(results_dir, "residuals.RDS"))

# Plot map
gears <- unique(plotgrid_residuals$Gear)
for(i in 1:length(gears)) {
  ggplot(plotgrid_residuals %>% filter(Gear == gears[i])) +
    geom_sf(aes(fill = Residual, color = Residual)) +
    scale_color_distiller(palette = "PuOr") +
    scale_fill_distiller(palette = "PuOr") +
    facet_wrap(~Year) +
    labs(
      fill = "Residual",
      color = "Residual",
      title = gears[i]
    ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  ggsave(filename = here(results_dir, paste0("residuals_", gears[i], ".png")),
         width = 8, height = 5, units = "in", dpi = 300)
}

# Pairwise predicted random effects -------------------------------------------
eps_array <- as.list(sdrep, report = FALSE, what = "Estimate")$epsilon_sct

effect_df <- data.frame(
  depth1 = as.vector(eps_array[, 1, ]),
  depth2 = as.vector(eps_array[, 2, ]),
  depth3 = as.vector(eps_array[, 3, ]),
  depth4 = as.vector(eps_array[, 4, ])
)

# Code up color by survey data availability
survey_status_by_year <- ifelse(
  year_set %in% c(2007, 2008), "no AVO",
  ifelse(year_set %in% c(2011, 2013, 2015, 2017), "no AT", "all surveys")
) %>%
  factor(levels = c("all surveys", "no AVO", "no AT"))

depths <- c("<0.5m", "0.5-3m", "3-16m", ">16m")

pairwise_plots <- function(df, labels, survey_status, transform = identity) {
  pairs <- combn(seq_len(ncol(df)), 2, simplify = FALSE)
  plot_list <- lapply(seq_along(pairs), function(i) {
    idx <- pairs[[i]]
    x <- transform(df[[idx[1]]])
    y <- transform(df[[idx[2]]])
    df_plot <- cbind.data.frame(var1 = x, var2 = y, surveys = survey_status)
    range_limits <- range(c(df_plot$var1, df_plot$var2), na.rm = TRUE)

    ggplot(df_plot, aes(x = var1, y = var2, color = surveys)) +
      geom_point(alpha = 0.3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      coord_fixed(xlim = range_limits, ylim = range_limits) +
      scale_color_viridis(begin = 0.1, end = 0.9, discrete = TRUE) +
      xlab(labels[idx[1]]) +
      ylab(labels[idx[2]]) +
      labs(color = NULL)
  })

  combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)

  return(combined_plot)
}

year_key <- rep(survey_status_by_year, each = dim(eps_array)[1])
pairwise_effects <- pairwise_plots(
  df = effect_df,
  labels = depths,
  survey_status = year_key,
  transform = identity
)
pairwise_effects
ggsave(pairwise_effects, filename = here(results_dir, "pairwise_effects.png"),
        width = 150, height = 150, units = "mm", dpi = 300, bg = "white")

# Pairwise predicted density by depth -----------------------------------------
Dhat_by_depth <- lapply(1:4, function(c) {
  as.vector(Dhat_gct[, c, ])
})

density_df <- data.frame(
  depth1 = Dhat_by_depth[[1]],
  depth2 = Dhat_by_depth[[2]],
  depth3 = Dhat_by_depth[[3]],
  depth4 = Dhat_by_depth[[4]]
)

density_year_key <- rep(survey_status_by_year, each = nrow(Dhat_gct))
pairwise_density <- pairwise_plots(
  df = density_df,
  labels = depths,
  survey_status = density_year_key,
  transform = log
)
pairwise_density
ggsave(pairwise_density, filename = here(results_dir, "pairwise_density.png"),
        width = 150, height = 150, units = "mm", dpi = 300, bg = "white")

# Plot densities & spatiotemporal term ----------------------------------------
plot_spatial_data <- function(grid, data_array, year_set, interval_labels, output_prefix, log_transform = TRUE) {
  n_intervals <- dim(data_array)[2]  # [g, c_index, t]  
  
  for(c_index in 1:n_intervals) {
    slice <- data_array[, c_index, , drop = FALSE]

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
    
    if(log_transform == TRUE) {
      plotgrid_long$value <- log(plotgrid_long$value)
    }

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
    ggsave(
      filename = here(results_dir, paste0(output_prefix, "_", interval_labels[c_index], ".png")),
      width = 7.5, height = 5, units = "in", dpi = 300
    )
    saveRDS(
      plotgrid_long, 
      here(results_dir, paste0(output_prefix, "_", interval_labels[c_index], ".rds"))
    )
  }
}

interval_labels = c("0.5", "0.5-3", "3-16", "16")
# Log density by depth interval
plot_spatial_data(grid, Dhat_gct, year_set, interval_labels, "Densities", log_transform = TRUE)
# Spatio-temporal term (eps) by depth interval
plot_spatial_data(grid, epshat_gct, year_set, interval_labels, "eps", log_transform = FALSE)

# Log density by survey 
D_bt_gt <- apply(Dhat_gct[, 1:3, ], c(1,3), sum)        # BT
D_at_gt <- apply(Dhat_gct[, 2:4, ], c(1,3), sum)        # AT
D_gzt <- array(NA, dim = c(nrow(D_bt_gt), 2, ncol(D_bt_gt)))  # Combine into one array [g, c_index, t]
D_gzt[, 1, ] <- D_bt_gt
D_gzt[, 2, ] <- D_at_gt
plot_spatial_data(grid, D_gzt, year_set, c("BT", "AT"), "Densities", log_transform = TRUE)

# Proportion of density by survey
prop_bt_gt <- D_bt_gt / apply(Dhat_gct, c(1,3), sum)    # BT proportion
prop_at_gt <- D_at_gt / apply(Dhat_gct, c(1,3), sum)    # AT proportion
D_gzt_prop <- array(NA, dim = c(nrow(D_bt_gt), 2, ncol(D_bt_gt)))
D_gzt_prop[, 1, ] <- prop_bt_gt
D_gzt_prop[, 2, ] <- prop_at_gt
plot_spatial_data(grid, D_gzt_prop, year_set, c("BT", "AT"), "Proportion", log_transform = FALSE)

# Time series of proportion available by survey -------------------------------
# Intercepts and data availability
cbind(t(parlist$beta_ct),
      tapply(dat$Abundance, INDEX = list(factor(dat$Year, levels = year_set), dat$Gear), FUN = length))

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

write.csv(avail_gear, here(results_dir, "availability_gear.csv"), row.names = FALSE)

# Get years where there was a survey
at_years <- unique(dat[dat$Gear == "AT1", ]$Year)
bt_years <- unique(dat[dat$Gear == "BT", ]$Year)

survey_yr_points <- avail_gear %>% 
  filter((Gear == "AT" & Year %in% at_years) | 
           (Gear == "BT" & Year %in% bt_years)) %>%
  mutate(Gear = factor(Gear, levels = c("AT", "BT")))

gear_plot <- ggplot() +
  geom_line(data = avail_gear, 
            aes(x = Year, y = Proportion, color = Gear)) +
  geom_point(data = survey_yr_points,
             aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = avail_gear, 
              aes(x = Year, ymin = (Proportion - 2 * SD), ymax = (Proportion + 2 * SD), fill = Gear), alpha = 0.4) +
  scale_color_manual(values = c("#35a1ab", "#3d5297")) +
  scale_fill_manual(values = c("#35a1ab", "#3d5297")) +
  ylim(0, NA) +
  xlab("")
gear_plot

ggsave(gear_plot, filename = here(results_dir, "avail_gear_plot.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Bar plot of proportion available by depth -----------------------------------
avail_depth <- data.frame(t(prop_ct))
colnames(avail_depth) <- c("<0.5m", "0.5-3m", "3-16m", ">16m")
avail_depth$Year <- year_set
avail_depth <- reshape2::melt(avail_depth, 
                              id.vars = "Year",
                              variable.name = "Height",
                              value.name = "Proportion") %>%
  mutate(Height = factor(Height, levels = c(">16m", "3-16m", "0.5-3m", "<0.5m")))

write.csv(avail_depth, here(results_dir, "availability_depth.csv"), row.names = FALSE)

depth_plot <- ggplot(avail_depth) +
  geom_bar(aes(x = Year, y = Proportion, fill = Height), 
           position = "fill", stat = "identity") +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.1, end = 0.9)
depth_plot

ggsave(depth_plot, filename = here(results_dir, "avail_depth_plot.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Index by depth (with SE) ----------------------------------------------------
# Get index values by depth interval
ind_depth <- data.frame(Year = year_set,
                        depth1 = t(index_ct)[,1],
                        depth2 = t(index_ct)[,2],
                        depth3 = t(index_ct)[,3],
                        depth4 = t(index_ct)[,4]) %>%
  melt(id.vars = "Year", variable.name = "Height", value.name = "Estimate") %>%
  # Get index SD by depth interval
  bind_cols(SD = melt(data.frame(Year = year_set, 
                                 t(index_se_ct)), 
                      id.vars = "Year", value.name = "SD")$SD) %>%
  mutate(Height = factor(Height, 
                         levels = c("depth4", "depth3", "depth2", "depth1"), 
                         labels = c(">16m", "3-16m", "0.5-3m", "<0.5m"))) %>%
  mutate(Estimate = Estimate / 1000000000,
         SD = SD / 1000000000)

write.csv(ind_depth, here(results_dir, "index_depth.csv"), row.names = FALSE)

ind_depth_plot <- ggplot() +
  geom_line(data = ind_depth, 
            aes(x = Year, y = Estimate, color = Height)) +
  # geom_point(data = survey_yr_points,
  #            aes(x = Year, y = Proportion, color = Gear, shape = Gear)) +
  geom_ribbon(data = ind_depth, 
              aes(x = Year, ymin = (Estimate - 2 * SD), ymax = (Estimate + 2 * SD), fill = Height), alpha = 0.4) +
  scale_color_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.1, end = 0.9) +
  scale_fill_viridis(option = "mako", discrete = TRUE, direction = -1, begin = 0.1, end = 0.9) +
  ylab("Index of Abundance (Mt)") + xlab("")
ind_depth_plot

ggsave(ind_depth_plot, filename = here(results_dir, "index_depth_plot.png"),
       width = 150, height = 90, units = "mm", dpi = 300)

# Combine together ------------------------------------------------------------
# Both plots together
avail_both <- cowplot::plot_grid(depth_plot, gear_plot, ncol = 1)
avail_both

ggsave(avail_both, filename = here(results_dir, "avail_both.png"),
       width = 150, height = 150, units = "mm", dpi = 300)


# ==============================================================================
# Plot BT density inputs
# ==============================================================================

# Filter dat for bottom trawl rows
dat_bt <- dat %>% filter(Gear == "BT")

# Dynamically identify the CPUE / Abundance column name in dat
cpue_col <- if ("Abundance" %in% names(dat_bt)) {
  "Abundance"
} else if ("b_i" %in% names(dat_bt)) {
  "b_i"
} else {
  "CPUE"
}

# Apply log1p transformation for better visual contrast across catch magnitudes
dat_bt <- dat_bt %>%
  mutate(log_cpue = log1p(.data[[cpue_col]]))

# Extract unique BT survey years and calculate 2D grid dimensions
bt_years    <- sort(unique(dat_bt$Year))
n_years_bt  <- length(bt_years)

n_cols_years <- ceiling(sqrt(n_years_bt))
n_rows_years <- ceiling(n_years_bt / n_cols_years)

cat("BT Survey Years Count:", n_years_bt, "\n")
cat("Grid Layout:", n_rows_years, "rows x", n_cols_years, "columns\n")


p_bt_grid <- ggplot(dat_bt, aes(x = Lon, y = Lat)) +
  # Haul location points colored by log catch density
  geom_point(aes(color = log_cpue), size = 1.3, alpha = 0.9) +
  scale_color_viridis_c(
    option = "plasma",
    name   = expression(log("CPUE" + 1))
  ) +
  # Wrap into a balanced 2D matrix of years
  facet_wrap(~ Year, ncol = n_cols_years) +
  coord_quickmap() +
  labs(
    title    = "Bottom Trawl (BT) Survey Input CPUE",
    subtitle = "Observed pollock catch per unit effort across Eastern Bering Sea shelf survey years",
    x        = "Longitude",
    y        = "Latitude"
  )

print(p_bt_grid)

calc_width  <- n_cols_years * 3.5
calc_height <- n_rows_years * 3.2

ggsave(here(results_dir, "EBS_BT_input_data_grid.png"), p_bt_grid, width = calc_width, height = calc_height, dpi = 300)

# ==============================================================================
# Plot AT density inputs
# ==============================================================================

# Identify observation column in dat
obs_col <- if ("Abundance" %in% names(dat)) {
  "Abundance"
} else if ("b_i" %in% names(dat)) {
  "b_i"
} else {
  "CPUE"
}

# Sum AT depth layers (AT1, AT2, AT3) per station location (Year, Lon, Lat)
dat_at_sum <- dat %>%
  filter(grepl("^AT", Gear)) %>%
  group_by(Year, Lon, Lat) %>%
  summarize(Total_Density = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop") %>%
  mutate(log_density = log1p(Total_Density))

# Dynamic 2D grid layout for AT survey years
at_years    <- sort(unique(dat_at_sum$Year))
n_years_at  <- length(at_years)

n_cols_years <- ceiling(sqrt(n_years_at))
n_rows_years <- ceiling(n_years_at / n_cols_years)

cat("AT Survey Years Count:", n_years_at, "\n")
cat("Grid Layout:", n_rows_years, "rows x", n_cols_years, "columns\n")

p_at_grid <- ggplot(dat_at_sum, aes(x = Lon, y = Lat)) +
  geom_point(aes(color = log_density), size = 1.3, alpha = 0.9) +
  scale_color_viridis_c(
    option = "magma",
    name   = expression(log("Density" + 1))
  ) +
  facet_wrap(~ Year, ncol = n_cols_years) +
  coord_quickmap() +
  labs(
    title    = "Acoustic Trawl (AT) Survey Input Density",
    subtitle = "Observed pollock density integrated across water column layers per station",
    x        = "Longitude",
    y        = "Latitude"
  )

print(p_at_grid)

calc_width  <- n_cols_years * 3.5
calc_height <- n_rows_years * 3.2
ggsave(here(results_dir, "EBS_AT_input_data_grid.png"), p_at_grid, width = calc_width, height = calc_height, dpi = 300)


# ==============================================================================
# Plot AVO inputs
# ==============================================================================

# Sum AVO layers (AVO2 + AVO3) per acoustic sampling station (Year, Lon, Lat)
dat_avo_sum <- dat %>%
  filter(grepl("^AVO", Gear)) %>%
  group_by(Year, Lon, Lat) %>%
  summarize(sA_total_loc = sum(.data[[obs_col]], na.rm = TRUE), .groups = "drop") %>%
  mutate(log_sA = log1p(sA_total_loc))

# Dynamic 2D grid layout for AVO survey years
avo_years    <- sort(unique(dat_avo_sum$Year))
n_years_avo  <- length(avo_years)

n_cols_years <- ceiling(sqrt(n_years_avo))
n_rows_years <- ceiling(n_years_avo / n_cols_years)

cat("AVO Survey Years Count:", n_years_avo, "\n")
cat("Grid Layout:", n_rows_years, "rows x", n_cols_years, "columns\n")

p_avo_grid <- ggplot(dat_avo_sum, aes(x = Lon, y = Lat)) +
  geom_point(aes(color = log_sA), size = 1.3, alpha = 0.9) +
  scale_color_viridis_c(
    option = "plasma",
    name   = expression(log(s[A] + 1))
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
    title    = "Acoustic Vessel-of-Opportunity (AVO) Survey Input Backscatter",
    subtitle = expression("Observed backscatter (" * s[A] * ") summed across midwater layers (AVO2 + AVO3) per station"),
    x        = "Longitude",
    y        = "Latitude"
  )

print(p_avo_grid)

calc_width  <- n_cols_years * 3.5
calc_height <- n_rows_years * 3.2
ggsave(here(results_dir, "EBS_AVO_input_data_grid.png"), p_avo_grid, width = calc_width, height = calc_height, dpi = 300)

# Calculate cAIC --------------------------------------------------------------
library(Matrix)

# Extract conditional negative log-likelihood
nll_cond <- obj$report()$nll_data

# Fixed effect degrees of freedom (number of estimated non-random parameters)
p_fixed <- length(opt$par)

# Create map to fix all parameters except random effects at their MLE values
parlist_hat <- obj$env$parList()
map_all_fixed <- lapply(parlist_hat, function(x) factor(rep(NA, length(x))))

# Un-map the random effect structures so they are treated as active parameters
map_all_fixed$epsilon_sct <- NULL
map_all_fixed$beta_ct     <- NULL
map_all_fixed$omega_sc    <- NULL

# Build temporary RTMB objects for random effect Hessians (without random = ...)
obj_u_joint <- MakeADFun(
  func = function(p) jnll_spde(p, what = "jnll"),
  par  = parlist_hat,
  map  = map_all_fixed,
  silent = TRUE
)

obj_u_cond <- MakeADFun(
  func = function(p) jnll_spde(p, what = "cond"),
  par  = parlist_hat,
  map  = map_all_fixed,
  silent = TRUE
)

# Extract empirical Bayes estimates vector for random effects
u_hat <- obj_u_joint$par

# Compute sparse Hessians w.r.t. random effects
H_joint <- obj_u_joint$he(u_hat)  # Sparse joint Hessian
H_cond  <- obj_u_cond$he(u_hat)   # Sparse conditional Hessian

# Calculate Effective Degrees of Freedom for random effects: tr(H_joint^-1 * H_cond)
edf_u <- sum(diag(Matrix::solve(H_joint, H_cond)))

# Total Effective Degrees of Freedom and cAIC
EDF <- p_fixed + edf_u
EDF
cAIC <- 2 * nll_cond + 2 * EDF
cAIC

# Make a table and write to .csv
caic_table <- data.frame(
  Metric = c("Conditional NLL", "Fixed Effect DF", "Random Effect EDF", "Total EDF", "cAIC"),
  Value  = c(nll_cond, p_fixed, edf_u, EDF, cAIC)
)
write.csv(caic_table, file = here(results_dir, "cAIC.csv"), row.names = FALSE)
