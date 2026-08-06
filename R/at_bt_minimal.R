# Model, without diagnostics and plotting, from at_bt_ridge_correct.R

install <- "minimal"
source("R/requirements.R")

# Data, mesh, parameter setup -------------------------------------------------
year <- 2025
dat <- read.csv(here("data", year, "dat_all.csv"))

load(here("data", "ebs_grid.Rdata"))

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
  # REPORT(yhat)
  # REPORT(Ptrawl_t)
  # REPORT(Paccoustic_t)
  # REPORT(Btrawl_t)
  # REPORT(Baccoustic_t)
  # REPORT(Btotal_t)
  # bias-correction and SEs (be parsimonious to avoid memory issue)
  # ADREPORT(Btrawl_t)
  # ADREPORT(Baccoustic_t)
  # ADREPORT(Btotal_t)
  # if(isTRUE(extra_adreport)) {
  #   ADREPORT(Ptrawl_t)
  #   ADREPORT(Paccoustic_t)
  # }
  ADREPORT(index_ct)
  return(out)
}

map <- list()
map$invf_rho <- factor(NA)
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
start <- Sys.time()
opt <- nlminb(obj$par, 
              obj$fn, 
              obj$gr, 
              control = list(iter.max = 1e4, eval.max = 1e4, trace = 1))
end <- Sys.time()
fit_time <- difftime(end, start, units = "mins")
fit_time

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
                  getReportCovariance = TRUE)
rep <- obj$report()

# save(obj, opt, parlist, Hess, biascor, sdrep, rep, year_set, file = here(results_dir, "model.RData"))
