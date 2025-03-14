#' Index of abundance for Eastern Bering Sea walleye pollock combining biomass 
#' estimates from the MACE acoustic trawl and GAP bottom trawl surveys. The 
#' original model was published by Monnahan et al. (2020) in the ICES journal:
#' https://academic.oup.com/icesjms/article/78/5/1826/6278035
#' 
#' This script is based on an updated version of the model by Jim Thorson. The
#' model was moved to RTMB, which allowed a ridge correction approach to be 
#' implemented.
#' 
#' Code updated and maintained by Nate Lauffenburger and Sophia Wassermann

library(RTMB)
library(fmesher)
library(Matrix)
library(sf)
library(viridisLite)
library(here)

# data(acoustic_and_trawl, package = "FishStatsUtils" )
# dat <- subset(acoustic_and_trawl, Year == 2018)
# dat <- acoustic_and_trawl
dat <- read.csv(here("data", "data_real.csv"))

# Exclude years as sensitivity
# dat = subset( dat, Year %in% c(2007:2010, 2012, 2014, 2016, 2018))

dat_sf <- st_as_sf(dat, coords = c("Lon", "Lat"))

#data("eastern_bering_sea_grid", package="FishStatsUtils")
#extrap = eastern_bering_sea_grid
#year_set = sort(unique(dat$Year))
year_set <- min(dat$Year):max(dat$Year)

# Get extrapolation grid from shapefile
domain <- st_read(here("shapefiles", "EBSshelf.shp"))
domain <- st_geometry(domain)
domain <- st_transform(domain, crs = st_crs("EPSG:4326"))
grid <- st_make_grid(domain, cellsize = c(0.25,0.25))
grid <- st_intersection(grid, domain)
grid <- st_make_valid(grid)
extrap <- st_coordinates(st_centroid(grid))
extrap <- cbind('Lon' = extrap[, 1], 
                'Lat' = extrap[, 2], 
                'Area_in_survey_km2' = st_area(grid) / 1e6 )

# Unpack data
b_i <- dat$Catch_KG
Gear <- dat$Gear
t_i <- dat$Year - min(dat$Year) + 1

# Construct mesh
mesh <- fm_mesh_2d(dat[, c('Lon','Lat')], cutoff = 0.5)
spde <- fm_fem(mesh, order = 2)
A_is <- fm_evaluator(mesh, loc = as.matrix(dat[, c('Lon', 'Lat')]))$proj$A
A_gs <- fm_evaluator(mesh, loc = as.matrix(extrap[, c('Lon', 'Lat')]))$proj$A
area_g <- extrap[, 'Area_in_survey_km2']

# Extract
M0 <- spde$c0
M1 <- spde$g1
M2 <- spde$g2

parlist <- list(
  mu_c = rep(0, 3),
  beta_ct = array(0, dim = c(3, max(t_i))),
  epsilon_sct = array(0, dim = c(mesh$n, 3, max(t_i))),
  omega_sc = array(0, dim = c(mesh$n, 3)),
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
  nll_prior = nll_beta = nll_data = nll_epsilon = nll_omega = 0
  for(i in seq_along(b_i)) {
    if(Gear[i] == "BT") {
      yhat <- exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 1, t_i[i]]) + beta_ct[1, t_i[i]] + mu_c[1] + omega_ic[i, 1]) + 
              exp(ln_q + sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2])
    }
    if(Gear[i] == "AT2") {
      yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 2, t_i[i]]) + beta_ct[2, t_i[i]] + mu_c[2] + omega_ic[i, 2])
    }  
    if(Gear[i] == "AT3") {
      yhat <- exp(sum(A_is[i, ] * epsilon_sct[, 3, t_i[i]]) + beta_ct[3, t_i[i]] + mu_c[3] + omega_ic[i, 3])
    }
    nll_data <- nll_data - RTMB:::Term(dtweedie(x = b_i[i], 
                                                mu = yhat, 
                                                phi = phi, 
                                                p = p, 
                                                log = TRUE))
  }
  
  for(t_index in 1:max(t_i)) {
  for(c_index in 1:3) {
    if(t_index == 1) {
      nll_epsilon <- nll_epsilon - dgmrf(epsilon_sct[, c_index, t_index], 
                                         Q = Q_epsilon, 
                                         log = TRUE)
    } else {
      nll_epsilon <- nll_epsilon - dgmrf(epsilon_sct[, c_index, t_index], 
                                         mu = rho * epsilon_sct[,c_index, t_index - 1], 
                                         Q = Q_epsilon, 
                                         log = TRUE)
    }
  }}
  
  for(c_index in 1:3 ) {
    nll_omega <- nll_omega - dgmrf(omega_sc[, c_index], 
                                   Q = Q_omega, 
                                   log = TRUE)
  }
  
  for(t_index in 1:max(t_i) ) {
  for(c_index in 1:3) {
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
  if(what == "jnll") {
    out <- nll_data + nll_epsilon + nll_beta + nll_omega + nll_prior
  } 
  if(what == "diag") {
    out <- list(nll_data = nll_data,
                nll_epsilon = nll_epsilon,
                nll_beta = nll_beta,
                nll_omega = nll_omega,
                nll_prior = nll_prior)
  }
  
  # Make index
  index_ct <- matrix(0, nrow = 3, ncol = max(t_i))
  omega_gc <- A_gs %*% omega_sc
  epsilon_gct = D_gct = array(0, dim = c(length(area_g), 3, max(t_i)))
  for(t_index in 1:max(t_i)) {
  for(c_index in 1:3) {
    epsilon_gct[, c_index,t_index] <- (A_gs %*% epsilon_sct[, c_index, t_index])[, 1]
    D_gct[,c_index,t_index] <- area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index])[, 1]
    index_ct[c_index,t_index] <- sum(area_g * exp(A_gs %*% epsilon_sct[, c_index, t_index] + beta_ct[c_index, t_index] + mu_c[c_index] + omega_gc[, c_index]))
  }}
  
  Btrawl_t <- colSums(index_ct[1:2, ])
  Baccoustic_t <- colSums(index_ct[2:3, ])
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
  
  # bias correction and SEs (be parsimonious to avoid memory issue)
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
obj <- build_obj()  # build model object
opt <- nlminb(obj$par, obj$fn, obj$gr, 
              control = list(iter.max = 1e4, eval.max = 1e4, trace = 1))
parlist <- obj$env$parList()
Hess <- optimHess(opt$par, obj$fn, obj$gr)

# Get epsilon estimator
biascor <- sdreport(obj, 
                    par.fixed = opt$par,
                    hessian.fixed = Hess,
                    getReportCovariance = FALSE,
                    #bias.correct.control = list(sd = FALSE, split = NULL, nsplit = 10),
                    #skip.delta.method = FALSE, 
                    bias.correct = TRUE )

# Get SEs
extra_adreport <- TRUE
obj <- build_obj()
sdrep <- sdreport(obj,
                  par.fixed = opt$par,
                  hessian.fixed = Hess, 
                  bias.correct = FALSE,
                  getReportCovariance = FALSE)
rep <- obj$report()

# Extract & plot results ------------------------------------------------------
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

prop_bt <- colSums(index_ct[1:2, ]) / colSums(index_ct)
prop_at <- colSums(index_ct[2:3, ]) / colSums(index_ct)

png(file = here("output", "Fig_5_comparison.png"), 
    width = 5, height = 6, res = 200, units="in")
  par(mfrow = c(2, 1), mar = c(3, 3, 1, 1))
  barplot(prop_ct, 
          inside = TRUE,
          space = 0)
  matplot(x = year_set, y = cbind(prop_bt, prop_at), 
          col = c("blue", "orange"), lwd = 3, type = "l", lty = "solid",
          ylim = c(0, 1))
  polygon(x = c(year_set, rev(year_set)),
          y = c(prop_at + 2 * SD_report$Paccoustic, rev(prop_at - 2 * SD_report$Paccoustic)),
          col = rgb(1, 0.5, 0.5, alpha = 0.2), 
          border = NA)
  polygon(x = c(year_set, rev(year_set)),
          y = c(prop_bt + 2 * SD_report$Ptrawl, rev(prop_bt - 2 * SD_report$Ptrawl)),
          col = rgb(0, 0, 1, alpha = 0.2), 
          border = NA)
dev.off()

# Custom script from Jim Thorson for adding a plot legend
source(here("R","add_legend.R"))

for(c_index in 1:3) {
  png(file = here("output", paste0("Densities_", c("low", "med", "hi")[c_index], ".png")), 
      width = 7.5, height = 6, units = "in", res = 200)
    par(mfrow=c(3, 4))
    logD_gt <- log(Dhat_gct[, c_index, , drop = FALSE])
    logD_gt <- ifelse(logD_gt < max(logD_gt - log(1000)), NA, logD_gt)
    plotgrid <- st_sf(grid, logD_gt, crs = st_crs(grid))
    for(t in 1:(dim(Dhat_gct)[[3]])) {
      plot(plotgrid[, t], max.plot = 20, border = NA, key.pos = NULL, reset = FALSE, pal = viridis, main = year_set[t])
      add_legend(round(range(plotgrid[[t]], na.rm=TRUE), 2), legend_y = c(0.6, 1), legend_x = c(1, 1.05), col = viridis(10))
    }
  dev.off()
}

# Spatio-temporal term
for(c_index in 1:3){
  png(file = here("output", paste0("eps_", c("low", "med", "hi")[c_index], ".png")), 
      width = 7.5, height = 6, units = "in", res = 200)
    par(mfrow=c(3, 4))
    plotgrid = st_sf(grid, epshat_gct[, c_index, , drop = FALSE], crs = st_crs(grid))
    for(t in 1:(dim(Dhat_gct)[[3]])) {
      plot(plotgrid[, t], max.plot = 20, border = NA, key.pos = NULL, reset = FALSE, pal = viridis, main = year_set[t])
      add_legend(round(range(plotgrid[[t]], na.rm = TRUE), 2), legend_y = c(0.6, 1), legend_x = c(1, 1.05), col = viridis(10))
    }
  dev.off()
}

D_bt_gt <- apply(Dhat_gct[, 1:2, ], MARGIN = c(1, 3), FUN = sum)
D_at_gt <- apply(Dhat_gct[, 2:3, ], MARGIN = c(1, 3), FUN = sum)
prop_bt_gt <- apply(Dhat_gct[, 1:2, ], MARGIN = c(1, 3), FUN = sum) / apply(Dhat_gct, MARGIN = c(1, 3), FUN = sum)
prop_at_gt <- apply(Dhat_gct[, 2:3, ], MARGIN = c(1, 3), FUN = sum) / apply(Dhat_gct, MARGIN = c(1, 3), FUN = sum)
D_gzt <- aperm(abind::abind(D_bt_gt, D_at_gt, prop_bt_gt, prop_at_gt, along = 3), c(1, 3, 2))

for(c_index in 1:4) {
  png(file = here("output", paste0("Densities_", c("BT", "AT", "BTprop", "ATprop")[c_index], ".png")), 
      width = 7.5, height = 6, units = "in", res = 200)
    par(mfrow=c(3, 4))
    if(c_index %in% 1:2){
      Y_gt <- log(D_gzt[, c_index, , drop = FALSE])
      Y_gt <- ifelse(Y_gt < max(Y_gt - log(1000)), NA, Y_gt)
    } else {
      Y_gt <- D_gzt[, c_index, , drop = FALSE]
    }
    plotgrid <- st_sf(grid, Y_gt, crs = st_crs(grid))
    for(t in 1:(dim(Dhat_gct)[[3]])) {
      if (c_index %in% c(1,3)) {
         dat_subset = subset( dat_sf, Year == year_set[t] & dat$Gear == "BT")
      } else {
        dat_subset <- subset(dat_sf, Year == year_set[t] & dat$Gear != "BT")
      }
      plot(plotgrid[, t], max.plot = 20, border = NA, key.pos = NULL, reset = FALSE, pal = viridis, main = year_set[t])
      add_legend(round(range(plotgrid[[t]], na.rm = TRUE), 2), legend_y = c(0.6, 1), legend_x = c(1, 1.05), col = viridis(10))
      plot(st_geometry(dat_subset), add = TRUE, pch = 20, cex = 0.2, col = rgb(0, 0, 0, 0.2))
    }
  dev.off()
}

# Intercepts and data availability
cbind( 
  t(parlist$beta_ct),
  tapply(dat$Catch_KG, INDEX = list(factor(dat$Year, levels = year_set), dat$Gear), FUN = length)
)
