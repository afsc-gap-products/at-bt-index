# Install dependencies for either the minimal example or the full modelling & plotting

# Make sure package installer ("pak") exists
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Minimal CRAN packages
minimal_pkgs <- c(
  "RTMB",
  "fmesher",
  "Matrix",
  "sf",
  "here",
  "dplyr"
)

# Additional CRAN packages for the full model
full_cran_pkgs <- c(
  "viridis",
  "ggplot2",
  "remotes",
  "reshape2",
  "tidyr",
  "DHARMa",
  "tweedie"
)

# GitHub packages for the full model ("PackageName" = "repo/path")
full_github_pkgs <- c(
  "akgfmaps"   = "afsc-gap-products/akgfmaps",
  "ggsidekick" = "seananderson/ggsidekick"
)

# Install CRAN packages
cran_targets <- minimal_pkgs
if (install == "full") {
  cran_targets <- c(cran_targets, full_cran_pkgs)
}

missing_cran <- cran_targets[!sapply(cran_targets, requireNamespace, quietly = TRUE)]

if (length(missing_cran) > 0) {
  message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
  pak::pkg_install(missing_cran)
}

#  Install gitHub packages & set plot theme (full only)
if (install == "full") {
  missing_gh <- full_github_pkgs[!sapply(names(full_github_pkgs), requireNamespace, quietly = TRUE)]
  
  if (length(missing_gh) > 0) {
    message("Installing missing GitHub packages: ", paste(names(missing_gh), collapse = ", "))
    pak::pkg_install(unname(missing_gh))
  }
  
  # Load ggsidekick and set default plot theme
  library(ggsidekick)
}

# Load installed packages
pkgs_to_load <- cran_targets[cran_targets != "tweedie"]  # breaks model compilation if loaded
if (install == "full") {
  pkgs_to_load <- c(pkgs_to_load, names(full_github_pkgs))
}

# Dynamically attach all packages to the workspace
invisible(lapply(pkgs_to_load, library, character.only = TRUE))
# Post-load setup for 'full' model
if (install == "full") {
  ggplot2::theme_set(theme_sleek())
}

message("Requirements for the ", install, " model installed successfully.")