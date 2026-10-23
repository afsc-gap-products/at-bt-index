# at-bt-index

> Code is always in development  
>  
> This document is for informational purposes only and does not necessarily represent the views or official position of the Department of Commerce, the National Oceanic and Atmospheric Administration, or the National Marine Fisheries Service.

---

## Maintainers

This code is primarily maintained by:

- **Sophia Wassermann**

Alaska Fisheries Science Center  
National Marine Fisheries Service  
National Oceanic and Atmospheric Administration  
Seattle, WA 98195

---

## Overview

This repository contains code to fit a spatiotemporal index model that combines acoustic trawl survey (AT) and bottom trawl survey (BT) biomass estimates for eastern Bering Sea walleye pollock. The model is structured by depth intervals (<0.5 m, 0.5-3 m, 3-16 m, and >16 m from the bottom) and incorporates backscatter from the acoustic vessels of opportunity (AVO). The workflow estimates survey-specific biomass, uncertainty, and produces the diagnostic plots and summary tables used for index reporting.

---

## Application Structure

The project is organized around a small set of R scripts and supporting directories:

### Core model scripts

- `R/at_bt_ridge_correct.R`  
  Fits the full model, calculates biomass indices by depth and gear, and generates diagnostic plots and output tables.

- `R/at_bt_minimal.R`  
  Provides a simplified implementation of the model without full diagnostics and plotting.

- `R/requirements.R`  
  Installs and loads the package dependencies needed for the minimal or full model workflow.

- `R/google_drive.R`  
  Provides utilities for uploading and downloading model output from Google Drive.

### Supporting project files

- `data/`  
  Stores model input data, yearly data tables, and the EBS grid used for extrapolation.

- `shapefiles/`  
  Holds spatial files used to define survey extent and generate model grids.

- `output/`  
  Folder for generated model output and figure products.

### General workflow

- Prepare survey data and model grid  
- Fit the spatiotemporal index model by depth layer  
- Estimate biomass available to the BT and AT surveys  
- Generate diagnostics, summary tables, and figures  
- Save output products for reporting and reproducibility

---

# NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes the entire risk related to its use or performance. The information in this repository is intended to be a collaborative effort and does not legally bind the United States as a government entity.

Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the United States Government, Department of Commerce, or National Oceanic and Atmospheric Administration.

---

# NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105).

The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce employees; to the extent that copyright protection may not be available under the laws of the United States, the Department of Commerce authorizes those foreign countries to use the work only for their internal governmental purposes. In those countries, the work is provided without associated rights or privileges.

---

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) |  
[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) |  
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
