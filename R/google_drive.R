# Script for uploading data to google drive when working on GCP

library(gargle)
library(googledrive)
library(purrr)

# Authorize and connect to google drive using NOAA email ----------------------
gdrive_email <- rstudioapi::showPrompt(title = "Email",
                                       message = "Email for Google Drive",
                                       default = "")

drive_auth(token = credentials_user_oauth2(
  scopes = "https://www.googleapis.com/auth/drive", 
  email = gdrive_email))

drive_user()  # check user account

# Upload results directory to google drive ------------------------------------
# Access drive folder via the string at the end of the URL (click into it in google drive)
drive_folder <- as_id("1U_RpXBnILwWoEWVDmi1ctSScZZ6sXqX5")  

# List local files in the results directory (defined in at_bt_ridge_correct.R)
results_files <- list.files(results_dir, full.names = TRUE)

# Upload all files to google drive
walk(results_files, ~ drive_upload(
  media = .x,
  path = drive_folder,
  overwrite = TRUE # replace existing files?
))

# Download a directory from google drive --------------------------------------
# List items in drive folder
contents <- drive_ls(drive_get(as_id("1vUfBMqi2rUqqbQzSxddgDMln1NYiR8qk")))

# Set local destination folder
local_dir <- here("Results", "Results 8-13-26")
if (!dir.exists(local_dir)) dir.create(local_dir)

# Download files
walk2(
  contents$id,
  contents$name,
  ~ drive_download(
    file = as_id(.x),
    path = file.path(local_dir, .y),
    overwrite = TRUE  # replace existing files?
  )
)
