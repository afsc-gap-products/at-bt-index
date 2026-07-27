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
