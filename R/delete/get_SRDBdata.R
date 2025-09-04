
#Get soil respiration data from the Soil Respiraiton Database
#KAM June 10, 2025

# Load the packages
library(httr)
library(utils)

# Define the URL of the specific release asset
# You can find the asset URL by navigating to the release page on GitHub and copying the download link for the file you want
release_url <- "https://github.com/bpbond/srdb/archive/refs/tags/v20250503a.zip"

# Define the destination file path for the downloaded zip
dir <- print(getwd())
destfile <- "/data/srdb-20250503a"

# Use download.file() to download the file
download.file(url = release_url, destfile = paste0(dir, destfile), mode = "wb")

extraction_directory <- "/data/srdb"
dir.create(file.path(dir, extraction_directory))

# Check if the file was downloaded successfully
if (file.exists(paste0(dir,destfile))) {
  # Try unzipping the file and catch any errors
  tryCatch({
    unzip(paste0(dir,destfile), exdir = paste0(dir,extraction_directory))
    cat("Download and extraction of release data complete!\n")
  }, error = function(e) {
    cat("Error in extraction:", e$message, "\n")
  })
} else {
  cat("Failed to download the release file.\n")
}

# Optionally, clean up by removing the downloaded zip file
file.remove(paste0(dir, destfile))
