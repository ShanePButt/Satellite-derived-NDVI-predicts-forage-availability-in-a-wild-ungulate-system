#-------------------------- download the data from GEE for the first time -------------------------------------

install.packages("devtools")
devtools::install_github("logan-berner/LandsatTS", build_vignettes = TRUE)

# Load packages for data handling etc.
library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(rgee)

# Load LandsatTS package
library(LandsatTS)

#If needed, set python environment
#rgee::ee_clean_pyenv()
#rgee::ee_install_set_pyenv(py_path = "C:\\Users\\shane\\anaconda3")

# Intialize the Earth Engine with rgee
ee_install()
ee_Authenticate()
ee_Initialize()

# Specify a region 
# Load the shapefile
shapefile <- "C:\\Users\\s2460885\\OneDrive - University of Edinburgh\\University of Edinburgh PhD\\Rum Shape File\\isle_of_rum.shp"
rum <- st_read(shapefile)

# Use lsat_get_pixel_centers to retrieve pixel centers and plot to a file that can be added to this documentation.
# We set plot_map to a file path (or just TRUE) to view
pixel_list_test_poly <- lsat_get_pixel_centers(rum, plot_map = "man/figures/lsat_get_pixel_centers.png")

rum_csv_data <- lsat_export_ts(pixel_list_test_poly,
                               start_doy = 1,
                               end_doy = 365,
                               start_date = "1984-01-01",
                               end_date = "today")

 Monitor export progress, waiting for last export to have finished
map(lsat_export_ts(pixel_list_test_poly), ee_monitoring)

reexport_task <- lsat_export_ts(lsat_export_ts(pixel_list_test_poly), chunks_from = "region")


# Install and load necessary packages
install.packages("googledrive")
install.packages("dplyr")

library(googledrive)
library(dplyr)

# Authenticate and access the Google Drive folder
drive_auth()  # Follow the authentication process in the browser
folder_name <- "lsatTS_export"  # Folder name
folder <- drive_find(pattern = folder_name, type = "folder")

# List CSV files in the folder
csv_files <- drive_get(as_id(folder$id))

# Download and read CSV files
all_data <- lapply(csv_files$name, function(file_name) {
  file_path <- paste0("/gdrive/My Drive/", folder_name, "/", file_name)
  temp_file <- tempfile(fileext = ".csv")
  drive_download(file_path, path = temp_file)
  read.csv(temp_file)
}) %>%
  bind_rows()


# Set the directory containing the CSV files
directory <- "C:/Users/shane/OneDrive - University of Edinburgh/University of Edinburgh PhD/NDVI Data/LandsatTS/RawData"

# Get a list of all CSV files in the directory
csv_files <- list.files(directory, full.names = TRUE, pattern = "\\.csv$", recursive = TRUE)

# Read each CSV file into a list of data frames
dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
combined_df <- do.call(rbind, dfs)


# Set the directory for the combined file
directory <- "C:/Users/shane/OneDrive - University of Edinburgh/University of Edinburgh PhD/NDVI Data/LandsatTS"


# Specify the full path to save the combined CSV file
combined_file <- file.path(directory, "combined_data.csv")

# Save the combined data frame as a CSV file
write.csv(combined_df, file = combined_file, row.names = FALSE)

head(combined_df)
