library(dplyr)
library(lubridate)
library(ggplot2)
# Read the MODIS masterfile
MODIS <- read.csv("C:/Users/s2460885/Documents/MODIS/MCD12Q2_MasterFile_Inc_VegTypes_2.csv")

MODIS <- MODIS %>%
  mutate(year = year(ymd(calendar_date)))

# Assign pixel IDs based on unique (latitude, longitude) pairs
MODIS <- MODIS %>%
  mutate(coord = paste(latitude, longitude)) %>%  # Combine latitude and longitude into a single column
  group_by(coord) %>%                             # Group by this combined column
  mutate(pixel = cur_group_id()) %>%              # Assign a unique ID within each group
  ungroup()                                       # Ungroup to finish

MODIS <- as.data.frame(MODIS)

# Read the Landsat masterfile
Landsat <- read.csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs_VegClassified.csv")


############# PRUNE LANDSAT DATASET

# Read the Landsat masterfile
Landsat <- read.csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs_VegClassified.csv")


Landsat_clean <- na.omit(Landsat)  # Remove rows with NA
Landsat_clean <- Landsat_clean[!(Landsat_clean$year %in% c(1984, 1985, 1986, 1987, 1988, 1989, 1990, 2004, 2012)), ]
unique(Landsat_clean$year)

# Filter out specific vegetation groups
Landsat_clean <- Landsat_clean %>%
  filter(!Vegetation_Group %in% c("Poor Dry Grass", "NA", "Null", "Maritime Cliff")) %>%
  na.omit()  # Also remove rows with any NA values

#Save the final Landsat file 
write.csv(Landsat_clean, "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_Model_Dataset.csv", row.names = FALSE)


############# JOIN LANDSAT AND MODIS

# Check column names in Landsat and MODIS
colnames(Landsat)
colnames(MODIS)

Landsat <- Landsat_clean

# Ensure the necessary columns are present
required_columns_landsat <- c("longitude", "latitude", "year", "Vegetation_Group", "ndvi.max", "ndvi.max.doy")
required_columns_modis <- c("longitude", "latitude", "year", "Vegetation_Group", "MidGreenup", "MidGreendown", "Maturity", "EVI_Amplitude", "EVI_Maximum", "EVI_Area")

missing_columns_landsat <- setdiff(required_columns_landsat, colnames(Landsat))
missing_columns_modis <- setdiff(required_columns_modis, colnames(MODIS))

if (length(missing_columns_landsat) > 0) {
  stop("Missing columns in Landsat data: ", paste(missing_columns_landsat, collapse = ", "))
}

if (length(missing_columns_modis) > 0) {
  stop("Missing columns in MODIS data: ", paste(missing_columns_modis, collapse = ", "))
}

library(sf)

# Convert to spatial data frames and check for invalid geometries
landsat <- st_as_sf(Landsat, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_make_valid()

modis <- st_as_sf(MODIS, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_make_valid()

# Reproject to a suitable projected CRS (UTM)
landsat <- st_transform(landsat, crs = 32630)  # UTM zone 30N, UK
modis <- st_transform(modis, crs = 32630)

# Define MODIS grid based on its pixel size (500m resolution)
modis_pixel_size <- 500 # meters

# Create a grid covering the area of the MODIS data
modis_bbox <- st_bbox(modis)
modis_grid <- st_make_grid(st_as_sfc(modis_bbox), cellsize = modis_pixel_size, square = TRUE)

# Convert grid to spatial data frame
modis_grid_sf <- st_as_sf(modis_grid)
modis_grid_sf$grid_id <- 1:nrow(modis_grid_sf)

# Aggregate Landsat data within each MODIS grid cell
landsat_agg <- st_join(landsat, modis_grid_sf) %>%
  group_by(grid_id, year, Vegetation_Group) %>%
  summarize(ndvi_max = mean(ndvi.max, na.rm = TRUE),
            ndvi_max_doy = mean(ndvi.max.doy, na.rm = TRUE),
            .groups = "drop")

landsat_agg <- st_transform(landsat_agg, crs=4326)
modis <- st_transform(modis, crs=4326)
modis_grid_sf <- st_transform(modis_grid_sf, crs=4326)

landsat_agg <- as.data.frame(landsat_agg)

# Join aggregated Landsat data with MODIS data
combined_data <- st_join(modis, modis_grid_sf) %>%
  left_join(landsat_agg, by = c("grid_id", "year", "Vegetation_Group"))

combined_data <- as.data.frame(combined_data)

#Remove years & veg groups where there's no Landsat data (2004 and 2012 mainly)
combined_data <- na.omit(combined_data)
combined_data <- as.data.frame(combined_data)

library(tidyr)

combined_data <- combined_data %>%
  separate(coord, into = c("lat","long"), sep = " ")

# Create a unique list of pixel and coord
pixel_list <- combined_data %>%
  distinct(pixel, lat, long) %>%
  arrange(lat, long)

# Convert lat and long to numeric type
pixel_list$lat <- as.numeric(pixel_list$lat)
pixel_list$long <- as.numeric(pixel_list$long)

#obtain outer edge pixels
min_lat <- min(combined_data$lat)
max_lat <- max(combined_data$lat)
min_long <- min(combined_data$long)
max_long <- max(combined_data$long)

# Remove pixels by the sea and outer edges
combined_data <- combined_data %>%
  filter(
    lat != min_lat & 
      lat != max_lat &
      long != min_long & 
      long != max_long &
      !pixel %in% c(60, 65) #top right corner of map
  )

###################### CHECK PIXEL REMOVAL

# Create a new unique list of pixel and coord
pixel_list2 <- combined_data %>%
  distinct(pixel, lat, long) %>%
  arrange(lat, long)
# Convert lat and long to numeric type
pixel_list2$lat <- as.numeric(pixel_list2$lat)
pixel_list2$long <- as.numeric(pixel_list2$long)

# Load the leaflet library
library(leaflet)

# Plot the original pixels
leaflet_map <- leaflet(data = pixel_list) %>%
  # Add OpenStreetMap tiles as the background
  addTiles() %>%
  # Add red points for each pixel using lat/long
  addCircleMarkers(~long, ~lat, color = "red", radius = 5, popup = ~as.character(pixel)) %>%
  # Optionally, center the map on the mean latitude/longitude
  setView(lng = mean(pixel_list$long), lat = mean(pixel_list$lat), zoom = 12)

# Plot the new map without the pixels we removed
leaflet_map2 <- leaflet(data = pixel_list2) %>%
  # Add OpenStreetMap tiles as the background
  addTiles() %>%
  # Add red points for each pixel using lat/long
  addCircleMarkers(~long, ~lat, color = "red", radius = 5, popup = ~as.character(pixel)) %>%
  # Optionally, center the map on the mean latitude/longitude
  setView(lng = mean(pixel_list$long), lat = mean(pixel_list$lat), zoom = 12)

# Check the maps
leaflet_map
leaflet_map2


############### LANDSAT AND MODIS COMBINED MODELS

# Drop geometry columns before writing
combined_data <- combined_data[, !grepl("^geometry", names(combined_data))]

#Save the combined data file
write.csv(combined_data, "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_MODIS_Combined_Model_Dataset.csv", row.names = FALSE)
