library(MODISTools)
library(sf)
library(dplyr)


# List available bands
bands = mt_bands("MCD12Q2")
bands$band

bandsOfInterest <- bands[c(1, 3, 5, 7,9,11,13,15,22), ]$band


# Function to generate a grid of points within a bounding box with specific resolution
generate_grid <- function(lon_min, lon_max, lat_min, lat_max, resolution) {
  # Convert resolution from meters to degrees for latitude
  res_lat_deg <- resolution / 111320 # 111320 meters per degree at the equator
  
  # Convert resolution from meters to degrees for longitude based on average latitude
  avg_lat <- (lat_min + lat_max) / 2
  res_lon_deg <- resolution / (111320 * cos(avg_lat * pi / 180))
  
  x_seq <- seq(from = lon_min, to = lon_max, by = res_lon_deg)
  y_seq <- seq(from = lat_min, to = lat_max, by = res_lat_deg)
  
  grid_points <- expand.grid(lon = x_seq, lat = y_seq)
  grid_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326) # WGS 84 CRS
  
  return(grid_sf)
}

# Function to process MODIS data and extract EVI measures
process_modis_data <- function(shapefile, product, band, start, end, lon_min, lon_max, lat_min, lat_max, resolution) {
  # Load the shapefile
  shp <- st_read(shapefile)
  
  # Generate a grid of MODIS pixels within the bounding box
  grid_sf <- generate_grid(lon_min, lon_max, lat_min, lat_max, resolution)
  
  # Transform the shapefile to the same CRS as the MODIS grid
  shp <- st_transform(shp, st_crs(grid_sf))
  
  # Find which grid points intersect with the shapefile
  intersection <- st_intersects(grid_sf, shp, sparse = FALSE)
  unique_points <- grid_sf[rowSums(intersection) > 0, ]
  
  # Convert the grid points to a data frame
  df <- data.frame(
    site_name = rep("unique_pixel", nrow(unique_points)),
    lon = st_coordinates(unique_points)[, "X"],
    lat = st_coordinates(unique_points)[, "Y"]
  )
  
  # Download MODIS data using mt_batch_subset
  modis_data <- mt_batch_subset(df = df,
                                product = product,
                                band = band,
                                start = start,
                                end = end,
                                internal = TRUE)
  
  # Return processed data
  return(modis_data)
}


# Define the bounding coordinates of the MODIS tile
lon_min <- -20.0000
lon_max <- 0.0167
lat_min <- 50.0000
lat_max <- 60.0000
resolution <- 500  # resolution in meters (500 meters)

# List of vegetation type shapefiles
vegetation_shapefiles <- c(
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Acid Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Blanket Bog.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Calcareous Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Dry Heath.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Wet Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Wet Heath.shp"
  #"C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Maritime Cliff.shp",
  #"C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Poor Dry Grass.shp"
)

# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[1],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[1], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}


#2nd band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[2],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[2], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}



#3rd band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[3],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[3], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

#4th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[4],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[4], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

#5th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[5],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[5], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

#6th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[6],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[6], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

#7th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[7],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[7], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

#8th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[8],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[8], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}


#9th band of interest
# Iterate over each vegetation type shapefile
for (shapefile in vegetation_shapefiles) {
  # Process MODIS data for the current vegetation type
  modis_data <- process_modis_data(shapefile, 
                                   product = "MCD12Q2", 
                                   band = bandsOfInterest[9],  
                                   start = "2000-01-01", 
                                   end = "2024-06-01", 
                                   lon_min = lon_min, 
                                   lon_max = lon_max, 
                                   lat_min = lat_min, 
                                   lat_max = lat_max, 
                                   resolution = resolution)
  
  # Define the file path for the CSV
  file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", bandsOfInterest[9], "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
  
  # Save the dataframe as a CSV file
  write.csv(modis_data, file = file_name, row.names = FALSE)
}

##ADD THE VEGETATION GROUP AND VEGETATION TYPE TO EACH CSV FILE

# Define the mapping of vegetation groups to their codes
veg_mapping <- data.frame(
  Vegetation_Group = c("Acid Grass", "Blanket Bog", "Calcareous Grass", "Dry Heath", "Wet Grass", "Wet Heath"),
  Vegetation_Code = c("AG", "BB", "CG", "DH", "WG", "WH")
)

# Define the list of vegetation shapefiles and corresponding vegetation groups
vegetation_shapefiles <- c(
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Acid Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Blanket Bog.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Calcareous Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Dry Heath.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Wet Grass.shp",
  "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/Wet Heath.shp"
)

vegetation_groups <- c("Acid Grass", "Blanket Bog", "Calcareous Grass", "Dry Heath", "Wet Grass", "Wet Heath")

# Define the bands of interest
bandsOfInterest <- c("EVI_Amplitude.Num_Modes_01", "EVI_Area.Num_Modes_01", "EVI_Minimum.Num_Modes_01", "Dormancy.Num_Modes_01", "Greenup.Num_Modes_01", "Maturity.Num_Modes_01", "MidGreendown.Num_Modes_01", "MidGreenup.Num_Modes_01", "Senescence.Num_Modes_01")

library(readr)

# Loop through each combination of shapefile and band
for (i in seq_along(vegetation_shapefiles)) {
  shapefile <- vegetation_shapefiles[i]
  veg_group <- vegetation_groups[i]
  veg_code <- veg_mapping %>% filter(Vegetation_Group == veg_group) %>% pull(Vegetation_Code)
  
  for (band in bandsOfInterest) {
    # Construct the file name
    file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", band, "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
    
    # Read the CSV file
    if (file.exists(file_name)) {
      data <- read_csv(file_name)
      
      # Add the Vegetation_Code and Vegetation_Group columns
      data <- data %>%
        mutate(Vegetation_Code = veg_code,
               Vegetation_Group = veg_group)
      
      # Save the modified data back to a CSV file
      write_csv(data, file_name)
    }
  }
}



##COMBINE THE VEGETATION TYPES TO CREATE A SINGLE FILE FOR EACH BAND
# Function to read, add columns, and combine CSV files for each band
combine_files_by_band <- function(band) {
  combined_data <- data.frame()
  
  for (i in seq_along(vegetation_shapefiles)) {
    shapefile <- vegetation_shapefiles[i]
    veg_group <- vegetation_groups[i]
    veg_code <- veg_mapping %>% filter(Vegetation_Group == veg_group) %>% pull(Vegetation_Code)
    
    # Construct the file name
    file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", band, "_", basename(tools::file_path_sans_ext(shapefile)), ".csv")
    
    # Read the CSV file
    if (file.exists(file_name)) {
      data <- read_csv(file_name)
      
      # Update True_Value calculation to set NA if value equals 32767
      data <- data %>%
        mutate(True_Value = ifelse(value == 32767, NA, value * scale))
      
      # Add the Vegetation_Code and Vegetation_Group columns if not already added
      data <- data %>%
        mutate(Vegetation_Code = veg_code,
               Vegetation_Group = veg_group)
      
      # Combine the data
      combined_data <- bind_rows(combined_data, data)
    }
  }
  
  # Write the combined data to a new CSV file
  combined_file_name <- paste0("C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_", band, "_combined.csv")
  write_csv(combined_data, combined_file_name)
}

# Combine files for each band
for (band in bandsOfInterest) {
  combine_files_by_band(band)
}


#Read the files back into R

# Define the file paths for the combined CSV files
combined_files <- list(
  EVI_Amplitude = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_EVI_Amplitude.Num_Modes_01_combined.csv",
  EVI_Minimum = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_EVI_Minimum.Num_Modes_01_combined.csv",
  EVI_Area = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_EVI_Area.Num_Modes_01_combined.csv",
  Dormancy = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_Dormancy.Num_Modes_01_combined.csv",
  Greenup = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_Greenup.Num_Modes_01_combined.csv",
  MidGreenup = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_MidGreenup.Num_Modes_01_combined.csv",
  MidGreendown = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_MidGreendown.Num_Modes_01_combined.csv",
  Maturity = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_Maturity.Num_Modes_01_combined.csv",
  Senescence = "C:/Users/s2460885/Documents/MODIS/Robin's Veg Classifications/MCD12Q2_Senescence.Num_Modes_01_combined.csv"
)

# Read the files into dataframes and convert to data frame
EVI_Amplitude <- as.data.frame(read_csv(combined_files$EVI_Amplitude))
EVI_Minimum <- as.data.frame(read_csv(combined_files$EVI_Minimum))
EVI_Area <- as.data.frame(read_csv(combined_files$EVI_Area))
Dormancy <- as.data.frame(read_csv(combined_files$Dormancy))
Greenup <- as.data.frame(read_csv(combined_files$Greenup))
MidGreenup <- as.data.frame(read_csv(combined_files$MidGreenup))
MidGreendown <- as.data.frame(read_csv(combined_files$MidGreendown))
Maturity <- as.data.frame(read_csv(combined_files$Maturity))
Senescence <- as.data.frame(read_csv(combined_files$Senescence))


# Check the first few rows of each dataframe to confirm they were read correctly
#head(EVI_Amplitude)
#head(EVI_Minimum)
#head(EVI_Area)

library(dplyr)
library(tidyr)



#CREATE THE MASTER TABLE FOR MODIS
# Combine the data frames
combined_df <- bind_rows(EVI_Amplitude, EVI_Minimum, EVI_Area, Dormancy, Greenup, MidGreenup, MidGreendown, Maturity, Senescence)

# Remove the value and scale column pre pivot (true_value already calculated)
combined_df <- combined_df %>%
  select(-value, -scale, -units)

# Pivot the band column by True_Value
combined_df <- combined_df %>%
  pivot_wider(names_from = band, values_from = True_Value)

combined_df <- as.data.frame(combined_df)


# Rename the column
combined_df <- combined_df %>%
  rename(EVI_Amplitude = EVI_Amplitude.Num_Modes_01,
         EVI_Area = EVI_Area.Num_Modes_01,
         EVI_Minimum = EVI_Minimum.Num_Modes_01,
         Dormancy = Dormancy.Num_Modes_01,
         Greenup = Greenup.Num_Modes_01,
         MidGreenup = MidGreenup.Num_Modes_01,
         MidGreendown = MidGreendown.Num_Modes_01,
         Maturity = Maturity.Num_Modes_01,
         Senescence = Senescence.Num_Modes_01)


# Calculate EVI_Maximum
combined_df <- combined_df %>%
  mutate(EVI_Maximum = EVI_Amplitude + EVI_Minimum)

combined_df

# Convert the phenology metrics from "days since 1st Jan 1970" to "days since 1st Jan in the focal year": 

#Make calendar_date a Date object
combined_df$calendar_date <- as.Date(combined_df$calendar_date)

#Calculate the number of days from 01/01/1970 to each calendar_date
days_since_1970 <- as.integer(combined_df$calendar_date - as.Date("1970-01-01"))

#Subtract days_since_1970 from the relevant columns
combined_df <- combined_df %>%
  mutate(
    Dormancy = Dormancy - days_since_1970,
    Greenup = Greenup - days_since_1970,
    MidGreenup = MidGreenup - days_since_1970,
    MidGreendown = MidGreendown - days_since_1970,
    Maturity = Maturity - days_since_1970,
    Senescence = Senescence - days_since_1970
  )

#print(combined_df)


# Save the dataframe as a CSV file
write.csv(combined_df, "C:/Users/s2460885/Documents/MODIS/MCD12Q2_MasterFile_Inc_VegTypes_2.csv", row.names = FALSE)
