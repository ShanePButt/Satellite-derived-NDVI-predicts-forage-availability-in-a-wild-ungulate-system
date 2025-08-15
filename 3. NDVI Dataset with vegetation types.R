library(sf)
library(dplyr)

# Read the shapefile
rum_vegclassified <- st_read("C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/rum_NorthBlock_VegClassified.shp")

# Read the LandsatTS_Phenology_Min10Obs CSV file
landsat_data <- read.csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs.csv")

# Convert the LandsatTS_Phenology_Min10Obs data to an sf object
landsat_sf <- st_as_sf(landsat_data, coords = c("longitude", "latitude"), crs = 4326)

# Perform spatial join
joined_data <- st_join(landsat_sf, rum_vegclassified)

# View the resulting data
print(joined_data)

# Save the joined data as a new shapefile
st_write(joined_data, "C:/Users/s2460885/Documents/LandsatTS/LandsatTS_VegClassified_Shapefile.shp")

LandsatTS_VegClassified <- joined_data 
rm(joined_data)

LandsatTS_VegClassified

library(dplyr)

# Extract latitude and longitude from geometry
LandsatTS_VegClassified_df <- LandsatTS_VegClassified %>%
  mutate(latitude = st_coordinates(geometry)[, "Y"],  # Extract latitude from geometry
         longitude = st_coordinates(geometry)[, "X"]) %>%  # Extract longitude from geometry
  select(sample.id, year, latitude, longitude, NVC1, IMP1, NVC0, NVCs, Vgttn_G, n.obs, ndvi.gs.avg, ndvi.gs.med, ndvi.gs.q90, ndvi.max, ndvi.max.lwr, ndvi.max.upr, ndvi.max.doy) %>%
  as.data.frame()

# Remove the geometry column
LandsatTS_VegClassified_df <- LandsatTS_VegClassified_df %>%
  select(-geometry)
 
LandsatTS_VegClassified_df

# Save the dataframe as a CSV file
write.csv(LandsatTS_VegClassified_df, "C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs_VegClassified.csv", row.names = FALSE)

