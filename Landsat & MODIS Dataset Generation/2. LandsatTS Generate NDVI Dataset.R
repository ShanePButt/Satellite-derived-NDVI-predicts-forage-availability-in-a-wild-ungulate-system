#-------------------------- download the data from GEE for the first time -------------------------------------

# Load packages for data handling etc.
library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(rgee)

# Load LandsatTS package
library(LandsatTS)
library(readr)

lsat.dt <- read_csv("C:/Users/s2460885/Documents/LandsatTS/combined_data.csv")

lsat.dt <- lsat_format_data(lsat.dt)


min(lsat.dt$doy) #31 before removing clouds etc., 76 after
max(lsat.dt$doy) #316 before removing clouds etc., 269 after

#Clean data (cloud, shadow, snow etc.)
lsat.dt <- lsat_clean_data(lsat.dt, geom.max = 30, cloud.max = 95, sza.max = 60, filter.cfmask.snow = T, filter.cfmask.water = T, filter.jrc.water = T)

head(lsat.dt)



# Define the file path
file_path <- "C:/Users/s2460885/Documents/LandsatTS/lsat_clean_data_95cloudmax.csv"

# Save the dataframe as a CSV file
write.csv(lsat.dt, file = file_path, row.names = FALSE)

library(readr)
lsat.dt <- read_csv("C:/Users/s2460885/Documents/LandsatTS/lsat_clean_data_95cloudmax.csv")


# Define the file path
file_path <- "C:/Users/s2460885/Documents/LandsatTS/Data_Summary.csv"

data.summary.dt <- lsat_summarize_data(lsat.dt)

# Save the dataframe as a CSV file
write.csv(data.summary.dt, file = file_path, row.names = FALSE)

# Print a message indicating the file has been saved
cat("Data summary saved to:", file_path, "\n")

# Compute NDVI or other vegetation index
lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')


# Cross-calibrate NDVI among sensors using an approach based on Random Forest machine learning
lsat.dt.xcal <- lsat_calibrate_rf(lsat.dt, 
                             band.or.si = 'ndvi', 
                             doy.rng = 91:273,
                             train.with.highlat.data = T,
                             overwrite.col = T)


# Define a file path
file_path <- "C:/Users/s2460885/Documents/LandsatTS/LandsatTS_RF_XCalibrated.csv"

# Save the dataframe as a CSV file
write.csv(lsat.dt.xcal, file = file_path, row.names = FALSE)

library(readr)
#lsat.dt.xcal <- read_csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_RF_XCalibrated.csv")

# Fit phenological models (cubic splines) to each time series
lsat.dt.phn <- lsat_fit_phenological_curves(lsat.dt.xcal, si = 'ndvi', window.yrs = 7, window.min.obs = 10, si.min = 0.15, spar = 0.78, spl.fit.outfile = F, progress = T, test.run = F)
lsat.dt.phn
write.csv(lsat.dt.phn, file = "C:/Users/s2460885/Documents/LandsatTS/LandsatTS_PhenologicalCurves.csv")

#lsat.dt.phn <-  read_csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_PhenologicalCurves.csv")


#Evaluate the performance of modeled NDVImax
lsat_evaluate_phenological_max(lsat.dt.phn, si = 'ndvi')

#summarise the growing seasons - final dataset
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.dt.phn, si = 'ndvi', min.frac.of.max = 0.6)
lsat.gs.dt
write.csv(lsat.gs.dt, file = "C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs.csv")

lsat.gs.dt <- read_csv("C:/Users/s2460885/Documents/LandsatTS/LandsatTS_Phenology_Min10Obs.csv")

