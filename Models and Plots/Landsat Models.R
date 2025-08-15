
# Read the Landsat masterfile
Landsat_clean <- read.csv("C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_Model_Dataset.csv")

head(Landsat_clean)

#################################### glmmTMB ###################################

library(glmmTMB)
library(sf)
library(spdep)
library(tidyverse)

# Read the Landsat masterfile
Landsat_clean <- read.csv("C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_Model_Dataset.csv")

# create a position variable for the autocorrelation structure     
Landsat_clean$pos <- numFactor(round(Landsat_clean$longitude,3), 
                               round(Landsat_clean$latitude,3)) 

Landsat_clean$ID <- factor(rep(1,nrow(Landsat_clean)))

# Fit model with spatial random effects and WITHOUT vegetation group at all
LS_Model_Spatial_NoVeg <- glmmTMB(
  ndvi.max ~ scale(year) + 
    (1 | sample.id) +
    (1 | year) +
    exp(pos + 0|ID),
  data = Landsat_clean,
  family = gaussian,
  REML = FALSE
)

saveRDS(LS_Model_Spatial_NoVeg, file = "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial_NoVeg_REMLFALSE.rds")

summary(LS_Model_Spatial_NoVeg)

# Fit model with spatial random effects and veg group
LS_Model_Spatial <- glmmTMB(
  ndvi.max ~ scale(year) + Vegetation_Group + 
    (1 | sample.id) +
    (1 | year) +
    exp(pos + 0|ID),  # Spatial random effect
  data = Landsat_clean,
  family = gaussian,
  REML = FALSE
)


saveRDS(LS_Model_Spatial, file = "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial_REMLFALSE.rds")

summary(LS_Model_Spatial)



# Fit model with spatial random effects AND interaction
LS_Model_Spatial_Interaction <- glmmTMB(
  ndvi.max ~ scale(year) * Vegetation_Group + 
    (1 | sample.id) +
    (1 | year) +
    exp(pos + 0|ID),  # Spatial random effect
  data = Landsat_clean,
  family = gaussian,
  REML = FALSE
)

saveRDS(LS_Model_Spatial_Interaction, file = "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial_Interaction_REMLFALSE.rds")

summary(LS_Model_Spatial_Interaction)

# Test for main effect of vegetation group
anova(LS_Model_Spatial_NoVeg, LS_Model_Spatial, test = "Chisq")


# Test if slopes differ among vegetation types
anova(LS_Model_Spatial, LS_Model_Spatial_Interaction, test = "Chisq")


#Interaction model is best; fit that with REML = T

# Fit model with spatial random effects AND interaction
LS_Model_Spatial_Interaction_Final <- glmmTMB(
  ndvi.max ~ scale(year) * Vegetation_Group + 
    (1 | sample.id) +
    (1 | year) +
    exp(pos + 0|ID),  # Spatial random effect
  data = Landsat_clean,
  family = gaussian,
  REML = TRUE
)

saveRDS(LS_Model_Spatial_Interaction_Final, file = "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial_Interaction.rds")

summary(LS_Model_Spatial_Interaction_Final)


#Get variance
vc <- VarCorr(LS_Model_Spatial_Interaction_Final)

str(vc)

# Extract variance components directly
sigma_sample <- as.numeric(vc$cond$sample.id[1])
sigma_year <- as.numeric(vc$cond$year[1])
sigma_residual <- sigma(LS_Model_Spatial)^2  # Residual variance

# Compute variance proportions
ICC_sample <- sigma_sample / (sigma_sample + sigma_year + sigma_residual)
ICC_year <- sigma_year / (sigma_sample + sigma_year + sigma_residual)
total_variance_explained <- (sigma_sample + sigma_year) / (sigma_sample + sigma_year + sigma_residual)

# Print results
cat("Total variance:", sigma_sample + sigma_year + sigma_residual, "\n")
cat("Residual Variance:", sigma_residual, "\n")
cat("Sample.ID Variance:", sigma_sample, "\n")
cat("Year Variance:", sigma_year, "\n")


cat("Prop explained by sample.id:", ICC_sample, "\n")
cat("Prop explained by year:", ICC_year, "\n")
cat("Prop explained by residual:", sigma_residual/(sigma_sample + sigma_year + sigma_residual), "\n")


# Convert to a tidy data frame
vc_df <- as.data.frame(vc)

# Print variance components
print(vc_df)

