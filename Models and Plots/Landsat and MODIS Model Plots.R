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
Landsat_clean <- read.csv("C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_Model_Dataset.csv")

library(lme4)
library(lmerTest)

############# MODIS PLOTS

# Subset the data to include only the vegetation groups with at least 5 pixels
MODIS_subset <- MODIS %>%
  filter(Vegetation_Group %in% c("Wet Heath", "Blanket Bog", "Wet Grass")) %>%
  na.omit()

# Run the linear mixed-effects model
MODIS_Model_subset <- lmer(EVI_Amplitude ~ year + Vegetation_Group + (1 | pixel) + (1 | year), 
                           data = MODIS_subset)

# Summarize the model results
summary(MODIS_Model_subset)

library(ggeffects)
predictions <- ggpredict(MODIS_Model_subset, terms = c("year [2000:2023]", "Vegetation_Group"))

# Define custom pastel colors
custom_colors <- c(
  "Blanket Bog" = "#FFB6C1",        # LightPink
  "Wet Heath" = "#DDA0DD",          # Plum
  "Wet Grass" = "#7FFFF0",          # PaleTurquoise
  "Maritime Cliff" = "#FFEB3B",     # Pastel Yellow
  "Calcareous Grass" = "green4",  # Green
  "Acid Grass" = "#D2B48C",         # Tan
  "Poor Dry Grass" = "#ADFF2F",     # GreenYellow
  "Alpine Heath" = "#FF69B4",       # HotPink
  "Dry Heath" = "#FFA07A",          # LightSalmon
  "Woodland" = "#CD5C5C",           # IndianRed
  "Null" = "#FFFFFF"                # White
)

#plot model prediction against raw data
modis_eviamp <- ggplot() +
  # Raw data points
  geom_jitter(data = MODIS_subset, 
              aes(x = year, y = EVI_Amplitude, color = Vegetation_Group),
              alpha = 0.5, size = 1.4, width = 0.2, height = 0) +
  
  # Model prediction lines
  geom_line(data = predictions, 
            aes(x = x, y = predicted, color = group), size = 1.5) +
  
  # Confidence ribbon
  geom_ribbon(data = predictions, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.3, color = NA) +
  
  # Labels and theme
  labs(x = "Year",
       y = "EVI Amplitude",
       color = "Vegetation Group",
       fill = "Vegetation Group") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5))  # Center the title



# Display the plot
modis_eviamp

#Save them
ggsave(
  filename = "MODIS EVI Amplitude Model Plot.pdf",
  plot = modis_eviamp,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "MODIS EVI Amplitude Model Plot.png",
  plot = modis_eviamp,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)


##MODIS Maturity

#MODIS Phenology Model (Maturity Date)

MODIS_Mat <- lmer(Maturity ~ year + Vegetation_Group + (1|pixel) + (1|year),
                  data = MODIS_subset)

summary(MODIS_Mat)

predictionsmat <- ggpredict(MODIS_Mat, terms = c("year [2000:2023]", "Vegetation_Group"))

##plot model prediction against raw data
modis_mat <- ggplot() +
  # Raw data points
  geom_jitter(data = MODIS_subset, 
              aes(x = year, y = Maturity, color = Vegetation_Group),
              alpha = 0.5, size = 1.4, width = 0.2, height = 0) +
  
  # Model prediction lines
  geom_line(data = predictionsmat, 
            aes(x = x, y = predicted, color = group), size = 1.5) +
  
  # Confidence ribbon
  geom_ribbon(data = predictionsmat, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  # Labels and theme
  labs(x = "Year",
       y = "Maturity Date",
       color = "Vegetation Group",
       fill = "Vegetation Group") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(xlim = c(2001, 2023), ylim = c(160, 210)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5))  # Center the title


# Display the plot
modis_mat

#Save them
ggsave(
  filename = "MODIS Maturity Model Plot.pdf",
  plot = modis_mat,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "MODIS Maturity Model Plot.png",
  plot = modis_mat,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)

######################## LANDSAT ##########################
library(glmmTMB)
LS_Model_Spatial <- readRDS(LS_Model_Spatial, file = "C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial.rds")
summary(LS_Model_Spatial)


# Predict values from the model
#Landsat_clean$predicted <- predict(LS_Model1)

library(ggeffects)


# Create a grid of values for prediction
years <- 1991:2023
veg_groups <- unique(Landsat_clean$Vegetation_Group)

# Create a new dataframe for predictions
newdata <- expand.grid(
  year = years,
  Vegetation_Group = veg_groups,
  sample.id = NA,  # Random effects will be marginalized over
  ID = factor(1),
  pos = numFactor(0, 0)  # Dummy spatial position; needed for structure
)

# If you scaled year, match the scaling!
year_mean <- mean(Landsat_clean$year, na.rm = TRUE)
year_sd <- sd(Landsat_clean$year, na.rm = TRUE)
newdata$year_scaled <- (newdata$year - year_mean) / year_sd
newdata$scale.year <- newdata$year_scaled  # name must match model



# Predict without random effects
newdata$pred <- predict(LS_Model_Spatial, newdata = newdata, re.form = NA)
# Predict with standard errors
pred_out <- predict(LS_Model_Spatial, newdata = newdata, re.form = NA, se.fit = TRUE)

# Add to newdata
newdata$pred <- pred_out$fit
newdata$se <- pred_out$se.fit

# 95% confidence intervals
newdata$conf.low <- newdata$pred - 1.96 * newdata$se
newdata$conf.high <- newdata$pred + 1.96 * newdata$se

# Plot the model predictions with raw data
lsatplot <- ggplot() +
  geom_jitter(data = Landsat_clean, 
              aes(x = year, y = ndvi.max, color = Vegetation_Group), 
              alpha = 0.05, size = 0.5) +
  geom_line(data = newdata, 
            aes(x = year, y = pred, color = Vegetation_Group), 
            size = 1) +
  # Confidence ribbon
  geom_ribbon(data = newdata, 
              aes(x = year, ymin = conf.low, ymax = conf.high, fill = Vegetation_Group),
              alpha = 0.05, color = NA) +
  labs(title = "NDVI Trends by Vegetation Group (Landsat)",
       x = "Year", y = "NDVI Max") +
  xlim(1991,2024) +
  ylim(0.5,0.9) +
  labs(title = "Model-Predicted NDVI Trends by Vegetation Group",
       x = "Year",
       y = "NDVI Max",
       color = "Vegetation Group",
       fill = "Vegetation Group") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),  
        axis.title = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))  # Center the title

lsatplot

#WITH INTERACTION
library(glmmTMB)
LS_Model_Spatial_Interaction <- readRDS("C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/LS_Model_Spatial_Interaction.rds")

LS_Model_Spatial_Interaction$call

summary(LS_Model_Spatial_Interaction)

vc <- VarCorr(LS_Model_Spatial_Interaction)

# Extract variance per grouping factor (only the first variance if multiple per group)
sapply(vc, function(x) {
  # x is a matrix with variances/correlations; extract diagonal variances
  diag(x)^2
})


# Predict without random effects
newdata$pred_int <- predict(LS_Model_Spatial_Interaction, newdata = newdata, re.form = NA)

# Predict with standard errors
pred_int_out <- predict(LS_Model_Spatial_Interaction, newdata = newdata, re.form = NA, se.fit = TRUE)


# Add to newdata
newdata$pred_int <- pred_int_out$fit
newdata$se_int <- pred_int_out$se.fit

# 95% confidence intervals
newdata$conf.low_int <- newdata$pred_int - 1.96 * newdata$se_int
newdata$conf.high_int <- newdata$pred_int + 1.96 * newdata$se_int


# Plot the model predictions with raw data
lsatplot_int <- ggplot() +
  geom_jitter(data = Landsat_clean, 
              aes(x = year, y = ndvi.max, color = Vegetation_Group), 
              alpha = 0.03, size = 0.05) +
  geom_line(data = newdata, 
            aes(x = year, y = pred_int, color = Vegetation_Group), 
            linewidth = 1) +
  # Confidence ribbon
  geom_ribbon(data = newdata, 
              aes(x = year, ymin = conf.low_int, ymax = conf.high_int, fill = Vegetation_Group),
              alpha = 0.1, color = NA) +
  coord_cartesian(xlim = c(1991, 2024), ylim = c(0.45, 0.9)) +
  labs(
    x = "Year",
    y = "NDVI Max",
    color = "Vegetation Group",
    fill = "Vegetation Group"
  ) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5))  # Center the title
  
lsatplot_int

#Save them
ggsave(
  filename = "Landsat NDVI Interaction Model Plot.pdf",
  plot = lsatplot_int,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 8, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "Landsat NDVI Interaction Model Plot.png",
  plot = lsatplot_int,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 8,
  height = 5,
  dpi = 300
)


# Plot the model predictions with raw data
lsatplot_int2 <- ggplot() +
  geom_jitter(data = Landsat_clean, 
              aes(x = year, y = ndvi.max, color = Vegetation_Group), 
              alpha = 0.01, size = 0.05) +
  geom_line(data = newdata, 
            aes(x = year, y = pred_int, color = Vegetation_Group), 
            linewidth = 1) +
    coord_cartesian(xlim = c(1991, 2024), ylim = c(0.65, 0.72)) +
  labs(
    x = "Year",
    y = "NDVI Max",
    color = "Vegetation Group",
    fill = "Vegetation Group"
  ) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5))  # Center the title

lsatplot_int2

#Save them
ggsave(
  filename = "Landsat NDVI Interaction Model Plot Zoomed - Supplementary.pdf",
  plot = lsatplot_int2,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 8, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "Landsat NDVI Interaction Model Plot Zoomed - Supplementary.png",
  plot = lsatplot_int2,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 8,
  height = 5,
  dpi = 300
)



