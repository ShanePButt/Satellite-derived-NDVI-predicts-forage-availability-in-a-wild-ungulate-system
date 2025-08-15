################################## STANDING CROP VS NDVI MODEL ####################################

# Load necessary libraries
library(dplyr)
library(lme4)
library(ggeffects)
library(ggplot2)

# Define file paths
stcp_path <- "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Data Files/StCp_Shane.csv"
landsat_path <- "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Data Files/Landsat_Model_Dataset.csv"

# Read in the data
stcp <- read.csv(stcp_path)
landsat <- read.csv(landsat_path)

landsat_filtered <- landsat %>%
  filter(Vegetation_Group == "Calcareous Grass")


stcp_summary <- stcp %>%
  group_by(Year) %>%
  summarise(JunJulStCp = mean(JunJulStCp, na.rm = TRUE))

stcp_joined <- landsat_filtered %>%
  left_join(stcp_summary, by = c("year" = "Year"))

# Years to exclude
years_to_exclude <- c(1987, 1988, 1989, 1990, 2004, 2012)

# Filter them out
stcp_joined <- stcp_joined %>%
  filter(!year %in% years_to_exclude)

stcp_joined

# Rescale JunJulStCp (center and scale)
stcp_joined$JunJulStCp_scaled <- scale(stcp_joined$JunJulStCp)

# Rescale NDVI (center and scale)
stcp_joined$NDVI_Max_scaled <- scale(stcp_joined$ndvi.max)

stcp_joined

# Fit the model
ndvi_model <- lmer(NDVI_Max_scaled ~  JunJulStCp + (1|sample.id) + (1|year), data = stcp_joined)

# Summary
summary(ndvi_model)


# Summary of the model
summary(ndvi_model)
VarCorr(ndvi_model)


library(ggplot2)

# Standardized residuals
stcp_joined$resid <- residuals(ndvi_model, scaled = TRUE)

# QQ plot
qqnorm(stcp_joined$resid)
qqline(stcp_joined$resid)

# Histogram
hist(stcp_joined$resid, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals")

# Extract fitted values
stcp_joined$fitted <- fitted(ndvi_model)

# Residuals vs Fitted
ggplot(stcp_joined, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted values", y = "Standardized residuals", title = "Residuals vs Fitted Values")




################################ SCALED ################################
library(lme4)
library(ggplot2)
library(ggeffects)
library(viridis)  # Just in case

# Fit the model
ndvi_model_scaled <- lmer(NDVI_Max_scaled ~ JunJulStCp_scaled  + (1 | sample.id) + (1 | year), data = stcp_joined)
summary(ndvi_model_scaled)

library(MuMIn)
r.squaredGLMM(ndvi_model_scaled)

# Get predictions
ndvi_predictions_scaled <- ggpredict(ndvi_model_scaled, terms = "JunJulStCp_scaled [all]")

yearly_means <- stcp_joined %>%
  group_by(year) %>%
  summarise(
    mean_sc = mean(JunJulStCp_scaled, na.rm = TRUE),
    mean_ndvi = mean(NDVI_Max_scaled, na.rm = TRUE),
    se_ndvi = sd(NDVI_Max_scaled, na.rm = TRUE) / sqrt(n())
  )

yearly_means
# Plot
ndvi_plot_continuous_year <- ggplot() +
  # Raw data points colored by Year (continuous)
  geom_jitter(data = stcp_joined,
              aes(x = JunJulStCp_scaled, y = NDVI_Max_scaled, color = year),
              alpha = 0.06, size = 1.6, width = 0.03, height = 0) +
  
  # Model prediction line
  geom_line(data = ndvi_predictions_scaled,
            aes(x = x, y = predicted),
            color = "#E31A1C", size = 1.2, linetype = "dashed") +
  
  # Confidence ribbon
  geom_ribbon(data = ndvi_predictions_scaled,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "#E31A1C", alpha = 0.3) +
  # Error bars colored by Year
  geom_errorbar(data = yearly_means,
                aes(x = mean_sc, ymin = mean_ndvi - se_ndvi, ymax = mean_ndvi + se_ndvi, color = year),
                inherit.aes = FALSE,
                width = 0.1, size = 0.8) +
  
  # Points at mean values colored by Year
  geom_point(data = yearly_means,
             aes(x = mean_sc, y = mean_ndvi , color = year),
             inherit.aes = FALSE,
             size = 2) +
  # Labels and theme
  labs(x = "Scaled Live Standing Crop",
       y = "Scaled NDVI Max",
       color = "Year") +
  # Continuous color scale: purple b green b yellow
  scale_color_viridis_c(option = "viridis", direction = 1, limits = c(1990, 2023), breaks = c(1991,2000,2010,2020)) +
  ylim (-2.5,2) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

# Display the plot
ndvi_plot_continuous_year

# Save the plot
ggsave(
  filename = "NDVI vs Standing Crop.pdf",
  plot = ndvi_plot_continuous_year,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 10, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "NDVI vs Standing Crop.png",
  plot = ndvi_plot_continuous_year,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 10,
  height = 5,
  dpi = 300
)


####################################### GLMMTMB ##############################################

library(glmmTMB)
library(sf)
library(spdep)

# create a position variable for the autocorrelation structure     
stcp_joined$pos <- numFactor(round(stcp_joined$longitude,3), 
                               round(stcp_joined$latitude,3)) 

stcp_joined$ID <- factor(rep(1,nrow(stcp_joined)))


# Fit model with spatial random effects
stcp_ndvi_model_spatial <- glmmTMB(
  NDVI_Max_scaled ~ JunJulStCp_scaled +  
    (1 | sample.id) +
    (1 | year) +
    exp(pos + 0|ID),  # Spatial random effect
  data = stcp_joined,
  family = gaussian
)


summary(stcp_ndvi_model_spatial)
summary(ndvi_model_scaled)

###################################### STANDING CROP ONLY #########################################

# Convert Year to numeric if it's not already
stcp$Year_num <- as.numeric(as.character(stcp$Year))

# Fit a linear mixed model with random intercept for Plot
sc_model <- lmer(JunJulStCp ~ Year + (1 | Plot) #+ (1|Year)
                 , data = stcp)

# Summary
summary(sc_model)


library(ggplot2)
library(dplyr)

# Calculate annual mean and standard error
annual_summary <- stcp %>%
  group_by(Year) %>%
  summarise(
    mean_sc = mean(JunJulStCp, na.rm = TRUE),
    se_sc = sd(JunJulStCp, na.rm = TRUE) / sqrt(n())
  )

library(ggeffects)

# Get model predictions across a sequence of years
sc_predictions <- ggpredict(sc_model, terms = "Year [all]")

library(ggplot2)

# Raw + fitted trend line plot
standing_crop_plot <- ggplot() +
  # Raw data points
  geom_jitter(data = stcp,
              aes(x = Year, y = JunJulStCp*100),
              alpha = 0.3, size = 1.6, width = 0, height = 0, color = "#2A9D8F") +
  
  # Model prediction line
  geom_line(data = sc_predictions,
            aes(x = x, y = predicted*100),
            color = "#F42A99", size = 1.2, linetype = "dashed") +
  # Mean B1 SE per year (error bars)
  geom_errorbar(data = annual_summary,
                aes(x = Year, ymin = (mean_sc - se_sc)*100, ymax = (mean_sc + se_sc)*100),
                width = 0.8, color = "#2A9D8F", size = 0.8) +
  geom_point(data = annual_summary,
             aes(x = Year, y = mean_sc*100),
             size = 3, color = "#2A9D8F") +
  # Confidence interval ribbon
  geom_ribbon(data = sc_predictions,
              aes(x = x, ymin = conf.low*100, ymax = conf.high*100),
              fill = "#F42A99", alpha = 0.3) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = expression("Live Standing Crop (g/m"^2*")")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  xlim(1986,2024) +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5))  # Center the title

standing_crop_plot

#Save them
ggsave(
  filename = "Standing Crop Plot.pdf",
  plot = standing_crop_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 10, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "Standing Crop Plot.png",
  plot = standing_crop_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 10,
  height = 5,
  dpi = 300
)
