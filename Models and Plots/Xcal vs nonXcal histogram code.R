library(LandsatTS)
library(ggplot2)

# Read the Landsat masterfile
Landsat <- read.csv("C:/Users/s2460885/Documents/LandsatTS/Final Datasets for Models/Landsat_Model_Dataset.csv")

#Compute trends in annual vegetation greenness using lsat_calc_trend from LandsatTS
trends <- lsat_calc_trend(Landsat, si = 'ndvi.max', yrs = 1991:2023, sig = 0.05)

trends


lsat_plot_trend_hist(trends, xlim = c(-40,60))


#NON X CAL VERSION
# Read the Landsat masterfile
Landsat_nonxcal <- read.csv("C:/Users/s2460885/Documents/LandsatTS/Non XCalibrated/LandsatTS_Phenology_Min10Obs_Non_XCalibrated.csv")

#Compute trends in annual vegetation greenness using lsat_calc_trend
trends_nonxcal <- lsat_calc_trend(Landsat_nonxcal, si = 'ndvi.max', yrs = 1991:2023, sig = 0.05)

trends_nonxcal

lsat_plot_trend_hist_2 <- function(dt, xlim = c(-30, 30)) {
  
  dt <- data.table::data.table(dt)
  si <- first(dt$si)
  period <- unlist(strsplit(dt$trend.period, split = 'to')[1])
  first.yr <- period[1]
  last.yr <-  period[2]
  
  hist <- ggplot2::ggplot(dt, ggplot2::aes(total.change.pcnt, fill = ggplot2::after_stat(x))) +
    ggplot2::geom_histogram(bins = 50, size = 0.25, color = 'gray20') +
    ggplot2::scale_fill_gradient2(low = "darkgoldenrod4", mid = 'white', high = "darkgreen", 
                                  limits = xlim, midpoint = 0) +
    ggplot2::labs(
      y = 'Number of sample sites',
      x = paste0("Relative change in Landsat ",
                 gsub('.MAX', 'max', toupper(si)),
                 ' from ', first.yr, ' to ', last.yr, ' (%)')
    ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      legend.position = 'none',
      axis.text       = ggplot2::element_text(size = 14),
      axis.title      = ggplot2::element_text(size = 16, face = "bold"),
      plot.title      = ggplot2::element_text(size = 18, hjust = 0.5),
      strip.text      = ggplot2::element_text(size = 14)
    ) +
    ggplot2::xlim(xlim[1], xlim[2])
  
  return(hist)
}

lsat_plot_trend_hist_2(trends_nonxcal, xlim = c(-40,60))



library(data.table)
library(ggplot2)
library(dplyr)

# 1. Add source label
trends$Source <- "XCal"
trends_nonxcal$Source <- "Non-XCal"

# 2. Combine datasets
combined <- rbind(
  as.data.table(trends),
  as.data.table(trends_nonxcal)
)

# 3. Plot overlayed histogram
overlay_histogram_plot <- ggplot(combined, aes(x = total.change.pcnt, fill = Source)) +
  geom_histogram(position = "identity", bins = 50, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("XCal" = "darkorange", "Non-XCal" = "darkgreen")) +
  labs(
    x = "Relative change in NDVI max from 1991 to 2023 (%)",
    y = "Number of sample sites",
    fill = "Dataset"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0.5)
  ) +
  xlim(-40, 60)

overlay_histogram_plot

ggsave(
  filename = "Histogram xcal vs non xcal.pdf",
  plot = overlay_histogram_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "Histogram xcal vs non xcal.png",
  plot = overlay_histogram_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)
