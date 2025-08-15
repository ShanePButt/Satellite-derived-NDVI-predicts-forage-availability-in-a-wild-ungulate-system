# Load required libraries
library(sf)



# Read the shapefile
north_block <- st_read("C:/Users/s2460885/Documents/Rum Shape File/isle_of_rum.shp")

rum <- st_read("C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/rum.shp")

st_crs(rum)
st_crs(north_block)

# Assign CRS to rum (if not already assigned)
if(is.na(st_crs(rum))) {
  rum <- st_set_crs(rum, 27700)  # Assign British National Grid CRS
}

# Transform rum to EPSG:4326 (WGS 84)
rum <- st_transform(rum, crs = st_crs(4326))

# Check the current CRS of north_block
st_crs(north_block)
st_crs(rum)

library(ggplot2)

# Visualize the shapefile
ggplot() +
  geom_sf(data = north_block) +
  theme_minimal()


##************ROBIN'S HABITAT MAP CODE FOR VEG CLASSIFICATIONS *************
library(sp)
library(sfheaders)
library(ggplot2)
library(raster)
library(geojson)
library(geojsonio)
library(rgee)

rumplot <- 
ggplot()+
  geom_sf(data=rum, aes(fill=NVC1))

rumplot
##Simplify vegetation types
rum$NVC0<-rum$NVC1
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="CG10a","CG10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="CG10b","CG10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="CG10c","CG10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="CG10d","CG10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="CG11a","CG11")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H10a","H10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H10b","H10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H10c","H10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H10d","H10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H14a","H14")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H18b","H18")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H20a","H20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H20c","H20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H20d","H20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H21a","H21")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H21b","H21")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="H7c","H7")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M10a","M10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M10aiii","M10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M15a","M15")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M15b","M15")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M15c","M15")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M15d","M15")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M15$","M15")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M17a","M17")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M17b","M17")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M17c","M17")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M23a","M23")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M23b","M23")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M25a","M25")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M25b","M25")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M32b","M32")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M6c","M6")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="M6d","M6")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="MC10a","MC10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="MC10b","MC10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="MC8c","MC8")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="MG6a","MG6")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U10a","U10")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U17a","U17")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U17d","U17")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U20a","U20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U20b","U20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U20c","U20")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U4a","U4")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U4b","U4")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U4c","U4")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U4d","U4")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U4e","U4")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U5a","U5")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U5c","U5")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U5d","U5")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U5e","U5")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U6a","U6")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U6c","U6")
rum$NVC0<-replace(rum$NVC0,rum$NVC0=="U7a","U7")

##Plot simplified
ggplot()+
  geom_sf(data=rum, aes(fill=NVC0))

###
##Even more simplified vegetation map
rum$NVCs<-rum$NVC0
##Blanket bog (BB)= M1 + M17 (+ M10)
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M1","BB")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M17","BB")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M10","BB")
##Wet heath (WH) = M15
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M15","WH")
##Wet grassland (WG) - M23 + M25 + M6 (+M32)
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M23","WG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M25","WG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M6","WG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="M32","WG")
##Maritime cliff (MC) = MC8 + MC10
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="MC8","MC")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="MC10","MC")
##Calc grass (CG) = CG10, 11, 12
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="CG10","CG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="CG11","CG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="CG12","CG")
##Acid grass (AG) = U4 + MG6
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U4","AG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="MG6","AG")
##Poor dry grass (PG) = U5,6,
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U5","PG")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U6","PG")
##Alpine heath (AH) = U10, 7
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U10","AH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U7","AH")
##Dry heath (DH) = H7,10,11, 14, 18, 20, 21
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H7","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H10","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H11","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H14","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H18","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H20","DH")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="H21","DH")
##Ignore (Null) U17, U20
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U17","Null")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="U20","Null")
##Woodland (WD)= W11, 17,23
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="W11","WD")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="W17","WD")
rum$NVCs<-replace(rum$NVCs,rum$NVCs=="W23","WD")

##************ROBIN'S HABITAT MAP CODE FOR VEG CLASSIFICATIONS *************
library(dplyr)

rum <- rum %>%
  mutate(Vegetation_Group = case_when(
    NVCs == "BB" ~ "Blanket Bog",
    NVCs == "WH" ~ "Wet Heath",
    NVCs == "WG" ~ "Wet Grass",
    NVCs == "MC" ~ "Maritime Cliff",
    NVCs == "CG" ~ "Calcareous Grass",
    NVCs == "AG" ~ "Acid Grass",
    NVCs == "PG" ~ "Poor Dry Grass",
    NVCs == "AH" ~ "Alpine Heath",
    NVCs == "DH" ~ "Dry Heath",
    NVCs == "WD" ~ "Woodland",
    TRUE ~ "Null"
  ))

rum <- st_make_valid(rum)

rum

###############
############## clipped_rum IS THE PART WHERE IT GETS CUT DOWN TO THE SAME SIZE AS GREG'S VEGETATIONLESS SHAPEFILE OF THE STUDY AREA


# Suppress warning about planar coordinates assumption
clipped_rum <- st_intersection(rum, north_block)

# Check the attributes of clipped_rum to make sure all fields are retained
str(clipped_rum)


# Save the updated shapefile with the corrected CRS
st_write(clipped_rum, "C:/Users/s2460885/Documents/Rum Shape File/RumVeg_Original/Robin Classifications/rum_NorthBlock_VegClassified.shp", append = FALSE)



## FOR SUPPLEMENTARY MATERIAL, TABLE OF THE DIFFERENT SUBCOMMUNITIES AND THEIR CORRESPONDING COMMUNITY

library(dplyr)
library(sf)

# Create lookup table for NVC0 descriptions
nvc_lookup <- tibble::tibble(
  NVC0 = c("U4", "MG6", "U10", "U7", "M17", "M10", "M1",
           "CG10", "CG11", "CG12", "H10", "H20", "H21", "H14", "H7", "H11", "H18",
           "MC10", "MC8", "U20", "U17", "U5", "U6", "M25", "M23", "M6", "M32",
           "M15", "W11", "W17", "W23"),
  NVC_Description = c(
#row1    
    "Festuca ovina-Agrostis capillaris-Galium saxatile grassland",
    "Lolium perenne-Cynosurus cristatus grassland",
    "Carex bigelowii-Racomitrium lanuginosum moss-heath",
    "Nardus stricta-Carex bigelowii grass-heath",
    "Scirpus cespitosus-Eriophorum vaginatum blanket mire",
    "Carex dioica-Pinguicula vulgaris mire",
    "Sphagnum auriculatum bog pool community",
#row2
    "Festuca ovina-Agrostis capillaris-Thymus praecox grassland",
    "Festuca ovina-Agrostis capillaris-Alchemilla alpina grassland",
    "Festuca ovina-Alchemilla alpina-Silene acaulis dwarf-herb community",
    "Calluna vulgaris-Erica cinerea heath",
    "Vaccinium myrtillus-Racomitrium lanuginosum heath",
    "Calluna vulgaris-Vaccinium myrtillus-Sphagnum capillifolium heath",
    "Calluna vulgaris-Racomitrium lanuginosum heath",
    "Calluna vulgaris-Scilla verna heath",
    "Calluna vulgaris-Carex arenaria heath",
    "Vaccinium myrtillus-Deschampsia flexuosa heath",
#row3
    "Festuca rubra-Plantago spp. maritime grassland",
    "Festuca rubra-Armeria maritima maritime grassland",
    "Pteridium aquilinum-Galium saxatile community",
    "Luzula sylvatica-Geum rivale tall-herb community",
    "Nardus stricta-Galium saxatile grassland",
    "Juncus squarrosus-Festuca ovina grassland",
    "Molinia caerulea-Potentilla erecta mire",
    "Juncus effusus/acutiflorus-Galium palustre rush-pasture",
    "Carex echinata-Sphagnum recurvum/auriculatum mire",
    "Philonotis fontana-Saxifraga stellaris spring",
#row4
    "Scirpus cespitosus-Erica tetralix wet heath",
    "Quercus petraea-Betula pubescens-Oxalis acetosella woodland",
    "Quercus petraea-Betula pubescens-Dicranum majus woodland",
    "Ulex europaeus-Rubus fruticosus scrub"
  )
)

############ STUDY AREA #############
# Summarize total area by NVC0 and Vegetation_Group
area_summary <- clipped_rum %>%
  group_by(Vegetation_Group, NVC0) %>%
  summarise(
    total_area_m2 = sum(AREA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add NVC descriptions if you already have them in a lookup table (optional)
  left_join(nvc_lookup, by = c("NVC0" = "NVC0")) %>%
  # Reorder and rename columns
  select(Vegetation_Group, NVC0 = NVC0, NVC_Description, total_area_m2)

# Extract and sort the data
summary_table <- area_summary %>%
  st_drop_geometry() %>%  # Remove geometry column
  arrange(Vegetation_Group, total_area_m2) %>%  # Sort
  select(Vegetation_Group, NVC0, NVC_Description, total_area_m2)

# Print table
print(summary_table)

write.csv(summary_table,
          "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Supplementary Materials/rum_nvc_summary.csv",
          row.names = FALSE)

############## WHOLE ISLAND #############
# Summarize total area by NVC0 and Vegetation_Group
area_summary2 <- rum %>%
  group_by(Vegetation_Group, NVC0) %>%
  summarise(
    total_area_m2 = sum(AREA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add NVC descriptions if you already have them in a lookup table (optional)
  left_join(nvc_lookup, by = c("NVC0" = "NVC0")) %>%
  # Reorder and rename columns
  dplyr::select(Vegetation_Group, NVC0 = NVC0, NVC_Description, total_area_m2)

# Extract and sort the data
summary_table2 <- area_summary2 %>%
  st_drop_geometry() %>%  # Remove geometry column
  arrange(Vegetation_Group, total_area_m2) %>%  # Sort
  dplyr::select(Vegetation_Group, NVC0, NVC_Description, total_area_m2)

# Print table
print(summary_table2)

write.csv(summary_table2,
          "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Supplementary Materials/rum_nvc_summary_whole_island.csv",
          row.names = FALSE)


###################### PLOT OF STUDY SITE BY VEGETATION TYPE ################

# Define custom pastel colors
custom_colors <- c(
  "Blanket Bog" = "#FFB6C1",        # LightPink
  "Wet Heath" = "#DDA0DD",          # Plum
  "Wet Grass" = "#7FFFE0",          # PaleTurquoise
  "Maritime Cliff" = "#FFEB3B",     # Pastel Yellow
  "Calcareous Grass" = "green4",  # Green
  "Acid Grass" = "#D2B48C",         # Tan
  "Poor Dry Grass" = "#ADFF2F",     # GreenYellow
  "Alpine Heath" = "#FF69B4",       # HotPink
  "Dry Heath" = "#FFA07A",          # LightSalmon
  "Woodland" = "#CD5C5C",           # IndianRed
  "Null" = "#FFFFFF"                # White
)

##Plot simplified
ggplot()+
  geom_sf(data=rum, aes(fill=Vegetation_Group))


# Plot with custom colors
vegplot <- ggplot() +
  geom_sf(data = clipped_rum, aes(fill = Vegetation_Group), colour = "grey2", size = 0.1) +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal() +
  theme(legend.key.size = unit(2, "lines")) +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "Study Area Vegetation Map.pdf",
  plot = vegplot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "Study Area Vegetation Map.png",
  plot = vegplot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)



###################### PLOTS OF ISLAND FOR SUPPLEMENTARY ####################

library(dplyr)
library(colorspace)
library(tibble)
# colour list
vg_colours <- c(
  "Blanket Bog"     = "#FFB6C1",  # LightPink
  "Wet Heath"       = "#DDA0DD",  # Plum
  "Wet Grass"       = "#7FFFE0",  # PaleTurquoise
  "Maritime Cliff"  = "#FFEB3B",  # Pastel Yellow
  "Calcareous Grass"= "green4",   # Green
  "Acid Grass"      = "#D2B48C",  # Tan
  "Poor Dry Grass"  = "#ADFF2F",  # GreenYellow
  "Alpine Heath"    = "#FF69B4",  # HotPink
  "Dry Heath"       = "#FFA07A",  # LightSalmon
  "Woodland"        = "#CD5C5C",  # IndianRed
  "Null"            = "#FFFFFF"   # White
)


#veg code NVC lookup
nvc_lookup <- summary_table2 %>% 
  dplyr::select(Vegetation_Group, NVC0) %>% 
  distinct() %>% 
  mutate(VegGroup_NVC = paste(Vegetation_Group, NVC0, sep = " - "))

#fan out to lighter and darker shades from veg group shade
shade_seq <- function(base_colour, n) {
  if (n == 1) return(base_colour)
  half <- floor(n / 2)
  dark  <- seq(0.3, -0.3, length.out = half)
  light <- seq(-0.3,  0.3, length.out = n - half)
  c(
    vapply(dark,  \(lvl) colorspace::darken (base_colour, lvl), character(1)),
    vapply(light, \(lvl) colorspace::lighten(base_colour, lvl), character(1))
  )
}

#build colour palette
nvc_palette <- nvc_lookup %>% 
  group_by(Vegetation_Group) %>% 
  mutate(col = shade_seq(vg_colours[Vegetation_Group[1]], n())[row_number()]) %>% 
  ungroup() %>% 
  dplyr::select(VegGroup_NVC, col) %>% 
  tibble::deframe()          # named vector:  c("Blanket Bog - M1" = "#ffb6c1", …)


clipped_rum <- clipped_rum %>% 
  mutate(VegGroup_NVC = paste(Vegetation_Group, NVC0, sep = " - "))

#plot
library(ggplot2)

study_area_NVC_plot <- ggplot() +
  geom_sf(data = clipped_rum, aes(fill = VegGroup_NVC), colour = "grey2", size = 0.1) +
  scale_fill_manual(values = nvc_palette, name = "Vegetation Group – NVC") +
  theme_minimal() +
  theme(legend.key.size = unit(1.25, "lines")) +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "study_area_NVC_plot.pdf",
  plot = study_area_NVC_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "study_area_NVC_plot.png",
  plot = study_area_NVC_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)

###### WHOLE ISLAND
rum <- rum %>% 
  mutate(VegGroup_NVC = paste(Vegetation_Group, NVC0, sep = " - "))

#plot
library(ggplot2)
nvc_palette
whole_island_NVC_plot <- ggplot() +
  geom_sf(data = rum, aes(fill = VegGroup_NVC), colour = "grey2", size = 0.1) +
  scale_fill_manual(values = nvc_palette, name = "Vegetation Group – NVC") +
  theme_minimal() +
  theme(legend.key.size = unit(1, "lines")) +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )


ggsave(
  filename = "whole_island_NVC_plot.pdf",
  plot = whole_island_NVC_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "whole_island_NVC_plot.png",
  plot = whole_island_NVC_plot,
  path = "C:/Users/s2460885/OneDrive - University of Edinburgh/University of Edinburgh PhD/My Papers/Paper Plots",
  width = 7,
  height = 5,
  dpi = 300
)
