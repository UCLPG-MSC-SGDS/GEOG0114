
setwd("~/Documents/Websites/GEOG0114/all_datasets/Week 1 - Dataset")

install.packages("tmap")

# Import data using read.csv() function 
Deprivation_data <- read.csv(file="London_LSOA_Deprivation_2019.csv", header = TRUE, sep = ",")
# Import data using read.csv() function 
Fire_data <- read.csv(file="London_LSOA_FireHazards_2019.csv", header = TRUE, sep = ",")


# Using the merge() function 
Full_data <- merge(Fire_data, Deprivation_data, by.x = "LSOA_code", by.y = "LSOA_code", all.x = TRUE)

# View the datasets
View(Full_data)

# Active the sf and tmap packages
library("sf")
library("tmap")

# Add shapefiles for LSOA and Boroughs
london_lsoa_shapefile <- read_sf("London_LSOA_areas.shp")
london_borough_shapefile <- read_sf("London_Boroughs_Shapefile.shp")

# Using the merge() function 
Spatial_data <- merge(london_lsoa_shapefile, Full_data, by.x = "LSOA_code", by.y = "LSOA_code", all.x = TRUE)

# View the datasets
View(Spatial_data)

# 
tm_shape(Spatial_data) + tm_polygons()

# Insert the “Spatial_data” object into the command line of 
# tm_shape(). No customisation has been applied here.

# Controlling transparencies for borders and areas
# old tmap (version 3.3)
tm_shape(Spatial_data) + tm_polygons(alpha = 0.1, border.alpha = 0.4)
# new tmap (> version 4.0)
tm_shape(Spatial_data) + tm_polygons(fill_alpha = 0.1, col_alpha = 0.4)

tm_shape(Spatial_data) + 
	tm_polygons(fill_alpha = 0.1, col_alpha = 0.4) +
tm_shape(london_borough_shapefile) +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black")

tm_shape(Spatial_data) +
	tm_polygons(fill = "IMD_Decile", fill.scale = tm_scale_categorical(values = "brewer.rd_yl_gn"),
		fill.legend = tm_legend(title = "Deprivation (Deciles)", frame = FALSE, item.space = 0),
		fill_alpha = 1, 
		col_alpha = 0.5, 
		col = "black", 
		lwd = 0.1) +
	tm_shape(london_borough_shapefile) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_compass(type = "arrow", position = c("right", "top")) + 
	tm_scalebar(position = c("left", "bottom"))
	