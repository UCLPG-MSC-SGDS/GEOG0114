

setwd("/Volumes/Anwar-HHD/GEOG0114/Github/2022_2023/datasets/week-1")

Fire_data <- read.csv(file="London_LSOA_FireHazards_2019.csv", header = TRUE, sep = ",")
Deprivation_data <- read.csv(file="London_LSOA_Deprivation_2019.csv", header = TRUE, sep = ",")
Full_data <- merge(Fire_data, Deprivation_data, by.x = 'LSOA_code', by.y = 'LSOA_code', all.x = TRUE)

library("sf")
library("tmap")

# Add shapefiles for LSOA and Boroughs
london_lsoa_shapefile <- read_sf("London_LSOA_areas.shp")
london_borough_shapefile <- read_sf("London_Boroughs_Shapefile.shp")
Spatial_data <- merge(london_lsoa_shapefile, Full_data, by.x = 'LSOA_code', by.y = 'LSOA_code', all.x = TRUE)

# Solutions to homework start here:

# Step 1: Calculate the incidence rate. 
# This requires creating a new variable in the `Spatial_data` data frame.
# Let call the new variable 'IncidenceFires'. 
# We are creating the 'IncidenceFires' column based on 'Fires2019' and 'TotalHouses2019'
# Its a rate per capita so we will divide the two and multiple it by 1000

Spatial_data$IncidenceFires <- (Spatial_data$Fires2019/Spatial_data$TotalHouses2019)*1000

# Step 2: Generate a incidence map according to the criteria specified in the hints

tm_shape(Spatial_data) + 
	tm_fill("IncidenceFires", style = "cont", title = "Incidence per 1000", 
		palette = "Reds") +
	tm_shape(london_borough_shapefile) + 	tm_text("ctyua16nm", size = "AREA") +
	tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
	tm_scale_bar(position = c("left", "bottom")) +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_layout(legend.outside = TRUE, legend.title.size = 0.8,  
		legend.text.size = 0.7, 
		legend.title.fontface = 2)

