setwd("/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 1 - Dataset")

library("tmap")
library("sf")

# clears all objects in RStudio's memory
rm(list = ls())
gc()

# while we have covered how to merge data - this is a refresher
Fire_data <- read.csv(file="London_LSOA_FireHazards_2019.csv", header = TRUE, sep = ",")


# Add shapefiles for LSOA and Boroughs
london_lsoa_shapefile <- read_sf("London_LSOA_areas.shp")
london_borough_shapefile <- read_sf("London_Boroughs_Shapefile.shp")
Spatial_data <- merge(london_lsoa_shapefile, Fire_data, by.x = 'LSOA_code', by.y = 'LSOA_code', all.x = TRUE)

# Solutions to homework start here:

# Step 1: Calculate the incidence rate. 
# This requires creating a new variable in the `Spatial_data` data frame.
# Let call the new variable 'IncidenceFires'. 
# We are creating the 'IncidenceFires' column based on 'Fires2019' and 'TotalHouses2019'
# Its a rate per capita so we will divide the two and multiple it by 1000

Spatial_data$IncidenceFires <- (Spatial_data$Fires2019/Spatial_data$TotalHouses2019)*1000

# we can create a map which shows the burden of fires with a continuous scale

# Step 2: Generate a incidence map 'with continuous scale' according to the criteria specified in the hints

tm_shape(Spatial_data) + 
	tm_polygons("IncidenceFires",
		fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1.5),
		fill.legend = tm_legend(title = "Fires per 1,000", frame = FALSE),
		fill_alpha = 1,
		col_alpha = 0.5,
		col = "white",
		lwd = 0.1) +
	tm_shape(london_borough_shapefile) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("ctyua16nm", size = "AREA", fontface = "bold") +
	tm_compass(type = "arrow", position = c("right", "top")) + 
	tm_scalebar(position = c("left", "bottom"))


tmap_mode("view")

map1 <-tm_shape(Spatial_data) + 
		tm_polygons("IncidenceFires",
			fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1.5),
			fill.legend = tm_legend(title = "Fires per 1,000", frame = FALSE),
			fill_alpha = 1,
			col_alpha = 0.5,
			col = "white",
			lwd = 0.1) +
		tm_shape(london_borough_shapefile) + 
		tm_polygons(fill_alpha = 0, col = "black") +
		tm_text("ctyua16nm", fontface = "bold") +
		tm_compass(type = "arrow", position = c("right", "top")) + 
		tm_scalebar(position = c("left", "bottom"))


install.packages("mapview")
library("mapview")
mapview(map1)