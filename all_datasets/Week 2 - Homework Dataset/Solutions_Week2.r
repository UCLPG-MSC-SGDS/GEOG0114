
# Set the working directory to where the disaster dataset is:

setwd("~/Documents/Websites/GEOG0114/all_datasets/Week 2 - Homework Dataset")

# load required packages:

library("units")
library("dplyr")
library("tidyverse")
library("sf")
library("tmap")

# Task one

# load datasets for task one and two
all_earthquakes <- st_read("All_earthquake_points_in_Turkey.shp")
turkey_adm_areas <- st_read("gadm41_TUR_1.shp")

# first visualise to see what the data looks like
tm_shape(all_earthquakes) + 
	tm_dots() +
tm_shape(turkey_adm_areas) + 
	tm_polygons(fill_alpha = 0)

# as you can see - there are points outside the study area. Let us clip/subset them
# create a outline of the region by calculating the areas before dissolving it into one
# big outline
turkey_adm_areas$area_est <- st_area(turkey_adm_areas)

# Now, dissolve the areas to outline
turkey_outline <- turkey_adm_areas %>% summarise(area = sum(area_est))

# Now, subset or cookie-cut the earthquake points that are only within Turkey
turkey_earthquakes <- all_earthquakes[turkey_outline,]

# now visualise
tm_shape(turkey_outline) + 
	tm_polygons(fill_alpha = 0) +
tm_shape(turkey_earthquakes) + 
	tm_dots()

# Task two
# Using the derived dataset i.e., turkey_earthquakes
# we are aggregating the points in those administrative areas i.e., turkey_adm_areas
turkey_adm_areas$total_earthquakes <- lengths(st_intersects(turkey_adm_areas, turkey_earthquakes))
# if you check the data frame # you can see a new column has been created with total counts of earthquakes per region. K. Maras 
# which has the highest burden of 140 earthquake occurrences.
View(turkey_adm_areas)

# Let us visualise this result as a map:
tm_shape(turkey_adm_areas) +
	tm_polygons(fill = "total_earthquakes", 
		fill.scale = tm_scale_intervals(n=6, style = "pretty", values = "-brewer.rd_bu"),
		fill.legend = tm_legend(title = "Total Earthquakes", frame = FALSE)) +
	tm_compass(type = "arrow", position = c("right", "top")) + 
	tm_scalebar(position = c("right", "bottom")) +
	tm_layout(frame = FALSE)

# Task three

#NOTE: THIS PROCESS IS QUITE INVOLVED

# load datasets for task three
focal_earthquake_point <- st_read("Earthquake_point_in_Konya.shp")
konya_buildings <- st_read("Konya_buildings_at_risk.shp")

# It is good practice to visual the dataset at face-value to have an understanding of it:

tm_shape(konya_buildings) + 
	tm_polygons(fill_alpha = 0) +
tm_shape(focal_earthquake_point) + 
	# increased the size of the single focal point to 0.5 and coloured it red to standout more
	tm_dots(size = 0.5, fill = "red", col = "red") +
tm_compass(type = "arrow", position = c("right", "top")) + 
tm_scalebar(position = c("right", "bottom")) +
tm_layout(frame = FALSE)

# Both shapefiles have CRS of 4326 (WGS 84) which are in decimal degrees. This is not a good system
# especially if you need to calculate a distance needed for creating a buffer zone!

# Let us use the World Mecartor 3857 CRS as its in meters. So let's change the CRS to 3857 
# for the shapefiles to be in meters. This makes specifying the distance a lot easier: 

konya_buildings_3857 <- st_transform(konya_buildings, 3857)
focal_earthquake_point_3857 <- st_transform(focal_earthquake_point, 3857)

# Let create the three buffer zones around the focal earthquake point: 

# create buffer for 2000m
destroyed_buffer_2000m <- st_buffer(focal_earthquake_point_3857, dist = 2000)
# second and third buffers are created for 0-5000 and 0-9000
sec_buffer_5000m <- st_buffer(focal_earthquake_point_3857, dist = 5000)
thr_buffer_9000m <- st_buffer(focal_earthquake_point_3857, dist = 9000)

# Now, we need to calculate the number of building that exist within 2000m, within 2000-5000m
# and from 5000-9000m. Tricky ones are the zones for 2000-5000m and 5000-9000m as they are 
# rings. We need to create 'ringed' zones from the buffers through taking the differences between
# the different buffer zones.

# We can use st_difference() to create the rings for buffer zones 2000-5000 and 5000-9000

# take the second buffer zone 5000m and substract the inner 2000m buffer from it to retain 2000-5000m
severe_damaged_buffer_2000_5000m <- st_difference(sec_buffer_5000m, destroyed_buffer_2000m)
# take the third buffer zone 9000m and substract the inner 5000m buffer from it to retain 5000-9000m
partial_damaged_buffer_5000_9000m <- st_difference(thr_buffer_9000m, sec_buffer_5000m)

# note don't worry about the warning message - that's R behaving badly!

# visual the current output:

# add buildings
tm_shape(konya_buildings_3857) + 
	tm_polygons(fill_alpha = 0) +
# add the single focal point of the earthquake
tm_shape(focal_earthquake_point_3857) + 
	tm_dots(size = 0.5, fill = "red", col = "red") +
# add the thirdmost outer ring (5000-9000m) and make fill colour pink, and line as dashes in pink colour
tm_shape(partial_damaged_buffer_5000_9000m) + 
	tm_polygons(fill = "pink", fill_alpha = 0.1, col = "pink", lwd = 1.1, lty="dashed") +
# add the second middle ring (2000-5000m)
tm_shape(severe_damaged_buffer_2000_5000m) + 
	tm_polygons(fill = "red", fill_alpha = 0.2, col = "red", lwd = 1.1, lty="dashed") +
# add the 1st ring as solid red polygon with transparency of 0.4
tm_shape(destroyed_buffer_2000m) + 
	tm_polygons(fill = "red", fill_alpha = 0.5, col = "red", lwd = 1.5) +
tm_compass(type = "arrow", position = c("right", "top")) + 
tm_scalebar(position = c("right", "bottom")) +
tm_layout(frame = FALSE)

# To get total number of buildings within each ringed region, we will need to first get the centroids of each building,
# and then count the number of points that intersect with each zone.

# You can do that by using the st_centroid() function:
konya_buildings_points <- konya_buildings_3857 %>% st_centroid()

# Now, calculate the number of buildings for the following:

# calculate the number of buildings within 2000m from the focal point
destroyed_buffer_2000m$total_buildings <- lengths(st_intersects(destroyed_buffer_2000m, konya_buildings_points))
# calculate the number of buildings within 2000-5000m from the focal point
severe_damaged_buffer_2000_5000m$total_buildings <- lengths(st_intersects(severe_damaged_buffer_2000_5000m, konya_buildings_points))
# calculate the number of buildings within 5000-9000m from the focal point
partial_damaged_buffer_5000_9000m$total_buildings <- lengths(st_intersects(partial_damaged_buffer_5000_9000m, konya_buildings_points))

# note, viewing inside these data frame - you should see total number of buildings for each buffer. The total number of
# buildings in that regions is 28,765. We can calculate prevalence (or proportion) of buildings by disaster outcomes i.e., 
# destroyed, severely damage, or partially damaged by the earthquake:

# Prevalence of buildings that sustain partial damage (5000-9000m from focal point) = 10734/28765 = 37.31%
# Prevalence of buildings that were severely damaged (2000-5000m from focal point) = 3180/28765 = 11.05%
# Prevalence of buildings that were completely destroyed (<2000m from focal point) = 294/28765 = 1.02%
# Prevalence of buildings that survival (>9000m from focal point) = 50.62%

# Let's put all the information together
# add buildings
tm_shape(konya_buildings_3857) + 
	tm_polygons(fill_alpha = 0) +
# add the single focal point of the earthquake
tm_shape(focal_earthquake_point_3857) + 
	tm_dots(size = 0.5, fill = "red", col = "red") +
# add the thirdmost outer ring (5000-9000m) and make fill colour pink, and line as dashes in pink colour
tm_shape(partial_damaged_buffer_5000_9000m) + 
	tm_polygons(fill = "pink", fill_alpha = 0.1, col = "pink", lwd = 1.1, lty="dashed") +
	# add the second middle ring (2000-5000m)
tm_shape(severe_damaged_buffer_2000_5000m) + 
	tm_polygons(fill = "red", fill_alpha = 0.2, col = "red", lwd = 1.1, lty="dashed") +
# add the 1st ring as solid red polygon with transparency of 0.4
tm_shape(destroyed_buffer_2000m) + 
	tm_polygons(fill = "red", fill_alpha = 0.5, col = "red", lwd = 1.5) +
tm_compass(type = "arrow", position = c("right", "top")) + 
tm_scalebar(position = c("right", "bottom")) +
tm_layout(frame = FALSE) +
tm_credits("Prevalence [%]\nPartial: 37.31%\nSevere: 11.05%\nDestroyed: 1.02%\nSurvived: 50.62%", position = c("left", "top"), size = 0.8)
