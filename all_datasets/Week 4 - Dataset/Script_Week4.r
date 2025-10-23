
setwd('/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 4 - Dataset')

# Load the packages with library()
library("sf")
library("tmap")
library("raster")
library("sp")
library("spatialEco")
library("BAMMtools")

# import rasters
temp <- raster("Kenya Mean Teperature.tif")
nvdi <- raster("Kenya NDVI.tif")
prec <- raster("Kenya Precipitation.tif")
popl <- raster("Kenya Population Density.tif")
elev <- raster("Kenya Elevation.tif")
arid <- raster("Kenya Aridity Index.tif")

# import shapefiles

# load the shapefiles
kenya_border <- st_read("Kenya_Border_3857.shp")
kenya_states <- st_read("Kenya_States_3857.shp")

# for instance temp and aridity
temp
arid

# For instance take the mean temperature for Kenya
# Spectral colours are useful for diverging scales "-brewer.spectral" is Rd-Or-Yl-Gr-Bu. "-brewer.spectral" reverses the order
tm_shape(temp) + 
	tm_raster(col.scale = tm_scale_continuous(values = "-brewer.spectral"),
		col.legend = tm_legend(title = "Mean Temperature", frame = FALSE)) +
tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
tm_layout(frame = FALSE)


# reclassify temperature as a binary or Boolean layer
temp
# lowest value = 1.2
# highest value = 29.6
# reclassify anything below 15 as 0, and the rest above 15 as 1

# reclassify the values into two groups 
# all values > 0 and <= 15 change to 0
# all values > 15 and <= 30 change to 1
temp_cl <- c(
	-Inf, 15, 0, 
	15, Inf, 1
	)

# convert into a matrix format
temp_cl_mat <- matrix(temp_cl, ncol = 3, byrow = TRUE)
# see matrix
temp_cl_mat
# apply matrix to reclassify() function to categorize the raster accordingly
temp_recl <- reclassify(temp, temp_cl_mat)

tm_shape(temp_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", "#F1948A"), labels = c("Unsuitable (<15.0)", "Suitable (15 & above)")),
		col.legend = tm_legend(title = "Temperature", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

# reclassify elevation as a binary or Boolean layer
elev
# lowest value = -11m (below sea level)
# highest value = 4865m (above sea level)
# reclassify anything below 1200m as 1, and the rest above 1200m as 0

# all values > -12 and <= 1199.999 change to 0
# all values > 1199.999 and <= 4900 change to 1
elev_cl <- c(
	-Inf, 1200, 1, 
	1200, Inf, 0
	)

# convert into a matrix format
elev_cl_mat <- matrix(elev_cl, ncol = 3, byrow = TRUE) 
# see matrix
elev_cl_mat
# apply matrix to reclassify() function to categorize the raster accordingly
elev_recl <- reclassify(elev, elev_cl_mat)

tm_shape(elev_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", "orange"), labels = c("Unsuitable (>1200m)", "Suitable (1200m & below)")),
		col.legend = tm_legend(title = "Elevation (m)", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

##### inside that code
# nvdi
nvdi

nvdi_cl <- c(
	-Inf, 0.5, 0, 
	0.5, Inf, 1
)

nvdi_cl_mat <- matrix(nvdi_cl, ncol = 3, byrow = TRUE)
nvdi_cl_mat
nvdi_recl <- reclassify(nvdi, nvdi_cl_mat)

tm_shape(nvdi_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", "green"), labels = c("Unsuitable (0.5 & Below)", "Suitable (> 0.5)")),
		col.legend = tm_legend(title = "NDVI (Vegetation)", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

# prec
prec

prec_cl <- c(
	-Inf, 350, 0, 
	350, Inf, 1
)

prec_cl_mat <- matrix(prec_cl, ncol = 3, byrow = TRUE) 
prec_cl_mat

prec_recl <- reclassify(prec, prec_cl_mat)

tm_shape(prec_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", "skyblue"), labels = c("Unsuitable (350mm & Below)", "Suitable (> 350mm)")),
		col.legend = tm_legend(title = "Precipitation (mm)", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

# popl
popl

popl_cl <- c(
	-Inf, 0, 0, 
	0, Inf, 1
)

popl_cl_mat <- matrix(popl_cl, ncol = 3, byrow = TRUE)
popl_cl_mat
popl_recl <- reclassify(popl , popl_cl_mat)

tm_shape(popl_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", "orange"), labels = c("Unsuitable (0 people)", "Suitable (at least 1 person)")),
		col.legend = tm_legend(title = "Population density", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

# arid
arid

arid_cl <- c(
	-Inf, 0.20, 0, 
	0.20, Inf, 1
)

arid_cl_mat <- matrix(arid_cl, ncol = 3, byrow = TRUE)
arid_cl_mat
arid_recl <- reclassify(arid, arid_cl_mat)

tm_shape(arid_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("grey", ""), labels = c("Unsuitable (0.2 & below)", "Suitable (> 0.2)")),
		col.legend = tm_legend(title = "Aridity (Dryness)", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE)

####

Suitable_LF_Binary <- temp_recl*nvdi_recl*prec_recl*elev_recl*popl_recl*arid_recl

tm_shape(Suitable_LF_Binary) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("#f0f0f0", "red"), labels = c("Zone: Not Suitable", "Zone: Highly Suitable")),
		col.legend = tm_legend(title = "Suitability Map (Binary)", frame = FALSE)) +
tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("NAME_1", size = "AREA") +
tm_layout(frame = FALSE)

# summation
RasterStack <- stack(temp_recl, nvdi_recl, prec_recl, elev_recl, popl_recl, arid_recl)
Suitable_LF_Summed <- calc(RasterStack, sum)
# check for minimum and maximum
Suitable_LF_Summed@data@min
Suitable_LF_Summed@data@max
# minimum = 2
# maximum = 6

tm_shape(Suitable_LF_Summed) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("#fff5f0", "#FADBD8", "#F5B7B1", "#F1948A", "#E74C3C"), 
		labels=c("Low (2)", "Modest (3)", "Medium (4)", "High (5)", "Highest (6)")),
		col.legend = tm_legend(title = "Suitability Map (Summation)", frame = FALSE)) +
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("NAME_1", size = "AREA") +
	tm_layout(frame = FALSE)


# AHP

temp_standardised_10 <- ((temp - temp@data@min)/(temp@data@max-temp@data@min))*10
plot(temp_standardised_10)

# repeat
prec_standardised_10 <- ((prec - prec@data@min)/(prec@data@max-prec@data@min))*10
plot(prec_standardised_10)

popl_standardised_10 <- ((popl - popl@data@min)/(popl@data@max-popl@data@min))*10
plot(popl_standardised_10)

nvdi_standardised_10 <- ((nvdi - nvdi@data@min)/(nvdi@data@max-nvdi@data@min))*10
plot(nvdi_standardised_10)

# Now flip the values by subtracting the values from the max value (here its 10)
plot(elev)
elev_standardised_10 <- ((elev - elev@data@min)/(elev@data@max-elev@data@min))*10
plot(elev_standardised_10)

flipped_elev_standardised_10 <- elev_standardised_10
values(flipped_elev_standardised_10) <- elev_standardised_10@data@max - values(elev_standardised_10)
plot(flipped_elev_standardised_10)

# use the rescaled columns in the formula
suitablemap_WLC <- (0.372*prec_jenks_recl + 0.356*temp_jenks_recl + 0.159*popl_jenks_recl + 0.077*rev_elev_jenks_recl + 0.037*nvdi_jenks_recl) * arid_recl 

suitablemap_WLC <- 0.372*prec_standardised_10 + 0.356*temp_standardised_10 + 0.159*popl_standardised_10 + 0.077*flipped_elev_standardised_10 + 0.037 * nvdi_standardised_10
suitablemap_WLC
# see plot
plot(suitablemap_WLC)

#| Component                      | Max Contribution (approx) |
#| ------------------------------ | ------------------------- |
#| 0.372 × 10 (precipitation)     | 3.72                      |
#| 0.356 × 10 (temperature)       | 3.56                      |
#| 0.159 × 10 (population)        | 1.59                      |
#| 0.077 × 10 (flipped elevation) | 0.77                      |
#| 0.037 × 10 (NDVI)              | 0.37                      |
#| **Total Theoretical Max**      | **10.01**                 |
	
# Since your observed max ≈ 6.786, that’s perfectly logical — no pixel achieved the maximum 10 across all factors simultaneously, 
# and some may have been reduced by the constraint layer.
# So, 6.786 = highest achievable weighted suitability given your dataset.

# re-standardised weighted suitability map, constrained by aridity layer
SWLC_standardised_10 <- ((suitablemap_WLC - suitablemap_WLC@data@min)/(suitablemap_WLC@data@max-suitablemap_WLC@data@min))*10
plot(SWLC_standardised_10)

#| Range       | Category  |
#| ----------- | --------- |
#| 0.00 – 0.20 | Very Low  |
#| 0.20 – 0.40 | Low       |
#| 0.40 – 0.60 | Moderate  |
#| 0.60 – 0.80 | High      |
#| 0.80 – 1.00 | Very High |
	

SWLC_cl <- c(
	-Inf,0,0,
  0,2,1,
	2,4,2, 
	4,6,3, 
	6,8,4, 
	8,10,5
	)

SWLC_cl_mat <- matrix(SWLC_cl, ncol = 3, byrow = TRUE)
SWLC_cl_mat
SWLC_recl <- reclassify(SWLC_standardised_10, SWLC_cl_mat)

table(SWLC_recl@data@values)

Add the constraint layer
# to filter only the values == 1
arid_recl[arid_recl[] !=0 ] = NA

# code for producing the final output
tm_shape(SWLC_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("white", "#74add1", "#e0f3f8", "#ffffbf", "#fee08b", "#f46d43"), 
		labels = c("Not Suitable (0)", "Very Low (1)", "Low (2)", "Moderate (3)", "High (4)", "Very High (5)")),
		col.legend = tm_legend(title = "Suitability Map (MCDA-AHP)", frame = FALSE)) +
	tm_shape(arid_recl) + 
	tm_raster(col.scale = tm_scale_categorical(values = "white"), 
		col.legend = tm_legend(show = FALSE)) + 
	tm_shape(kenya_states) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("NAME_1") +
	tm_layout(frame = FALSE)
		

# to filter only the values == 1
arid_recl[arid_recl[] !=0 ] = NA