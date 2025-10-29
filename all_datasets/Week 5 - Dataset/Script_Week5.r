rm(list = ls())

# Load the packages with library():
library("raster")
library("dismo")
library("tmap")
library("sf")
library("rJava")
library("spdep")
library("terra")
library("exactextractr")

setwd("/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 5 - Dataset")

# load data raster data
temp <- raster("California_temperature_summer_2018.tif")
prec <- raster("California_precipitation_summer_2018.tif")
dryn <- raster("California_dryness_summer_2018.tif")
ndvi <- raster("California_vegetation_summer_2018.tif")
elev <- raster("California_elevation_summer_2018.tif")
sevi <- raster("California_socdeprivation_summer_2018.tif")

temp

# load shapefile data for california
california_border <- read_sf("California_boundary.shp")
california_county <- read_sf("California_county.shp")

# load occurrence fire data in California
california_fires <- read.csv("California_Fire_Summer_2018.csv")

# code chunk format coordinates(data.frame) = ~x+y
# coordinates(california_fires) = ~longitude+latitude
# crs(california_fires) <- "+proj=longlat +datum=WGS84 +no_defs"

# Convert to sf object
california_fires <- st_as_sf(california_fires, coords = c("longitude", "latitude"), crs = 4326)

# map of fire points
tm_shape(california_county) + 
	tm_polygons() + 
tm_shape(california_fires) + 
	tm_dots(fill = "red", col = "black", size = 0.1)


m1 = tm_shape(temp) + 
	tm_raster(col.scale = tm_scale_continuous(values = "-brewer.spectral"),
		col.legend = tm_legend(title = "Mean Temperature", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) + 
	tm_title("A", position = c("left", "bottom"), size = 5)

m2 = tm_shape(prec) + 
	tm_raster(col.scale = tm_scale_continuous(values = "brewer.blues"),
		col.legend = tm_legend(title = "mm", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) + 
	tm_title("B", position = c("left", "bottom"), size = 5)

m3 = tm_shape(dryn) + 
	tm_raster(col.scale = tm_scale_continuous(values = "brewer.spectral"),
		col.legend = tm_legend(title = "mm/0.25yr", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) +
	tm_title("C", position = c("left", "bottom"), size = 5)

m4 = tm_shape(ndvi) + 
	tm_raster(col.scale = tm_scale_continuous(values = "brewer.greens"),
		col.legend = tm_legend(title = "Index", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) +
	tm_title("D", position = c("left", "bottom"), size = 5)

m5 = tm_shape(elev) + 
	tm_raster(col.scale = tm_scale_continuous(values = "brewer.spectral", midpoint = 1500),
		col.legend = tm_legend(title = "Meters", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) +
	tm_title("E", position = c("left", "bottom"), size = 5)

m6 = tm_shape(sevi) + 
	tm_raster(col.scale = tm_scale_continuous(values = "brewer.reds"),
		col.legend = tm_legend(title = "%", frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_layout(frame = FALSE) +
	tm_title("F", position = c("left", "bottom"), size = 5)

# stitch the maps together using tmap_arrange() function
tmap_options(component.autoscale = FALSE)
tmap_arrange(m1, m2, m3, m4, m5, m6, nrow = 2)

# data management part
envCovariates <- stack(temp, prec, dryn, ndvi, elev, sevi)
names(envCovariates) <- c("Temperature", "Precipitation", "Dryness", "NDVI", "Elevation", "Deprivation")

# set the seed
set.seed(20000430)
# we need to coerce 'sf' object california_border into 'sp' object for spsample to work
california_border_sp <- as(california_border, Class = "Spatial")
# here, spsample() generates twice number of fire occurrence points randomly within California's border
background_points <- spsample(california_border_sp, n=2*nrow(california_fires), "random")

# perform raster extraction from the environmental covariates on to points
california_fires_env <- extract(envCovariates, california_fires)
background_points_env <- extract(envCovariates, background_points)

# convert large matrix objects to data frame objects and add outcome `fire` indicator
california_fires_env <-data.frame(california_fires_env,fire=1)
background_points_env <-data.frame(background_points_env,fire=0)

# View one of the data frame
head(california_fires_env, n=5)
head(background_points_env, n=5)

# set same set.seed() as before
set.seed(20000430)
# using k-fold function to split data into 4 equal parts
select <- kfold(california_fires_env, 4)
# 25% of the fire data use for testing the model
california_fires_env_test <- california_fires_env[select==1,]
# 75% of the fire data use for training the model
california_fires_env_train <- california_fires_env[select!=1,]

# set same set.seed() as before
set.seed(20000430)
# repeat the process for the background points
select <- kfold(background_points_env, 4)
background_points_env_test <- background_points_env[select==1,]
background_points_env_train <- background_points_env[select!=1,]

training_data <- rbind(california_fires_env_train, background_points_env_train)
testing_data <- rbind(california_fires_env_test, background_points_env_test)

model_training <- maxent(x=training_data[,c(1:6)], p=training_data[,7], args=c("responsecurves"))

plot(model_training, pch=19, xlab = "Percentage [%]", cex=1.2)
response(model_training)

# model evaluation use the test data on the trained model for validation
cross_validation <- evaluate(p=testing_data[testing_data$fire==1,], a=testing_data[testing_data$fire==0,], model = model_training)
cross_validation 
plot(cross_validation, 'ROC', cex=1.2)

prob_wildfire <- predict(model_training, envCovariates)

# generate a publication-worthy figure
# map of probability 
tm_shape(prob_wildfire) +
	tm_raster(title = "Predicted probability", palette = '-RdYlBu', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
	tm_shape(california_county) + tm_polygons(alpha = 0, border.col = "black") +
	tm_layout(main.title = "Predicted Probability of Wild Fire [%]", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
		legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
	tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
	tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

tm_shape(prob_wildfire) + 
	tm_raster(col.scale = tm_scale_continuous(values = "-brewer.rd_yl_bu"),
		col.legend = tm_legend(title = expression(bold("Predicted Probability")), frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_scalebar(position=c("left", "bottom"), text.size = 1, breaks = c(0, 100, 200, 300)) +
	tm_layout(frame = FALSE)

# calculate thresholds of models
threshold_value <- threshold(cross_validation, "spec_sens")
# report value
threshold_value

# prepare threshold total map 
create_classes_vector <- c(0, threshold_value, 0, threshold_value, 1, 1)
create_clasess_matrix <- matrix(create_classes_vector, ncol = 3, byrow = TRUE)
create_clasess_matrix

# create new reclassify raster based on prob_wildfires
suitability_wildfires <- reclassify(prob_wildfire, create_clasess_matrix)

tm_shape(suitability_wildfires) + 
	tm_raster(col.scale = tm_scale_categorical(values = c("lightgrey", "red"), labels = c("Safe", "Trigger Points")),
		col.legend = tm_legend(title = expression(bold("Threshold")), frame = FALSE)) +
	tm_shape(california_county) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_scalebar(position=c("left", "bottom"), text.size = 1, breaks = c(0, 100, 200, 300)) +
	tm_layout(frame = FALSE)

rJava::.jcall("java/lang/System", "S", "getProperty", "java.version")
