

# set working directory
setwd("~/Documents/Websites/GEOG0114/all_datasets/Week 3 - Dataset")

install.packages("nngeo")
install.packages("spdep")
install.packages("sp")
install.packages("data.table")

# Load the packages with library()
library("tidyverse")
library("sf")
library("tmap")
library("nngeo")
library("spdep")
library("sp")
library("data.table")


# Step 1: import point and shapefile data

# load Camden boundaries
camden_oas <- st_read('OAs_camden_2011.shp', crs=27700)

# inspect
tm_shape(camden_oas) +
	tm_polygons()

# highlight E00004174
tm_shape(camden_oas) +
	tm_polygons(fill = "white", col= "black") +
	tm_shape(camden_oas[camden_oas$OA11CD=='E00004174',]) +
	tm_polygons(fill = "red")

# Euclidean distance

# assign our chosen OA to a variable 
chosen_oa <- 'E00004174'

# find 50 neighbours
# set maximum distance to 500m
# tells returns an adjacency matrix on which OA are neighbours

# identify neighbours
chosen_oa_neighbours <- st_nn(
	st_geometry(st_centroid(camden_oas[camden_oas$OA11CD==chosen_oa,])), 
	st_geometry(st_centroid(camden_oas)),
	sparse = TRUE,
	k = 50,
	maxdist = 500)

# churns out the rows of the neighbours of E00004174
neighbour_names <- camden_oas[chosen_oa_neighbours[[1]],]
neighbour_names <- neighbour_names$OA11CD

# inspect
# add base layer
tm_shape(camden_oas) + 
	tm_polygons(fill_alpha = 0, col = "black") +
# add neighbours layer to highlight only the neighbours of E00004174
tm_shape(camden_oas[camden_oas$OA11CD %in% neighbour_names,]) + 
	tm_polygons(fill = "green", col = "black") +
# add to show chosen OA E00004174
tm_shape(camden_oas[camden_oas$OA11CD==chosen_oa,]) + 
	tm_polygons(fill = "red", col = "black")


# Explore the contiguities

# for rook case
st_rook <- function(a, b = a) st_relate(a, b, pattern = 'F***1****')
# for queen case
st_queen <- function(a, b = a) st_relate(a, b, pattern = 'F***T****')

# identify neighbours based on ROOK
chosen_oa_neighbours_rook <- st_rook(st_geometry(camden_oas[camden_oas$OA11CD==chosen_oa,]), 
	st_geometry(camden_oas))

# get the names (codes) of these neighbours
neighbour_names_rk <- camden_oas[chosen_oa_neighbours_rook[[1]],]
neighbour_names_rk <- neighbour_names_rk$OA11CD

# inspect
# add base layer
tm_shape(camden_oas) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	# add neighbours layer to highlight only the neighbours of E00004174
	tm_shape(camden_oas[camden_oas$OA11CD %in% neighbour_names_rk,]) + 
	tm_polygons(fill = "green", col = "black") +
	# add to show chosen OA E00004174
	tm_shape(camden_oas[camden_oas$OA11CD==chosen_oa,]) + 
	tm_polygons(fill = "red", col = "black")


# identify neighbours based on Queen
chosen_oa_neighbours_queen <- st_queen(st_geometry(camden_oas[camden_oas$OA11CD==chosen_oa,]), 
	st_geometry(camden_oas))

# get the names (codes) of these neighbours
neighbour_names_qn <- camden_oas[chosen_oa_neighbours_queen[[1]],]
neighbour_names_qn <- neighbour_names_qn$OA11CD

# inspect
# add base layer
tm_shape(camden_oas) + 
	tm_polygons(fill_alpha = 0, col = "black") +
	# add neighbours layer to highlight only the neighbours of E00004174
	tm_shape(camden_oas[camden_oas$OA11CD %in% neighbour_names_qn,]) + 
	tm_polygons(fill = "green", col = "black") +
	# add to show chosen OA E00004174
	tm_shape(camden_oas[camden_oas$OA11CD==chosen_oa,]) + 
	tm_polygons(fill = "red", col = "black")


# Thefts
# load theft data
camden_theft <- read.csv('2019_camden_theft_from_person.csv')

# convert csv to sf object
camden_theft <- st_as_sf(camden_theft, coords = c('X','Y'), crs = 27700)

# inspect
tm_shape(camden_oas) +
	tm_polygons() +
	tm_shape(camden_theft) +
	tm_dots()

# look at the points and aggregate them
# thefts in Camden
camden_oas$n_thefts <- lengths(st_intersects(camden_oas, camden_theft))

# inspect
tm_shape(camden_oas) +
	tm_polygons(fill = "n_thefts", 
		fill.scale = tm_scale_intervals(breaks=c(0,1,50,100,150,200,250,300,350)),
		fill.legend = tm_legend(title ="Reported Thefts"))


camden_oas$sqrt_n_thefts <- sqrt(camden_oas$n_thefts)

tm_shape(camden_oas) +
	tm_polygons(fill = "sqrt_n_thefts", 
		fill.scale = tm_scale_intervals(breaks=c(0,1,5,10,15,20)),
		fill.legend = tm_legend(title ="Reported Thefts (Squared)"))


# global moran's I
class(camden_oas)

# STEP 1
# convert to sp
camden_oas_sp <- as_Spatial(camden_oas, IDs=camden_oas$OA11CD)
# inspect
class(camden_oas_sp)

# STEP 2
# create an nb object
camden_oas_nb <- poly2nb(camden_oas_sp, row.names=camden_oas_sp$OA11CD)
# inspect
class(camden_oas_nb)
# inspect
str(camden_oas_nb,list.len=10)

# STEP 3
# create the list weights object
nb_weights_list <- nb2listw(camden_oas_nb, style='W')
# inspect
class(nb_weights_list)

# n = number of regions
# nn = number of pairs
# s0 = sum of weights after standardisation
# s1 = measure of total connectivity strength (used in Moran's I test)
# s2 = higher order weight moment (used in computing p-values)

# Moran's I
mi_value <- moran(camden_oas_sp$n_thefts, 
	nb_weights_list,
	n=length(nb_weights_list$neighbours),
	S0=Szero(nb_weights_list)
	)

# inspect
mi_value

moran.test(camden_oas_sp$n_thefts, nb_weights_list)

# Local Moran's I
local_moran_camden_oa_theft <- localmoran(camden_oas_sp$n_thefts, nb_weights_list)

# rescaling
# rescale
camden_oas_sp$scale_n_thefts <- scale(camden_oas_sp$n_thefts)
# create a spatial lag variable 
camden_oas_sp$lag_scale_n_thefts <- lag.listw(nb_weights_list, camden_oas_sp$scale_n_thefts)
# convert to sf
camden_oas_moran_stats <- st_as_sf(camden_oas_sp)

# classification without significance value
camden_oas_moran_stats$quad_non_sig <- ifelse(camden_oas_moran_stats$scale_n_thefts > 0 & 
		camden_oas_moran_stats$lag_scale_n_thefts > 0, 
	'high-high', 
	ifelse(camden_oas_moran_stats$scale_n_thefts <= 0 & 
			camden_oas_moran_stats$lag_scale_n_thefts <= 0, 
		'low-low', 
		ifelse(camden_oas_moran_stats$scale_n_thefts > 0 & 
				camden_oas_moran_stats$lag_scale_n_thefts <= 0, 
			'high-low', 
			ifelse(camden_oas_moran_stats$scale_n_thefts <= 0 & 
					camden_oas_moran_stats$lag_scale_n_thefts > 0,
				'low-high',NA))))

# map all of the results here
tm_shape(camden_oas_moran_stats) +
	tm_polygons(fill = "quad_non_sig", fill.scale = tm_scale_categorical(values = c("#de2d26", "#fee0d2", "#deebf7", "#3182bd")))

# set a significance value
sig_level <- 0.1

# classification with significance value
camden_oas_moran_stats$quad_sig <- ifelse(camden_oas_moran_stats$scale_n_thefts > 0 & 
		camden_oas_moran_stats$lag_scale_n_thefts > 0 & 
		local_moran_camden_oa_theft[,5] <= sig_level, 
	'high-high', 
	ifelse(camden_oas_moran_stats$scale_n_thefts <= 0 & 
			camden_oas_moran_stats$lag_scale_n_thefts <= 0 & 
			local_moran_camden_oa_theft[,5] <= sig_level, 
		'low-low', 
		ifelse(camden_oas_moran_stats$scale_n_thefts > 0 & 
				camden_oas_moran_stats$lag_scale_n_thefts <= 0 & 
				local_moran_camden_oa_theft[,5] <= sig_level, 
			'high-low', 
			ifelse(camden_oas_moran_stats$scale_n_thefts <= 0 & 
					camden_oas_moran_stats$lag_scale_n_thefts > 0 & 
					local_moran_camden_oa_theft[,5] <= sig_level, 
				'low-high',
				ifelse(local_moran_camden_oa_theft[,5] > sig_level, 
					'not-significant', 
					'not-significant')))))


# map only the statistically significant results here
tm_shape(camden_oas_moran_stats) +
	tm_polygons(fill = "quad_sig", fill.scale = tm_scale_categorical(values = c("#de2d26", "#deebf7", "white")))