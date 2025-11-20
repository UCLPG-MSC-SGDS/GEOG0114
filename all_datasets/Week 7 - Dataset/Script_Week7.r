

setwd("/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 7 - Dataset")

library("sf")
library("tmap")
library("spdep")
library("sp")
library("spatialreg")

datafile <- read.csv(file = "London LSOA 2015 data.csv", header = TRUE, sep = ",")

LSOAshp <- read_sf("London LSOA Areas.shp")
BOROUGHshp <- read_sf("London Borough Areas.shp")

tm_shape(LSOAshp) + 
	tm_polygons() +
tm_shape(BOROUGHshp) + 
	tm_polygons(fill_alpha = 0, col = "black") +
tm_compass(position = c("right", "top")) + 
tm_scalebar(position = c("left", "bottom"))

# descriptive statistics
summary(datafile$AVEPRICE)
sd(datafile$AVEPRICE)

# Merge datafile to LSOAshp uniquely by using "LSOACODE column
spatialdatafile <- merge(LSOAshp, datafile, by.x = "LSOACODE", by.y = "LSOACODE")

# modern code
plot1 <- tm_shape(spatialdatafile) +
	tm_polygons(fill = "AVEPRICE", 
		fill.scale = tm_scale_intervals(n=7, style = "quantile", values = "brewer.greens"),
		fill.legend = tm_legend(frame = FALSE)) +
tm_shape(BOROUGHshp) +
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
tm_compass(position = c("right", "top")) +
tm_scalebar(position = c("left", "bottom")) +
tm_layout(frame = FALSE)

# plot the image object
plot1

plot2 <- tm_shape(spatialdatafile) +
	tm_polygons(fill = "AVEINCOME", 
		fill.scale = tm_scale_intervals(n=7, style = "quantile", values = "brewer.oranges"),
		fill.legend = tm_legend(frame = FALSE)) +
	tm_shape(BOROUGHshp) +
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)

plot3 <- tm_shape(spatialdatafile) +
	tm_polygons(fill = "IMDSCORE", 
		fill.scale = tm_scale_intervals(n=7, style = "quantile", values = "brewer.reds"),
		fill.legend = tm_legend(frame = FALSE)) +
	tm_shape(BOROUGHshp) +
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)

plot4 <- tm_shape(spatialdatafile) +
	tm_polygons(fill = "PTACAT", 
		fill.scale = tm_scale_categorical(values = "brewer.blues"),
		fill.legend = tm_legend(frame = FALSE)) +
	tm_shape(BOROUGHshp) +
	tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)

tmap_arrange(plot1, plot2, plot3, plot4, nrow = 2)

# lm() function builds a regression model and stores model output into the object 'modelMLR'
modelMLR <- lm(log10(AVEPRICE) ~ log10(AVEINCOME) + log10(IMDSCORE) + log10(PTAINDEX), data = spatialdatafile)
# Include the 'scipen=7' argument in the summary() function remove those annoying scientific notation!
options(scipen = 7)
# summary() calls report the output stored in object 'modelMLR'
summary(modelMLR)

# Extract residuals from "modelLMR" object and dump into "spatialdatafile" and call the column "RESIDUALS"
spatialdatafile$RESIDUALS <- modelMLR$residuals

# Reporting basic summary measures to have an idea of its distribution before plotting them on map
summary(spatialdatafile$RESIDUALS)

# plot the residuals
tm_shape(spatialdatafile) + 
	tm_polygons(fill = "RESIDUALS", 
		fill.scale = tm_scale_continuous(midpoint = 0, values = "-brewer.rd_bu"),
		fill.legend = tm_legend(frame = FALSE)) +
tm_shape(BOROUGHshp) + tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)

#generate unique number for each row
spatialdatafile$ROWNUM <- 1:nrow(spatialdatafile)
# We need to coerce the sf spatialdatafile object into a new sp object
spatialdatafile_2.0 <- as(spatialdatafile, "Spatial")
# Create spatial weights matrix for areas
Weights <- poly2nb(spatialdatafile_2.0, row.names = spatialdatafile_2.0$ROWNUM)
WeightsMatrix <- nb2mat(Weights, style='B')
Residual_WeightMatrix <- mat2listw(WeightsMatrix , style='W')
# Run the test on the regression model output object "modelMLR" using lm.morantest()
lm.morantest(modelMLR, Residual_WeightMatrix, alternative="two.sided")


modelSLY <- lagsarlm(log10(AVEPRICE) ~ log10(IMDSCORE) + log10(AVEINCOME) + log10(PTAINDEX), data = spatialdatafile_2.0, Residual_WeightMatrix)

# Report results with summary()
# We are interested in the rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(modelSLY)

# extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
spatialdatafile$RESID_SLY <- modelSLY$residuals
# use Moran's I test using moran.mc() function
moran.mc(spatialdatafile$RESID_SLY, Residual_WeightMatrix, 1000, zero.policy = T)

# generate the map
tm_shape(spatialdatafile) + 
	tm_polygons(fill = "RESID_SLY", 
		fill.scale = tm_scale_continuous(midpoint = 0, values = "-brewer.rd_bu"),
		fill.legend = tm_legend(frame = FALSE)) +
	tm_shape(BOROUGHshp) + tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)

# Interpretation of results using impacts
# impacts
Weights_2.0 <- as(Residual_WeightMatrix, "CsparseMatrix")
trMC <- trW(Weights_2.0, type="MC")
summary(impacts(modelSLY, tr = trMC, R=100), zstats=TRUE)

# spatial error model
modelSER <- errorsarlm(log10(AVEPRICE) ~ log10(IMDSCORE) + log10(AVEINCOME) + log10(PTAINDEX), data = spatialdatafile_2.0, Residual_WeightMatrix)

# Report results with summary()
# We are interested in the rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(modelSER)

# extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
spatialdatafile$RESID_SER <- modelSER$residuals
# use Moran's I test using moran.mc() function
moran.mc(spatialdatafile$RESID_SER, Residual_WeightMatrix, 1000, zero.policy = T)

# generate the map
tm_shape(spatialdatafile) + 
	tm_polygons(fill = "RESID_SER", 
		fill.scale = tm_scale_continuous(midpoint = 0, values = "-brewer.rd_bu"),
		fill.legend = tm_legend(frame = FALSE)) +
	tm_shape(BOROUGHshp) + tm_polygons(fill_alpha = 0, col = "black") +
	tm_text("BOROUGHN", size = "AREA") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom")) +
	tm_layout(frame = FALSE)