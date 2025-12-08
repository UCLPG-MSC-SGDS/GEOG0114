

# load the packages with library()
library("spgwr")
library("car")
library("sf")
library("tmap")
library("spdep")
library("sp")

setwd("~/Desktop/General Stuff/GEOG0114/Week 7 - Dataset")

# add house price and covariate data 
data.london <- read.csv("London LSOA 2015 data.csv")

# import shapefile data
lsoa.shp <- read_sf("London LSOA Areas.shp")
borough.shp <- read_sf("London Borough Areas.shp")

spatialdatafile <- merge(lsoa.shp, data.london, 
                         by.x = "LSOACODE", by.y = "LSOACODE")

# extraction of centroids to be used for computing the bandwidth
spatialdatafile <- st_centroid(spatialdatafile)
spatialdatafile <- cbind(spatialdatafile, st_coordinates(spatialdatafile))


BwG <- gwr.sel(
  log10(AVEPRICE) ~ log10(AVEINCOME) + log10(IMDSCORE) + log10(PTAINDEX),
  data = spatialdatafile, coords = cbind(spatialdatafile$X, spatialdatafile$Y),
  adapt = TRUE
  )


# start a timer
start.timer <- proc.time()
# gwr() - model function
gwr.model <- gwr(log10(AVEPRICE) ~ log10(AVEINCOME) + log10(IMDSCORE) + log10(PTAINDEX),
                 data = spatialdatafile,
                 coords = cbind(spatialdatafile$X, spatialdatafile$Y),
                 adapt=BwG,
                 hatmatrix = TRUE,
                 se.fit = TRUE)
# end the timer
end.timer <- proc.time() - start.timer
# report the time taken to compute this horrendous model
end.timer

gwr.data <- as.data.frame(gwr.model$SDF)
write.csv(gwr.data, file = "gwr output in practical.csv", row.names = FALSE)

gwr.model

lsoa_result <- st_drop_geometry(spatialdatafile[,c(1,2)])

lsoa_result$CoefLogInc <- gwr.data[,"log10.AVEINCOME."]
lsoa_result$CoefLogIMD <- gwr.data[,"log10.IMDSCORE."]
lsoa_result$CoefLogPTAL <- gwr.data[,"log10.PTAINDEX."]

lsoa_result$SELogInc <- gwr.data[,"log10.AVEINCOME._se"]
lsoa_result$SEfLogIMD <- gwr.data[,"log10.IMDSCORE._se"]
lsoa_result$SELogPTAL <- gwr.data[,"log10.PTAINDEX._se"]

lsoa_result$localR2 <- gwr.data[,"localR2"]

lsoa_result_gwr <- merge(lsoa.shp, lsoa_result, 
                         by.x = "LSOACODE", by.y = "LSOACODE")

tm_shape(lsoa_result_gwr) + 
  tm_polygons(fill = "CoefLogIMD", col_alpha = 0.1, 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"),
              fill.legend = tm_legend(frame = FALSE, 
                                      title="Coefficient: Log(IMD) [%]",
                                      bg.alpha = 0,
                                      position = tm_pos_out())) +
  tm_shape(borough.shp) + tm_polygons(fill_alpha = 0, col = "black") +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)


lsoa_result_gwr$tstatIMD <- lsoa_result_gwr$CoefLogIMD/lsoa_result_gwr$SEfLogIMD
lsoa_result_gwr$significant <- cut(lsoa_result_gwr$tstatIMD,
                                   breaks = c(min(lsoa_result_gwr$tstatIMD), -1.96, 1.96, max(lsoa_result_gwr$tstatIMD)),
                                   labels = c("Reduction: Significant", "Not Significant", "Increase: Significant"))

tm_shape(lsoa_result_gwr) +
  tm_polygons(fill = "significant", col_alpha = 0.1, 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE, title = "Significance", position = tm_pos_out(), item.space = -0.2)) +
tm_layout(frame = FALSE)



# map localR2 to examine model performance
tm_shape(lsoa_result_gwr) + 
  tm_polygons(fill = "localR2", col_alpha = 0.1, 
              fill.scale = tm_scale_continuous(values = "brewer.spectral"),
              fill.legend = tm_legend(frame = FALSE, title="Adaptive: Local R2", position = tm_pos_out())) +
  tm_shape(borough.shp) + tm_polygons(fill_alpha = 0, col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)























