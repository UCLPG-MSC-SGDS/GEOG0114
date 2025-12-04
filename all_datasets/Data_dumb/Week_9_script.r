

# USE: new packages we need to install
# install the SpatialEpi (version 1.2.8)
install.packages("SpatialEpi")

# install INLA (version 25.10.19)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# get update version of INLA
inla.upgrade()

# USE: library
library("SpatialEpi")
library("INLA")
library("sf")
library("tmap")
library("spdep")

rm(list = ls())
gc()

# PART A

# USE: set the working directory
setwd("/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 9 - Dataset")

# USE: load the data sets
Lira_2017_cross_sectional <- read.csv("Lira_2017_cross_sectional.csv")

# USE: load the shapefile
campina_grande_neighbourhoods <- read_sf("Campina_Grande_Lira2017.shp")

# USE: Generate an empty map to visualise the spatial configuration and hierarchy of neighbourhoods in Campina Grande
tm_shape(campina_grande_neighbourhoods) + 
	tm_polygons(fill_alpha = 0, col = "black") +
tm_text("NeighNum") +
tm_compass(position = c("right", "top")) + 
tm_scalebar(position = c("left", "top")) +
tm_layout(frame = FALSE)

# USE: cross-sectional scenario
# calculate expected number 
Lira_2017_cross_sectional$Expected <- expected(population = Lira_2017_cross_sectional$Total, 
	cases = Lira_2017_cross_sectional$InfestedNum, 
	n.strata = 1)

Lira_2017_cross_sectional$Expected <- round(Lira_2017_cross_sectional$Expected, .1)

# merge the dataset to shapefile
analysis_cross_data <- merge(campina_grande_neighbourhoods, 
	Lira_2017_cross_sectional, 
	by.x = c("osmID", "NeighNum", "NeighbNam"),
	by.y = c("osmID", "NeighNum", "NeighbNam")
	)

# make sure to reorder the dataset in correct order
analysis_cross_data <- analysis_cross_data[order(analysis_cross_data$NeighNum),]
row.names(analysis_cross_data) <- 1:nrow(analysis_cross_data)

# create the neighbourhood matrix as an INLA object
adjacencyMatrix <- poly2nb(analysis_cross_data)
names(adjacencyMatrix) <- analysis_cross_data$NeighbNam
# check
head(adjacencyMatrix, n = 10)

# create it into read graph structure for inla()
nb2INLA("adjacencyObject.adj", adjacencyMatrix)
g <- inla.read.graph(filename = "adjacencyObject.adj")

# model set-up
analysis_cross_data$id_area_structured <- 1:nrow(analysis_cross_data)
analysis_cross_data$id_area_unstructured <- 1:nrow(analysis_cross_data)

# create formula
infestation_risk <- InfestedNum ~ 1 + Temperature + Precipitation + Urbanisation +
	f(id_area_structured, model = "besag", graph = g, scale.model = TRUE) + f(id_area_unstructured, model = "iid")

results_cross_sectional <- inla(infestation_risk, 
	family = "poisson", 
	data = analysis_cross_data, 
	E = Expected,
	control.predictor = list(compute = TRUE), 
	control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor = TRUE)
)

# show results
summary(results_cross_sectional)

# extract the results report them as relative risks
options(scipen = 7)
fixed <- results_cross_sectional$summary.fixed
RR_table <- exp(fixed[, c("mean", "0.025quant", "0.5quant", "0.975quant")])
RR_table[c(2:4),]

# examine the posterior distribution of estimated coefficients

beta_temp <- inla.smarginal(results_cross_sectional$marginals.fixed$Temperature)
beta_prec <- inla.smarginal(results_cross_sectional$marginals.fixed$Precipitation)
beta_urbn <- inla.smarginal(results_cross_sectional$marginals.fixed$Urbanisation)

post_dist_beta <- data.frame(beta_temp, beta_prec, beta_urbn)

colnames(post_dist_beta)[1] <- "beta_temp_coef"
colnames(post_dist_beta)[2] <- "beta_temp_density"
colnames(post_dist_beta)[3] <- "beta_prec_coef"
colnames(post_dist_beta)[4] <- "beta_prec_density"
colnames(post_dist_beta)[5] <- "beta_urbn_coef"
colnames(post_dist_beta)[6] <- "beta_urbn_density"

# convert to relative risk
post_dist_beta$beta_temp_coef <- exp(post_dist_beta$beta_temp_coef)
post_dist_beta$beta_prec_coef <- exp(post_dist_beta$beta_prec_coef)
post_dist_beta$beta_urbn_coef <- exp(post_dist_beta$beta_urbn_coef)

# generate probability density plot of the posterior samples
plot(post_dist_beta$beta_temp_coef, post_dist_beta$beta_temp_density, type = "l", 
	main = "Posterior of Relative Risk for Temperature", 
	xlab = "Estimated Relative Risk", 
	ylab = "Posterior Probability Density (Plausibility)")

# Add vertical dashed line at the mean
abline(v = 1.0891722, lty = "dashed", col = "darkgrey", lwd = 4)
# add vertical line for the lower 95% CrI value
abline(v = 0.6057726, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for the upper 95% CrI value
abline(v = 1.959489, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for Null Value
abline(v = 1.00, col = "red", lwd = 2)

prob <- 1 - inla.pmarginal(1.089, exp(results_cross_sectional$marginals.fixed$Temperature))
prob

# 0.789 (or 78.9%)

x  <- post_dist_beta$beta_temp_coef
y  <- post_dist_beta$beta_temp_density
threshold_rr <- 1.089

# Plot the posterior density line
plot(x, y, type = "l",
	main = "Exceedance Probability [P(RR > 1)] = 0.731 or 73.1%",
	xlab = "Estimated Relative Risk",
	ylab = "Posterior Probability Density (Plausibility)")

# Identify region above the threshold
idx <- which(x >= threshold_rr)

# Shade the area under the curve for x > threshold
polygon(
	c(x[idx], rev(x[idx])),
	c(y[idx], rep(0, length(idx))),
	col = "#fee0d2",  # or any color you want
	border = NA
)

# Add vertical dashed line at the mean
abline(v = 1.0891722, lty = "dashed", col = "darkgrey", lwd = 4)
# add vertical line for the lower 95% CrI value
abline(v = 0.6057726, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for the upper 95% CrI value
abline(v = 1.959489, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for Null Value
abline(v = 1.00, col = "red", lwd = 2)


# precipitation
plot(post_dist_beta$beta_prec_coef, post_dist_beta$beta_prec_density, type = "l", 
	main = "Posterior of Relative Risk for Precipitation", 
	xlab = "Estimated Relative Risk", 
	ylab = "Posterior Probability Density (Plausibility)")

# Add vertical dashed line at the mean
abline(v = 1.0048164, lty = "dashed", col = "darkgrey", lwd = 4)
# add vertical line for the lower 95% CrI value
abline(v = 0.9744624, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for the upper 95% CrI value
abline(v = 1.036122, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for Null Value
abline(v = 1.00, col = "red", lwd = 2)

prob <- 1 - inla.pmarginal(1.00, exp(results_cross_sectional$marginals.fixed$Precipitation))
prob

# urbanisation
plot(post_dist_beta$beta_urbn_coef, post_dist_beta$beta_urbn_density, type = "l", 
	main = "Posterior of Relative Risk for Urbanisation", 
	xlab = "Estimated Relative Risk", 
	ylab = "Posterior Probability Density (Plausibility)")

# Add vertical dashed line at the mean
abline(v = 0.9483598, lty = "dashed", col = "darkgrey", lwd = 4)
# add vertical line for the lower 95% CrI value
abline(v = 0.5924214, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for the upper 95% CrI value
abline(v = 1.517014, lty = "dashed", col = "darkgrey", lwd = 2)
# add vertical line for Null Value
abline(v = 1.00, col = "red", lwd = 2)

prob <- 1 - inla.pmarginal(1.00, exp(results_cross_sectional$marginals.fixed$Urbanisation))
prob

# mapping
riskratio <- results_cross_sectional$summary.fitted.values
head(riskratio, n=10)

analysis_cross_data$RR <- riskratio[, "mean"]       # Relative risk
analysis_cross_data$LL <- riskratio[, "0.025quant"] # Lower credibility limit
analysis_cross_data$UL <- riskratio[, "0.975quant"] # Upper credibility limit

head(analysis_cross_data)

summary(analysis_cross_data$RR)
#min:0.2829
#max:1.5497

# Notes 1: create risk categories for legend for map
# Notes 2: RR = 1 is the mid-point of the legend
RiskCategorylist <- c("<0.50", "0.50 to 0.75", "0.76 to 0.99", "1.00-1.009 (null value)",
	"1.01 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.50+")

# Create the colours for the above categories - from extreme blues to extreme reds
RRPalette <- c("#33a6fe","#98cffe","#cbe6fe","#fef9f9","#fed5d5","#feb1b1","#fe8e8e","#fe2424")

# Now generate categories
analysis_cross_data$RelativeRiskCat <- NA
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 0 & analysis_cross_data$RR <= 0.50] <- -3
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 0.50 & analysis_cross_data$RR <= 0.75] <- -2
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 0.75 & analysis_cross_data$RR < 1] <- -1
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR>= 1 & analysis_cross_data$RR < 1.01] <- 0
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR>= 1.01 & analysis_cross_data$RR <= 1.10] <- 1
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 1.10 & analysis_cross_data$RR <= 1.25] <- 2
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 1.25 & analysis_cross_data$RR <= 1.50] <- 3
analysis_cross_data$RelativeRiskCat[analysis_cross_data$RR> 1.50 & analysis_cross_data$RR <= 2] <- 4

# create categories to define if an area has significant increase or decrease or not all 
analysis_cross_data$Significance <- NA
analysis_cross_data$Significance[analysis_cross_data$LL<1 & analysis_cross_data$UL>1] <- 0    # NOT SIGNIFICANT
analysis_cross_data$Significance[analysis_cross_data$LL==1 | analysis_cross_data$UL==1] <- 0  # NOT SIGNIFICANT
analysis_cross_data$Significance[analysis_cross_data$LL>1 & analysis_cross_data$UL>1] <- 1    # SIGNIFICANT INCREASE
analysis_cross_data$Significance[analysis_cross_data$LL<1 & analysis_cross_data$UL<1] <- -1   # SIGNIFICANT DECREASE

# exceedance probabilities
analysis_cross_data$ExceedProb <- sapply(results_cross_sectional$marginals.fitted.values,
	FUN = function(marg){1 - inla.pmarginal(q = 1.00, marginal = marg)})
# rounding the values
analysis_cross_data$ExceedProb <- round(analysis_cross_data$ExceedProb, 3)
# categorising 
analysis_cross_data$ProbCat <- NA
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0 & analysis_cross_data$ExceedProb< 0.01] <- 1
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0.01 & analysis_cross_data$ExceedProb< 0.20] <- 2
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0.20 & analysis_cross_data$ExceedProb< 0.40] <- 3
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0.40 & analysis_cross_data$ExceedProb< 0.60] <- 4
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0.60 & analysis_cross_data$ExceedProb< 0.80] <- 5
analysis_cross_data$ProbCat[analysis_cross_data$ExceedProb>=0.80 & analysis_cross_data$ExceedProb<= 1.00] <- 6
# labelling
ProbCategorylist <- c("<0.01", "0.01-0.20", "0.20-0.39", "0.40-0.59", "0.60-0.79", "0.80-1.00")


map_A <- tm_shape(analysis_cross_data) + 
	tm_polygons(fill = "RelativeRiskCat", 
		fill.scale = tm_scale_categorical(values = RRPalette, labels = RiskCategorylist),
		fill.legend = tm_legend(frame = FALSE, "Relative Risk: Mosquito Infestation")) +
	tm_text("NeighNum") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "top")) +
	tm_layout(frame = FALSE)

map_B <- tm_shape(analysis_cross_data) + 
	tm_polygons(fill = "Significance", 
		fill.scale = tm_scale_categorical(values = c("blue", "white", "red"), 
			labels = c("Decreased Risk: Significant", "Not Significant", "Increased Risk: Significant")),
		fill.legend = tm_legend(frame = FALSE, title = "Significance")) +
	tm_text("NeighNum") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "top")) +
	tm_layout(frame = FALSE)

map_C <- tm_shape(analysis_cross_data) + 
	tm_polygons(fill = "ProbCat", 
		fill.scale = tm_scale_categorical(values = "brewer.reds", labels = ProbCategorylist),
		fill.legend = tm_legend(frame = FALSE, title = "Exceedance Probability: P(RR > 1.00)")) +
	tm_text("NeighNum") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "top")) +
	tm_layout(frame = FALSE)

tmap_arrange(map_A, map_B, map_C, nrow = 1)

# PART B

analysis_long_data <- read.csv("Lira_2017_Longitudinal.csv")

# id_area_structured
# id_area_unstructured

analysis_long_data$id_area_structured <- analysis_long_data$NeighNum
analysis_long_data$id_area_unstructured <- analysis_long_data$NeighNum
analysis_long_data$id_time <- analysis_long_data$LIRAa

# create formula for spatio-temporal model
infestation_risk_temporal <- InfestedNum ~ 1 + Temperature + Precipitation + Urbanisation +
	f(id_area_structured, model = "besag", graph = g, scale.model = TRUE) + 
	f(id_area_unstructured, model = "iid") +
	id_time

results_longitudinal <- inla(infestation_risk_temporal, 
	family = "poisson", 
	data = analysis_long_data, 
	E = Expected,
	control.predictor = list(compute = TRUE), 
	control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor = TRUE)
)

summary(results_longitudinal)

# extract the results report them as relative risks
options(scipen = 7)
fixed_temp <- results_longitudinal$summary.fixed
RR_table_temp <- exp(fixed_temp[, c("mean", "0.025quant", "0.5quant", "0.975quant")])
RR_table_temp[c(2:5),]


# mapping
riskratio_temp <- results_longitudinal$summary.fitted.values
head(riskratio, n=10)

analysis_long_data$RR <- riskratio_temp[, "mean"]       # Relative risk
analysis_long_data$LL <- riskratio_temp[, "0.025quant"] # Lower credibility limit
analysis_long_data$UL <- riskratio_temp[, "0.975quant"] # Upper credibility limit

analysis_long_data$ExceedProb <- sapply(results_longitudinal$marginals.fitted.values,
	FUN = function(marg){1 - inla.pmarginal(q = 1.00, marginal = marg)})

analysis_long_data$ExceedProb <- round(analysis_long_data$ExceedProb, 3)

# split the dataset accordingly by time
analysis_long_data_jan <- analysis_long_data[analysis_long_data$id_time == 1,]
analysis_long_data_apr <- analysis_long_data[analysis_long_data$id_time == 2,]
analysis_long_data_jul <- analysis_long_data[analysis_long_data$id_time == 3,]