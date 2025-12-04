

# DON'T USE: creating cross-sectional dataset for 2017
library(dplyr)

Lira_2017 <- rbind(Lira2017_1, Lira2017_2, Lira2017_3)

Lira_2017_cross_sectional <- Lira_2017 %>%
	group_by(osmID, NeighNum, NeighbNam) %>%
	summarise(
		Total = sum(Total, na.rm = TRUE),
		InfestedNum = sum(InfestedNum, na.rm = TRUE),
		Temperature = mean(Temperature, na.rm = TRUE),
		Precipitation = mean(Precipitation, na.rm = TRUE),
		Urbanisation = mean(Urbanisation, na.rm = TRUE),
		.groups = "drop"
	)

Lira_2017_cross_sectional <- Lira_2017_cross_sectional[order(Lira_2017_cross_sectional$NeighNum),]
write.csv(Lira_2017_cross_sectional, file = "Lira_2017_cross_sectional.csv", row.names = FALSE, fileEncoding = "UTF-8")

#create the longitudinal dataset
# load the different dataset 1, 2 and 3
Lira2017_1 <- read.csv("Lira_January_2017.csv")
Lira2017_2 <- read.csv("Lira_April_2017.csv")
Lira2017_3 <- read.csv("Lira_July_2017.csv")

Lira_2017_longitudinal <- rbind(Lira2017_1, Lira2017_2, Lira2017_3)
Lira_2017_longitudinal <- Lira_2017_longitudinal[,-c(11,13)]

# make sure to reorder the dataset in correct order
analysis_long_data <- Lira_2017_longitudinal[order(Lira_2017_longitudinal$NeighNum, Lira_2017_longitudinal$LIRAa),]
row.names(analysis_long_data) <- 1:nrow(analysis_long_data)

# calculate expected value
analysis_long_data$Expected <- expected(population = analysis_long_data$Total, 
	cases = analysis_long_data$InfestedNum, 
	n.strata = 1)

write.csv(analysis_long_data, file = "Lira_2017_Longitudinal.csv", row.names = FALSE, fileEncoding = "UTF-8")