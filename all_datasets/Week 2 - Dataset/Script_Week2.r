
setwd("/Users/anwarmusah/Documents/Websites/GEOG0114/all_datasets/Week 2 - Dataset")

# Load the packages with library()
library("tidyverse")
library("dplyr")
library("units")
library("sf")
library("tmap")

london_schools <- st_read('school_data_london_2016.shp')
london_wards <- st_read('london_wards.shp')
TL_greenspace <- st_read('TL_GreenspaceSite.shp')
TQ_greenspace <- st_read('TQ_GreenspaceSite.shp')

# reproject london schools from Web Mercator to BNG 
london_schools_prj <- st_transform(london_schools, 27700)
# check CRS 
st_crs(london_schools_prj)

# dissolve
london_outline <- london_wards %>% summarise(area = sum(HECTARES))

# subset London schools
london_schools_prj_ss <- london_schools_prj[london_outline,]

# join greenspace data sets together
greenspace = rbind(TQ_greenspace, TL_greenspace)

# subset and clip
london_greenspace <- greenspace[london_outline,] %>% st_intersection(london_outline)

# inspect
tm_shape(london_outline) + 
	tm_polygons() + 
	tm_shape(london_greenspace) + 
	tm_polygons()

# calculate area
london_greenspace$area_m <- st_area(london_greenspace)
# filter large greenspaces
large_london_greenspace <- london_greenspace %>% filter(area_m > set_units(50000.0, m^2))
# inspect
tm_shape(london_outline) + 
	tm_polygons() + 
	tm_shape(large_london_greenspace) + 
	tm_polygons()

# greenspace buffer
gs_buffer_400m <- st_buffer(large_london_greenspace, dist=400)

# dissolve greenspace buffer
gs_buffer_400m_single <- gs_buffer_400m %>% summarise(area = sum(st_area(gs_buffer_400m)))

# inspect
tm_shape(london_outline) + 
	tm_polygons() + 
	tm_shape(gs_buffer_400m_single) + 
	tm_polygons()

# schools within 400m of greenspace
london_schools_gs <- london_schools_prj_ss[gs_buffer_400m_single,]

# greenspace access
london_schools_prj_ss$gs_access <- st_intersects(london_schools_prj_ss, gs_buffer_400m_single, sparse=FALSE)

# inspect
tm_shape(london_schools_prj_ss) + 
	tm_dots(fill = "gs_access", 
		fill.scale = tm_scale_categorical(values = c("red", "darkgreen")), 
		fill.legend = tm_legend(title = "Greenspace Access", frame = FALSE)
		)

# total number of schools in each ward 
london_wards$total_schools <- lengths(st_intersects(london_wards, london_schools_prj_ss))
# total number of schools with greenspace access in each ward
london_wards$gs_schools <- lengths(st_intersects(london_wards, london_schools_gs))
# percentage of schools with greenspace access
london_wards$gs_rate <- (london_wards$gs_schools/london_wards$total_schools)*100

tm_shape(london_wards) + 
	tm_polygons(fill = "gs_rate",
		fill.scale = tm_scale_intervals(n = 5, style = "pretty", values = "brewer.greens"),
		fill.legend = tm_legend(title = "Greenspace Access [%]", frame = FALSE)
		)