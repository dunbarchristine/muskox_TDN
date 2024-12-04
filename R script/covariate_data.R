

#installing the package raster and exactextractr to calculated 300 m buffer around cameras
install.packages("raster")
install.packages("exactextractr")

library(raster)
library(exactextractr)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)


#load camera location data (see muskox detections map script for production of data file) renaming file cam_data 
cam_data <- Habitat_types

#load boundary and water polygon data
TDN <- st_read("TDN_Boundary.shp")
water_shapefile <- "~/Desktop/Analysis/Learning/learning/TDN_water_features.shp"
water_polygon <- st_read(water_shapefile)

#load landcover of canada data 

lcc <- raster('landcover-2020-classification.tif')

#change location data to points and correct coordinate system
#cams_sf<-st_as_sf(cam_data,coords=c("longitude","latitude"),crs=4326,remove=FALSE)
#cams_sf_lcc<-st_transform(cams_sf,crs=st_crs(lcc))
#plot(cams_sf_landcover$geometry)

#check distance between points
st_distance(cam_data)

#Transform boundary data
#not sure if this is the correct crs
TDN_lcc <- st_transform(TDN, crs = st_crs(lcc))
cam_lcc <- st_transform(cam_data, st_crs(lcc))

#plot lcc with tdn boundary and camera plots
lcc_cropped<-crop(lcc,TDN_lcc)
plot(lcc_cropped, axes = FALSE)
plot(st_geometry(TDN_lcc), add = TRUE)
plot(st_geometry(cam_lcc),add = TRUE)
#change lcc resolution, redo all analysis with this
#do we actually need to do this if we don't use DUC data as well?
#takes a long time to change resolution
#lcc_10<-disaggregate(lcc,fact=3)

lcc_classes<-read.csv("lcc_2020_classes.csv")

##2.1. Summarizing land cover classifications
  
  #Across all of TDN

landcov_mode <- exact_extract(lcc, TDN_lcc, 'mode', 
                              append_cols = 'Name', progress = FALSE) %>%
  inner_join(lcc_classes, by=c(mode = 'value'))

#Water is predominant landcover in TDN

##2.2. Summary functions

###2.2.1. Lancover by %
landcov_percent <- exact_extract(lcc, TDN_lcc, function(df) {
  df %>%
    dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(Name, value) %>%
    summarize(freq = sum(frac_total))
}, summarize_df = TRUE, include_cols = 'Name', progress = FALSE)

landcov_percent %>%
  inner_join(lcc_classes, by = 'value') %>%
  group_by(Name) %>%
  arrange(desc(freq)) %>%
  #slice_head(n = 3) %>%
  mutate(freq = sprintf('%0.1f%%', 100*freq)) %>%
  knitr::kable()

###2.2.2. Landcover by Area
landcov_areas <- exact_extract(lcc, TDN_lcc, function(df) {
  df %>%
    group_by(Name, value) %>%
    summarize(area_km2 = sum(coverage_area) / 1e6)
}, summarize_df = TRUE, coverage_area = TRUE, include_cols = 'Name', progress = FALSE)

landcov_areas %>%
  inner_join(lcc_classes, by = 'value') %>%
  group_by(Name) %>%
  arrange(desc(area_km2)) %>%
  #slice_head(n = 3) %>%
  #mutate(area_km2 = sprintf('%0.1f%%', 100*area_km2)) %>%
  knitr::kable()

#plotting 
landcov_areas_plot<-landcov_areas %>%
  inner_join(lcc_classes, by = 'value') %>%
  arrange(desc(area_km2)) 

#plot distribution of landcover types across entire PA
ggplot(landcov_areas_plot,aes(x= reorder(code,-area_km2),y=area_km2))+
  geom_bar(stat = "identity")

ggplot(landcov_areas_plot, aes(x = reorder(code, -area_km2), y = area_km2, fill = class)) +
  geom_bar(stat = "identity") +
  labs(x = "Land Classes", y = "Area (km²)", fill = "Land Cover Class") +
  scale_fill_manual(values = c("Sub-polar or polar shrubland-lichen-moss" = "plum", "Water" = "burlywood4", "Temperate or sub-polar needleleaf forest" = "palegreen", "Temperate or sub-polar grassland" = "wheat", "Sub-polar or polar grassland-lichen-moss" = "pink", "Mixed forest" = "seagreen", "Temperate or sub-polar shrubland" = "cornflowerblue", "Barren lands" = "mediumslateblue", "Sub-polar taiga needleleaf forest" = "khaki1", "Temperate or sub-polar broadleaf deciduous forest" = "darkolivegreen", "Sub-polar or polar barren-lichen-moss" = "darkgoldenrod1")) +  # Custom colors
  theme_minimal()

#creating buffers around each camera (300 m for now)

#Create buffers of 300m around each camera
buffers_300<-st_buffer(cam_lcc,dist=300)

#Write shapefile
st_write(buffers_300,"camera_buffers_300.shp",append = FALSE)

#plot
plot(buffers_300$geometry)

#Create buffers of 550m around each camera, an approximate equivalent of the survey hexagons used in survey design
buffers_550<-st_buffer(cam_lcc,dist=550)

#Write shapefile
st_write(buffers_550,"camera_buffers_550.shp",append = FALSE)

#Create buffers of 1622.50m around each camera, half the minimum distance between sites
buffers_1622<-st_buffer(cam_lcc,dist=1622.5)

#Write shapefile
st_write(buffers_1622,"camera_buffers_1622.shp",append = FALSE)

#plot
plot(buffers_1622$geometry)

#Create buffers of 3245m around each camera, just the minimum distance between sites
buffers_3245<-st_buffer(cams_sf_lcc,dist=3245)

#Write shapefile
st_write(buffers_3245,"Processed_Data/camera_buffers_3245.shp",append = FALSE)

#plot
plot(buffers_3245$geometry)


###3.1.1 LCC Landcover Proportions 300m
landcov_fracs <- exact_extract(lcc, buffers_300, function(df) {
  df %>%
    group_by(value) %>%
    summarize(total = (sum(coverage_fraction))) %>% 
    mutate(total_area=total*(30*30)) %>% #multiply by area of one pixel 
    mutate(total_proportion=total/sum(total))
}, summarize_df = TRUE, append_cols = names(buffers_300), progress = T)

#remove total and total_area columns
tmp<- landcov_fracs %>% 
  dplyr::select(-total, -total_area)

#add class names,
landcov_fracs_join<-left_join(tmp,lcc_classes,by="value") %>% 
  dplyr::select(-class,-value) %>% 
  pivot_wider(values_from = total_proportion,names_from = code,names_prefix = "X300_",values_fill = 0)

######## creating 











