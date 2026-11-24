library(raster)
library(tidyverse)
library(terra)
library(writexl)
library(ggplot2)
library(gridExtra)
library(ggspatial)
library(tidyterra)

#cropping arcticdem to tdn boundary 
TDN_boundary_projected_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(ArcticDEM)) %>%
  st_buffer(1000)

#extract the elevation at camera site. we are using camera buffer because it has a geometry column.
arctic_DEM_500_mosaic_camera <- terra::extract(ArcticDEM, camera_buffer, fun=mean)

#add the value to the locations file if needed
camera_buffer$Arctic_DEM_500m_elevation_m<- arctic_DEM_500_mosaic_camera$arcticdem_mosaic_500m_v4.1_dem

#deleting old elevation column that gave NAs
model_variables <- model_variables %>%
  select(-Elevation)

ArcticDEM_cropped<-crop(ArcticDEM,TDN_boundary_projected_ArcticDEM)
plot(ArcticDEM_cropped, axes = FALSE)
plot(st_geometry(ArcticDEM_cropped), add = TRUE)
plot(st_geometry(camera_locations),add = TRUE)

plot(ArcticDEM_cropped)  # First plot the raster
plot(TDN_boundary_projected_ArcticDEM, add = TRUE, border = "red", lwd = 2)  # Overlay boundary


#Terrain Ruggedness Index (TRI)
#ArcticDEM data
TRI_Results_ArcticDEM <- spatialEco::tri(ArcticDEM_cropped, s = 3, exact = FALSE)
#ArcticDEM for TDI
TDN_tri_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(TRI_Results_ArcticDEM))

#ArcticDEM data with data from Claudia (arctic_DEM_500_mosaic_camera). Not working, needs to be spatraster

# TRI_Results_ArcticDEM_mosaic <- spatialEco::tri(arctic_DEM_500_mosaic_camera, s = 3, exact = FALSE)
#  #ArcticDEM for TDI
# TDN_tri_ArcticDEM_mosaic <- st_transform(TDN_boundary, crs = st_crs(TRI_Results_ArcticDEM_mosaic))

#calculating TRI. grabbed this code from Claudias GitHub
TDN_tri_arctic<-  terra::terrain(ArcticDEM, v="TRI", unit = "degrees")

#saving model_variables as RDS
saveRDS(ArcticDEM_cropped,"~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")
ArcticDEM_cropped <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")

#adding Arctic_DEM_500m_elevation_m to model_variables instead of having the previous column "Elevations". "Elevations" column was giving me NAs
model_variables <- model_variables %>%
  dplyr::left_join(
    camera_buffer %>%
      dplyr::select(location, Arctic_DEM_500m_elevation_m),
    by = "location"
  )

# Extract values for each camera location
elevations <- raster::extract(ArcticDEM_cropped, camera_locations)
esker_camera_distances <- terra::extract(esker_dist, camera_locations)
TRI_extracted <- raster::extract(TDN_tri_arctic, camera_locations)

camera_locations_df <- camera_locations_df %>%
  left_join(
    model_variables %>% dplyr::select(location, TRI_extracted),
    by = "location"
  )

all_variables <- all_variables %>%
  left_join(
    model_variables %>% dplyr::select(location, TRI_extracted),
    by = "location"
  )


camera_locations_unique <- camera_locations_df %>%
  dplyr::select(location, TRI_extracted) %>%
  distinct(location, .keep_all = TRUE)

all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_unique, by = "location")

camera_locations_unique2 <- camera_locations_df %>%
  dplyr::select(location, TRI_extracted) %>%
  distinct(location, .keep_all = TRUE)

all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_unique2, by = "location")


#adding terrain ruggedness index (TRI) to all_variables
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% dplyr::select(location, TRI_extracted), by = "location")


# #made new dataset that included tri and grizz_per_day and gray_wolf_per_day
# all_variables_with_tri <- all_variables %>%
#   left_join(camera_locations_df %>% dplyr::select(location, TRI_extracted), by = "location")

#adding new variables to "all_variables_with_tri_and_species". This dataset is used to make model_variables (in 7.model dataset script)
all_variables_with_tri_and_species <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/N_days_effort, 
         gray_wolf_per_day = gray_wolf/N_days_effort)



############### eskers ################
# Check for empty geometries
esker_data_empty <- esker_data[st_is_empty(esker_data), ]
esker_data_non_empty <- esker_data[!st_is_empty(esker_data), ]

# Display number of empty geometries
nrow(esker_data_empty)
nrow(esker_data_non_empty)

# creating camera buffer bounding box 
camera_buffer_bb <- camera_buffer %>%
  st_buffer(100000) %>%
  st_bbox() %>%
  st_as_sfc()

# Remove empty geometries
esker_data <- esker_data[!st_is_empty(esker_data), ]

# Remove the Z dimension
esker_data_cropped <- st_zm(esker_data) %>%
  st_transform(32612) %>%
  st_intersection(camera_buffer_bb)


#getting distance between cameras and eskers

rast <- terra::rast(esker_data_cropped, resolution = 30)
esker_rast <- rasterize(esker_data_cropped, rast)
esker_dist <- distance(esker_rast)


st_write(camera_locations, "SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_repo.shp")

#making inset nwt boundary 
# Download landmass data for the entire world from the naturalearth and naturalearthdata packages
world_landmass <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(st_crs(Canada_Boundaries))

#Download Canada boundaries
Canada_Boundaries<-st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/gpr_000a11a_e.shp") %>%
  st_transform(Canada_Boundaries, crs = 3347)

#Transform Canadian Boundaries to NAD83
Canada_Boundaries_NWT <- Canada_Boundaries %>% st_transform(st_crs(TDN_boundary))

NWT_Boundary <- Canada_Boundaries_NWT %>%
  filter(PREABBR == "N.W.T.")

saveRDS(NWT_Boundary, "spatial/shapefiles/TDN_Boundary.rds")

world_landmass_NWT <- world_landmass %>% 
  st_transform(st_crs(TDN_boundary))

NWT_landmass <- st_intersection(world_landmass_NWT, NWT_Boundary)

saveRDS(NWT_landmass, "spatial/shapefiles/TDN_Boundary.rds")

# Save the plot as a PNG with a transparent background
png("nwt_boundary.png", bg = "transparent", width = 800, height = 600)

# Create the plot
ggplot() +
  geom_sf(data = NWT_Boundary, fill = NA, color = "black") +  # Boundary with no fill, black border
  geom_sf(data = NWT_landmass, fill = "thistle3") +  # Landmass with light grey fill
  geom_sf(data = TDN_boundary, fill = "royalblue2", color = "black") +  # Boundary with limegreen fill, black border
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA))   # Transparent background for the plot area


#trying to plot basic SCANFI landcover map for TDN 
ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TDN Landcover Types", color = "Habitat")


#making map of all nwt ecoregions
nwt_ecoregions_2 <- nwt_ecoregions %>%
  group_by(ECO2_NAM_1) %>% 
  summarize(geometry = st_union(geometry)) 


#making map for just tdn ecoregions
tdn_ecoregions_2 <- cropped_ecoregions_TDN_Boundary %>%
  group_by(ECO2_NAM_1) %>% 
  summarize(geometry = st_union(geometry)) 


# Set the correct CRS (replace with your known EPSG if different)
st_crs(tdn_ecoregions) <- 32612

# Ensure both datasets use the same CRS
tdn_ecoregions <- st_transform(tdn_ecoregions, crs = st_crs(model_variables))

# Perform spatial join
model_variables_joined <- st_join(model_variables, tdn_ecoregions, left = TRUE)

tdn_buffer <- st_buffer(tdn_ecoregions_2, dist = 70000)  # 75 km buffer

#plotting map of tdn ecoregions and camera locations
ggplot(data = tdn_ecoregions_2) +
  annotation_map_tile(type = "cartolight", zoom = 6) +
  
  geom_sf(aes(fill = ECO2_NAM_1), color = "black", size = 0.2) +
  
  geom_sf(
    data = camera_locations,
    aes(color = "Camera Cluster Location"),
    size = 2
  ) +
  
  scale_fill_manual(
    name = "Thaıdene Nëné Ecoregions",
    values = c("burlywood", "darkseagreen")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Camera Cluster Location" = "black")
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_sf(
    xlim = st_bbox(tdn_buffer)[c("xmin", "xmax")],
    ylim = st_bbox(tdn_buffer)[c("ymin", "ymax")],
    expand = FALSE
  )




bb <- st_bbox(tdn_ecoregions_2)

ggplot(data = tdn_ecoregions_2) +
  annotation_map_tile(type = "cartolight", zoom = 6) +
  
  geom_sf(aes(fill = ECO2_NAM_1), color = "black", size = 0.2) +
  
  geom_sf(
    data = camera_locations,
    aes(color = "Camera Cluster Location"),
    size = 2
  ) +
  
  scale_fill_manual(
    name = "Thaıdene Nëné Ecoregions",
    values = c("burlywood", "darkseagreen")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Camera Cluster Location" = "black")
  ) +
  
  coord_sf(
    xlim = c(bb["xmin"] - 2, bb["xmax"] + 2),
    ylim = c(bb["ymin"] - 2, bb["ymax"] + 2),
    expand = FALSE
  )



ggplot(data = tdn_ecoregions_2) +
  geom_sf(aes(fill = ECO2_NAM_1), color = "black", size = 0.2) +
  geom_sf(
    data = camera_locations,
    aes(color = "Camera Locations"),
    size = 2
  ) +
  scale_fill_manual(
    name = "TDN Ecoregions",
    values = c("burlywood", "darkseagreen")
  ) +
  scale_color_manual(
    name = "",
    values = c("Camera Locations" = "red")
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#Calculating the Ecoregion for each Camera location
locs_ecoregions <- cameras_sf %>%
  st_transform(crs = st_crs(ecoregions_cropped)) #change the projection to match the raster

# Find overlaps
ecoregions_overlap <- st_join(locs_ecoregions, ecoregions_cropped)
ecoregions_overlap <- as.data.frame(ecoregions_overlap)

# Make a new column in cameras (if needed)
cameras <- cameras %>% 
  left_join(ecoregions_overlap %>% select(location, ECO1_NAM_1, ECO2_NAM_1, ECO3_NAM_1, ECO4_NAM_1, ecoregion), by="location")



#comparing scanfi and lcc to determine which landcover dataset to use/justification for choosing lc/scanfi
lcc_scanfi_cor <- cor(lcc_cameras_prop_columns_variables_only, overlap_SCANFI_cameras_table_variables_only)

#looking at how correlated all the variables are 
model_subset <- all_variables_with_tri_and_species %>% ungroup() %>%
  select(-15:-24, -28, -29) %>%
  distinct() %>%
  select(-1)

correlation_matrix <- cor(model_subset, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")

#making plots for weekly muskox detections in regard to landcover 
muskox_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water)
            ) %>%
  mutate(Muskox_per_week = Muskox/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_week_mean = mean(Muskox_per_week), 
            sd = sd(Muskox_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )

#adding SD
muskox_detections1 <- model_variables %>%
  st_drop_geometry() %>%
  group_by(location, cluster, season) %>%
  summarise(
    Muskox = sum(Muskox),
    N_days_effort = sum(N_days_effort),
    
    Shrub = mean(Shrub), 
    Trees = mean(Trees),
    Groundcover = mean(Groundcover),
    Water = mean(Water)
  ) %>%
  mutate(Muskox_per_week = Muskox / N_days_effort * 7) %>%
  pivot_longer(
    cols = c("Trees", "Groundcover", "Shrub", "Water"),
    names_to = "landcover_class",
    values_to = "proportion"
  ) %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(
    Muskox = sum(Muskox),
    N_days_effort = sum(N_days_effort),
    
    Muskox_per_week_mean = mean(Muskox_per_week, na.rm = TRUE),
    sd_Muskox_per_week   = sd(Muskox_per_week, na.rm = TRUE),
    se_Muskox_per_week   = sd_Muskox_per_week / sqrt(n()),
    
    num_cameras = n(),
    num_cluster = length(unique(cluster))
  )
#summarize across seasons (camera_summary) and skip the initial summary across cameras and season (camera_season).
#That way your ranges won't have partial days.
camera_summary <- model_variables1 %>%
  group_by(season) %>%
  summarise(
    mean_days = mean(N_days_effort, na.rm = TRUE),
    sd_days = sd(N_days_effort, na.rm = TRUE),
    se_days = sd_days / sqrt(sum(!is.na(N_days_effort))),
    min_days = min(N_days_effort, na.rm = TRUE),
    max_days = max(N_days_effort, na.rm = TRUE),
    n_obs = sum(!is.na(N_days_effort))
  )

#old code for SE
camera_season <- model_variables1 %>%
  + group_by(camera_id, season) %>%
  + summarise(
    + mean_weekly_effort = mean(N_days_effort, na.rm = TRUE),
    + .groups = "drop"
    + )

camera_summary <- model_variables1 %>%
  group_by(season) %>%
  summarise(
    mean_days = mean(N_days_effort, na.rm = TRUE),
    sd_days = sd(N_days_effort, na.rm = TRUE),
    se_days = sd_days / sqrt(sum(!is.na(N_days_effort))),
    min_days = min(N_days_effort, na.rm = TRUE),
    max_days = max(N_days_effort, na.rm = TRUE),
    n_obs = sum(!is.na(N_days_effort))
  )

model_variables1_fixed <- model_variables1 %>%
  select(-N_days_effort) %>%
  left_join(weekly_effort,
            by = c("location", "year", "week"))

camera_summary <- model_variables1_fixed %>%
  group_by(season) %>%
  summarise(
    mean_days = mean(N_days_effort, na.rm = TRUE),
    sd_days   = sd(N_days_effort, na.rm = TRUE),
    se_days   = sd_days / sqrt(sum(!is.na(N_days_effort))),
    min_days  = min(N_days_effort, na.rm = TRUE),
    max_days  = max(N_days_effort, na.rm = TRUE),
    n_obs     = sum(!is.na(N_days_effort))
  )

#making it so no values show on the plot that go below zero
muskox_detections_plot <- muskox_detections %>%
  mutate(ymin = Muskox_per_week_mean - sd,
         ymax = Muskox_per_week_mean + sd) %>%
  filter(ymax > 0)  # only keep points where there's something above zero

#plotting
ggplot(muskox_detections_plot, aes(x = landcover_class, y = Muskox_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = pmax(ymin, 0), ymax = ymax)) +
  facet_wrap(~season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "Majority Land Cover Class",
    y = "Average Muskox Detection Per Week"
  ) +
  scale_x_discrete(labels = c(
    "Shrub" = "Shrub Land Cover",
    "Trees" = "Treed Land Cover",
    "Groundcover" = "Groundcover",
    "Water" = "Water Cover"
  ))

#checking how many winter detections of muskox
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Winter") %>%
  summarise(total = sum(Muskox, na.rm = TRUE))

#checking how many summer detections of muskox
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Summer") %>%
  summarise(total = sum(Muskox, na.rm = TRUE))

#now looking at 61 winter detections occurred across how many cameras?:
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Winter") %>%
  group_by(location) %>%
  summarise(Muskox = sum(Muskox, na.rm = TRUE), .groups = "drop") %>%
  filter(Muskox > 0) %>%
  summarise(
    total_detections = sum(Muskox),
    n_cameras = n()
  )

#looking at how many summer detections (706) occurred across how many cameras: 
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Summer") %>%
  group_by(location) %>%
  summarise(Muskox = sum(Muskox, na.rm = TRUE), .groups = "drop") %>%
  filter(Muskox > 0) %>%
  summarise(
    total_detections = sum(Muskox),
    n_cameras = n()
  )

#now looking at how many SITES in winter (60 sites total) not how many cameras:
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Winter") %>%
  group_by(cluster) %>%
  summarise(Muskox = sum(Muskox, na.rm = TRUE), .groups = "drop") %>%
  filter(Muskox > 0) %>%
  summarise(
    total_detections = sum(Muskox),
    n_cameras = n()
  )


#now looking at how many SITES in summer (60 sites total) not how many cameras:
model_variables %>%
  st_drop_geometry() %>%
  filter(season == "Summer") %>%
  group_by(cluster) %>%
  summarise(Muskox = sum(Muskox, na.rm = TRUE), .groups = "drop") %>%
  filter(Muskox > 0) %>%
  summarise(
    total_detections = sum(Muskox),
    n_cameras = n()
  )


#making plots for weekly muskox detections in regard to fire
muskox_detections_fire <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort),
            fire_age0 = mean(fire_age0),
            fire_age1 = mean(fire_age1),
            fire_age2 = mean(fire_age2),
            fire_age3 = mean(fire_age3),
            fire_age4 = mean(fire_age4),
  ) %>%
  mutate(Muskox_per_week = Muskox/N_days_effort*7) %>%
  pivot_longer(cols = c("fire_age0", "fire_age1", "fire_age2","fire_age3","fire_age4"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_week_mean = mean(Muskox_per_week), 
            sd = sd(Muskox_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
se = sd / sqrt(num_cameras)   # ✅ Standard Error
)


#making it so no values show on the plot that go below zero
muskox_detections_fire_plot <- muskox_detections_fire %>%
  mutate(ymin = Muskox_per_week_mean - sd,
         ymax = Muskox_per_week_mean + sd) %>%
  filter(ymax > 0)  # only keep points where there's something above zero

#plotting
ggplot(muskox_detections_fire_plot, aes(x = landcover_class, y = Muskox_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(
    ymin = pmax(ymin, 0),  # cap lower bound at zero
    ymax = ymax
  )) +
  facet_wrap(~season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "Years Post Fire",
    y = "Average Muskox Detection Per Week"
  ) +
  scale_x_discrete(labels = c(
    "fire_age0" = "0–10",
    "fire_age1" = "11–20",
    "fire_age2" = "21–30",
    "fire_age3" = "31–50",
    "fire_age4" = "Unburned"
  )) +
  coord_cartesian(ylim = c(0, NA))


write_xlsx(muskox_detections1, "muskox_detections1.xlsx")

#making simple bar graphs showing average muskox detections
ggplot(muskox_detections, aes(x = landcover_class, y = Muskox_per_week_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    #title = "Average Monthly Muskox Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Muskox Detections per Week",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#looking at grizzly detections by landcover type
grizzly_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(grizzly_bear = sum(grizzly_bear), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(grizz_per_week = grizzly_bear/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(grizzly_bear = sum(grizzly_bear), 
            N_days_effort = sum(N_days_effort),
            grizz_per_week_mean = mean(grizz_per_week), 
            sd = sd(grizz_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
se = sd / sqrt(num_cameras))   # ✅ Standard Error



#plotting
ggplot(grizzly_detections, aes(x = landcover_class, y = grizz_per_month_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = grizz_per_month_mean - sd, ymax = grizz_per_month_mean + sd)) +
  facet_wrap(~season) +
  labs(
    x = "Land Cover Class",
    y = "Average Grizzly Detection Per Month"
  )



#making simple bar grpahs showing average grizz detections
ggplot(grizzly_detections, aes(x = landcover_class, y = grizz_per_month_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    title = "Average Monthly Grizzly Bear Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Grizzly Bear Detections per Month",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


write_xlsx(grizzly_detections, "grizzly_detections.xlsx")

#looking at wolf detections by landcover type
wolf_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(gray_wolf = sum(gray_wolf), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(wolf_per_week = gray_wolf/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(gray_wolf = sum(gray_wolf), 
            N_days_effort = sum(N_days_effort),
            wolf_per_week_mean = mean(wolf_per_week), 
            sd = sd(wolf_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )

#plotting
ggplot(wolf_detections, aes(x = landcover_class, y = wolf_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = wolf_per_week_mean - sd, ymax = wolf_per_week_mean + sd)) +
  facet_wrap(~season) +
  labs(
    x = "Land Cover Class",
    y = "Average Gray Wolf Detection Per Week"
  )


write_xlsx(wolf_detections, "wolf_detections.xlsx")

#making simple bar grpahs showing average wolf detections
ggplot(wolf_detections, aes(x = landcover_class, y = wolf_per_month_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    title = "Average Monthly Gray Wolf Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Gray Wolf Detections per Month",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#making a bar graph with all species 
muskox_plot <- muskox_detections %>%
  transmute(
    species = "Muskox",
    landcover_class,
    season,
    mean = Muskox_per_week_mean,
    sd
  )

grizzly_plot <- grizzly_detections %>%
  transmute(
    species = "Grizzly Bear",
    landcover_class,
    season,
    mean = grizz_per_week_mean,
    sd
  )

wolf_plot <- wolf_detections %>%
  transmute(
    species = "Grey Wolf",
    landcover_class,
    season,
    mean = wolf_per_week_mean,
    sd
  )


all_detections <- bind_rows(
  muskox_plot,
  grizzly_plot,
  wolf_plot
)

ggplot(all_detections,
       aes(x = landcover_class, y = mean, fill = landcover_class)) +
  geom_col(width = 0.7) +
  facet_grid(species ~ season) +
  labs(
    x = "Dominant Land Cover Class",
    y = "Average Detections per Week",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "royalblue",
    "Trees" = "olivedrab",
    "Shrub" = "red4",
    "Groundcover" = "yellowgreen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )




muskox_avg_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort),
             Shrub = mean(Shrub), 
             Trees = mean(Trees),
             Groundcover = mean(Groundcover),
             Water = mean(Water),
  ) %>%
  mutate(Muskox_per_month = Muskox/N_days_effort*30) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_month_mean = mean(Muskox_per_month), 
            sd = sd(Muskox_per_month),
            num_cameras = n(),
            num_cluster = length(unique(cluster)))

#predator detections by landcover type
predator_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Predators = sum(Predators), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(predator_per_week = Predators/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Predators = sum(Predators), 
            N_days_effort = sum(N_days_effort),
            predator_per_week_mean = mean(predator_per_week), 
            sd = sd(predator_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )




#predicting muskox detections with predators 
# Step 1: Convert cluster column in dataset to factor
predator_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted = 0,
    Shrub = 0,
    Predators_new = seq(0, 5, by = 1),
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    Predators= MuMIn::stdize(Predators_new, source = Predator)
  )

# Step 2: Predict
predators_prediction <- predator_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(predators_prediction, aes(x = Predators_new,
                                      y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = Predators, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Number of Predators",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()

#predicting muskox detections for TRI
# Step 1: Convert cluster column in dataset to factor
TRI_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted_new = seq(2, 260, by = 1),
    Shrub = 0,
    Predators = 0,
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    TRI_extracted = MuMIn::stdize(TRI_extracted_new, source = TRI_extracted)
  )

# Step 2: Predict
TRI_prediction <- TRI_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(TRI_prediction, aes(x = TRI_extracted_new,
                                 y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = TRI_extracted, y = 0.12),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Terrain Ruggedness Index",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()

#predicting muskox detections for shrub 
# Step 1: Convert cluster column in dataset to factor
Shrub_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted = 0,
    Shrub_new = seq(0, 1, by = 1),
    Predators = 0,
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    Shrub = MuMIn::stdize(Shrub_new, source = Shrub)
  )

# Step 2: Predict
Shrub_prediction <- Shrub_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(Shrub_prediction, aes(x = Shrub_new,
                           y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = Shrub, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Shrub Land Cover",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()

#trying to put all these prediction plots into one image
###
all_predictions <- bind_rows(
  predators_prediction %>%
    mutate(
      x = Predators_new,
      predictor = "Weekly Detection Frequency of Predators"
    ) %>%
    dplyr::select(x, predicted_muskox_week, predictor),
  
  TRI_prediction %>%
    mutate(
      x = TRI_extracted_new,
      predictor = "Terrain Ruggedness Index"
    ) %>%
    dplyr::select(x, predicted_muskox_week, predictor),
  
  Shrub_prediction %>%
    mutate(
      x = Shrub_new,
      predictor = "Shrub Land Cover"
    ) %>%
    dplyr::select(x, predicted_muskox_week, predictor)
)

#plotting all predictions together 
ggplot(all_predictions, aes(x = x, y = predicted_muskox_week)) +
  geom_line(size = 1) +
  facet_wrap(
    ~ predictor,
    scales = "free_x",
    nrow = 1,
    strip.position = "bottom"
  ) +
  labs(
    x = NULL,
    y = "Predicted Muskox Detections per Week"
  ) +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 18)
  )

#generating confidence intervals 
predators_prediction <- predator_data %>%
  mutate(
    pred_link = predict(mod_pred_sum_1,
                        newdata = .,
                        allow.new.levels = TRUE,
                        type = "link",
                        se.fit = TRUE)$fit,
    
    se_link = predict(mod_pred_sum_1,
                      newdata = .,
                      allow.new.levels = TRUE,
                      type = "link",
                      se.fit = TRUE)$se.fit
  ) %>%
  mutate(
    lower_link = pred_link - 1.96 * se_link,
    upper_link = pred_link + 1.96 * se_link,
    
    predicted_muskox_week = exp(pred_link),
    lower = exp(lower_link),
    upper = exp(upper_link)
  )

ggplot(predators_prediction,
       aes(x = Predators_new,
           y = predicted_muskox_week)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              fill = "grey70") +
  
  geom_line(linewidth = 1) +
  
  geom_rug(
    data = model_variables,
    aes(x = Predators, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  
  labs(
    x = "Number of Predators",
    y = "Predicted Muskox Detections per Week"
  ) +
  theme_minimal()

###
add_ci <- function(model, newdata) {
  
  pred <- predict(model,
                  newdata = newdata,
                  allow.new.levels = TRUE,
                  type = "link",
                  se.fit = TRUE)
  
  newdata$fit_link <- pred$fit
  newdata$se_link  <- pred$se.fit
  
  newdata$predicted_muskox_week <- exp(newdata$fit_link)
  newdata$lower <- exp(newdata$fit_link - 1.96 * newdata$se_link)
  newdata$upper <- exp(newdata$fit_link + 1.96 * newdata$se_link)
  
  return(newdata)
}

predators_prediction <- add_ci(mod_pred_sum_1, predator_data)
TRI_prediction       <- add_ci(mod_pred_sum_1, TRI_data)
Shrub_prediction     <- add_ci(mod_pred_sum_1, Shrub_data)

all_predictions <- dplyr::bind_rows(
  
  predators_prediction %>%
    dplyr::mutate(
      x = Predators_new,
      predictor = "Weekly Detection\nFrequency of Predators"
    ) %>%
    dplyr::select(x, predicted_muskox_week, lower, upper, predictor),
  
  TRI_prediction %>%
    dplyr::mutate(
      x = TRI_extracted_new,
      predictor = "Terrain Ruggedness Index"
    ) %>%
    dplyr::select(x, predicted_muskox_week, lower, upper, predictor),
  
  Shrub_prediction %>%
    dplyr::mutate(
      x = Shrub_new,
      predictor = "Shrub Land Cover"
    ) %>%
    dplyr::select(x, predicted_muskox_week, lower, upper, predictor)
)

ggplot(all_predictions,
       aes(x = x, y = predicted_muskox_week)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              fill = "grey70") +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(~ predictor,
             scales = "free_x",
             nrow = 1) +
  
  labs(
    x = NULL,
    y = "Predicted Muskox Detections per Week"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(size = 10), #numbers 
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15), #muskox detections per week 
    strip.text   = element_text(size = 12, face = "bold")
  )


### New Prediction code Nov 16 2026 ###

summer_data <- model_variables %>% 
  
  filter(season == "Summer") 


TRI_data <- data.frame( 
  
  TRI_extracted = seq( 
    
    min(summer_data$TRI_extracted, na.rm = TRUE), 
    
    max(summer_data$TRI_extracted, na.rm = TRUE), 
    
    length.out = 100 
    
  ), 
  
  Predators = mean(summer_data$Predators, na.rm = TRUE), 
  
  Shrub = mean(summer_data$Shrub, na.rm = TRUE), 

  N_days_effort = 7, 
  
  cluster = summer_data$cluster[1] 
  
) 

#generate new predictions 

TRI_pred <- predict( 
  
  mod_pred_sum_1, 
  
  newdata = TRI_data, 
  
  type = "link", 
  
  se.fit = TRUE, 
  
  re.form = NA 
  
) 

TRI_prediction <- TRI_data %>% 
  
  mutate( 
    
    fit_link = TRI_pred$fit, 
    
    se_link = TRI_pred$se.fit, 
    
    predicted_muskox_week = exp(fit_link), 
    
    lower = exp(fit_link - 1.96 * se_link), 
    
    upper = exp(fit_link + 1.96 * se_link) 
    
  ) 

#then plot 

ggplot(TRI_prediction, 
       
       aes(x = TRI_extracted, 
           
           y = predicted_muskox_week)) + 

  geom_ribbon( 
    
    aes(ymin = lower, ymax = upper), 
    
    alpha = 0.2, 
    
    fill = "grey70" 
    
  ) + 
  geom_line(linewidth = 1) + 
  
  labs( 
    
    x = "Terrain Ruggedness Index", 
    
    y = "Predicted Muskox Detections per Week" 
    
  ) + 
  theme_minimal() 

#Shrub prediction code: 
  
  # Step 1: Create prediction data 
  
  Shrub_data <- model_variables %>% 
  
  filter(season == "Summer") %>% 
  
  ungroup() %>% 
  
  modelr::data_grid( 
    
    TRI_extracted = 0, 
    
    Shrub = seq(0, 1, by = 0.01), 
    
    Predators = 0, 
    
    N_days_effort = 7, 
    
    cluster = "new" 
    
  ) 

# Step 2: Generate predictions 

Shrub_pred <- predict( 
  
  mod_pred_sum_1, 
  
  newdata = Shrub_data, 
  
  type = "link", 
  
  se.fit = TRUE, 
  
  re.form = NA 
  
) 

# Step 3: Calculate predictions and 95% confidence intervals 

Shrub_prediction <- Shrub_data %>% 
  
  mutate( 
    
    fit_link = Shrub_pred$fit, 
    
    se_link = Shrub_pred$se.fit, 
    
    predicted_muskox_week = exp(fit_link), 
    
    lower = exp(fit_link - 1.96 * se_link), 
    
    upper = exp(fit_link + 1.96 * se_link) 
    
  ) 

# Step 4: Plot 

ggplot(Shrub_prediction, 
       
       aes(x = Shrub, 
           
           y = predicted_muskox_week)) + 
  
  geom_ribbon( 
    
    aes(ymin = lower, ymax = upper), 
    
    alpha = 0.2, 
    
    fill = "grey70" 
    
  ) + 
  
  geom_line(linewidth = 1) + 
  
  geom_rug( 
    
    data = model_variables %>% filter(season == "Summer"), 
    
    aes(x = Shrub, y = 0), 
    
    inherit.aes = FALSE, 
    
    sides = "b", 
    
    alpha = 0.3, 
    
    position = position_jitter(height = 0.02) 
    
  ) + 
  
  scale_x_continuous(breaks = seq(0, 1, 0.2)) + 
  
  labs( 
    
    x = "Shrub Land Cover", 
    
    y = "Predicted Muskox Detections per Week" 
    
  ) + 
  
  
  
  theme_minimal() 



#Predator prediction code: 
  
  predator_data <- model_variables %>% 
  
  filter(season == "Summer") %>% 
  
  ungroup() %>% 
  
  modelr::data_grid( 
    
    TRI_extracted = 0, 
    
    Shrub = 0, 
    
    Predators = seq(0, 5, by = 0.1), 
    
    N_days_effort = 7, 
    
    cluster = "new" 
    
  ) 

# Step 2: Generate predictions 

predator_pred <- predict( 
  
  mod_pred_sum_1, 
  
  newdata = predator_data, 
  
  type = "link", 
  
  se.fit = TRUE, 
  
  re.form = NA 
  
) 

# Step 3: Calculate predictions and 95% confidence intervals 

predators_prediction <- predator_data %>% 
  
  mutate( 
    
    fit_link = predator_pred$fit, 
    
    se_link = predator_pred$se.fit, 
    
    
    
    predicted_muskox_week = exp(fit_link), 
    
    lower = exp(fit_link - 1.96 * se_link), 
    
    upper = exp(fit_link + 1.96 * se_link) 
    
  ) 

# Step 4: Plot 

ggplot(predators_prediction, 
       
       aes(x = Predators, 
           
           y = predicted_muskox_week)) + 
  
  # 95% confidence interval 
  
  geom_ribbon( 
    
    aes(ymin = lower, ymax = upper), 
    
    alpha = 0.2, 
    
    fill = "grey70" 
    
  ) + 
  
  # Prediction line 
  
  geom_line(linewidth = 1) + 
  
  # Observed predator detections 
  
  geom_rug( 
    
    data = model_variables %>% filter(season == "Summer"), 
    
    aes(x = Predators, y = 0), 
    
    inherit.aes = FALSE, 
    
    sides = "b", 
    
    alpha = 0.3, 
    
    position = position_jitter(height = 0.02) 
    
  ) + 
  
  # Axis scales 
  
  scale_x_continuous(breaks = 0:5) + 
  
  scale_y_continuous(breaks = 0:4) + 
  
  coord_cartesian( 
    
    xlim = c(0, 5), 
    
    ylim = c(0, 4) 
    
  ) + 
  
  
  
  # Labels 
  
  labs( 
    
    x = "Number of Predators", 
    
    y = "Predicted Muskox Detections per Week" 
    
  ) + 
  
  
  
  theme_minimal() 



#generating 95% CI 
# Extract model coefficients 

coef_table <- summary(mod_pred_sum_1)$coefficients$cond 

# Calculate 95% confidence intervals 

CI_lower <- coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"] 

CI_upper <- coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"] 


# Create table 

Table_2 <- data.frame( 
  
  Variable = rownames(coef_table), 
  
  Beta = coef_table[, "Estimate"], 
  
  SE = coef_table[, "Std. Error"], 
  
  CI_Lower = CI_lower, 
  
  CI_Upper = CI_upper, 
  
  P_value = coef_table[, "Pr(>|z|)"] 
  
) 

Table_2 








SCANFI_landcover_cropped <- rast("spatial/SCANFI_landcover_cropped.tif")

tdn_boundary_rast <- rasterize(vect(TDN_boundary), SCANFI_landcover_cropped, field = "Name") 

landcover_tdn <- crosstab(c(tdn_boundary_rast, SCANFI_landcover_cropped), long = TRUE) 

landcover_tdn_table <- landcover_tdn %>%
  as_tibble() %>%
  group_by(Name) %>%
  mutate(landcover_prop = n/sum(n))

SCANFI_landcover_cameras <- camera_locations %>%
  mutate(landcover = terra::extract(SCANFI_landcover_cropped, camera_locations))


#map of tdn boundary, scanfi landcover and camera locations
ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) + 
  geom_sf(data = camera_locations, color = "limegreen", size = 3) +  
  #geom_sf(data = comb_overlap_SCANFI_and_selected_mammals_week_df$Muskox)
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Plot of cropped_SCANFI_TDN_boundary and camera_locations")


#dominate land cover classes in TDN 
cropped_SCANFI_TDN_Boundary <- crop(SCANFI_landcover_cropped, TDN_boundary, mask = TRUE)

Camera_buffer_zones <- rasterize(vect(camera_buffer), SCANFI_landcover_cropped, field = "location") 

overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

# Ecoregions
TDN_ecoregions_2 <- readRDS("Data/Raw/Ecoregions/TDN_ecoregions.rds") %>%
  group_by(ECO2_NAM_1) %>%
  dplyr::summarise(geometry = st_union(geometry)) # Combine geometries per group

TDN_ecoregions <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/FMD_NWT_EcoRegions.shp")
  

#looking at the dominant landcover class for each camera 

camera_dominant_cover <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  slice_max(n, with_ties = FALSE) %>%  # dominant landcover per camera
  ungroup()

camera_counts <- camera_dominant_cover %>%
  count(cover, name = "n_cameras")

camera_counts


#making TRI map
library(terra)

# Make sure boundary and raster have same CRS
TDN_boundary_vect <- vect(st_transform(TDN_boundary, crs(TDN_tri_arctic)))

# Crop
TRI_crop <- crop(TDN_tri_arctic, TDN_boundary_vect)

# Mask
TRI_mask <- mask(TRI_crop, TDN_boundary_vect)

ggplot() +
  tidyterra::geom_spatraster(
    data = TRI_mask,
    aes(fill = TRI)
  ) +
  geom_sf(
    data = TDN_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_viridis_c(
    name = "Terrain Ruggedness Index",
    option = "magma"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

library(terra)
library(sf)
library(ggplot2)
library(tidyterra)

# Reproject TRI raster to WGS84
TRI_wgs84 <- terra::project(
  TDN_tri_arctic,
  "EPSG:4326"
)

# Reproject boundary to WGS84
TDN_boundary_wgs84 <- st_transform(
  TDN_boundary,
  crs = 4326
)

# Convert boundary to terra vector
TDN_boundary_vect <- terra::vect(TDN_boundary_wgs84)

# Crop and mask TRI raster to boundary
TRI_wgs84_crop <- terra::crop(
  TRI_wgs84,
  TDN_boundary_vect
)

TRI_wgs84_mask <- terra::mask(
  TRI_wgs84_crop,
  TDN_boundary_vect
)

# Check raster layer name
names(TRI_wgs84_mask)

# Plot
ggplot() +
  
  tidyterra::geom_spatraster(
    data = TRI_wgs84_mask,
    aes(fill = TRI)
  ) +
  
  geom_sf(
    data = TDN_boundary_wgs84,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  
  scale_fill_viridis_c(
    name = "Terrain Ruggedness Index",
    option = "magma"
  ) +
  
  coord_sf(
    expand = FALSE
  ) +
  
  labs(
    title = "Terrain Ruggedness Index"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# ---------------------------------------------------
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)

# ---------------------------------------------------
# 1. TRI raster (your true source)
# ---------------------------------------------------
TRI <- terra::terrain(ArcticDEM, v = "TRI")

# ---------------------------------------------------
# 2. IMPORTANT: match CRS BEFORE cropping
# ---------------------------------------------------
TDN_boundary_proj <- st_transform(
  TDN_boundary,
  crs = crs(TRI)
)

# convert to terra vector AFTER transformation
TDN_vect <- terra::vect(TDN_boundary_proj)

# ---------------------------------------------------
# 3. Crop + mask (NOW it will work)
# ---------------------------------------------------
TRI_crop <- terra::crop(TRI, TDN_vect)
TRI_mask <- terra::mask(TRI_crop, TDN_vect)

# check range (this is your TRUE working TRI)
global(TRI_mask, range, na.rm = TRUE)

# ---------------------------------------------------
# 4. Extract for model (same raster!)
# ---------------------------------------------------
TRI_extracted <- terra::extract(TRI_mask, camera_locations)

# ---------------------------------------------------
# 5. OPTIONAL: reproject ONLY for plotting
# ---------------------------------------------------
TRI_plot <- terra::project(TRI_mask, "EPSG:4326")

TDN_boundary_wgs84 <- st_transform(TDN_boundary, 4326)

# ---------------------------------------------------
# 6. Plot
# ---------------------------------------------------
ggplot() +
  tidyterra::geom_spatraster(
    data = TRI_mask
  ) +
  scale_fill_viridis_c(
    name = "Terrain Ruggedness Index",
    option = "magma"
  ) +
  theme_minimal()
  geom_sf(
    data = TDN_boundary_wgs84,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_viridis_c(
    name = "Terrain Ruggedness Index",
    option = "magma"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )



  library(terra)
  library(sf)
  library(ggplot2)
  library(tidyterra)
  
  # ---------------------------------------------------
  # 1. Create TRUE Terrain Ruggedness Index from DEM
  # ---------------------------------------------------
  TRI <- terra::terrain(
    ArcticDEM,
    v = "TRI"
  )
  
  # Check full raster range (sanity check)
  global(TRI, range, na.rm = TRUE)
  
  # ---------------------------------------------------
  # 2. Align boundary CRS to TRI raster
  # ---------------------------------------------------
  TDN_vect <- terra::vect(TDN_boundary)
  TDN_vect <- terra::project(TDN_vect, crs(TRI))
  
  # ---------------------------------------------------
  # 3. Crop + mask to study area
  # ---------------------------------------------------
  TRI_mask <- terra::mask(
    terra::crop(TRI, TDN_vect),
    TDN_vect
  )
  
  # Check final study-area range
  global(TRI_mask, range, na.rm = TRUE)
  
  # ---------------------------------------------------
  # 4. TRUE TRI MAP
  # ---------------------------------------------------
  
  TRI_wgs84 <- terra::project(TRI_mask, "EPSG:4326")
  
  TDN_wgs84 <- st_transform(TDN_boundary, 4326)
  
  
  ggplot() +
    tidyterra::geom_spatraster(
      data = TRI_wgs84
    ) +
    geom_sf(
      data = TDN_wgs84,
      fill = NA
    ) +
    scale_fill_viridis_c(option = "magma") +
    coord_sf(expand = FALSE) +
    theme_void()
  
  ggplot() +
    
    tidyterra::geom_spatraster(
      data = TRI_mask
    ) +
    
    geom_sf(
      data = TDN_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.5
    ) +
    
    scale_fill_viridis_c(
      name = "Terrain Ruggedness Index",
      option = "magma"
    ) +
    
    coord_sf(expand = FALSE) +
    
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  library(terra)
  library(sf)
  library(ggplot2)
  library(tidyterra)
  
  # ---------------------------------------------------
  # 1. Use YOUR real TRI raster
  # ---------------------------------------------------
  TRI <- TDN_TRI
  
  # ---------------------------------------------------
  # 2. Boundary to same CRS
  # ---------------------------------------------------
  TDN_vect <- terra::vect(TDN_boundary)
  TDN_vect <- terra::project(TDN_vect, crs(TRI))
  
  # ---------------------------------------------------
  # 3. Crop + mask
  # ---------------------------------------------------
  TRI_mask <- terra::mask(
    terra::crop(TRI, TDN_vect),
    TDN_vect
  )
  
  # ---------------------------------------------------
  # 4. Check correct range
  # ---------------------------------------------------
  global(TRI_mask, range, na.rm = TRUE)
  
  # ---------------------------------------------------
  # 5. Plot TRUE TRI map
  # ---------------------------------------------------
  
  ggplot() +
    
    tidyterra::geom_spatraster(
      data = TRI_mask
    ) +
    
    geom_sf(
      data = TDN_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.5
    ) +
    
    scale_fill_viridis_c(
      option = "magma",
      na.value = "transparent"
    ) +
    
    coord_sf(expand = FALSE, clip = "on") +
    
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border = element_blank()
    )

  library(terra)
  
 
  
  # 2. Plot
  # ggplot() +
  #   
  #   tidyterra::geom_spatraster(data = TRI_class) +
  #   
  #   geom_sf(data = TDN_boundary, fill = NA) +
  #   
  #   scale_fill_manual(
  #     values = tri_cols,
  #     name = "Terrain Ruggedness Index",
  #     labels = c(
  #       "0–50",
  #       "50–100",
  #       "100–200",
  #       "200–400",
  #       "400–900"
  #     ),
  #     na.value = "transparent",
  #     na.translate = FALSE
  #   ) +
  #   
  #   theme_void()
  
  # 
  # ggplot() +
  #   
  #   annotation_map_tile(type ="cartolight", zoom = 9) +
  #   
  #   tidyterra::geom_spatraster(data = TRI_web, alpha = 0.85) +
  #   
  #   geom_sf(data = TDN_web, fill = NA, colour = "black") +
  #   
  #   scale_fill_manual(
  #     values = tri_cols,
  #     name = "Terrain Ruggedness Index",
  #     labels = c("0–50","50–100","100–200","200–400","400–900"),
  #     na.value = "transparent",
  #     na.translate = FALSE
  #   ) +
  #   
  #   coord_sf(crs = sf::st_crs(3857)) +
  #   
  #   theme_void() +
  #   
  #   theme(
  #     legend.position = c(0.98, 0.08),
  #     legend.justification = c(1, 0),
  #     legend.background = element_rect(fill = "white", colour = "NA", linewidth = 0.3),
  #     legend.title = element_text(size = 10),
  #     legend.text = element_text(size = 9)
  #   )
  
  breaks <- c(0, 25, 50, 75, 150, 10000)

  
  # 1. Define colours FIRST
  tri_cols <- c(
    "0" ="grey",
    "1" = "black",
    "2" = "gold",
    "3" = "firebrick1",
    "4" = "olivedrab3",
    "5" = "royalblue1"
  )
  
  TDN_TRI <- rast("~/Desktop/Analysis/Learning/learning/Spatial/TDN_TRI")
  
  plot(TDN_TRI)
  TRI <- TDN_TRI

  TDN_vect <- terra::vect(TDN_boundary)
  TDN_vect <- terra::project(TDN_vect, crs(TRI))
  
  TRI_mask <- terra::mask(
    terra::crop(TRI, TDN_vect),
    TDN_vect)
  
 
  TRI_class <- terra::classify(
    TRI_mask,
    rcl = cbind(breaks[-length(breaks)], breaks[-1], 1:5)
  )
  
  plot(TRI_class)
  
  TRI_web <- terra::project(TRI_class, "EPSG:3857")
  TRI_web <- as.factor(TRI_web)
  TDN_web <- sf::st_transform(TDN_boundary, 3857)
  
  #plotting TRI Map with Base Map 
  ggplot() +
    
    annotation_map_tile(type = "cartolight", zoom = 8) +
    
    tidyterra::geom_spatraster(data = TRI_web, alpha = 0.85) +
    
    geom_sf(data = TDN_web, fill = NA, colour = "black") +
    scale_fill_grass_b(palette = "celsius", breaks = seq(0, 900, 50)) +
    # scale_fill_manual(
    #   values = tri_cols,
    #   name = "Terrain\nRuggedness Index",
    #   labels = c("0–50", "50–100", "100–200", "200–400", "400–900"),
    #   na.value = "transparent",
    #   na.translate = FALSE
    # ) +
    coord_sf(crs = sf::st_crs(3857)) +
    
    theme_void() +
    theme(
      legend.position = c(0.99, 0.08),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", colour = NA),
      
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA)
    )
  
  # With discrete values
  factor <- volcano2_rast |>
    mutate(cats = cut(elevation,
                      breaks = c(100, 120, 130, 150, 170, 200),
                      labels = c(
                        "Very Low", "Low", "Average", "High",
                        "Very High"
                      )
    ))
  
  ggplot() +
    geom_spatraster(data = factor, aes(fill = cats)) +
    scale_fill_grass_d(palette = "soilmoisture")
  
  #saving to upload into QGIS
  terra::writeRaster(
    TRI_mask,
    "TRI_mask.tif",
    overwrite = TRUE
  )
  
  
  #new code from Val
  library(terra)
  library(tidyterra)
  library(ggplot2)
  library(ggspatial)
  

  
  # Load the raster (path will be different)
  TDN_TRI <- rast("~/Desktop/Analysis/Learning/learning/Spatial/TDN_TRI") |>
    # Trim the NAs away
    trim()
  
  # Plot to see
  plot(TDN_TRI)
  
  # Reclassify
  TRI_class <- TDN_TRI  |>
    mutate(cats = cut(TDN_TRI,
                      breaks = breaks,
                      labels = labels))
  
  # Plot to check reclass has worked
  plot(TRI_class[["cats"]])
  
  #plotting TRI Map with Base Map
  ggplot() +
    
    annotation_map_tile(type = "cartolight", zoom = 8) +
    
    geom_spatraster(data = TRI_class, aes(fill = cats)) +
    
    # This is commented out for me, since I don't have the TDN_web
    # But don't forget it, I remove the code for it since it was
    # confusing me
    geom_sf(data = TDN_web, fill = NA, colour = "black") +
    
    scale_fill_manual(
      values = tri_cols, # defined above
      name = "Terrain\nRuggedness Index",
      labels = labels, # defined above
      na.value = "transparent",
      na.translate = FALSE
    ) +
    
    coord_sf() +
    theme_void() +
    theme(
      legend.position = c(0.99, 0.08),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA)
    )
  
 #removing code
  library(terra)
  library(sf)
  library(ggplot2)
  library(tidyterra)
  
 
  
  cats_raster <- TRI_class[["cats"]]
  tdn        <- TDN_web
  
  # (no need to reproject unless you want; keeping UTM consistent is fine)
  # cats <- terra::project(cats, "EPSG:32612")
  # tdn  <- st_transform(tdn, 32612)
  
  # define breaks, labels and color => they will correspond to each other
  breaks <- c(-1, 1, 50, 100, 250, 1000)
  labels <- c("0–1", "1–50", "50–100", "100–250", "250–1000")
  tri_cols <- c("black","skyblue4","yellow","mediumturquoise","magenta3")
  
  library(ggplot2)
  library(terra)
  library(sf)
  library(tidyterra)
  
  cats_raster <- TRI_class[["cats"]]
  
  ggplot() +
    
    geom_spatraster(data = cats_raster) +
    
    geom_sf(data = TDN_web,
            fill = NA,
            colour = "black",
            linewidth = 0.6) +
    
    scale_fill_manual(
      values = tri_cols,
      name = "Terrain\nRuggedness Index",
      labels = labels,
      na.value = "transparent",
      na.translate = FALSE
    ) +
    
    coord_sf(expand = FALSE) +
    
    theme_void() +
    
    theme(
      legend.position = c(0.8, 0.4),
      legend.justification = c(0, 0.5),
      
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7)
    )

  library(ggplot2)
  library(terra)
  library(sf)
  library(tidyterra)
  library(ggspatial)
  
  ggplot() +
    
    geom_spatraster(data = cats_raster) +
    
    geom_sf(data = TDN_web,
            fill = NA,
            colour = "black",
            linewidth = 0.6) +
    
    geom_sf(data = camera_buffer,
            colour = "red",
            size = 1.5) +
    
    scale_fill_manual(
      values = tri_cols,
      name = "Terrain\nRuggedness Index",
      labels = labels,
      na.value = "transparent",
      na.translate = FALSE
    ) +
    
    coord_sf(expand = FALSE) +
    
    annotation_scale(
      location = "bl",
      unit_category = "metric",
      width_hint = 0.25,
      pad_x = unit(5, "cm"),
      pad_y = unit(2.5, "cm")
    ) +
    
    theme_void() +
    
    theme(
      legend.position = c(0.8, 0.4),
      legend.justification = c(0, 0.5),
      
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7),
      
      # legend.background = element_rect(
      #   fill = "white",
      #   colour = "black",
      #   linewidth = 0.4
      # ),
      
      legend.box.margin = margin(6, 6, 6, 6),
      legend.spacing = unit(4, "pt")
    )


  library(dplyr)
  
  model_variables1 <- model_variables %>%
    mutate(camera_id = paste(cluster, camera, sep = "_"))
  
  
  
  
  
  
  