

#the extents didnt match so Chat GPT told me to do this
SCANFI_landcover_cropped_aligned <- crop(SCANFI_landcover_cropped, Camera_buffer_zones)
SCANFI_landcover_resampled <- resample(SCANFI_landcover_cropped_aligned, Camera_buffer_zones, method = "near")

#now run this
overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_resampled), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

#creating new table with just numeric variables to run correlations
overlap_SCANFI_cameras_table_variables_only <- overlap_SCANFI_cameras_table[, -1]

# Spread the data into separate columns for each land cover type
overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
  dplyr::select(-n) %>%  
  pivot_wider(
    names_from = SCANFI_att_nfiLandCover_SW_2020_v1.2,  # Create a column for each land cover type
    values_from = landcover_prop,  # Take values from the 'land_cover_prop' column
    values_fill = list(landcover_prop = 0)  # Fill missing values with 0 (no land cover)
  )

#adding the land cover names column to overlap_SCANFI_cameras_table
overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
  rename(
    Bryoid = `1`,
    Herbs = `2`,
    Rock = `3`,
    Shrub = `4`,
    `Treed broadleaf` = `5`,
    `Treed conifer` = `6`,
    `Treed mixed` = `7`,
    Water = `8`
  )





#combining overlap_SCANFI_cameras_table and selected_mammals_week to become one dataset called
# Merge the datasets based on a common column (e.g., "location")
comb_overlap_SCANFI_and_selected_mammals_week<- merge(overlap_SCANFI_cameras_table, 
                                                      selected_mammals_week, 
                                                      by = "location",   # Column in both datasets that should be used to join
                                                      all.x = TRUE)      # Keep all rows from overlap_SCANFI_cameras_table (left join)


# Write to Excel
write_xlsx(comb_overlap_SCANFI_and_selected_mammals_week, "comb_overlap_SCANFI_and_selected_mammals_week.xlsx")

# Convert camera_locations to a data frame if it's an sf object
camera_locations_df <- st_as_sf(camera_locations) %>%
  st_drop_geometry() 

# Merge the datasets based on 'camera_id'
comb_overlap_SCANFI_and_selected_mammals_week <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                                                       camera_locations_df[, c("location", "elevation")], 
                                                       by = "location", 
                                                       all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

saveRDS(comb_overlap_SCANFI_and_selected_mammals_week, "~/Desktop/Analysis/Learning/learning/RDS files/comb_overlap_SCANFI_and_selected_mammals_week.rds")
comb_overlap_SCANFI_and_selected_mammals_week <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/comb_overlap_SCANFI_and_selected_mammals_week.rds")

# Convert tibble to a regular data.frame (if necessary)
camera_locations_df <- as.data.frame(camera_locations_df)



#adding tdn ecoregions to all_variables_with_tri_and_species
model_variables <- merge(all_variables_with_tri_and_species, 
                                            camera_locations_df[, c("location")], 
                                            by = "location", 
                                          all.x = TRUE) 
#saving model_variables as RDS
saveRDS(model_variables, "~/Desktop/Analysis/Learning/learning/RDS files/model_variables.rds")
model_variables <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/model_variables.rds")


# #making new dataset with just variables I will be testing
# model_variables_only <- model_variables %>%
#   ungroup() %>%  # Ungroup the dataset to prevent warnings
#   select(`Treed broadleaf`, `Treed conifer`, `Treed mixed`, Bryoid, Shrub, Water, Herbs,
#          gray_wolf, grizzly_bear, Muskox, Elevation, season, esker_camera_distances,
#          fire_age0, fire_age1, fire_age2, fire_age3, fire_age4, n_days_effort, cluster, log_esker_camera_distances,
#          grizzly_per_day, gray_wolf_per_day, TRI_extracted, ECO2_NAM_1) %>%
#   select(-matches("^location$"))



#renaming columns/removing columns
model_variables <- model_variables %>%
  select(-Arctic_DEM_500m_elevation_m.y, -geometry.y) %>%  # Remove unwanted columns
  rename(
    Arctic_DEM_500m_elevation_m = Arctic_DEM_500m_elevation_m.x,
    geometry = geometry.x
  )


write.csv(model_variables, file = "~/Desktop/Analysis/Learning/CSV Files/model_variables.csv")











