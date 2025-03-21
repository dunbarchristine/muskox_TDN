




overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

#adding the land cover names column to overlap_SCANFI_cameras_table
# 
# overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
#   mutate(land_cover_name = case_when(
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 1 ~ "Bryoid",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 2 ~ "Herbs",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 3 ~ "Rock",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 4 ~ "Shrub",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 5 ~ "Treed broadleaf",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 6 ~ "Treed conifer",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 7 ~ "Treed mixed",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 8 ~ "Water"
#   ))

# Spread the data into separate columns for each land cover type
overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
  dplyr::select(-n) %>%  
  pivot_wider(
    names_from = cover,  # Create a column for each land cover type
    values_from = landcover_prop,  # Take values from the 'land_cover_prop' column
    values_fill = list(landcover_prop = 0)  # Fill missing values with 0 (no land cover)
  )

#combining overlap_SCANFI_cameras_table and selected_mammals_week to become one dataset called

# Merge the datasets based on a common column (e.g., "location")
comb_overlap_SCANFI_and_selected_mammals_week<- merge(overlap_SCANFI_cameras_table, 
                                                      selected_mammals_week, 
                                                      by = "location",   # Column in both datasets that should be used to join
                                                      all.x = TRUE)      # Keep all rows from overlap_SCANFI_cameras_table (left join)


#exporting "select_mammals_week" to an excel file
# Write to Excel
write_xlsx(comb_overlap_SCANFI_and_selected_mammals_week, "comb_overlap_SCANFI_and_selected_mammals_week.xlsx")


# Merge the datasets based on 'camera_id'
comb_overlap_SCANFI_and_selected_mammals_week <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                                                       camera_locations_df[, c("location", "elevation")], 
                                                       by = "location", 
                                                       all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

# Convert tibble to a regular data.frame (if necessary)
camera_locations_df <- as.data.frame(camera_locations_df)

# Now perform the left join
comb_overlap_SCANFI_and_selected_mammals_week <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  dplyr::left_join(camera_locations_df %>% dplyr::select(location, distance_to_esker), 
                   by = "location")

# # Remove the 'm' and convert to numeric
# comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker <- 
#   as.numeric(gsub("m", "", comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker))
# 
# # Rename the column to include 'm'
# colnames(comb_overlap_SCANFI_and_selected_mammals_week)[
#   which(names(comb_overlap_SCANFI_and_selected_mammals_week) == "distance_to_esker")] <- "distance_to_esker_m"

#moving the column "cluster" to the dataset combined_variables_fire

# Assuming both datasets have a common key for joining (e.g., an ID column like "id")
combined_variables_and_fire <- combined_variables_and_fire %>%
  left_join(dplyr::select(comb_overlap_SCANFI_and_selected_mammals_week_df, location, cluster), by = "location")


#creating a dataset with only variables I will be testing. Removing location, cluster, year, week columns. Doing this so I can run a correlation test.

variables_only <- all_variables %>%
  ungroup() %>%  # Ungroup the dataset to prevent warnings
  select(Treed_broadleaf, Treed_conifer, Treed_mixed, Bryoid, Shrub, Water, Herbs,
         gray_wolf, grizzly_bear, elevations, esker_camera_distances,
         fire_age0, fire_age1, fire_age2, fire_age3, fire_age4, log_esker_camera_distances,
         grizzly_per_day, gray_wolf_per_day) %>%
  select(-matches("^location$"))








