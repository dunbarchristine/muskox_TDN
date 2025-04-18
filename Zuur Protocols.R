


#checking data with protocols from zuur et al. 2010


#looking for explanatory variable outliers
all_variables |> 
  mutate(log_esker_camera_distances = as.numeric(esker_camera_distances)) %>%
  dplyr::select(c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs","gray_wolf", "grizzly_bear","Barren-ground Caribou", "elevations", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4")) %>%
  pivot_longer(cols = c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Water", "Shrub", "Herbs", "gray_wolf", "grizzly_bear","Barren-ground Caribou", "elevations", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4")) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(all_variables), each = n_distinct(name)))) +
  #geom_point(aes(x = value, y = name)) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking for muskox outliers
all_variables |> 
  #mutate(distance_to_esker_m = as.numeric(distance_to_esker_m)) %>%
  dplyr::select(c(Muskox)) %>%
  pivot_longer(cols = c(Muskox)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(all_variables), each = 1))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking at homogeneity for variance
all_variables |> 
  ggplot() +
  geom_boxplot(aes(y = Muskox, x = cluster)) +
  theme_bw()


#What are the relationships between Y and X variables?
Z <- as.vector(as.matrix(all_variables[, c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Barren-ground Caribou", "elevations", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4")]))
Y10 <- rep(all_variables$Muskox, 17)
MyNames <- names(all_variables[,c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Barren-ground Caribou", "elevations", "esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4")])
ID10 <- rep(MyNames, each = length(all_variables$Muskox))
tibble(response = Y10, vars = Z, varnames = ID10) %>%
  ggplot(aes(x = vars, y = response)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~varnames, scales = "free")

#creating a binned instead to see/understand data more clearly
temp <- tibble(response = Y10, vars = Z, varnames = ID10) %>%
  mutate(response_binned = case_when(
    response == 0 ~ "0",
    response <= 50 ~ "0-50",
    TRUE ~ ">50"
  ))

all_variables %>%
  ungroup() %>%
  mutate(log_esker_camera_distances = log(esker_camera_distances + 1)) %>%
  dplyr::select("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Barren-ground Caribou", "elevations", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4") %>%
  cor()

#running a correlation test with dataset "variables_only". This dataset includes just the variables I will be using in my models, not including location, camera cluster, year, week, etc.

# Compute the correlation matrix for all numeric variables in 'variables_only'
cor_matrix <- cor(variables_only, use = "complete.obs") #complete.obs ensures that only complete cases (rows with no missing values) are used to compute the correlation.

#comparing scanfi and lcc to determine which landcover dataset to use/justification for choosing lc/scanfi
lcc_scanfi_cor <- cor(lcc_cameras_prop_columns_variables_only, overlap_SCANFI_cameras_table_variables_only)




