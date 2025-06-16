
library(ggcorrplot)
library(corrplot)
#checking data with protocols from zuur et al. 2010


#looking for explanatory variable outliers
model_variables |> 
  mutate(log_esker_camera_distances = as.numeric(esker_camera_distances)) %>%
  dplyr::select(c("Treed_broadleaf", "Treed_conifer", "Treed_mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Arctic_DEM_500m_elevation_m", "esker_camera_distances", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4", "TRI_extracted", "grizzly_per_day", "gray_wolf_per_day")) %>%
  pivot_longer(cols = c("Treed_broadleaf", "Treed_conifer", "Treed_mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Arctic_DEM_500m_elevation_m", "esker_camera_distances", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4", "TRI_extracted", "grizzly_per_day", "gray_wolf_per_day")) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(model_variables), each = n_distinct(name)))) +
  #geom_point(aes(x = value, y = name)) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#was getting error message from the code above saying: error in 'pivot_longer()': "cant combine 'treed broadleaf' <double> and 'ECO2_NAM_1' <character>. Chatgpt gave me this code: and it seemed to work?
model_variables |>
  mutate(log_esker_camera_distances = as.numeric(esker_camera_distances)) %>%
  dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything()) |>
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(model_variables), each = n_distinct(name)))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable", y = "Order of the data") +
  theme_bw()


#looking for muskox outliers
model_variables |> 
  #mutate(distance_to_esker_m = as.numeric(distance_to_esker_m)) %>%
  dplyr::select(c(Muskox)) %>%
  pivot_longer(cols = c(Muskox)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(model_variables), each = 1))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking at homogeneity for variance
model_variables |> 
  ggplot() +
  geom_boxplot(aes(y = Muskox, x = cluster)) +
  theme_bw()


#What are the relationships between Y and X variables?
Z <- as.vector(as.matrix(model_variables[, c("Treed_broadleaf", "Treed_conifer", "Treed_mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Arctic_DEM_500m_elevation_m", "log_esker_camera_distances", "esker_camera_distances","fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4", "TRI_extracted", "season")]))
Y10 <- rep(model_variables$Muskox, 19)
MyNames <- names(model_variables[,c("Treed_broadleaf", "Treed_conifer", "Treed_mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "Arctic_DEM_500m_elevation_m", "esker_camera_distances", "log_esker_camera_distances", "fire_age0", "fire_age1", "fire_age2", "fire_age3", "fire_age4", "TRI_extracted", "season")])
ID10 <- rep(MyNames, each = length(model_variables$Muskox))
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
cor_matrix <- cor(model_variables, use = "complete.obs") #complete.obs ensures that only complete cases (rows with no missing values) are used to compute the correlation.

# Select only numeric columns
numeric_data <- model_variables %>% select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Basic plot
corrplot(cor_matrix, method = "color")


