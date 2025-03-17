


#checking data with protocols from zuur et al. 2010



#looking for explanatory variable outliers
combined_variables_and_fire |> 
  mutate(distance_to_esker_m = as.numeric(distance_to_esker_m)) %>%
  dplyr::select(c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "Gray Wolf", "Grizzly Bear", "Barren-ground Caribou", "elevation", "distance_to_esker_m", "fire_age0", "fire_age1", "fire_age2", "fire_age3")) %>%
  pivot_longer(cols = c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Water", "Shrub", "Herbs", "Gray Wolf", "Grizzly Bear", "Barren-ground Caribou", "elevation", "distance_to_esker_m", "fire_age0", "fire_age1", "fire_age2", "fire_age3")) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(combined_variables_and_fire), each = n_distinct(name)))) +
  #geom_point(aes(x = value, y = name)) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking for muskox outliers
combined_variables_and_fire |> 
  #mutate(distance_to_esker_m = as.numeric(distance_to_esker_m)) %>%
  dplyr::select(c(Muskox)) %>%
  pivot_longer(cols = c(Muskox)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(combined_variables_and_fire), each = 1))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking at homogeneity for variance
combined_variables_and_fire |> 
  ggplot() +
  geom_boxplot(aes(y = Muskox, x = cluster)) +
  theme_bw()


#What are the relationships between Y and X variables?
Z <- as.vector(as.matrix(combined_variables_and_fire[, c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "Gray Wolf", "Grizzly Bear", "Barren-ground Caribou", "elevation", "distance_to_esker_m", "fire_age0", "fire_age1", "fire_age2", "fire_age3")]))
Y10 <- rep(combined_variables_and_fire$Muskox, 16)
MyNames <- names(combined_variables_and_fire[,c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "Gray Wolf", "Grizzly Bear", "Barren-ground Caribou", "elevation", "distance_to_esker_m", "fire_age0", "fire_age1", "fire_age2", "fire_age3")])
ID10 <- rep(MyNames, each = length(combined_variables_and_fire$Muskox))
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

combined_variables_and_fire %>%
  mutate(log_distance_to_esker = log(distance_to_esker)) %>%
  dplyr::select("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Water", "Herbs", "Gray Wolf", "Grizzly Bear", "elevation", "log_distance_to_esker", "fire_age0", "fire_age1", "fire_age2", "fire_age3") %>%
  cor()




