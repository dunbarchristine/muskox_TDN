


#checking data with protocols from zuur et al. 2010

#looking for explanatory variable outliers
comb_overlap_SCANFI_and_selected_mammals_week |> 
  mutate(distance_to_esker = as.numeric(distance_to_esker)) %>%
  dplyr::select(c(`Treed broadleaf`, `Treed conifer`, `Treed mixed`, Bryoid, Shrub, Water, Herbs, gray_wolf, grizzly_bear, elevation, distance_to_esker)) %>%
  pivot_longer(cols = c(`Treed broadleaf`, `Treed conifer`, `Treed mixed`, Bryoid, Shrub, Water, Herbs, gray_wolf, grizzly_bear, elevation, distance_to_esker)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(comb_overlap_SCANFI_and_selected_mammals_week), each = 11))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking for muskox outliers
comb_overlap_SCANFI_and_selected_mammals_week |> 
  mutate(distance_to_esker = as.numeric(distance_to_esker)) %>%
  dplyr::select(c(Muskox)) %>%
  pivot_longer(cols = c(Muskox)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(comb_overlap_SCANFI_and_selected_mammals_week), each = 1))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()

#looking at homogeneity for variance
comb_overlap_SCANFI_and_selected_mammals_week |> 
  ggplot() +
  geom_boxplot(aes(y = Muskox, x = cluster)) +
  theme_bw()


#What are the relationships between Y and X variables?
Z <- as.vector(as.matrix(comb_overlap_SCANFI_and_selected_mammals_week[, c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "elevation", "distance_to_esker")]))
Y10 <- rep(comb_overlap_SCANFI_and_selected_mammals_week$Muskox, 11)
MyNames <- names(comb_overlap_SCANFI_and_selected_mammals_week[,c("Treed broadleaf", "Treed conifer", "Treed mixed", "Bryoid", "Shrub", "Water", "Herbs", "gray_wolf", "grizzly_bear", "elevation", "distance_to_esker")])
ID10 <- rep(MyNames, each = length(comb_overlap_SCANFI_and_selected_mammals_week$Muskox))
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
