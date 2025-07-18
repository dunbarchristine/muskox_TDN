
install.packages("MASS")
install.packages("MuMIn")

library(vcd)
library(lme4)
library(MuMIn)
library(corrplot)
library(glmmTMB)
library(performance)
library(broom.mixed)
library(stringr)
library(flextable)    
library(MASS)


#making a model set 
#using a negative binomal mixed effects model with camera cluster and camera ID as fixed effects
## model set concertaining entire year of data

#making global models####

mod_null_all_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables)

#annual predation
mod_pred_all_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                          family="nbinom2", data = model_variables) #both grizz and wolf

mod_pred_all_2.1 <-  glmmTMB(Muskox ~ scale(grizzly_bear) + offset(log(n_days_effort)) + (1|cluster),
                             family="nbinom2", data = model_variables) #grizzly bear only

mod_pred_all_2.2 <- glmmTMB(Muskox ~ scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                            family="nbinom2", data = model_variables) #gray wolf only

#annual food
mod_food_all_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) 

mod_food_all_3.1 <- glmmTMB(Muskox ~ scale(Shrub) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just shrub variable (estimate: -0.25)

mod_food_all_3.2 <- glmmTMB(Muskox ~ scale(Herbs) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just herbs variable (estimate: 0.078)

mod_food_all_3.3 <- glmmTMB(Muskox ~ scale(Treed_broadleaf) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just Treed_broadleaf variable (estimate: 0.107)

mod_food_all_3.4 <- glmmTMB(Muskox ~ scale(Bryoid) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just bryoid variable (estimate: -0.0123)

mod_food_all_3.5 <- glmmTMB(Muskox ~ scale(fire_age1) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just fire_age1 variable (estimate: 0.2134)

mod_food_all_3.6 <- glmmTMB(Muskox ~ scale(fire_age2) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just fire_age2 variable (estimate: -0.1442)

mod_food_all_3.7 <- glmmTMB(Muskox ~ scale(fire_age3) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just fire_age3 variable (estimate: 0.008601)

mod_food_all_3.8 <- glmmTMB(Muskox ~ scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just fire_age0 variable (estimate: -0.04603)

mod_food_all_3.9 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running just fire variables for food hypothesis

mod_food_all_3.10 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running just vegetation variables for food hypothesis to compare to just fire variables 

#annual thermoregulation
mod_therm_all_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #all variables I think support thermoregulation hypothesis

mod_therm_all_4.1 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just treed_mixed variable (estimate: -0.1126)

mod_therm_all_4.2 <- glmmTMB(Muskox ~ scale(`Treed_conifer`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just treed_conifer variable (estimate: 0.1482)

mod_therm_all_4.3 <- glmmTMB(Muskox ~ scale(`Water`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just water variable (estimate: 0.08528)

mod_therm_all_4.4 <- glmmTMB(Muskox ~ scale(`log_esker_camera_distances`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #running model with just log_esker_camera_distances variable (estimate: 0.01676)

mod_therm_all_4.5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`log_esker_camera_distances`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(TRI_extracted) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #just landcover variables, no fire

mod_global_all_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) +  
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables) #this model will run with both cluster/location, and just with cluster/ just with location.





#making summer model####
mod_null_sum_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables  %>% filter(season == "Summer"))

#summer predation 
mod_pred_sum_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                          family="nbinom2", data = model_variables %>% filter(season == "Summer")) #both grizz and wolf

mod_pred_sum_2.1 <-  glmmTMB(Muskox ~ scale(grizzly_bear) + offset(log(n_days_effort)) + (1|cluster),
                             family="nbinom2", data = model_variables %>% filter(season == "Summer")) #grizzly bear only

mod_pred_sum_2.2 <- glmmTMB(Muskox ~ scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                         family="nbinom2", data = model_variables %>% filter(season == "Summer")) #gray wolf only

#summer food
mod_food_sum_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 
df = model_variables %>% filter(season == "Summer")
mod_food_sum_3.1 <- glmmTMB(Muskox ~ scale(Shrub) + 
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = df ) #running model with just shrub variable (estimate: -0.25)

mod_food_sum_3.2 <- glmmTMB(Muskox ~ scale(Herbs) +
                            offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just herbs variable (estimate: 0.078)

mod_food_sum_3.3 <- glmmTMB(Muskox ~ scale(Treed_broadleaf) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just Treed_broadleaf variable (estimate: 0.107)

mod_food_sum_3.4 <- glmmTMB(Muskox ~ scale(Bryoid) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just bryoid variable (estimate: -0.0123)

mod_food_sum_3.5 <- glmmTMB(Muskox ~ scale(fire_age1) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just fire_age1 variable (estimate: 0.2134)

mod_food_sum_3.6 <- glmmTMB(Muskox ~ scale(fire_age2) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just fire_age2 variable (estimate: -0.1442)

mod_food_sum_3.7 <- glmmTMB(Muskox ~ scale(fire_age3) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just fire_age3 variable (estimate: 0.008601)

mod_food_sum_3.8 <- glmmTMB(Muskox ~ scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just fire_age0 variable (estimate: -0.04603)

mod_food_sum_3.9 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running just fire variables for food hypothesis

mod_food_sum_3.10 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) +
                            offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running just vegetation variables for food hypothesis to compare to just fire variables 

#summer thermoregulation
mod_therm_sum_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #all variables I think support thermoregulation hypothesis

mod_therm_sum_4.1 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) +
                             offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just treed_mixed variable (estimate: -0.1126)

mod_therm_sum_4.2 <- glmmTMB(Muskox ~ scale(`Treed_conifer`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just treed_conifer variable (estimate: 0.1482)

mod_therm_sum_4.3 <- glmmTMB(Muskox ~ scale(`Water`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just water variable (estimate: 0.08528)

mod_therm_sum_4.4 <- glmmTMB(Muskox ~ scale(`log_esker_camera_distances`) +
                               offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just log_esker_camera_distances variable (estimate: 0.01676)

mod_therm_sum_4.5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`log_esker_camera_distances`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(TRI_extracted) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 



#running global summer model with shrub and not trees. fire_age4 is giving NAs, deleted fire_age4 as it should be used as a reference
mod_global_sum_shrub <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Shrub) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 

#running global summer model with trees and not shrubs. fire_age2 is giving NAs. deleted fire_age4 as it should be used as a reference
mod_global_sum_trees <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Predators) + scale(Groundcover) + scale(Trees) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                  offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 

#running global summer model with shrub values subtracted from tree values. fire_age2 again giving NAs. deleted fire_age4 as it should be used as a reference
mod_global_sum_trees_2 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(tree_shrub) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                  offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer" )%>% mutate(tree_shrub = Trees - Shrub)) 


#looking at stepAIC. code from katerina

step_shrub_both <- stepAIC(mod_global_sum_shrub, direction="both", trace = TRUE) 

summary(mod_global_sum_shrub)
AIC(mod_global_sum_shrub)



#ecoregions########
mod_global_sum_shrub_taiga <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Shrub) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                  offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer", ECO2_NAM_1 == "Taiga Shield")) 


mod_global_sum_trees_taiga <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Trees) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                   offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer", ECO2_NAM_1 == "Taiga Shield")) 


#running global summer model with trees and not shrubs
mod_global_sum_trees_plains <- glmmTMB(Muskox ~ scale(log_esker_camera_distances) + scale(Trees) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                  offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer", ECO2_NAM_1 == "Southern Arctic: Tundra Plains")) 


mod_global_sum_shrubs_plains <- glmmTMB(Muskox ~ scale(log_esker_camera_distances) + scale(Shrub) + scale(Groundcover) + scale(Predators) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                   offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer", ECO2_NAM_1 == "Southern Arctic: Tundra Plains")) 







mod_global_sum_shrubs_trees <- glmmTMB(Muskox ~ scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Trees) + scale(Shrub) + scale(Groundcover) + scale(Predators) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                         offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 

#### trouble shooting cluster/location for summer models ####
#cluster/location. trying to see if shrub and treed conifer estimates are high
mod_therm_sum_5.3 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(Herbs)+ scale(Shrub) + scale(Bryoid) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                               offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #just habitat variables

#location. trying to see if shrub and treed conifer estimates are high
mod_therm_sum_5.4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(Herbs)+ scale(Shrub) + scale(Bryoid) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                               offset(log(n_days_effort)) + (1|location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #just habitat variables

#just cluster. trying to see if shrub and treed conifer estimates are high
mod_therm_sum_5.5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(Herbs)+ scale(Shrub) + scale(Bryoid) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) + 
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #just habitat variables

#looking at cluster, location, cluster/location
mod_global_sum_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age2) + scale(fire_age0) + scale(Shrub) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) +  scale(Arctic_DEM_500m_elevation_m) + 
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #location only. removed esker, fireage2 and bryoid because they were correlated with a few variables

mod_global_sum_5.1 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Shrub) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #cluster/location. removed esker, fireage2 and bryoid because they were correlated with a few variables

mod_global_sum_5.2 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(fire_age1) + scale(log_esker_camera_distances) + scale(fire_age3) + scale(fire_age0) + scale(fire_age2) + scale(Shrub) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) + scale(Arctic_DEM_500m_elevation_m) +
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #cluster only. removed esker, fireage2 and bryoid because they were correlated with a few variables



#making more winter models, most with just one variable. Similar to what I did with the summer models----
mod_null_win_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables  %>% filter(season == "Winter"))

#winter predation
mod_pred_win_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                          family="nbinom2", data = model_variables %>% filter(season == "Winter")) #both grizz and wolf

mod_pred_win_2.1 <-  glmmTMB(Muskox ~ scale(grizzly_bear) + offset(log(n_days_effort)) + (1|cluster),
                             family="nbinom2", data = model_variables %>% filter(season == "Winter")) #grizzly bear only

mod_pred_win_2.2 <- glmmTMB(Muskox ~ scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster),
                            family="nbinom2", data = model_variables %>% filter(season == "Winter")) #gray wolf only

#winter food
mod_food_win_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) 

mod_food_win_3.1 <- glmmTMB(Muskox ~ scale(Shrub) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just shrub variable (estimate: -0.25)

mod_food_win_3.2 <- glmmTMB(Muskox ~ scale(Herbs) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just herbs variable (estimate: 0.078)

mod_food_win_3.3 <- glmmTMB(Muskox ~ scale(Treed_broadleaf) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just Treed_broadleaf variable (estimate: 0.107)

mod_food_win_3.4 <- glmmTMB(Muskox ~ scale(Bryoid) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just bryoid variable (estimate: -0.0123)

mod_food_win_3.5 <- glmmTMB(Muskox ~ scale(fire_age1) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just fire_age1 variable (estimate: 0.2134)

mod_food_win_3.6 <- glmmTMB(Muskox ~ scale(fire_age2) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just fire_age2 variable (estimate: -0.1442)

mod_food_win_3.7 <- glmmTMB(Muskox ~ scale(fire_age3) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just fire_age3 variable (estimate: 0.008601)

mod_food_win_3.8 <- glmmTMB(Muskox ~ scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just fire_age0 variable (estimate: -0.04603)

mod_food_win_3.9 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running just fire variables for food hypothesis

mod_food_win_3.10 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running just vegetation variables for food hypothesis to compare to just fire variables 

#winter thermoregulation
mod_therm_win_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #all variables I think support thermoregulation hypothesis

mod_therm_win_4.1 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just treed_mixed variable (estimate: -0.1126)

mod_therm_win_4.2 <- glmmTMB(Muskox ~ scale(`Treed_conifer`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just treed_conifer variable (estimate: 0.1482)

mod_therm_win_4.3 <- glmmTMB(Muskox ~ scale(`Water`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just water variable (estimate: 0.08528)

mod_therm_win_4.4 <- glmmTMB(Muskox ~ scale(`log_esker_camera_distances`) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #running model with just log_esker_camera_distances variable (estimate: 0.01676)

mod_therm_win_4.5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(`log_esker_camera_distances`) + scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(TRI_extracted) +
                               offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #just landcover variables, no fire

mod_global_win_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) +  
                              offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #this model will run with both cluster/location, and just with cluster/ just with location.



#### model selection ###########

##model selection for global models####
fmListGlobal<-model.sel(mod_null_all_1=mod_null_all_1, mod_pred_all_2=mod_pred_all_2, mod_pred_all_2.1=mod_pred_all_2.1,mod_pred_all_2.2=mod_pred_all_2.2, mod_food_all_3=mod_food_all_3, mod_food_all_3.1=mod_food_all_3.1, mod_food_all_3.2=mod_food_all_3.2, mod_food_all_3.3=mod_food_all_3.3, mod_food_all_3.4=mod_food_all_3.4, mod_food_all_3.5=mod_food_all_3.5, mod_food_all_3.6=mod_food_all_3.6, mod_food_all_3.7=mod_food_all_3.7, mod_food_all_3.8=mod_food_all_3.8, mod_food_all_3.9=mod_food_all_3.9,
                     mod_food_all_3.10=mod_food_all_3.10, mod_therm_all_4=mod_therm_all_4, mod_therm_all_4.1=mod_therm_all_4.1,mod_therm_all_4.2=mod_therm_all_4.2, mod_therm_all_4.3=mod_therm_all_4.3, mod_therm_all_4.4=mod_therm_all_4.4, mod_therm_all_4.5=mod_therm_all_4.5, model_global_all_5=model_global_all_5)
fmListGlobal

##model selection for summer models
fmListSum<-model.sel(mod_null_sum_1=mod_null_sum_1, mod_pred_sum_2=mod_pred_sum_2, mod_pred_sum_2.1=mod_pred_sum_2.1,mod_pred_sum_2.2=mod_pred_sum_2.2, mod_food_sum_3=mod_food_sum_3, mod_food_sum_3.1=mod_food_sum_3.1, mod_food_sum_3.2=mod_food_sum_3.2, mod_food_sum_3.3=mod_food_sum_3.3, mod_food_sum_3.4=mod_food_sum_3.4, mod_food_sum_3.5=mod_food_sum_3.5, mod_food_sum_3.6=mod_food_sum_3.6, mod_food_sum_3.7=mod_food_sum_3.7, mod_food_sum_3.8=mod_food_sum_3.8, mod_food_sum_3.9=mod_food_sum_3.9,
                     mod_food_sum_3.10=mod_food_sum_3.10, mod_therm_sum_4=mod_therm_sum_4, mod_therm_sum_4.1=mod_therm_sum_4.1,mod_therm_sum_4.2=mod_therm_sum_4.2, mod_therm_sum_4.3=mod_therm_sum_4.3, mod_therm_sum_4.4=mod_therm_sum_4.4, mod_therm_sum_4.5=mod_therm_sum_4.5, model_global_sum_5=model_global_sum_5)
fmListSum

## model selection of winter models
fmListWin<-model.sel(mod_null_win_1=mod_null_win_1, mod_pred_win_2=mod_pred_win_2, mod_pred_win_2.1=mod_pred_win_2.1,mod_pred_win_2.2=mod_pred_win_2.2, mod_food_win_3=mod_food_win_3, mod_food_win_3.1=mod_food_win_3.1, mod_food_win_3.2=mod_food_win_3.2, mod_food_win_3.3=mod_food_win_3.3, mod_food_win_3.4=mod_food_win_3.4, mod_food_win_3.5=mod_food_win_3.5, mod_food_win_3.6=mod_food_win_3.6, mod_food_win_3.7=mod_food_win_3.7, mod_food_win_3.8=mod_food_win_3.8, mod_food_win_3.9=mod_food_win_3.9,
                     mod_food_win_3.10=mod_food_win_3.10, mod_therm_win_4=mod_therm_win_4, mod_therm_win_4.1=mod_therm_win_4.1,mod_therm_win_4.2=mod_therm_win_4.2, mod_therm_win_4.3=mod_therm_win_4.3, mod_therm_win_4.4=mod_therm_win_4.4, mod_therm_win_4.5=mod_therm_win_4.5, model_global_win_5=model_global_win_5)
fmListWin

library(visreg)

visreg(mod_food_sum_3)
print(mod_food_sum_3)



#summer model averaging. Just looking for fun, probably wont use this
summer_top_models <- subset(fmListSum, delta <= 2)
summer_modelav <-model.avg(summer_top_models)
summary(summer_modelav)


#### dredging ####

#annual dredge model####
model_annual <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) +  
                          offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = data_summer, na.action = "na.fail")

annual_dredge <- dredge(model_annual, fixed = ~offset(log(n_days_effort)) + (1|cluster))

#summer dredge model with just cluster
data_summer <- model_variables %>% filter(season == "Summer")
model_global_sum_5 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(Shrub) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(TRI_extracted) +  
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = data_summer, na.action = "na.fail")

summer_dredge <- dredge(model_global_sum_5, fixed = ~cond(offset(log(n_days_effort))) + (1|cluster))

#winter dredge model
data_winter <- model_variables %>% filter(season == "Winter") 
model_global_win_5 <- glmmTMB(Muskox ~ scale(fire_age1) + scale(Shrub) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(TRI_extracted) +  
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = data_winter, na.action = "na.fail")

winter_dredge <- dredge(model_global_win_5, fixed = ~cond(offset(log(n_days_effort))) + (1|cluster)) #not running, lots of convergence issues: "Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting') 2: In finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old) :"


#calculating variance inflation factors (VIFs)
performance::check_collinearity(mod_global_sum_shrub)
performance::check_collinearity(mod_global_sum_trees)
performance::check_collinearity(mod_global_sum_shrubs_trees)
 
#adding bryoid and herbs together to create new variable called "groundcover"
model_variables <- model_variables %>%
mutate(Groundcover = Herbs + Bryoid)

#adding grizzly and wolves together to create new variable called "predators"
model_variables <- model_variables %>%
  mutate(Predators = gray_wolf + grizzly_bear)

#adding all trees together to create new variable called "Trees"
model_variables <- model_variables %>%
  mutate(Trees = Treed_conifer + Treed_mixed + Treed_broadleaf)

#adding fire ages together to create new variables called Fire_0 (ages 0-20 yrs old)
model_variables <- model_variables %>%
  mutate(Fire_0 = fire_age0 + fire_age1)

#adding fire ages together to create new variables called Fire_1 (21 - > 30 yrs old)
model_variables <- model_variables %>%
  mutate(Fire_1 = fire_age2 + fire_age3)

### trying to make output tables 
# Tidy the model results
model_results <- tidy(mod_global_sum_shrub, effects = "fixed")

#removing bracket from the variable intercept
model_results <- model_results %>%
  mutate(
    term = ifelse(term == "(Intercept)", "Intercept", term)
  )

#capitalizing all variable names
model_results <- model_results %>%
  mutate(
    term = gsub("scale\\((.*?)\\)", "\\1", term, ignore.case = TRUE),  # Remove any case of scale()
    term = str_replace_all(term, "_", " "),
    term = str_to_title(term)
  )

#alphabetizing variables 
model_results <- model_results %>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", term)) %>%
  mutate(is_intercept = term == "Intercept") %>%
  arrange(desc(is_intercept), term) %>%   # Intercept first, then alphabetize
  select(-is_intercept)


#renaming TRI Extracted
model_results <- model_results %>%
  mutate(term = ifelse(term == "Tri Extracted", "TRI Extracted", term))

# Format table
flextable_model <- model_results %>%
  dplyr::mutate(term = gsub("scale\\(|\\)", "", term)) %>%  # Clean term names
  flextable() %>%
  autofit()

# View in RStudio
flextable_model


