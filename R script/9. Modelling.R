
install.packages("MASS")
install.packages("MuMIn")

library(vcd)
library(lme4)
library(MuMIn)
library(corrplot)
library(glmmTMB)


#making a model set 
#using a negative binomal mixed effects model with camera cluster and camera ID as fixed effects
## model set concertaining entire year of data

#making global models
mod_null_all_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)
mod_pred_all_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
                          family="nbinom2", data = model_variables)
mod_food_all_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables) 
mod_therm_all_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)
model_global_all_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) +   
                                offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)




#making summer model
mod_null_sum_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables  %>% filter(season == "Summer"))

mod_pred_sum_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
                          family="nbinom2", data = model_variables %>% filter(season == "Summer")) #both grizz and wolf

mod_pred_sum_2.1 <-  glmmTMB(Muskox ~ scale(grizzly_bear) + offset(log(n_days_effort)) + (1|cluster/location),
                             family="nbinom2", data = model_variables %>% filter(season == "Summer")) #grizzly bear only

mod_pred_sum_2.2 <- glmmTMB(Muskox ~ scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
                         family="nbinom2", data = model_variables %>% filter(season == "Summer")) #gray wolf only

mod_food_sum_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 

mod_food_sum_3.1 <- glmmTMB(Muskox ~ scale(Shrub) +
                              offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just shrub variable (estimate: -0.25)

mod_food_sum_3.2 <- glmmTMB(Muskox ~ scale(Herbs) +
                            offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just herbs variable (estimate: 0.078)

mod_food_sum_3.3 <- glmmTMB(Muskox ~ scale(Treed_broadleaf) +
                              offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just Treed_broadleaf variable (estimate: 0.107)

mod_food_sum_3.4 <- glmmTMB(Muskox ~ scale(Bryoid) +
                              offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #running model with just bryoid variable (estimate: -0.0123)



mod_therm_sum_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer"))
model_global_sum_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) + scale(TRI_extracted) +  
                                offset(log(n_days_effort)) + (1|location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) #this model will run with both cluster/location, and just with cluster/ just with location.



#making winter model 
mod_null_win_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables  %>% filter(season == "Winter"))
mod_pred_win_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
                          family="nbinom2", data = model_variables %>% filter(season == "Winter"))
mod_food_win_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed_broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                            offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Winter")) 
mod_therm_win_4 <- glmmTMB(Muskox ~ scale(`Treed_mixed`)+ scale(`Treed_broadleaf`)+ scale(`Treed_conifer`) + scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
                             offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Winter"))
model_global_win_5 <- glmmTMB(Muskox ~ scale(`Treed_mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed_broadleaf`) + scale(`Treed_conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) +   
                                offset(log(n_days_effort)) + (1|location), family="nbinom2", data = model_variables %>% filter(season == "Winter")) #this model wont run with cluster, but will run with location. Will not run with both cluster/location. 



##model selection for global models
fmListGlobal<-model.sel(mod_null_all_1=mod_null_all_1, mod_pred_all_2=mod_pred_all_2, mod_food_all_3=mod_food_all_3, mod_therm_all_4=mod_therm_all_4, model_global_all_5=model_global_all_5)
fmListGlobal

##model selection for summer models
fmListSum<-model.sel(mod_null_sum_1=mod_null_sum_1, mod_pred_sum_2=mod_pred_sum_2, mod_food_sum_3=mod_food_sum_3, mod_therm_sum_4=mod_therm_sum_4, model_global_sum_5=model_global_sum_5)
fmListSum

#mod_therm_sum_4 so far is the best fit
summary(mod_therm_sum_4)
## model selection of winter models
fmListWin<-model.sel(mod_null_win_1=mod_null_win_1, mod_pred_win_2=mod_pred_win_2, mod_food_win_3=mod_food_win_3, mod_therm_win_4=mod_therm_win_4, model_global_win_5=model_global_win_5)
fmListWin

data_winter <- model_variables %>% filter(season == "Winter") %>% drop_na()
  model_global_win_5 <- glmmTMB(Muskox ~ scale(`Treed mixed`) + scale(Shrub) + scale(Herbs) +   
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = data_winter, na.action = "na.fail")
  
winter_dredge <- dredge(model_global_win_5, fixed = ~offset(log(n_days_effort)) + (1|cluster))


data_summer <- model_variables %>% filter(season == "Summer") %>% drop_na()
model_global_sum_5 <- glmmTMB(Muskox ~ scale(`Treed mixed`) + scale(Shrub) + scale(Herbs) +   
                                offset(log(n_days_effort)) + (1|cluster), family="nbinom2", data = data_summer, na.action = "na.fail")
summer_dredge <- dredge(model_global_sum_5, fixed = ~offset(log(n_days_effort)) + (1|cluster))



  