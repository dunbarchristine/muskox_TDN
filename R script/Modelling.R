
install.packages("MASS")
install.packages("MuMIn")

library(vcd)
library(lme4)
library(MuMIn)
library(corrplot)
library(glmmTMB)

#making a data set summarized across clusters, weeks and years
TDN_camera_clusters <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  group_by(cluster, week, year) %>%
  summarise(grizzly_count_sum = sum(grizzly_bear),
            muskox_count_sum = sum(Muskox),
            gray_wolf_sum = sum(gray_wolf),
            n_days_effort_sum = sum(n_days_effort),
            Treed_conifer_average = mean(`Treed conifer`),
            Bryoid_average = mean(Bryoid)) %>%
  mutate(grizzly_per_day = grizzly_count_sum/n_days_effort_sum, 
         gray_wolf_per_day = gray_wolf_sum/n_days_effort_sum)

#making an un-summarized data set with a different row for each camera for each week for each year
#added two columns for number of grizzlies and gray wolves detected per day in a week
all_variables <- all_variables %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

#made new dataset that included tri and grizz_per_day and gray_wolf_per_day
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")

all_variables_with_tri_and_species <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

all_variables <- all_variables %>%
  rename_with(~ gsub(" ", "_", .))

#making a model set 
#using a negative binomal mixed effects model with camera cluster and camera ID as fixed effects
## model set concertaining entire year of data

#making global models
mod_null_all_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)
mod_pred_all_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
family="nbinom2", data = model_variables)
mod_food_all_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables) 
mod_therm_all_4 <- glmmTMB(Muskox ~ scale(`Treed mixed`)+ scale(`Treed broadleaf`)+ scale(`Treed conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)
model_global_all_5 <- glmmTMB(Muskox ~ scale(`Treed mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed broadleaf`) + scale(`Treed conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) +   
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables)

#making summer model
mod_null_sum_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables  %>% filter(season == "Summer"))
mod_pred_sum_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
family="nbinom2", data = model_variables %>% filter(season == "Summer"))
mod_food_sum_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer")) 
mod_therm_sum_4 <- glmmTMB(Muskox ~ scale(`Treed mixed`)+ scale(`Treed broadleaf`)+ scale(`Treed conifer`)+ scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer"))
model_global_sum_5 <- glmmTMB(Muskox ~ scale(`Treed mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed broadleaf`) + scale(`Treed conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) +   
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Summer"))

#making winter model (mod_pred_win_2, 4 not running)
mod_null_win_1 <- glmmTMB(Muskox ~ 1+ offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables  %>% filter(season == "Winter"))
mod_pred_win_2 <- glmmTMB(Muskox ~ scale(grizzly_bear) + scale(gray_wolf) + offset(log(n_days_effort)) + (1|cluster/location),
family="nbinom2", data = model_variables %>% filter(season == "Winter"))
mod_food_win_3 <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Winter")) 
mod_therm_win_4 <- glmmTMB(Muskox ~ scale(`Treed mixed`)+ scale(`Treed broadleaf`)+ scale(`Treed conifer`) + scale(Water) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) +
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Winter"))
model_global_win_5 <- glmmTMB(Muskox ~ scale(`Treed mixed`) + scale(log_esker_camera_distances) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) + scale(TRI_extracted) + scale(Shrub) + scale(Bryoid) + scale(Herbs) + scale(`Treed broadleaf`) + scale(`Treed conifer`) + scale(gray_wolf) + scale(grizzly_bear) + scale(Water) +   
offset(log(n_days_effort)) + (1|cluster/location), family="nbinom2", data = model_variables %>% filter(season == "Winter"))

##model selection for global models
fmListSum<-model.sel(mod_null_all_1=mod_null_all_1, mod_pred_all_2=mod_pred_all_2, mod_food_all_3=mod_food_all_3, mod_therm_all_4=mod_therm_all_4, model_global_all_5=model_global_all_5)
fmListSum

##model selection for summer models
fmListSum<-model.sel(mod_null_sum_1=mod_null_sum_1, mod_pred_sum_2=mod_pred_sum_2, mod_food_sum_3=mod_food_sum_3, mod_therm_sum_4=mod_therm_sum_4, model_global_sum_5=model_global_sum_5)
fmListSum

#mod_therm_sum_4 so far is the best fit
summary(mod_therm_sum_4)
## model selection of winter models
fmListWin<-model.sel(mod_null_win_1=mod_null_win_1, mod_pred_win_2=mod_pred_win_2, mod_food_win_3=mod_food_win_3, mod_therm_win_4=mod_therm_win_4)
fmListWin

#checking for NAs in "model_variables" dataset
sum(is.na(model_variables))

#checking which columns exactly have NAs. Elevation and TRI_extracted have NAs.
colSums(is.na(model_variables))















#making null model
mod_null <- glmmTMB(Muskox ~ 1+
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = model_variables)

#predation model 
#summer model for predation
mod_pre_sum <- glmmTMB(Muskox ~ grizzly_bear + gray_wolf + 
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    family="nbinom2",
                    data = model_variables %>% filter(season == "Summer"))


#winter model for predation 
mod_pre_win <- glmmTMB(Muskox ~ grizzly_bear + gray_wolf + 
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   family="nbinom2",
                   data = model_variables %>% filter(season == "Winter"))


#food model
#summer model for landcover (food hypothesis)
mod_food_sum <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                     family="nbinom2",
                    data = model_variables %>% filter(season == "Summer")) 


#winter model for landcover (food hypothesis)
mod_food_win <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Herbs) + scale(Bryoid) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                       offset(log(n_days_effort)) +
                       (1|cluster/location),
                     family="nbinom2",
                     data = model_variables %>% filter(season == "Winter")) 


#thermoregulation model
#summer model for thermoregulation 
mod_therm_summer <- glmmTMB(Muskox ~ `Treed mixed`+`Treed broadleaf`+`Treed conifer`+ Water + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                   family="nbinom2",
                    data = model_variables  %>% filter(season == "Summer"))

#winter model for thermoregulation 
mod_therm_win <- glmmTMB(Muskox ~ `Treed mixed`+`Treed broadleaf`+`Treed conifer`+ Water + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted +
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   family="nbinom2",
                   data = model_variables %>% filter(season == "Winter"))

#model with all variables
model_all <- glmmTMB(Muskox ~ `Treed mixed` + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted + Shrub + Bryoid + Herbs + `Treed broadleaf` + `Treed conifer` + gray_wolf + grizzly_bear + season + Water +   
                offset(log(n_days_effort)) +
                (1|cluster/location),
                family="nbinom2",
                data = model_variables)














#trying more simple model because some of the previous ones did not run
#making null model
mod_1 <- glmer(Muskox ~ 1+
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#predation model
mod_2 <- glmer(Muskox ~ grizzly_per_day + gray_wolf_per_day + Treed_conifer + Treed_broadleaf + treed_mixed + elevations + log_esker_camera_distances + TRI_extracted +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables_with_tri)

#food model
mod_3 <- glmer(Muskox ~ Bryoid + Shrub + Herbs + Treed_broadleaf + fire_age0 + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#thermoregulation model
mod_4 <- glmer(Muskox ~ Treed_conifer + Treed_broadleaf + Treed_mixed + elevations + log_esker_camera_distances + fire_age0 + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#comparing model fit of all models using AIC 
fmList<-model.sel(mod_1=mod_1, mod_2=mod_2, mod_3=mod_3, mod_4=mod_4)
fmList

#in depth summary of best fitting model which is mod_4
summary(mod_4)

correlation_matrix <- cor(all_variables, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")

###### testing out models, trying out different structures 
mod <- glm(muskox_count_sum ~ grizzly_per_day +
             gray_wolf_per_day + 
             Treed_conifer_average +
             Bryoid_average + 
             offset(log(n_days_effort_sum)),
           data = TDN_camera_clusters,
           family = "poisson")

summary(mod)


mod2 <- glmer(muskox_count_sum ~ grizzly_per_day +
             gray_wolf_per_day + 
             Treed_conifer_average +
             Bryoid_average + 
             offset(log(n_days_effort_sum)) +
               (1|cluster),
           data = TDN_camera_clusters,
           family = "poisson")

summary(mod)

#using raw data. trying a mixed effect model
mod3 <- glmer(Muskox ~ grizzly_per_day +
                gray_wolf_per_day + 
                `Treed conifer` +
                Bryoid + #random effects
                offset(log(n_days_effort)) + 
                (1|cluster/location), #fixed effect 
              data = TDN_camera_clusters,
              family = "poisson") #type of mixed effect 


fit <- goodfit(TDN_camera_clusters$Muskox)
summary(fit)
rootogram(fit)
Ord_plot(TDN_camera_clusters$Muskox)
distplot(TDN_camera_clusters$Muskox, type = "poisson")
distplot(TDN_camera_clusters$Muskox, type = "nbinom")

anova(mod, test = "Chisq")

library(AER)
deviance(mod)/mod$df.residual
dispersiontest(mod) #this is telling us there is over dispersion, cant use poisson model

library(pscl)
model1 <- zeroinfl(muskox_count_sum ~ grizzly_per_day +
                       gray_wolf_per_day + 
                       Treed_conifer_average +
                       Bryoid_average + 
                       offset(log(n_days_effort_sum)),
                     data = TDN_camera_clusters,
                     dist = "poisson")
AIC(mod, model1)

model2 <- MASS::glm.nb(muskox_count_sum ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     Treed_conifer_average +
                     Bryoid_average + 
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters
                   )

model3 <- zeroinfl(muskox_count_sum ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     Treed_conifer_average +
                     Bryoid_average + 
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters,
                   dist = "negbin")

model4 <- zeroinfl(muskox_count_sum ~ 1+
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters,
                   dist = "negbin")


AIC(mod, model1, model2, model3)

mod_nb <- glmer.nb(Muskox ~ grizzly_per_day +
                      gray_wolf_per_day + 
                      `Treed conifer` +
                      Bryoid + 
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = TDN_camera_clus)

summary(mod_nb)                  

mod_nb <- glmer.nb(Muskox ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     `Treed conifer` +
                     Bryoid + 
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   data = TDN_camera_clus)





glmmTMB::fixef()


  