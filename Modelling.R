
install.packages("MASS")
install.packages("MuMIn")

library(vcd)
library(lme4)
library(MuMIn)
library(corrplot)

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

all_variables <- all_variables %>%
  rename_with(~ gsub(" ", "_", .))

#making a model set 
#using a negative binomal mixed effects model with camera cluster and camera ID as fixed effects

#making null model
mod_nb1 <- glmer.nb(Muskox ~ 1+
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#predation model
mod_nb2 <- glmer.nb(Muskox ~ grizzly_per_day + gray_wolf_per_day + 
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#food model
mod_nb3 <- glmer.nb(Muskox ~ Bryoid + Shrub + Herbs + Treed_broadleaf + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#thermoregulation model
mod_nb4 <- glmer.nb(Muskox ~ Treed_conifer + Treed_broadleaf + Treed_mixed + elevations + log_esker_camera_distances + fire_age0 + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#running everything together just to see if it runs 
mod_nb5 <- 

fmList<-model.sel(mod_nb1=mod_nb1, mod_nb2=mod_nb2, mod_nb3=mod_nb3, mod_nb4=mod_nb4)
fmList

summary()




#trying more simple model because some of the previous ones did not run
#making null model
mod_1 <- glmer(Muskox ~ 1+
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

#predation model
mod_2 <- glmer(Muskox ~ grizzly_per_day + gray_wolf_per_day + Treed_conifer + Treed_broadleaf + treed_mixed + elevations + log_esker_camera_distances +
                      offset(log(n_days_effort)) +
                      (1|cluster/location),
                    data = all_variables)

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








  