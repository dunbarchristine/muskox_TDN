
install.packages("MASS")

library(vcd)

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

library(lme4)

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

TDN_camera_clus <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

#using raw data
mod3 <- glmer(Muskox ~ grizzly_per_day +
                gray_wolf_per_day + 
                `Treed conifer` +
                Bryoid + 
                offset(log(n_days_effort)) +
                (1|cluster/location),
              data = TDN_camera_clusters,
              family = "poisson")

plot(mod3)

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











  