
# Load packages
library(ggplot2); theme_set(theme_bw())
library(scales)
library(ggrepel)
library(MASS)
library(dplyr)
library(gridExtra)
library(tidyr)

# set working directory
setwd("~/Documents/nyc-felonies")

# load the reg_dataset dataframe
load(file="datasets/reg_dataset_R_obj")


# Define helper functions ############################################################

# define an outlier checker
# http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r
IsOutlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# define a function to plot case influence stats
PlotCIStats <- function(model, x_var, label_leverage=TRUE, label_studRes=TRUE, label_cooksDist=TRUE){
  
  temp_plot_data <- data.frame(date=x_var,
                               leverage=hatvalues(model),
                               studRes=studres(model),
                               cooksDist=cooks.distance(model))
  
  plot_leverage <- ggplot(temp_plot_data, aes(x=date, y=leverage)) +
    geom_point(size=0.7) +
    geom_hline(yintercept=(2*length(model$coefficients)/nrow(temp_plot_data)), linetype="dotted") +
    {
      if(label_leverage==TRUE){
        geom_text_repel(data = filter(temp_plot_data, leverage > (2*length(model$coefficients)/nrow(temp_plot_data))),
                        aes(label=format(date, format="%b %d")),
                        size=3.5, nudge_x=0.01)
      } else NULL
    } +
    labs(x=NULL, y="Leverage")
  
  plot_studRes <- ggplot(temp_plot_data, aes(x=date, y=studRes)) +
    geom_point(size=0.7) +
    geom_hline(yintercept=c(-2, 2), linetype="dotted") +
    {
      if(label_studRes==TRUE){
        geom_text_repel(data = filter(temp_plot_data, abs(studRes) > 2),
                        aes(label=format(date, format="%b %d"), nudge_y=sign(studRes)*2),
                        size=3.5, nudge_x=0.01)
      } else NULL
    } +
    labs(x=NULL, y="Studentized Residual")
  
  plot_cooksDist <- ggplot(temp_plot_data, aes(x=date, y=cooksDist)) +
    geom_point(size=0.7) +
    geom_hline(yintercept=1, linetype="dotted") +
    {
      if(label_cooksDist==TRUE){
        geom_text_repel(data = filter(temp_plot_data, cooksDist > 0.03),
                        aes(label=format(date, format="%b %d")),
                        size=3.5, nudge_y=0.15, nudge_x=0.01)
      } else NULL
      
    } +
    labs(x="Observation (date)", y="Cook's Distance")
  
  plot_allCI <- grid.arrange(plot_leverage, plot_studRes, plot_cooksDist, nrow=3, ncol=1)
  
  return(plot_allCI)
  
}



# Are the number of felonies different on warmer days than cooler days? ############################################################

# create an indicator variable indicating if daily min temp is above or below the yearly median
reg_dataset <- reg_dataset %>%
  mutate(is_warm=factor(ifelse(temp_min_degF >= median(temp_min_degF), 1, 0)))

# plots
ggplot(reg_dataset, aes(x=date, y=1, fill=is_warm)) +
  geom_bar(stat="identity", width = 1) +
  labs(y=NULL, x="Date") +
  theme(axis.text.y=element_blank())
ggplot(reg_dataset, aes(x=date, y=as.numeric(as.character(is_warm)))) +
  geom_line() +
  geom_point(aes(color=is_warm)) +
  labs(y="Cooler Days (0) vs Warmer Days (1)", x="Date") +
  theme(axis.text.y=element_blank())
ggplot(reg_dataset, aes(x=is_warm, y=felonies)) +
  geom_boxplot() +
  labs(x="Cooler Days (0) vs Warmer Days (1)", y="Felonies", title="Felonies on Cooler Days vs Warmer Days")
ggsave(filename="model_felonies_plots/felonies_vs_isWarm.png", width=6.125, height=3.5, units="in")

reg_dataset %>%
  group_by(is_warm) %>%
  summarize(min=min(felonies),
            pct25=quantile(felonies, 0.25), median=median(felonies), pct75=quantile(felonies, 0.75),
            max=max(felonies),
            mean=mean(felonies), sd=sd(felonies))

# SD in each group is nearly equal. two sample t-test is valid
t.test(formula=felonies~is_warm, data=reg_dataset, var.equal=TRUE, conf.level=0.95)
# overwhelming evidence of difference

# although we don't need to remove outliers (doing so will make the results even more significant)
# boxplot excluding outliers
ggplot(filter(reg_dataset, felonies < 350 | is_warm == 1), aes(x=is_warm, y=felonies)) +
  geom_boxplot() +
  labs(x="Cooler Days (0) vs Warmer Days (1)", y="Felonies", title="Felonies on Cooler Days vs Warmer Days (excluding outliers)")
ggsave(filename="model_felonies_plots/felonies_vs_isWarm_exclOutliers.png", width=6.125, height=3.5, units="in")
# do two sample t-test excluding outliers
t.test(formula=felonies~is_warm, data=reg_dataset, var.equal=TRUE, conf.level=0.95, subset = felonies < 350 | is_warm == 1)


# Is there a linear association between felonies and temp? ############################################################

# plot felonies vs min temp
ggplot(reg_dataset, aes(x=temp_min_degF, y=felonies)) +
  geom_point() +
  labs(x="Minimum Temperature (degrees F)", y="Felonies")
ggsave(filename="model_felonies_plots/felonies_vs_minTemp.png", width=6.125, height=3.5, units="in")

lm1 <- lm(formula = felonies ~ temp_min_degF, data=reg_dataset)
summary(lm1)

# check the residuals
temp_plot_data <- reg_dataset %>%
  mutate(fitted=eval(lm1$fitted.values),
         resid=eval(lm1$residuals),
         is_resid_outlier=IsOutlier(eval(lm1$residuals)))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted Values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_plots/lm1_residuals.png", width=6.125, height=3.5, units="in")

# check for serial correlation
ggplot(temp_plot_data,
       aes(x=date, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Date", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_plots/lm1_serial.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)

# plot the case influence statistics
lm1_cistat_plot <- PlotCIStats(model = lm1, x_var = reg_dataset$date, label_leverage = FALSE)
ggsave(filename="model_felonies_plots/lm1_caseInfluenceStats.png",
       plot = lm1_cistat_plot, width=10, height=6, units="in")


# Excl days with problematic studRes or cooksDist
lm2 <- lm(formula = felonies ~ temp_min_degF, data=reg_dataset,
          subset = abs(studres(lm1)) < 2 & cooks.distance(lm1) < 1)
summary(lm2)
# the relationship barely changes when excluding days with problematic CI stats


# Is there a higher order association between felonies and temp?
lm3 <- lm(formula = felonies ~ poly(temp_min_degF, degree = 2, raw = TRUE), data=reg_dataset)
summary(lm3)
# 2nd order term is not significant


# Is there an association between felonies and temp after accounting for everything else?  ############################################################

lm4 <- lm(formula =
            felonies ~ temp_min_degF + any_precip + temp_min_degF*any_precip +
            is_holiday + is_school_day + day_of_week,
          data=reg_dataset)
summary(lm4)


# check the residuals
temp_plot_data <- reg_dataset %>%
  mutate(fitted=eval(lm4$fitted.values),
         resid=eval(lm4$residuals),
         is_resid_outlier=IsOutlier(eval(lm4$residuals)))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted Values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_plots/lm4_residuals.png", width=6.125, height=3.5, units="in")

# check for serial correlation
ggplot(temp_plot_data,
       aes(x=date, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Date", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_plots/lm4_serial.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)

# plot the case influence statistics
lm4_cistat_plot <- PlotCIStats(model = lm4, x_var = reg_dataset$date, label_leverage = T)
ggsave(filename="model_felonies_plots/lm4_caseInfluenceStats.png",
       plot = lm4_cistat_plot, width=10, height=6, units="in")
# all of the holidays have high leverage, but their studRes and cooksDist are fine

# Excl days with problematic studRes or cooksDist
lm5 <- lm(formula =
            felonies ~ temp_min_degF + any_precip + temp_min_degF*any_precip +
            is_holiday + is_school_day + day_of_week,
          data=reg_dataset,
          subset = abs(studres(lm4)) < 2 & cooks.distance(lm4) < 1)
summary(lm5)

# is_holiday and is_school_day are now significant

# what was removed?
filter(reg_dataset, abs(studres(lm4)) >= 2 | cooks.distance(lm4) >= 1)  %>% summary()

# plot felonies vs temp, color=problemDayType
temp_plot_data <- reg_dataset %>%
  mutate(problematicObs=as.numeric(abs(studres(lm4)) >= 2 | cooks.distance(lm4) >= 1)) %>%
  select(date, problematicObs, is_holiday, is_school_day, temp_min_degF, felonies) %>%
  mutate(dayType=ifelse(is_school_day==1 & is_holiday==1,
                        "Other",
                        ifelse(is_school_day==1,
                               "School Day",
                               ifelse(is_holiday==1,
                                      "Holiday",
                                      "Other"))),
         problemDayType=ifelse(problematicObs==TRUE, dayType, NA))
ggplot(temp_plot_data, aes(x=temp_min_degF, y=felonies)) +
  geom_point(aes(color=problemDayType), size=0.7) +
  geom_text_repel(data = filter(temp_plot_data, problematicObs==TRUE),
                  aes(label=format(date, format="%b %d"), color=problemDayType), size=3.5, #fontface="bold",
                  show.legend = FALSE) +
  labs(x="Minimum Temperature (degrees F)", y="Felonies", color="")
ggsave(filename="model_felonies_plots/lm4_felonies_vs_temp_problematicObs.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)



# plot partial residual plots for is_holiday and is_school_day
temp_plot_data <- reg_dataset %>%
  mutate(problematicObs=as.numeric(abs(studres(lm4)) >= 2 | cooks.distance(lm4) >= 1)) %>%
  select(date, problematicObs, is_holiday, is_school_day, temp_min_degF, felonies) %>%
  mutate(dayType=ifelse(is_school_day==1 & is_holiday==1,
                        "Other",
                        ifelse(is_school_day==1,
                               "School Day",
                               ifelse(is_holiday==1,
                                      "Holiday",
                                      "Other"))),
         problemDayType=ifelse(problematicObs==TRUE, dayType, NA),
         lm4resid=eval(lm4$residuals),
         preslm4_is_holiday=lm4resid+(as.numeric(as.character(is_holiday))*lm4$coefficients["is_holiday1"][[1]]),
         preslm4_is_school_day=lm4resid+(as.numeric(as.character(is_school_day))*lm4$coefficients["is_school_day1"][[1]]))
set.seed(1)
ggplot(temp_plot_data, aes(x=is_holiday, y=preslm4_is_holiday)) +
  geom_jitter(size=0.7, width = 0.5, aes(color=factor(problematicObs))) +
  labs(x="Non-holiday (0) vs Holiday (1)\n[jittered]", y="Partial Residual\n(felonies, adjusted for all covariates)", color="Promblematic\n Observation")
ggsave(filename="model_felonies_plots/lm4_pres_isHoliday.png", width=6.125, height=3.5, units="in")
set.seed(1)
ggplot(temp_plot_data, aes(x=is_school_day, y=preslm4_is_school_day)) +
  geom_jitter(size=0.7, width = 0.5, aes(color=factor(problematicObs))) +
  labs(x="Non-School Day (0) vs School Day (1)\n[jittered]", y="Partial Residual\n(felonies, adjusted for all covariates)", color="Promblematic\n Observation")
ggsave(filename="model_felonies_plots/lm4_pres_isSchoolDay.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)


# Is there an association between felonies and temp after accounting for everything else? Remove temp*precip  ############################################################

# Remove interaction between precip and temp
lm6 <- lm(formula =
            felonies ~ temp_min_degF + any_precip +
            is_holiday + is_school_day + day_of_week,
          data=reg_dataset)
summary(lm6)

# use extra sum of squares F test to see if day_of_week is significant
anova(lm6, lm(formula = felonies ~ temp_min_degF + any_precip + is_holiday + is_school_day, data=reg_dataset))

# Excl days with problematic studRes or cooksDist
lm7 <- lm(formula =
            felonies ~ temp_min_degF + any_precip +
            is_holiday + is_school_day + day_of_week,
          data=reg_dataset,
          subset = abs(studres(lm6)) < 2 & cooks.distance(lm6) < 1)
summary(lm7)

# list of the problematic observations
reg_dataset %>% filter(abs(studres(lm6)) >= 2 | cooks.distance(lm6) >= 1)



# Save image and objects  ############################################################

save.image("05_model_felonies.RData")

reg_dataset_flagged <- reg_dataset %>%
  mutate(problematic_obs=factor(ifelse(abs(studres(lm6)) >= 2 | cooks.distance(lm6) >= 1, 1, 0))) %>%
  select(date, felonies, temp_min_degF, any_precip, is_holiday, is_school_day, day_of_week, problematic_obs)
save(reg_dataset_flagged, file = "datasets/reg_dataset_flagged_R_obj")

