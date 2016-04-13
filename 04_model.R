
# Load packages
library(ggplot2); theme_set(theme_bw())
library(ggrepel)
library(MASS)
library(dplyr)
library(gridExtra)
# library(GGally)
# library(tidyr)

# set working directory
setwd("~/Documents/nyc-felonies")

# load the reg_dataset dataframe
load(file="datasets/reg_dataset_R_obj")


# questions of interest ############################################################

# are the number of felonies associated with temp? (either min or max temp)
# is felonies associated with presence of precipitation?
# does the association between precip and felonies depend on temperature?
# after taking temp into account, is felonies associated with accidents?
# is felonies associated with holidays?
# is felonies associated with school days?
# is felonies associated with day of week?


# model 1 ############################################################

# model 1 with all variables
mod1 <- lm(formula = felonies ~
             temp_min_degF + temp_max_degF +
             any_precip +
             any_precip * temp_min_degF + any_precip * temp_max_degF +
             accidents +
             is_holiday + is_school_day + day_of_week,
           data = reg_dataset)
summary(mod1)

# check the residuals of the fitted model
# plot(mod1)
temp_plot_data <- reg_dataset %>%
  mutate(fitted=eval(mod1$fitted.values),
         resid=eval(mod1$residuals))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, resid > 75), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_plots/mod1_residuals.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)


# model 2 ############################################################

# too many redundant variables in mod1
# model 2 removes the terms with temp_max_degF (which had a lower correlation during eda,
# and with the inclusion of temp_min_degF, isn't significant)
mod2 <- lm(formula = felonies ~
             temp_min_degF +
             any_precip +
             any_precip * temp_min_degF +
             accidents +
             is_holiday + is_school_day + day_of_week,
           data = reg_dataset)
summary(mod2)

# test if the removal makes mod2 significantly different from mod1
anova(mod2, mod1)
# large p-value: removing temp_max_degF doesn't significantly change the model
# adj Rsquared also increased

# check the residuals of the fitted model
# plot(mod2)
temp_plot_data <- reg_dataset %>%
  mutate(fitted=eval(mod2$fitted.values),
         resid=eval(mod2$residuals))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, resid > 75), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_plots/mod2_residuals.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)


# plot case influence statistics
temp_mod <- mod2
temp_plot_data <- data.frame(date=reg_dataset$date,
                             leverage=hatvalues(temp_mod),
                             studRes=studres(temp_mod),
                             cooksDist=cooks.distance(temp_mod))
plot_leverage <- ggplot(temp_plot_data, aes(x=date, y=leverage)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=(2*length(temp_mod$coefficients)/nrow(temp_plot_data)), linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, leverage > (2*length(temp_mod$coefficients)/nrow(temp_plot_data))),
                  aes(label=format(date, format="%b %d")),
                  size=3.5, nudge_y=0.025, nudge_x=0.01) +
  labs(x="Observation (date)", y="Leverage") +
  ylim(0, 0.2)
plot_studRes <- ggplot(temp_plot_data, aes(x=date, y=studRes)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=c(-2, 2), linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, abs(studRes) > 2),
                  aes(label=format(date, format="%b %d"), nudge_y=sign(studRes)*2),
                  size=3.5, nudge_x=0.01) +
  labs(x="Observation (date)", y="Studentized Residual") +
  ylim(-7, 7)
plot_cooksDist <- ggplot(temp_plot_data, aes(x=date, y=cooksDist)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=1, linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, cooksDist > 0.03),
                  aes(label=format(date, format="%b %d")),
                  size=3.5, nudge_y=0.15, nudge_x=0.01) +
  labs(x="Observation (date)", y="Cook's Distance") +
  ylim(0 ,1)
plot_allCI <- grid.arrange(plot_leverage, plot_studRes, plot_cooksDist, nrow=3, ncol=1)
ggsave(filename="model_plots/mod2_caseInfluenceStats.png", plot=plot_allCI, width=10, height=6, units="in")
rm(temp_mod, temp_plot_data, plot_leverage, plot_studRes, plot_cooksDist, plot_allCI)



# model 3 ############################################################

# test if day_of_week is significant
mod3 <- lm(formula = felonies ~
             temp_min_degF +
             any_precip +
             any_precip * temp_min_degF +
             accidents +
             is_holiday + is_school_day,
           data = reg_dataset)
summary(mod3)

# test if the removal of day_of_week makes mod3 significantly different from mod2
anova(mod3, mod2)
# p-value 2.372e-05 : overwhelming evidence that felonies is associated with day_of_week
# stay with mod2



# model 4 ############################################################

# neither is_holiday nor is_school_day is significant individually
# test if theyre is significant when used together
mod4 <- lm(formula = felonies ~
             temp_min_degF +
             any_precip +
             any_precip * temp_min_degF +
             accidents +
             day_of_week,
           data = reg_dataset)
summary(mod4)

# test if the removal of is_holiday, is_school_day makes mod4 significantly different from mod2
anova(mod4, mod2)
# large p-value 0.8227 : felonies is not associated with is_holiday, is_school_day


# check the residuals of the fitted model
# plot(mod4)
temp_plot_data <- reg_dataset %>%
  mutate(fitted=eval(mod4$fitted.values),
         resid=eval(mod4$residuals))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, resid > 75), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_plots/mod4_residuals.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)

# plot case influence statistics
temp_mod <- mod4
temp_plot_data <- data.frame(date=reg_dataset$date,
                             leverage=hatvalues(temp_mod),
                             studRes=studres(temp_mod),
                             cooksDist=cooks.distance(temp_mod))
plot_leverage <- ggplot(temp_plot_data, aes(x=date, y=leverage)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=(2*length(temp_mod$coefficients)/nrow(temp_plot_data)), linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, leverage > (2*length(temp_mod$coefficients)/nrow(temp_plot_data))),
                  aes(label=format(date, format="%b %d")),
                  size=3.5, nudge_y=0.025, nudge_x=0.01) +
  labs(x="Observation (date)", y="Leverage") +
  ylim(0, 0.2)
plot_studRes <- ggplot(temp_plot_data, aes(x=date, y=studRes)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=c(-2, 2), linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, abs(studRes) > 2),
                  aes(label=format(date, format="%b %d"), nudge_y=sign(studRes)*2),
                  size=3.5, nudge_x=0.01) +
  labs(x="Observation (date)", y="Studentized Residual") +
  ylim(-7, 7)
plot_cooksDist <- ggplot(temp_plot_data, aes(x=date, y=cooksDist)) +
  geom_point(size=0.7) +
  geom_hline(yintercept=1, linetype="dotted") +
  geom_text_repel(data = filter(temp_plot_data, cooksDist > 0.03),
                  aes(label=format(date, format="%b %d")),
                  size=3.5, nudge_y=0.15, nudge_x=0.01) +
  labs(x="Observation (date)", y="Cook's Distance") +
  ylim(0 ,1)
plot_allCI <- grid.arrange(plot_leverage, plot_studRes, plot_cooksDist, nrow=3, ncol=1)
ggsave(filename="model_plots/mod4_caseInfluenceStats.png", plot=plot_allCI, width=10, height=6, units="in")
rm(temp_mod, temp_plot_data, plot_leverage, plot_studRes, plot_cooksDist, plot_allCI)








