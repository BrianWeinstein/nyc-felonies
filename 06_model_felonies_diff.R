
# Load packages
library(ggplot2); theme_set(theme_bw())
library(scales)
library(ggrepel)
library(MASS)
library(dplyr)
library(gridExtra)
# library(tidyr)

# set working directory
setwd("~/Documents/nyc-felonies")

# load the reg_dataset dataframe
load(file="datasets/reg_dataset_flagged_R_obj")


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



# Define felonies_diff and temp_min_degF_diff ############################################################

reg_dataset_diff <- bind_rows(data.frame(date=as.Date("2014-12-31"),
                                         felonies=as.integer(256),
                                         temp_min_degF=as.numeric(27.14), # same temp as on Jan 1 2015
                                         any_precip=factor(0, levels=c(0, 1)),
                                         is_holiday=factor(0, levels=c(0,1))),
                              reg_dataset_flagged) %>%
  arrange(date) %>%
  mutate(felonies_diff=felonies - lag(felonies, 1),
         temp_min_degF_diff=temp_min_degF - lag(temp_min_degF, 1),
         any_precip_diff=factor(as.numeric(as.character(any_precip)) - lag(as.numeric(as.character(any_precip)), 1)),
         is_holiday_diff=factor(as.numeric(as.character(is_holiday)) - lag(as.numeric(as.character(is_holiday)), 1))) %>%
  filter(date >= "2015-01-01" & date <= "2015-12-31") %>%
  mutate(temp_jump=factor(ifelse(temp_min_degF_diff > 8, 1, 0)))



# Is an increase in temp associated with an increase in felonies? ############################################################

summary(reg_dataset_diff)

# try paired t-test
# (felonies today - yesterday) on jump days
ggplot(filter(reg_dataset_diff, temp_jump==1), aes(y=felonies_diff, x=temp_jump)) +
  geom_boxplot()
tt1 <- t.test(x=filter(reg_dataset_diff, temp_jump==1)$felonies_diff,
              conf.level=0.95, alternative="two.sided")
tt1$p.value / 2
tt1$conf.int

# how does this compare to non-jump days?
ggplot(reg_dataset_diff, aes(y=felonies_diff, x=temp_jump)) +
  geom_boxplot()
tt2 <- t.test(formula=felonies_diff~temp_jump, data=reg_dataset_diff, var.equal=TRUE, conf.level=0.95)
tt2$p.value / 2
tt3 <- t.test(formula=felonies_diff~temp_jump, data=reg_dataset_diff, var.equal=TRUE, conf.level=0.95,
              subset = (temp_jump == 0 & abs(felonies_diff) <= 75) | (temp_jump == 1))
tt3$p.value / 2


# Is an increase in temp associated with an increase in felonies? ############################################################

lmd1 <- lm(formula = felonies_diff ~ temp_jump, data = reg_dataset_diff)
summary(lmd1)


lmd2 <- lm(formula =
             felonies_diff ~
             temp_jump + temp_min_degF + temp_jump*temp_min_degF +
             any_precip + is_holiday + is_school_day +day_of_week,
           data = as.data.frame(reg_dataset_diff))
summary(lmd2)

# check the residuals
temp_plot_data <- reg_dataset_diff %>%
  mutate(fitted=eval(lmd2$fitted.values),
         resid=eval(lmd2$residuals),
         is_resid_outlier=IsOutlier(eval(lmd2$residuals)))
ggplot(temp_plot_data,
       aes(x=fitted, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted Values", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_diff_plots/lmd2_residuals.png", width=6.125, height=3.5, units="in")

# check for serial correlation
ggplot(temp_plot_data,
       aes(x=date, y=resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Date", y="Residuals") +
  geom_text_repel(data = filter(temp_plot_data, is_resid_outlier==TRUE), aes(label=format(date, format="%b %d")), size=3.5)
ggsave(filename="model_felonies_diff_plots/lmd2_serial.png", width=6.125, height=3.5, units="in")
rm(temp_plot_data)

# plot the case influence statistics
lmd2_cistat_plot <- PlotCIStats(model = lmd2, x_var = reg_dataset_diff$date, label_leverage = T)
ggsave(filename="model_felonies_diff_plots/lmd2_caseInfluenceStats.png",
       plot = lmd2_cistat_plot, width=10, height=6, units="in")

# Excl days with problematic studRes or cooksDist
lmd3 <- lm(formula =
             felonies_diff ~
             temp_jump + temp_min_degF + temp_jump*temp_min_degF +
             any_precip + is_holiday + is_school_day +day_of_week,
           data = as.data.frame(reg_dataset_diff),
           subset = abs(studres(lmd2)) < 2 & cooks.distance(lmd2) < 1)
summary(lmd3)





# Remove interaction between temp_jump and temp_min_degF
lmd4 <- lm(formula =
             felonies_diff ~
             temp_jump + temp_min_degF +
             any_precip + is_holiday + is_school_day +day_of_week,
           data = as.data.frame(reg_dataset_diff))
summary(lmd4)

# plot the case influence statistics
lmd4_cistat_plot <- PlotCIStats(model = lmd4, x_var = reg_dataset_diff$date, label_leverage = T)
ggsave(filename="model_felonies_diff_plots/lmd4_caseInfluenceStats.png",
       plot = lmd4_cistat_plot, width=10, height=6, units="in")

# Excl days with problematic studRes or cooksDist
lmd5 <- lm(formula =
             felonies_diff ~
             temp_jump + temp_min_degF +
             any_precip + is_holiday + is_school_day +day_of_week,
           data = as.data.frame(reg_dataset_diff),
           subset = abs(studres(lmd4)) < 2 & cooks.distance(lmd4) < 1)
summary(lmd5)




# temp_min_degF isn't significant in either model
# any precip isn't significant in either model
# is_holiday isn't significant in either model
# is_school_day isn't significant in either model

lmd6 <- lm(formula = felonies_diff ~ temp_jump + day_of_week, data = as.data.frame(reg_dataset_diff))
summary(lmd6)

anova(lmd6, lm(formula = felonies_diff ~ temp_jump, data = as.data.frame(reg_dataset_diff)))

lmd6_cistat_plot <- PlotCIStats(model = lmd6, x_var = reg_dataset_diff$date, label_leverage = T)
ggsave(filename="model_felonies_diff_plots/lmd6_caseInfluenceStats.png",
       plot = lmd6_cistat_plot, width=10, height=6, units="in")

lmd7 <- lm(formula = felonies_diff ~ temp_jump + day_of_week,
           data = as.data.frame(reg_dataset_diff),
           subset= abs(studres(lmd6)) < 2 & cooks.distance(lmd6) < 1)
summary(lmd7)

anova(lmd7, lm(formula = felonies_diff ~ temp_jump, data = as.data.frame(reg_dataset_diff),
               subset= abs(studres(lmd6)) < 2 & cooks.distance(lmd6) < 1))


summary(lmd6)
# after accounting for day of week, the data provides suggestive, but inconclusive evidence that
# large (8 degree) temp increases is associated with 8.942 additional felonies per day
# two-sided p-value 0.10006
confint(lmd6)
# 95pct confint c(-1.723365, 19.606819) felonies per day

summary(lmd7)
# after removing problematic cases (with high studentized residuals), after accounting for day of week
# the data provides no evidence of an association between large temp increases and an increase in felonies
# two-sided p-value 0.185603


