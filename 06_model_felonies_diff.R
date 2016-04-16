
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
                                         temp_min_degF=as.numeric(27.14)), # same temp as on Jan 1 2015
                              reg_dataset_flagged) %>%
  arrange(date) %>%
  mutate(felonies_diff=felonies - lag(felonies, 1),
         temp_min_degF_diff=temp_min_degF - lag(temp_min_degF, 1)) %>%
  filter(date >= "2015-01-01" & date <= "2015-12-31") %>%
  mutate(temp_min_degF_diff_group=factor(ifelse(temp_min_degF_diff < -5,
                                                "(-Inf, -5)",
                                                ifelse(temp_min_degF_diff >= -5 & temp_min_degF_diff <= 5,
                                                       "[-5, 5]",
                                                       "(5, Inf)")), levels = c("(-Inf, -5)", "[-5, 5]", "(5, Inf)")),
         temp_jump=factor(ifelse(temp_min_degF_diff > 8, 1, 0)))



# Is a change in felonies associated with change in temp? ############################################################

summary(reg_dataset_diff)


# with 3 groups

ggplot(reg_dataset_diff, aes(x=temp_min_degF_diff_group, y=felonies_diff)) +
  geom_boxplot()

anova(lm(formula = felonies_diff ~ temp_min_degF_diff_group, data = reg_dataset_diff))

anova(lm(formula = felonies_diff ~ temp_min_degF_diff_group, data = reg_dataset_diff,
         subset = (
           (temp_min_degF_diff_group == "(-Inf, -5)") |
             (temp_min_degF_diff_group == "[-5, -5]" & abs(felonies_diff) < 100) |
             (temp_min_degF_diff_group == "(5, Inf)" & abs(felonies_diff) < 75)
         ) ))


# with 2 groups

ggplot(reg_dataset_diff, aes(x=temp_jump, y=felonies_diff)) +
  geom_boxplot()

ggplot(reg_dataset_diff, aes(fill=temp_jump, x=felonies_diff)) +
  geom_density(alpha=0.5)

anova(lm(formula = felonies_diff ~ temp_jump, data = reg_dataset_diff))

anova(lm(formula = felonies_diff ~ temp_jump, data = reg_dataset_diff,
         subset = (
           (temp_jump == 0 & felonies_diff >= -75 & felonies_diff <= 75) |
             (temp_jump == 1 & abs(felonies_diff) <= 75)
         ) ))

t.test(formula=felonies_diff ~ temp_jump, data=reg_dataset_diff, var.equal=TRUE, conf.level=0.95)

# try paired t-test
# (felonies today - yesterday on jump days)
t.test(x=filter(reg_dataset_diff, temp_jump==1)$felonies_diff,
       conf.level=0.95, alternative="greater")







ggplot(reg_dataset_diff, aes(x=temp_min_degF_diff, y=felonies_diff)) +
  geom_point(aes(color=factor(sign(temp_min_degF_diff)))) +
  geom_smooth()

lmd1 <- lm(formula = felonies_diff ~ temp_min_degF_diff, data = reg_dataset_diff)
summary(lmd1)

PlotCIStats(model = lmd1, x_var = reg_dataset_diff$date)

lmd2 <- lm(formula = felonies_diff ~ temp_min_degF_diff, data = reg_dataset_diff,
           subset = abs(studres(lmd1)) < 2 & cooks.distance(lmd1) < 1)
summary(lmd2)



lmd3 <- lm(formula = felonies_diff ~ temp_min_degF_diff_group, data = reg_dataset_diff)
summary(lmd3)

PlotCIStats(model = lmd3, x_var = reg_dataset_diff$date)

lmd4 <- lm(formula = felonies_diff ~ temp_min_degF_diff_group, data = reg_dataset_diff,
           subset = abs(studres(lmd3)) < 2 & cooks.distance(lmd3) < 1)
summary(lmd4)


lmd5 <- lm(formula = felonies_diff ~ temp_jump, data = reg_dataset_diff)
summary(lmd5)

PlotCIStats(model = lmd3, x_var = reg_dataset_diff$date)

lmd6 <- lm(formula = felonies_diff ~ temp_jump, data = reg_dataset_diff,
           subset = abs(studres(lmd3)) < 2 & cooks.distance(lmd3) < 1)
summary(lmd6)


