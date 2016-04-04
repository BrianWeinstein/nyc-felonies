
# Load libraries
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(GGally)

# set working directory
setwd("~/Documents/nyc-felonies")

# load the full_dataset dataframe
load(file="datasets/full_dataset_R_obj")
reg_dataset <- full_dataset

# examine numeric variables ############################################################

# plot a matrix of pairwise scatterplots for the numeric vars
plot.pairs <- ggpairs(data=select(reg_dataset,
                                  accidents, accident_injuriesDeaths,
                                  temp_min_degF, temp_max_degF, windspeed_avg_mph,
                                  school_attendance_pct, felonies),
                      lower=list(continuous=wrap("points", size=0.5)))
plot.pairs
png(filename="eda_plots/pairs_numeric.png", width=15, height=12, units="in", res=300)
print(plot.pairs)
dev.off()
rm(plot.pairs)

# first notice the recording error in the windspeed_avg_mph variable
# -2236.716306 mph on Nov 28 and 29
# remove those observations for now
# if we end up removing the windspeed_avg_mph variable later, we can add those obs back in
reg_dataset <- reg_dataset %>%
  filter(windspeed_avg_mph >= 0)

# plot a matrix of pairwise scatterplots for the numeric vars, excluding the bad obs
plot.pairs <- ggpairs(data=select(reg_dataset,
                                  accidents, accident_injuriesDeaths,
                                  temp_min_degF, temp_max_degF, windspeed_avg_mph,
                                  school_attendance_pct, felonies),
                      lower=list(continuous=wrap("points", size=0.5)))
plot.pairs
png(filename="eda_plots/pairs_numeric_exclOutliers.png", width=15, height=12, units="in", res=300)
print(plot.pairs)
dev.off()
rm(plot.pairs)

# many of the covariates are highly correlated
# temp_min_degF and temp_max_degF
# accidents and accident_injuriesDeaths

# school_atttendance_pct is either 0, or between 74.63 and 93.91
# not enough data between 0 and 74.63 for inference
# use the is_school_day indicator variable instead
reg_dataset$school_attendance_pct <- NULL

# felonies vs max temp
ggplot(reg_dataset, aes(x=temp_max_degF, y=felonies)) + geom_point()
ggsave(filename="eda_plots/felonies_vs_maxTemp.png", width=6.125, height=3.5, units="in")

# examine categorical variables ############################################################

# is_school_day
ggplot(reg_dataset, aes(x=is_school_day, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_isSchoolDay.png", width=6.125, height=3.5, units="in")
t.test(formula=felonies~is_school_day, data=reg_dataset, var.equal=TRUE, conf.level=0.95)
# fewer felonies on school days

# is_weekend
ggplot(reg_dataset, aes(x=is_weekend, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_isWeekend.png", width=6.125, height=3.5, units="in")
t.test(formula=felonies~is_weekend, data=reg_dataset, var.equal=TRUE, conf.level=0.95)
# no obvious difference on weekday vs weekend
# but keep in model for now

# day_of_week
ggplot(reg_dataset, aes(x=day_of_week, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_dayOfWeek.png", width=6.125, height=3.5, units="in")
anova(lm(felonies~day_of_week, data=reg_dataset)) # use a one way ANOVA F-test
# mean felonies is different for at least 1 day_of_week

# any_precip
ggplot(reg_dataset, aes(x=any_precip, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_anyPrecip.png", width=6.125, height=3.5, units="in")
t.test(formula=felonies~is_school_day, data=reg_dataset, var.equal=TRUE, conf.level=0.95)
# fewer felonies on days with precip

# is_holiday
ggplot(reg_dataset, aes(x=is_holiday, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_isHoliday.png", width=6.125, height=3.5, units="in")
t.test(formula=felonies~is_holiday, data=reg_dataset, var.equal=TRUE, conf.level=0.95)
# remove outliers
ggplot(filter(reg_dataset, felonies > 171 & felonies < 389), aes(x=is_holiday, y=felonies)) + geom_boxplot()
ggsave(filename="eda_plots/felonies_vs_isHoliday_exclOutliers.png", width=6.125, height=3.5, units="in")
t.test(formula=felonies~is_holiday, data=reg_dataset, var.equal=TRUE, conf.level=0.95,
       subset = felonies > 171 & felonies < 389)
# is_holiday is only significant once outliers are removed
# keep them in for now

# date
ggplot(reg_dataset, aes(x=date, y=felonies)) + geom_point()
ggsave(filename="eda_plots/felonies_vs_maxTemp.png", width=6.125, height=3.5, units="in")
# clearly a sinusoidal trend
# temp will likely explain this behavior


# save reg_dataset as csv and r object ############################################################

write.csv(reg_dataset, file = "datasets/reg_dataset.csv", row.names = FALSE)
save(reg_dataset, file="datasets/reg_dataset_R_obj")

