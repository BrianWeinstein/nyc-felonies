
# Load libraries
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(GGally)
library(tidyr)

# set working directory
setwd("~/Documents/nyc-felonies")

# load the full_dataset dataframe
load(file="datasets/full_dataset_R_obj")
reg_dataset <- full_dataset


# remove unnecessary/extra variables ############################################################

# no questions of interest around windspeed
reg_dataset$windspeed_avg_mph <- NULL


# examine numeric variables ############################################################

# plot a matrix of pairwise scatterplots for the numeric vars
plot.pairs <- ggpairs(data=select(reg_dataset,
                                  accidents, accident_injuriesDeaths,
                                  temp_min_degF, temp_max_degF,
                                  school_attendance_pct, felonies),
                      lower=list(continuous=wrap("points", size=0.5)))
plot.pairs
png(filename="eda_plots/pairs_numeric.png", width=15, height=12, units="in", res=300)
print(plot.pairs)
dev.off()
rm(plot.pairs)

# many of the numeric covariates are highly correlated
# temp_min_degF and temp_max_degF
# accidents and accident_injuriesDeaths

# restated: accidents and accident_injuriesDeaths are highly correlated
# also not clear if a traffic accidents with injuriesDeaths can contribute to the 
# felonies count (namely the grand larceny of motor vehicle, and murder and non-negligent manslaughter types)
# removing accident_injuriesDeaths from the dataset to help reduce the number of covariates
reg_dataset$accident_injuriesDeaths <- NULL

# school_atttendance_pct is either 0, or between 74.63 and 93.91
# not enough data between 0 and 74.63 for inference
# use the is_school_day indicator variable instead
reg_dataset$school_attendance_pct <- NULL

# felonies vs max temp
ggplot(reg_dataset, aes(x=temp_max_degF, y=felonies)) + geom_point()
ggsave(filename="eda_plots/felonies_vs_maxTemp.png", width=6.125, height=3.5, units="in")

# examine categorical variables ############################################################

# redefine day of week levels as factor(integers)
day_of_week_dict <- data.frame(day_of_week=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                               day_of_week_ndx=c(1, 2, 3, 4, 5, 6, 7))
reg_dataset <- left_join(reg_dataset, day_of_week_dict, by="day_of_week") %>%
  mutate(day_of_week=factor(day_of_week_ndx),
         day_of_week_ndx=NULL)
rm(day_of_week_dict)

# plot all the categorical vars together
ggplot(reg_dataset %>%
         select(felonies, is_school_day, is_weekend, day_of_week, any_precip, is_holiday) %>%
         gather(key = "metric", value = "value",
                c(is_school_day, is_weekend, day_of_week, any_precip, is_holiday)),
       aes(x=value, y=felonies)) +
  geom_boxplot() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3, switch = "x") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x=NULL)
ggsave(filename="eda_plots/facet_categorical.png", width=9, height=5, units="in", dpi = 300)


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
# remove from model, in favor of day_of_week (below)
reg_dataset$is_weekend <- NULL

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


# plot date ############################################################

# date
ggplot(reg_dataset, aes(x=date, y=felonies)) + geom_point()
ggsave(filename="eda_plots/felonies_vs_maxTemp.png", width=6.125, height=3.5, units="in")
# clearly a sinusoidal trend
# temp will likely explain this behavior


# save reg_dataset as csv and r object ############################################################

write.csv(reg_dataset, file = "datasets/reg_dataset.csv", row.names = FALSE)
save(reg_dataset, file="datasets/reg_dataset_R_obj")



# some questions of interest
# are the number of felonies associated with temp? (either min or max temp)
# is felonies associated with presence of precipitation?
# does the association between precip and felonies depend on temperature?
# after taking temp into account, is felonies associated with accidents?
# is felonies associated with holidays?
# is felonies associated with school days?
# is felonies associated with day of week?



