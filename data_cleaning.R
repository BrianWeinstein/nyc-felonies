
# Load libraries
library(dplyr)
library(lubridate)

# set working directory
setwd("~/Documents/nyc-felonies")


# NYPD_7_Major_Felony_Incidents ################################################################

# load raw dataset
felony_raw <- read.csv(file = "datasets/raw/NYPD_7_Major_Felony_Incidents_CUT.csv",
                       stringsAsFactors = FALSE)

# reformat timestamps, filter, rename vars, and order by date
felony <- felony_raw %>%
  mutate(timestamp=as.POSIXct(Occurrence.Date, format="%m/%d/%Y %I:%M:%S %p"),
         date=as.Date(timestamp)) %>%
  filter(date >= "2014-01-01" & date <= "2016-03-11") %>%
  rename(offense=Offense) %>%
  arrange(date)

# count number of felonies by day, by offense
felony_date_type <- felony %>%
  group_by(date, offense) %>%
  summarize(num_felonies=n())

# count number of felonies by day
felony_date <- felony_date_type %>%
  group_by(date) %>%
  summarize(num_felonies=sum(num_felonies))

# write to csv
write.csv(felony_date_type, file = "datasets/clean/felony_date_type.csv", row.names = FALSE)
write.csv(felony_date, file = "datasets/clean/felony_date.csv", row.names = FALSE)

# remove temp objects
rm(felony_raw, felony, felony_date_type)



# NYPD_Motor_Vehicle_Collisions ################################################################

# load raw dataset
accident_raw <- read.csv(file = "datasets/raw/NYPD_Motor_Vehicle_Collisions_CUT.csv",
                         stringsAsFactors = FALSE)

# reformat timestamps, filter, rename vars, and order by date
accident <- accident_raw %>%
  mutate(date=as.Date(DATE, format = "%m/%d/%Y")) %>%
  filter(date >= "2014-01-01" & date <= "2016-03-11") %>%
  rename(injuries=NUMBER.OF.PERSONS.INJURED,
         deaths=NUMBER.OF.PERSONS.KILLED) %>%
  arrange(date)

# count number of accidents, injuries, and deaths, by day
accident_date <- accident %>%
  group_by(date) %>%
  summarize(num_accidents=n(),
            num_injuries=sum(injuries),
            num_deaths=sum(deaths))


# write to csv
write.csv(accident_date, file = "datasets/clean/accident_date.csv", row.names = FALSE)

# remove temp objects
rm(accident_raw, accident)



# weather_702997 ################################################################

# load raw dataset
weather_raw <- read.csv(file = "datasets/raw/weather_702997.csv",
                        stringsAsFactors = FALSE)

# select vars, reformat timestamps, filter, rename vars, and order by date
weather_date <- weather_raw %>%
  select(DATE, PRCP, SNOW, TMAX, TMIN, AWND) %>%
  mutate(DATE=as.Date(as.character(DATE), format = "%Y%m%d")) %>%
  filter(DATE >= "2014-01-01" & DATE <= "2016-03-11") %>%
  mutate(precip_in = (PRCP / 10) * 0.0393701,
         snowfall_in = SNOW * 0.0393701,
         temp_min_degF = ((TMIN / 10) * (9/5)) + 32,
         temp_max_degF = ((TMAX / 10) * (9/5)) + 32,
         windspeed_avg_mph = (AWND / 10) *  2.23694) %>%
  select(date=DATE,
         precip_in,
         snowfall_in,
         temp_min_degF, temp_max_degF,
         windspeed_avg_mph) %>%
  arrange(date)

# write to csv
write.csv(weather_date, file = "datasets/clean/weather_date.csv", row.names = FALSE)

# remove temp objects
rm(weather_raw)



# ny_holidays ################################################################

# load raw dataset
holiday_raw <- read.csv(file = "datasets/raw/ny_holidays.csv",
                        stringsAsFactors = FALSE)

# reformat timestamps and order by date
holiday_date <- holiday_raw %>%
  mutate(date=as.Date(date, format = "%m/%d/%y")) %>%
  arrange(date)

# write to csv
write.csv(holiday_date, file = "datasets/clean/holiday_date.csv", row.names = FALSE)

# remove temp objects
rm(holiday_raw)



# school_attendance ################################################################

# load raw dataset
attendance_raw <- read.csv(file = "datasets/raw/school_attendance.csv",
                         stringsAsFactors = FALSE)

# reformat timestamps, rename vars, and order by date
attendance_date <- attendance_raw %>%
  mutate(date=as.Date(date, format = "%Y-%m-%d")) %>%
  rename(attendance_pct = attn_pct) %>%
  arrange(date)

# write to csv
write.csv(attendance_date, file = "datasets/clean/attendance_date.csv", row.names = FALSE)

# remove temp objects
rm(attendance_raw)



# join all datasets and create indicator variables ################################################################

# create a date list
all_dates <- data.frame(date = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 1))

# join all data frames to all_dates
full_dataset <- left_join(x = all_dates, y = felony_date, by = "date") %>%
  left_join(x = ., y = accident_date, by = "date") %>%
  left_join(x = ., y = weather_date, by = "date") %>%
  left_join(x = ., y = holiday_date, by = "date") %>%
  left_join(x = ., y = attendance_date, by = "date")
  
# replace NA with 0
full_dataset[is.na(full_dataset)] <- 0

# select and create indicator vars
full_dataset <- full_dataset %>%
  mutate(any_precip = as.factor(ifelse(precip_in > 0 | snowfall_in > 0, 1, 0)),
         accident_injuriesDeaths = num_injuries + num_deaths,
         is_school_day = as.factor(ifelse(attendance_pct > 0, 1, 0)),
         day_of_week = as.factor(weekdays(date)),
         is_weekend = as.factor(ifelse(day_of_week %in% c("Saturday", "Sunday"), 1, 0)),
         is_holiday = as.factor(is_holiday)) %>%
  select(date, day_of_week, is_weekend,
         felonies = num_felonies,
         accidents = num_accidents, accident_injuriesDeaths,
         any_precip, temp_min_degF, temp_max_degF, windspeed_avg_mph,
         is_holiday,
         school_attendance_pct = attendance_pct, is_school_day)

# write to csv
write.csv(full_dataset, file = "datasets/full_dataset.csv", row.names = FALSE)

# save as r object
save(full_dataset, file="datasets/full_dataset_R_obj")


