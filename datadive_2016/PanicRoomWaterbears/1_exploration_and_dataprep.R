library(tidyverse)
library(lubridate)

list.files("data")

testset <- read_csv("data/nettebad_test_set.csv")
testset <- testset %>% mutate(date = mdy(date))
trainset <- read_csv("data/nettebad_train_set.csv")
trainset <- trainset %>% mutate(date = mdy(date))
dwd_test <- read_csv("data/weather_dwd_test_set.csv")
dwd_test <- dwd_test %>% mutate(date = mdy(date))
dwd_train <- read_csv("data/weather_dwd_train_set.csv")
dwd_train <- dwd_train %>% mutate(date = mdy(date))
uni_train <- read_csv("data/weather_uni_osnabrueck_train_set.csv")
uni_train <- uni_train %>% mutate(date = ymd(date))
uni_test <- read_csv("data/weather_uni_osnabrueck_test_set.csv")
uni_test <- uni_test %>% mutate(date = mdy(date))

summary(trainset)

hist(trainset$visitors_pool_total, 100)
plot(trainset$visitors_pool_total, type = "l")
ggplot(trainset, aes(x = date, y = visitors_pool_total)) + geom_line()

#
# Add weekdays
#
trainset <- trainset %>% mutate(wday = factor(wday(date)))
testset <- testset %>% mutate(wday = factor(wday(date)))
ggplot(trainset, aes(y = visitors_pool_total, x = factor(wday))) + geom_boxplot()

#
# price
#
ggplot(trainset, aes(x = date, y = price_adult_90min)) + geom_line()
ggplot(trainset, aes(x = date, y = price_adult_max)) + geom_line()
ggplot(trainset, aes(x = date, y = price_reduced_90min)) + geom_line()
ggplot(trainset, aes(x = date, y = price_reduced_max)) + geom_line()
# Ratio
ggplot(trainset, aes(x = date, y = price_adult_max / price_reduced_max)) + geom_line()

# Add the ratio
trainset <- trainset %>% mutate(p_ratio = price_adult_max / price_reduced_max)
testset <- testset %>% mutate(p_ratio = price_adult_max / price_reduced_max)

#
# Time series models
#
library(forecast)
Acf(trainset$visitors_pool_total)
Pacf(trainset$visitors_pool_total)
library(xts)
temp <- xts(trainset$visitors_pool_total, order.by = trainset$date)
auto.arima(trainset$visitors_pool_total, stepwise = F)
temp <- to.weekly(temp)
temp <- temp[, 4]
auto.arima(trainset$visitors_pool_total, stepwise = F)

#
# Read in weather
#
dwd_train <- dwd_train %>% dplyr::rename(humidity_dwd = air_humidity_DWD,
                                  temp_dwd = air_temperature_daily_mean_DWD)
uni_train <- uni_train %>% dplyr::rename(humidity_uni = air_humidity_UniOS,
                                  temp_uni = temperature_UniOS)
dwd_test <- dwd_test %>% dplyr::rename(humidity_dwd = air_humidity_DWD,
                                  temp_dwd = air_temperature_daily_mean_DWD)
uni_test <- uni_test %>% dplyr::rename(humidity_uni = air_humidity_UniOS,
                                  temp_uni = temperature_UniOS)

# Plots for trainset
temp <- inner_join(uni_train, dwd_train, by = "date")
plot(temp$temp_uni, temp$temp_dwd)
plot(temp$humidity_uni, temp$humidity_dwd)

#
# Join with train and test set
#
trainset2 <- trainset
trainset2 <- left_join(trainset2, dwd_train, by = "date")
trainset2 <- left_join(trainset2, uni_train, by = "date")
# Merge temperatures
merged_temp <- trainset2 %>%
    select(temp_uni, temp_dwd)
merged_temp <- rowMeans(merged_temp, na.rm = T)
na <- which(is.na(merged_temp))
# Some manual imputation where it was simple
merged_temp[na[1]] <- 13.9
merged_temp[na[2]] <- 21.9
merged_temp[na[3]] <- 22
trainset2$temp <- merged_temp

merged_humidity <- trainset2 %>%
    select(humidity_uni, humidity_dwd)
merged_humidity <- rowMeans(merged_humidity, na.rm = T)
na <- which(is.na(merged_humidity))
merged_humidity[na[6]] <- 70
merged_humidity[na[7]] <- 69
# From Münster
humidity_m <- c(90, 89, 90, 79, 70)
merged_humidity[na[1:5]] <- humidity_m
trainset2$humidity <- merged_humidity

# Merge max. wind speeds
merged_wind_max <- trainset2 %>%
    select(wind_speed_max_DWD, wind_speed_max_UniOS)
merged_wind_max <- rowMeans(merged_wind_max, na.rm = T)
trainset2$wind_max <- merged_wind_max
# Still has some NAs

# Testset
testset2 <- testset
testset2 <- left_join(testset2, dwd_test, by = "date")
testset2 <- left_join(testset2, uni_test, by = "date")
testset2 <- testset2 %>% mutate(temp = temp_uni)

testset2 <- testset2 %>% mutate(humidity = humidity_uni)

testset2$wind_max <- testset2$wind_speed_max_UniOS

# Holiday dummy
trainset2 <- trainset2 %>% mutate(Ferien_NDS = as.numeric(school_holiday == 1))
trainset2 <- trainset2 %>% mutate(Ferien_NRW = as.numeric(school_holiday == 2))
testset2 <- testset2 %>% mutate(Ferien_NDS = as.numeric(school_holiday == 1))
testset2 <- testset2 %>% mutate(Ferien_NRW = as.numeric(school_holiday == 2))

#
# External weather data
#
ext_w <- read_csv("MUNOSNA_DWD.csv")
ext_w <- ext_w %>% mutate(date = mdy(date))
ext_w$WINDGESCHWINDIGKEIT[ext_w$WINDGESCHWINDIGKEIT == -999] <- NA
trainset2 <- left_join(trainset2, ext_w, by = "date")
testset2 <- left_join(testset2, ext_w, by = "date")

# Impute WINDGESCHWINDIGKEIT
windgesch_na <- which(is.na(trainset2$WINDGESCHWINDIGKEIT))
trainset2$WINDGESCHWINDIGKEIT[windgesch_na] <- trainset2$wind_speed_avg_UniOS[windgesch_na]
# No NAs in the test set

# Drop features
trainset2 <- trainset2 %>% select(date, visitors_pool_total, sportbad_closed,
                                  freizeitbad_closed, sauna_closed, kursbecken_closed,
                                  event, price_adult_90min, price_adult_max,
                                  price_reduced_90min, price_reduced_max,
                                  sloop_dummy, sloop_days_since_opening,
                                  school_holiday, bank_holiday, wday, humidity,
                                  temp, precipitation_DWD, snow_height_DWD,
                                  sunshine_hours_DWD, wind_max, air_pressure_UniOS,
                                  LUFTDRUCK_STATIONSHOEHE, WINDGESCHWINDIGKEIT,
                                  SONNENSCHEINDAUER, NIEDERSCHLAGSHOEHE)
testset2 <- testset2 %>% select(date, sportbad_closed,
                                  freizeitbad_closed, sauna_closed, kursbecken_closed,
                                  event, price_adult_90min, price_adult_max,
                                  price_reduced_90min, price_reduced_max,
                                  sloop_dummy, sloop_days_since_opening,
                                  school_holiday, bank_holiday, wday, humidity,
                                  temp, precipitation_DWD, snow_height_DWD,
                                  sunshine_hours_DWD, wind_max, air_pressure_UniOS,
                                  LUFTDRUCK_STATIONSHOEHE, WINDGESCHWINDIGKEIT,
                                  SONNENSCHEINDAUER, NIEDERSCHLAGSHOEHE)

# Expand wday to dummy variables
temp <- model.matrix(~wday-1, trainset2)
temp <- temp[, -7]
trainset2 <- cbind(trainset2, temp)
temp <- model.matrix(~wday-1, testset2)
temp <- temp[, -7]
testset2 <- cbind(testset2, temp)

# Month numbers
trainset2 <- trainset2 %>% mutate(monthnr = month(date))
temp <- model.matrix(~factor(monthnr)-1, trainset2)
trainset2 <- cbind(trainset2, temp)
testset2 <- testset2 %>% mutate(monthnr = month(date))
temp <- model.matrix(~ factor(monthnr) - 1, testset2)
testset2 <- cbind(testset2, temp)

# Week numbers
trainset2 <- trainset2 %>% mutate(weeknr = week(date))
temp <- model.matrix(~factor(weeknr)-1, trainset2)
trainset2 <- cbind(trainset2, temp)
testset2 <- testset2 %>% mutate(weeknr = week(date))
temp <- model.matrix(~ factor(weeknr) - 1, testset2)
testset2 <- cbind(testset2, temp)

ggplot(trainset2, aes(y = visitors_pool_total, x = factor(weeknr),
                      color = factor(wday1), shape = factor(wday1))) +
    geom_jitter(alpha = 0.7, size = 4)

#
# Add lags
#
visitors_lag <- c(rep(NA, 364), trainset2$visitors_pool_total)
trainset2 <- trainset2 %>% mutate(visitors_lag364 = visitors_lag[1:nrow(trainset2)])
testset2 <- testset2 %>%
    mutate(visitors_lag364 = visitors_lag[(nrow(trainset2) + 1):(nrow(trainset2) + nrow(testset2))])


visitors_lag <- c(rep(NA, 364), trainset2$visitors_pool_total)
trainset2 <- trainset2 %>% mutate(visitors_lag364 = visitors_lag[1:nrow(trainset2)])
testset2 <- testset2 %>%
    mutate(visitors_lag364 = visitors_lag[(nrow(trainset2) + 1):(nrow(trainset2) + nrow(testset2))])

#
# Number of closed sections
#
trainset2 <- trainset2 %>%
    mutate(nr_closed = sportbad_closed + freizeitbad_closed + sauna_closed + kursbecken_closed)
testset2 <- testset2 %>%
    mutate(nr_closed = sportbad_closed + freizeitbad_closed + sauna_closed + kursbecken_closed)

#
# Allow for some interactions or polynomials
#
trainset2 <- trainset2 %>% mutate(weeknr_sq = weeknr ** 2)
testset2 <- testset2 %>% mutate(weeknr_sq = weeknr ** 2)
trainset2 <- trainset2 %>% mutate(monthnr_sq = monthnr ** 2)
testset2 <- testset2 %>% mutate(monthnr_sq = monthnr ** 2)
testset2 <- testset2 %>% mutate(sloop_since_opening_sq = sloop_days_since_opening ** 2)

#
# Save
#
save(trainset2, file = "data/trainset2.RData")
save(testset2, file = "data/testset2.RData")
write_csv(trainset2, "data/trainset2.csv")
write_csv(testset2, "data/testset2.csv")




