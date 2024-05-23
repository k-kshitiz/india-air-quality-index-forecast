if (!require(caret)) install.packages("caret"); library(caret)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(DescTools)) install.packages("DescTools"); library(DescTools)
if (!require(RcppRoll)) install.packages("RcppRoll"); library(RcppRoll)
if (!require(lime)) install.packages("lime"); library(lime)
if (!require(mice)) install.packages("mice"); library(mice)
if (!require(plyr)) install.packages("plyr"); library(plyr)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(magrittr)) install.packages("magrittr"); library(magrittr)
if (!require(kernlab)) install.packages("kernlab"); library(kernlab)
if (!require(tree)) install.packages("tree"); library(tree)
if (!require(ggfortify)) install.packages("ggfortify"); library(ggfortify)
if (!require(jpeg)) install.packages("jpeg"); library(jpeg)
if (!require(factoextra)) install.packages("factoextra"); library(factoextra)
if (!require(stats)) install.packages("stats"); library(stats)
if (!require(readr)) install.packages("readr"); library(readr)
if (!require(zoo)) install.packages("zoo"); library(zoo)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(Metrics)) install.packages("Metrics"); library(Metrics)
if (!require(forecast)) install.packages("forecast"); library(forecast)
if (!require(tseries)) install.packages("tseries"); library(tseries)

#setwd("/Users/sss/Desktop/Data")

setwd("~/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/scripts/modeling")
path <- '/Users/kshitizsahay/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/data/preprocessed_dataset/daily/'

df_l <- read.csv("/Users/kshitizsahay/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/data/preprocessed_dataset/daily/lucknow_day_filled.csv")
df_d <- read.csv("/Users/kshitizsahay/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/data/preprocessed_dataset/daily/delhi_day_filled.csv")
df_c <- read.csv("/Users/kshitizsahay/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/data/preprocessed_dataset/daily/chennai_day_filled.csv")
df_b <- read.csv("/Users/kshitizsahay/Documents/University of Chicago/ADSP 31006 Time Series Analysis & Forecasting/Project/india-air-quality-index-forecast/data/preprocessed_dataset/daily/bengaluru_day_filled.csv")

df_l %>% head
df_l %>% tail

summary(df_l$AQI)
summary(df_d$AQI)
summary(df_c$AQI)
summary(df_b$AQI)

library(ggplot2)

ts_data_train <- function(df, variable){
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  ts_data <- ts(df[[variable]], start = c(2015, 1), frequency = 365)
  return(ts_data)
}

variable <- "AQI"

df_l_ts <- ts_data_train(df_l, variable)
df_d_ts <- ts_data_train(df_d, variable)
df_c_ts <- ts_data_train(df_c, variable)
df_b_ts <- ts_data_train(df_b, variable)

df_l_ts
df_d_ts
df_c_ts
df_b_ts

######### Checking Plot ###########

plot_time_series <- function(df, city_name, variable) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")  
  
  ggplot(data = df, aes(x = Date, y = get(variable), group = 1)) +
    geom_line(color = "blue") +
    labs(title = paste(city_name, "Time Series Plot of", variable),
         x = "Date",
         y = variable) +
    theme_minimal()
}

plot_time_series(df_l, "Lucknow", variable)
plot_time_series(df_d, "Delhi", variable)
plot_time_series(df_c, "Chennai", variable)
plot_time_series(df_b, "Bengaluru", variable)

df_c%>% tail

df_c[df_c$Date >= '2020-06-01',]
plot_time_series(df_c[df_c$Date >= '2020-06-01',], "Lucknow", variable)

######### Checking  ACF PACF ######### 

plot_acf_pacf <- function(df, city_name, variable) {
  
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  ts_data <- ts(df[[variable]], start = c(2015, 1), frequency = 365)
  
  
  par(mfrow = c(2, 1))
  
  acf(ts_data, main = paste(city_name, "ACF of", variable))
  
  pacf(ts_data, main = paste(city_name, "PACF of", variable))
}


variable <- "AQI"

plot_acf_pacf(df_l, "Lucknow", variable)
plot_acf_pacf(df_d, "Delhi", variable)
plot_acf_pacf(df_c, "Chennai", variable)
plot_acf_pacf(df_b, "Bengaluru", variable)



plot_acf_pacf_diff <- function(df, city_name, variable, diff) {
  
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  ts_data <- ts(df[[variable]], start = c(2015, 1), frequency = 365)
  diff_data <- diff(ts_data, differences = diff)
  
  par(mfrow = c(2, 1))
  
  acf(diff_data, main = paste(city_name, "ACF of", variable, "(", diff, " Differences)", sep = ""))
  
  pacf(diff_data, main = paste(city_name, "PACF of", variable, "(", diff, " Differences)", sep = ""))
}

variable <- "AQI"

plot_acf_pacf_diff(df_l, "Lucknow", variable, 1)
plot_acf_pacf_diff(df_d, "Delhi", variable, 1)
plot_acf_pacf_diff(df_c, "Chennai", variable, 1)
plot_acf_pacf_diff(df_b, "Bengaluru", variable, 1)

######### Prediction ######### 

df_l %>% head
df_l %>% tail

library(forecast)
library(Metrics)
library(ggplot2)
library(dplyr)


model <- auto.arima(df_l_ts)
model

df_check <- df_l
df_check
df_check$Date <- as.Date(df_check$Date, format = "%Y-%m-%d")
df_check

n <- nrow(df_check)
n

horizon<- 30

train_data <- df_check[1:(n - horizon), ]
test_data <- df_check[(n - horizon + 1):n, ]

variable <- 'AQI'
ts_train <- ts(train_data[[variable]], start = c(2015, 1), frequency = 365)
ts_test <- ts(test_data[[variable]], start = c(2020, 6), frequency = 365)

ts_train

ts_test

model <- auto.arima(ts_train)

forecasted_values <- forecast(model, h = horizon)$mean
forecasted_values


plot(forecasted_values, main = paste('city_name', "AQI Forecast"))
lines(test_data$Date, test_data[[variable]], col = "red")
lines(test_data$Date, test_data[[variable]], col = "red")

test_data[[variable]]
library(xts)


xts_train <- xts(train_data[[variable]], order.by = train_data$Date)
xts_train


forecast_and_evaluate <- function(df, city_name, variable, horizon = 30) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  n <- nrow(df)
  
  train_data <- df[1:(n - horizon), ]
  test_data <- df[(n - horizon + 1):n, ]
  
  ts_train <- ts(train_data[[variable]], start = c(2015, 1), frequency = 365)
  ts_test <- ts(test_data[[variable]], start = c(2020, 6), frequency = 365)
  
  model <- auto.arima(ts_train)
  
  forecasted_values <- forecast(model, h = horizon)$mean
  
  mse_value <- mean((test_data[[variable]] - forecasted_values)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(test_data[[variable]] - forecasted_values))
  mape_value <- mean(abs((test_data[[variable]] - forecasted_values) / test_data[[variable]])) * 100
  amape_value <- mean(abs((test_data[[variable]] - forecasted_values) / mean(test_data[[variable]]))) * 100
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  
  cat("City:", city_name, "\n")
  cat("MSE:", mse_value, "\n")
  cat("RMSE:", rmse_value, "\n")
  cat("MAE:", mae_value, "\n")
  cat("MAPE:", mape_value, "\n")
  cat("AMAPE:", amape_value, "\n")
  cat("AIC:", aic_value, "\n")
  cat("BIC:", bic_value, "\n")
  
  plot(forecast(model, h = horizon), main = paste(city_name, "AQI Forecast"))
  lines(test_data$Date, test_data[[variable]], col = "red")
  
  return(list(forecast = forecasted_values, test_data = ts_test, model = model))
}

variable <- "AQI"

forecast_l <- forecast_and_evaluate(df_l, "Lucknow", variable)

forecast_d <- forecast_and_evaluate(df_d, "Delhi", variable)

forecast_c <- forecast_and_evaluate(df_c, "Chennai", variable)

forecast_b <- forecast_and_evaluate(df_b, "Bengaluru", variable)


######### Only for the predicted period ######### 

forecast_and_evaluate <- function(df, city_name, variable, horizon = 30) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  n <- nrow(df)
  
  train_data <- df[1:(n - horizon), ]
  test_data <- df[(n - horizon + 1):n, ]
  
  ts_train <- ts(train_data[[variable]], start = c(2015, 1), frequency = 365)
  ts_test_length <- nrow(test_data)  # test_data의 행 수 계산
  
  model <- auto.arima(ts_train)
  
  forecasted_values <- forecast(model, h = ts_test_length)  # test_data의 길이에 맞춰 예측
  
  # 예측 기간만 추출
  forecasted_dates <- seq(from = max(train_data$Date) + 1, by = "day", length.out = ts_test_length)
  
  # 2020년 6월 이후 데이터만 선택
  selected_test_data <- subset(test_data, Date >= as.Date("2020-06-01"))
  
  # selected_forecasted_dates 계산 수정
  selected_forecasted_dates <- forecasted_dates[forecasted_dates >= as.Date("2020-06-01")]
  
  plot(selected_forecasted_dates, forecasted_values$mean[forecasted_dates >= as.Date("2020-06-01")], type='l', col = "red", main = paste(city_name, "AQI Forecast"), lty = 2)
  lines(selected_test_data$Date, selected_test_data[[variable]], col = "blue")  

  
  cat("City:", city_name, "\n")
  cat("Forecasted Period:", format(min(selected_forecasted_dates), "%Y-%m"), "to", format(max(selected_forecasted_dates), "%Y-%m"), "\n")
  
  return(forecasted_values)
}


variable <- "AQI"

forecast_l <- forecast_and_evaluate(df_l, "Lucknow", variable)

forecast_d <- forecast_and_evaluate(df_d, "Delhi", variable)

forecast_c <- forecast_and_evaluate(df_c, "Chennai", variable)

forecast_b <- forecast_and_evaluate(df_b, "Bengaluru", variable)


######### Metrics ######### 

forecast_and_evaluate <- function(df, city_name, variable, horizon = 30) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  n <- nrow(df)
  
  train_data <- df[1:(n - horizon), ]
  test_data <- df[(n - horizon + 1):n, ]
  
  ts_train <- ts(train_data[[variable]], start = c(2015, 1), frequency = 365)
  ts_test_length <- nrow(test_data)  # test_data의 행 수 계산
  
  model <- auto.arima(ts_train)
  
  forecasted_values <- forecast(model, h = ts_test_length)  # test_data의 길이에 맞춰 예측
  
  # 예측 기간만 추출
  forecasted_dates <- seq(from = max(train_data$Date) + 1, by = "day", length.out = ts_test_length)
  
  # 2020년 6월 이후 데이터만 선택
  selected_test_data <- subset(test_data, Date >= as.Date("2020-06-01"))
  selected_forecasted_values <- forecasted_values$mean[forecasted_dates >= as.Date("2020-06-01")]
  
  # 성능 평가 지표 계산
  mse_value <- mean((selected_test_data[[variable]] - selected_forecasted_values)^2)
  rmse_value <- sqrt(mse_value)
  mae_value <- mean(abs(selected_test_data[[variable]] - selected_forecasted_values))
  mape_value <- mean(abs((selected_test_data[[variable]] - selected_forecasted_values) / selected_test_data[[variable]])) * 100
  amape_value <- mean(abs((selected_test_data[[variable]] - selected_forecasted_values) / mean(selected_test_data[[variable]]))) * 100
  
  # 평가 지표 출력
  cat("City:", city_name, "\n")
  cat("Forecasted Period:", format(min(selected_test_data$Date), "%Y-%m"), "to", format(max(selected_test_data$Date), "%Y-%m"), "\n")
  cat("MSE:", mse_value, "\n")
  cat("RMSE:", rmse_value, "\n")
  cat("MAE:", mae_value, "\n")
  cat("MAPE:", mape_value, "\n")
  cat("AMAPE:", amape_value, "\n")
  
  return(list(model = model, MSE = mse_value, RMSE = rmse_value, MAE = mae_value, MAPE = mape_value, AMAPE = amape_value))
}

variable <- "AQI"

forecast_l <- forecast_and_evaluate(df_l, "Lucknow", variable)

forecast_d <- forecast_and_evaluate(df_d, "Delhi", variable)

forecast_c <- forecast_and_evaluate(df_c, "Chennai", variable)

forecast_b <- forecast_and_evaluate(df_b, "Bengaluru", variable)