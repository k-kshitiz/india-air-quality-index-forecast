library(readr)
library(imputeTS)
library(dplyr)
library(tools)
library(xts)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/GIGABYTE/Desktop/ªÛ¥[­ô¤j¾Ç12391638/Courses/3rd Quarter (2024 Spring)/ADSP 31006 IP02 Time Series Analysis and Forecasting/FINAL/archive/cleaned_data")

interpolation<- function(file_paths) {

  for (file_path in file_paths) {
    
    file_name <- file_path_sans_ext(basename(file_path))
    
    data <- read_csv(file_path) %>%
      select(-PM10, -Xylene, -NH3, -City)
    assign(file_name, data, envir = .GlobalEnv)

    filled_data <- na_ma(data, k = 15, weighting = "exponential") # front 15 days + back 15 days = 1 month
    assign(paste0(file_name, "_filled"), filled_data, envir = .GlobalEnv)
    
  }
  
}

################################################################################

day_list <- c('Lucknow_day.csv',
              'Mumbai_day.csv',
              'Bengaluru_day.csv',
              'Chennai_day.csv',
              'Delhi_day.csv')

interpolation(day_list)

any(is.na(Bengaluru_day_filled))
any(is.na(Chennai_day_filled))
any(is.na(Delhi_day_filled))
any(is.na(Lucknow_day_filled))
any(is.na(Mumbai_day_filled))

write_csv(Bengaluru_day_filled, "Bengaluru_day_filled.csv")
write_csv(Chennai_day_filled, "Chennai_day_filled.csv")
write_csv(Delhi_day_filled, "Delhi_day_filled.csv")
write_csv(Lucknow_day_filled, "Lucknow_day_filled.csv")
write_csv(Mumbai_day_filled, "Mumbai_day_filled.csv")

ggplot_na_distribution(Bengaluru_day_filled$AQI, title = 'Bengaluru_day_filled')
ggplot_na_distribution(Chennai_day_filled$AQI, title = 'Chennai_day_filled')
ggplot_na_distribution(Delhi_day_filled$AQI, title = 'Delhi_day_filled')
ggplot_na_distribution(Lucknow_day_filled$AQI, title = 'Lucknow_day_filled')
ggplot_na_distribution(Mumbai_day_filled$AQI, title = 'Mumbai_day_filled')

ggplot_na_imputations(Bengaluru_day$AQI, Bengaluru_day_filled$AQI, title = 'Bengaluru')
ggplot_na_imputations(Chennai_day$AQI, Chennai_day_filled$AQI, title = 'Chennai')
ggplot_na_imputations(Delhi_day$AQI, Delhi_day_filled$AQI, title = 'Delhi')
ggplot_na_imputations(Lucknow_day$AQI, Lucknow_day_filled$AQI, title = 'Lucknow')
ggplot_na_imputations(Mumbai_day$AQI, Mumbai_day_filled$AQI, title = 'Mumbai')

# Assuming ggplot_na_imputations function is defined and works as expected
plot_bengaluru <- ggplot_na_imputations(Bengaluru_day$AQI, Bengaluru_day_filled$AQI, title = 'Bengaluru')
plot_chennai <- ggplot_na_imputations(Chennai_day$AQI, Chennai_day_filled$AQI, title = 'Chennai')
plot_delhi <- ggplot_na_imputations(Delhi_day$AQI, Delhi_day_filled$AQI, title = 'Delhi')
plot_lucknow <- ggplot_na_imputations(Lucknow_day$AQI, Lucknow_day_filled$AQI, title = 'Lucknow')

# Arrange the plots in a 2x2 grid
grid.arrange(plot_bengaluru, plot_chennai, plot_delhi, plot_lucknow, nrow = 2, ncol = 2)
################################################################################

# hour_list <-  c('Lucknow_hour.csv',
#                 'Mumbai_hour.csv',
#                 'Bengaluru_hour.csv',
#                 'Chennai_hour.csv',
#                 'Delhi_hour.csv')
# 
# interpolation(hour_list)
# 
# any(is.na(Bengaluru_hour_filled))
# any(is.na(Chennai_hour_filled))
# any(is.na(Delhi_hour_filled))
# any(is.na(Lucknow_hour_filled))
# any(is.na(Mumbai_hour_filled))