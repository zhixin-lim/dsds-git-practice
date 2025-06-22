library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(simmer)
library(simmer.plot)
library(VGAM)
library(gridExtra)
library(fitdistrplus)
library(tidyr)


source("get_results.R")

# Only for multiple carpark
simulation_results <- run_sim_n_times_m(10,
                                        cp_list = c("3", "3a", "4", "5", "5b", "6b"),
                                        cp_state = c("open", "open", "open", "open", "open", "open"),
                                        cp_capacity = c(243, 67, 116, 70, 32, 173),
                                        cp_sheltered = c(100, 0, 0, 0, 0, 100),
                                        cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                                        arrival_rate = "historical",
                                        month_st = "Mar", month_en = "May",
                                        distance_matrix = matrix(c(
                                          0, 500, 1000, 150, 200, 250,
                                          500, 0, 800, 1300, 180, 230,
                                          1000, 800, 0, 600, 1100, 160,
                                          150, 1300, 600, 0, 500, 1000,
                                          200, 180, 1100, 500, 0, 500,
                                          250, 230, 160, 1000, 500, 0
                                        ), nrow = 6, byrow = TRUE,
                                        dimnames = list(cp_list, cp_list)))

# Access the results from the simulation
df_main <- simulation_results[[1]]
plist_red_util <- simulation_results[[2]]
plist_white_util <- simulation_results[[3]]
plist_red_use <- simulation_results[[4]]
plist_white_use <- simulation_results[[5]]

calculate_daily_utilization <- function(data) {
  daily_utilization <- numeric()
  for (day in unique(data$enter_time)) {
    lots_occupied <- sum(data$du_val[data$enter_time == day])
    utilization <- lots_occupied * 100 / capacity
    # daily_utilization is a vector which contains the daily util. rate
    daily_utilization <- c(daily_utilization, min(utilization, 100)) # to account for demand exceeding capacity
  }
  return(mean(daily_utilization))
}



cp_capacity = c(243, 67, 116, 70, 32, 173)
cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)

# Red
for (i in 1:6){
  cp = c("3", "3a", "4", "5", "5b", "6b")[i]
  cp_name = str_c("car_park_", cp)
  df_aux <- df_main %>%
    filter(resource==cp_name) %>%
    group_by(mon) %>%
    summarise(red_mean_parking = mean(red_mean_parking)*100) %>%
    rename(red_mean_parking_perc = red_mean_parking)
  capacity <- cp_capacity[i]*cp_red_perc[i]/100
  dfname <- paste0("red", "_", cp)
  df <- get(dfname)
  average_utilization_mar <- calculate_daily_utilization(df[df$month == 3, ])
  average_utilization_apr <- calculate_daily_utilization(df[df$month == 4, ])
  average_utilization_may <- calculate_daily_utilization(df[df$month == 5, ])
  result <- data.frame(
    month = c("Mar", "Apr", "May"),
    average_utilization = c(average_utilization_mar, average_utilization_apr, average_utilization_may)
  )
  merged_df <- merge(df_aux, result, by.x = "mon", by.y = "month") %>%
    rename(`estimated average util. rate in percentage`=red_mean_parking_perc,
           `ground truth util. rate in percentage`=average_utilization)
  print(merged_df)
  
}


# White
for (i in 1:6){
  cp = c("3", "3a", "4", "5", "5b", "6b")[i]
  cp_name = str_c("car_park_", cp)
  df_aux <- df_main %>%
    filter(resource==cp_name) %>%
    group_by(mon) %>%
    summarise(red_mean_parking = mean(red_mean_parking)*100) %>%
    rename(red_mean_parking_perc = red_mean_parking)
  capacity <- cp_capacity[i]*cp_red_perc[i]/100
  dfname <- paste0("white", "_", cp)
  df <- get(dfname)
  average_utilization_mar <- calculate_daily_utilization(df[df$month == 3, ])
  average_utilization_apr <- calculate_daily_utilization(df[df$month == 4, ])
  average_utilization_may <- calculate_daily_utilization(df[df$month == 5, ])
  result <- data.frame(
    month = c("Mar", "Apr", "May"),
    average_utilization = c(average_utilization_mar, average_utilization_apr, average_utilization_may)
  )
  merged_df <- merge(df_aux, result, by.x = "mon", by.y = "month") %>%
    rename(`estimated average util. rate in percentage`=red_mean_parking_perc,
           `ground truth util. rate in percentage`=average_utilization)
  print(merged_df)
  
}




