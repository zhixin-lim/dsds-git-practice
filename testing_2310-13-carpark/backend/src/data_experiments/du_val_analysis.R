library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(fitdistrplus)
library(VGAM)

df_5 <- read.csv("../data/df_5.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val)) 

df_6b <- read.csv("../data/df_6b.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val)) 

df_4 <- read.csv("../data/df_4.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val)) 

df_3 <- read.csv("../data/df_3.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val)) 

df_5b <- read.csv("../data/df_5b.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val)) 

df_3a <- read.csv("../data/df_3a.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19, du_val > 10) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         wkd = weekdays(enter_time),
         isweekend = wkd %in% c("Saturday", "Sunday"),
         month = month(enter_time)) %>%
  # Arrange in proper order
  arrange(enter_time) %>%
  filter(du_val < quantile(du_val, 0.75) + 1.5*IQR(du_val),
         du_val > quantile(du_val, 0.25) - 1.5*IQR(du_val))

# Create a function to generate the plots
plot_histogram_and_density <- function(df, is_weekend, slot_label) {
  breaks <- 70
  carpark_name <- sub("df_", "", deparse(substitute(df)))  # Extract carpark name from data frame name
  slot_name <- ifelse(slot_label == "white", "non-season", "season")
  title <- if (is_weekend) {
    title <- paste("Histogram of", slot_name, "parkers during weekends carpark", carpark_name)
  } else {
    title <- paste("Histogram of", slot_name, "parkers during weekdays carpark", carpark_name)
  }
  
  filtered_df <- df %>%
    filter(isweekend==is_weekend, slot==slot_label)
  hist_data <- hist(filtered_df$du_val, breaks = breaks, main = title, prob = TRUE, xlab="Duration")
  lines(density(filtered_df$du_val), col = "blue", lwd = 2)
  lines(density(filtered_df$du_val, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
  
  return(hist_data)
}

# Example usage for df_6b, df_4, df_3, df_5b, df_3a, df_5
hist_wday6b <- plot_histogram_and_density(df_6b, is_weekend = FALSE, slot_label = "white")
hist_wend6b <- plot_histogram_and_density(df_6b, is_weekend = TRUE,slot_label = "white")

hist_rday6b <- plot_histogram_and_density(df_6b, is_weekend = FALSE, slot_label = "red")
hist_rend6b <- plot_histogram_and_density(df_6b, is_weekend = TRUE,slot_label = "red")

hist_rday4 <- plot_histogram_and_density(df_4, is_weekend = FALSE,slot_label = "white")
hist_rend4 <- plot_histogram_and_density(df_4, is_weekend = TRUE,slot_label = "white")

hist_rday4 <- plot_histogram_and_density(df_4, is_weekend = FALSE,slot_label = "red")
hist_rend4 <- plot_histogram_and_density(df_4, is_weekend = TRUE,slot_label = "red")

hist_wday3 <- plot_histogram_and_density(df_3, is_weekend = FALSE,slot_label = "white")
hist_wend3 <- plot_histogram_and_density(df_3, is_weekend = TRUE,slot_label = "white")

hist_rday3 <- plot_histogram_and_density(df_3, is_weekend = FALSE,slot_label = "red")
hist_rend3 <- plot_histogram_and_density(df_3, is_weekend = TRUE,slot_label = "red")

hist_wday5b <- plot_histogram_and_density(df_5b, is_weekend = FALSE,slot_label = "white")
hist_wend5b <- plot_histogram_and_density(df_5b, is_weekend = TRUE,slot_label = "white")

hist_rday5b <- plot_histogram_and_density(df_5b, is_weekend = FALSE,slot_label = "red")
hist_rend5b <- plot_histogram_and_density(df_5b, is_weekend = TRUE,slot_label = "red")

hist_wday3a <- plot_histogram_and_density(df_3a, is_weekend = FALSE,slot_label = "white")
hist_wend3a <- plot_histogram_and_density(df_3a, is_weekend = TRUE,slot_label = "white")

hist_rday3a <- plot_histogram_and_density(df_3a, is_weekend = FALSE,slot_label = "red")
hist_rend3a <- plot_histogram_and_density(df_3a, is_weekend = TRUE,slot_label = "red")

hist_wday5 <- plot_histogram_and_density(df_5, is_weekend = FALSE,slot_label = "white")
hist_wend5 <- plot_histogram_and_density(df_5, is_weekend = TRUE,slot_label = "white")

hist_rday5 <- plot_histogram_and_density(df_5, is_weekend = FALSE,slot_label = "red")
hist_rend5 <- plot_histogram_and_density(df_5, is_weekend = TRUE,slot_label = "red")


###################
# Function to filter dataframes by month and by red/white slots
# Also plot histogram for inter-arrival time between cars
###################
plot_by_month <- function(df, month_number, slot_label) {
  carpark_name <- sub("df_", "", deparse(substitute(df)))
  slot_name <- ifelse(slot_label == "white", "non-season", "season")
  months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  title <- paste("Histogram of inter-arrival time for", slot_name, "parkers in", months[month_number], "for carpark", carpark_name)
  filtered_df <- df %>%
    filter(slot == slot_label) %>% 
    mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
           iat = ifelse(row_number() == 1, 0, iat)) %>%
    filter(month == month_number)
  res <- hist(filtered_df$iat, main = title, prob = TRUE, xlab = "Inter-arrival Time", breaks = 70)
}

plot_by_month(df_3, 1, "white")
plot_by_month(df_3, 2, "white")
plot_by_month(df_3, 3, "white")
plot_by_month(df_3, 4, "white")
plot_by_month(df_3, 5, "white")
plot_by_month(df_3, 6, "white")
plot_by_month(df_3, 7, "white")
plot_by_month(df_3, 8, "white")
plot_by_month(df_3, 9, "white")
plot_by_month(df_3, 10, "white")
plot_by_month(df_3, 11, "white")
plot_by_month(df_3, 12, "white")

plot_by_month(df_3, 1, "red")
plot_by_month(df_3, 2, "red")
plot_by_month(df_3, 3, "red")
plot_by_month(df_3, 4, "red")
plot_by_month(df_3, 5, "red")
plot_by_month(df_3, 6, "red")
plot_by_month(df_3, 7, "red")
plot_by_month(df_3, 8, "red")
plot_by_month(df_3, 9, "red")
plot_by_month(df_3, 10, "red")
plot_by_month(df_3, 11, "red")
plot_by_month(df_3, 12, "red")

plot_by_month(df_3a, 1, "white")
plot_by_month(df_3a, 2, "white")
plot_by_month(df_3a, 3, "white")
plot_by_month(df_3a, 4, "white")
plot_by_month(df_3a, 5, "white")
plot_by_month(df_3a, 6, "white")
plot_by_month(df_3a, 7, "white")
plot_by_month(df_3a, 8, "white")
plot_by_month(df_3a, 9, "white")
plot_by_month(df_3a, 10, "white")
plot_by_month(df_3a, 11, "white")
plot_by_month(df_3a, 12, "white")

plot_by_month(df_3a, 1, "red")
plot_by_month(df_3a, 2, "red")
plot_by_month(df_3a, 3, "red")
plot_by_month(df_3a, 4, "red")
plot_by_month(df_3a, 5, "red")
plot_by_month(df_3a, 6, "red")
plot_by_month(df_3a, 7, "red")
plot_by_month(df_3a, 8, "red")
plot_by_month(df_3a, 9, "red")
plot_by_month(df_3a, 10, "red")
plot_by_month(df_3a, 11, "red")
plot_by_month(df_3a, 12, "red")

plot_by_month(df_4, 1, "white")
plot_by_month(df_4, 2, "white")
plot_by_month(df_4, 3, "white")
plot_by_month(df_4, 4, "white")
plot_by_month(df_4, 5, "white")
plot_by_month(df_4, 6, "white")
plot_by_month(df_4, 7, "white")
plot_by_month(df_4, 8, "white")
plot_by_month(df_4, 9, "white")
plot_by_month(df_4, 10, "white")
plot_by_month(df_4, 11, "white")
plot_by_month(df_4, 12, "white")

plot_by_month(df_4, 1, "red")
plot_by_month(df_4, 2, "red")
plot_by_month(df_4, 3, "red")
plot_by_month(df_4, 4, "red")
plot_by_month(df_4, 5, "red")
plot_by_month(df_4, 6, "red")
plot_by_month(df_4, 7, "red")
plot_by_month(df_4, 8, "red")
plot_by_month(df_4, 9, "red")
plot_by_month(df_4, 10, "red")
plot_by_month(df_4, 11, "red")
plot_by_month(df_4, 12, "red")

# No data for the months of Jan, Feb, Mar for Carpark 5
#plot_by_month(df_5, 1, "white")
#plot_by_month(df_5, 2, "white")
#plot_by_month(df_5, 3, "white")
plot_by_month(df_5, 4, "white")
plot_by_month(df_5, 5, "white")
plot_by_month(df_5, 6, "white")
plot_by_month(df_5, 7, "white")
plot_by_month(df_5, 8, "white")
plot_by_month(df_5, 9, "white")
plot_by_month(df_5, 10, "white")
plot_by_month(df_5, 11, "white")
plot_by_month(df_5, 12, "white")

#plot_by_month(df_5, 1, "red")
#plot_by_month(df_5, 2, "red")
#plot_by_month(df_5, 3, "red")
plot_by_month(df_5, 4, "red")
plot_by_month(df_5, 5, "red")
plot_by_month(df_5, 6, "red")
plot_by_month(df_5, 7, "red")
plot_by_month(df_5, 8, "red")
plot_by_month(df_5, 9, "red")
plot_by_month(df_5, 10, "red")
plot_by_month(df_5, 11, "red")
plot_by_month(df_5, 12, "red")

plot_by_month(df_5b, 1, "white")
plot_by_month(df_5b, 2, "white")
plot_by_month(df_5b, 3, "white")
plot_by_month(df_5b, 4, "white")
plot_by_month(df_5b, 5, "white")
plot_by_month(df_5b, 6, "white")
plot_by_month(df_5b, 7, "white")
plot_by_month(df_5b, 8, "white")
plot_by_month(df_5b, 9, "white")
plot_by_month(df_5b, 10, "white")
plot_by_month(df_5b, 11, "white")
plot_by_month(df_5b, 12, "white")

plot_by_month(df_5b, 1, "red")
plot_by_month(df_5b, 2, "red")
plot_by_month(df_5b, 3, "red")
plot_by_month(df_5b, 4, "red")
plot_by_month(df_5b, 5, "red")
plot_by_month(df_5b, 6, "red")
plot_by_month(df_5b, 7, "red")
plot_by_month(df_5b, 8, "red")
plot_by_month(df_5b, 9, "red")
plot_by_month(df_5b, 10, "red")
plot_by_month(df_5b, 11, "red")
plot_by_month(df_5b, 12, "red")

plot_by_month(df_6b, 1, "white")
plot_by_month(df_6b, 2, "white")
plot_by_month(df_6b, 3, "white")
plot_by_month(df_6b, 4, "white")
plot_by_month(df_6b, 5, "white")
plot_by_month(df_6b, 6, "white")
plot_by_month(df_6b, 7, "white")
plot_by_month(df_6b, 8, "white")
plot_by_month(df_6b, 9, "white")
plot_by_month(df_6b, 10, "white")
plot_by_month(df_6b, 11, "white")
plot_by_month(df_6b, 12, "white")

plot_by_month(df_6b, 1, "red")
plot_by_month(df_6b, 2, "red")
plot_by_month(df_6b, 3, "red")
plot_by_month(df_6b, 4, "red")
plot_by_month(df_6b, 5, "red")
plot_by_month(df_6b, 6, "red")
plot_by_month(df_6b, 7, "red")
plot_by_month(df_6b, 8, "red")
plot_by_month(df_6b, 9, "red")
plot_by_month(df_6b, 10, "red")
plot_by_month(df_6b, 11, "red")
plot_by_month(df_6b, 12, "red")


# Based on distribution, most of the rate of arrivals can be modelled using lomax distribution

# Filter by red/white slots
red_3 <- df_3 %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_3 <- df_3 %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
red_3a <- df_3a %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_3a <- df_3a %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
red_4 <- df_4 %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_4 <- df_4 %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
red_5 <- df_5 %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_5 <- df_5 %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
red_5b <- df_5b %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_5b <- df_5b %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
red_6b <- df_6b %>% filter(slot == "red") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))
white_6b <- df_6b %>% filter(slot == "white") %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
         iat = ifelse(row_number() == 1, 0, iat))

# Finding suitable parameters to model red lots usage for Carpark 3 for each month
# This code is only ran for Carpark 3 Red slots to demo how the parameters are derived
# Please refer to arrival_dist.R for the parameters for all the carparks 
red_3_jan <- red_3 %>% filter(month == 1)
red_3_feb <- red_3 %>% filter(month == 2)
red_3_mar <- red_3 %>% filter(month == 3)
red_3_apr <- red_3 %>% filter(month == 4)
red_3_may <- red_3 %>% filter(month == 5)
red_3_jun <- red_3 %>% filter(month == 6)
red_3_jul <- red_3 %>% filter(month == 7)
red_3_aug <- red_3 %>% filter(month == 8)
red_3_sep <- red_3 %>% filter(month == 9)
red_3_oct <- red_3 %>% filter(month == 10)
red_3_nov <- red_3 %>% filter(month == 11)
red_3_dec <- red_3 %>% filter(month == 12)
fit1 <- fitdist(red_3_jan$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit2 <- fitdist(red_3_feb$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit3 <- fitdist(red_3_mar$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit4 <- fitdist(red_3_apr$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit5 <- fitdist(red_3_may$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit6 <- fitdist(red_3_jun$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit7 <- fitdist(red_3_jul$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit8 <- fitdist(red_3_aug$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit9 <- fitdist(red_3_sep$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit10 <- fitdist(red_3_oct$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit11 <- fitdist(red_3_nov$iat, "lomax", start = list(scale =1, shape3.q = 1))
fit12 <- fitdist(red_3_dec$iat, "lomax", start = list(scale =1, shape3.q = 1))

scale_para3r <- rep(-1,12)
shape3.q_para3r <- rep(-1,12)
scale_para3r[1] <- as.numeric(summary(fit1)$estimate[1])
scale_para3r[2] <- as.numeric(summary(fit2)$estimate[1])
scale_para3r[3] <- as.numeric(summary(fit3)$estimate[1])
scale_para3r[4] <- as.numeric(summary(fit4)$estimate[1])
scale_para3r[5] <- as.numeric(summary(fit5)$estimate[1])
scale_para3r[6] <- as.numeric(summary(fit6)$estimate[1])
scale_para3r[7] <- as.numeric(summary(fit7)$estimate[1])
scale_para3r[8] <- as.numeric(summary(fit8)$estimate[1])
scale_para3r[9] <- as.numeric(summary(fit9)$estimate[1])
scale_para3r[10] <- as.numeric(summary(fit10)$estimate[1])
scale_para3r[11] <- as.numeric(summary(fit11)$estimate[1])
scale_para3r[12] <- as.numeric(summary(fit12)$estimate[1])

shape3.q_para3r[1] <- as.numeric(summary(fit1)$estimate[2])
shape3.q_para3r[2] <- as.numeric(summary(fit2)$estimate[2])
shape3.q_para3r[3] <- as.numeric(summary(fit3)$estimate[2])
shape3.q_para3r[4] <- as.numeric(summary(fit4)$estimate[2])
shape3.q_para3r[5] <- as.numeric(summary(fit5)$estimate[2])
shape3.q_para3r[6] <- as.numeric(summary(fit6)$estimate[2])
shape3.q_para3r[7] <- as.numeric(summary(fit7)$estimate[2])
shape3.q_para3r[8] <- as.numeric(summary(fit8)$estimate[2])
shape3.q_para3r[9] <- as.numeric(summary(fit9)$estimate[2])
shape3.q_para3r[10] <- as.numeric(summary(fit10)$estimate[2])
shape3.q_para3r[11] <- as.numeric(summary(fit11)$estimate[2])
shape3.q_para3r[12] <- as.numeric(summary(fit12)$estimate[2])
scale_para3r
shape3.q_para3r
# Distribution to simulate inter-arrival times for Red slots at Carpark 3
# Formula works in the form rlomax(n, scale, shape3.q)
# Change n according to number of random variables to generate

# Carpark 3 Red Slots Jan
rlomax(1, scale=51.48, shape3.q=1.17)
# Carpark 3 Red Slots Feb
rlomax(1, scale=53.14, shape3.q=1.22)
# Carpark 3 Red Slots Mar
rlomax(1, scale=48.14, shape3.q=1.33)
# Carpark 3 Red Slots Apr
rlomax(1, scale=51.21, shape3.q=1.17)
# Carpark 3 Red Slots May
rlomax(1, scale=51.48, shape3.q=1.09)
# Carpark 3 Red Slots Jun
rlomax(1, scale=53.14, shape3.q=1.25)
# Carpark 3 Red Slots Jul
rlomax(1, scale=51.48, shape3.q=1.13)
# Carpark 3 Red Slots Aug
rlomax(1, scale=53.14, shape3.q=1.25)
# Carpark 3 Red Slots Sep
rlomax(1, scale=51.48, shape3.q=1.27)
# Carpark 3 Red Slots Oct
rlomax(1, scale=53.14, shape3.q=1.18)
# Carpark 3 Red Slots Nov
rlomax(1, scale=51.48, shape3.q=1.19)
# Carpark 3 Red Slots Dec
rlomax(1, scale=53.14, shape3.q=1.12)


# FUnction to plot inter-arrival times faceted by lot type across the year (general trend)
plot_iat_general <- function(df, slot_label) {
  carpark_name <- sub("df_", "", deparse(substitute(df)))
  slot_name <- ifelse(slot_label == "white", "non-season", "season")
  title <- paste("Histogram of inter-arrival time for", slot_name, "parkers in carpark", carpark_name)
  filtered_df <- df %>%
    filter(slot == slot_label) %>% 
    mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins")),
           iat = ifelse(row_number() == 1, 0, iat))
  res <- hist(filtered_df$iat, main = title, prob = TRUE, xlab = "Inter-arrival Time", breaks = 70)
}

plot_iat_general(df_3, "red")
plot_iat_general(df_3, "white")
plot_iat_general(df_3a, "red")
plot_iat_general(df_3a, "white")
plot_iat_general(df_4, "red")
plot_iat_general(df_4, "white")
plot_iat_general(df_5, "red")
plot_iat_general(df_5, "white")
plot_iat_general(df_5b, "red")
plot_iat_general(df_5b, "white")
plot_iat_general(df_6b, "red")
plot_iat_general(df_6b, "white")
