library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(simmer)
library(simmer.plot)
library(VGAM)
library(fitdistrplus)
library(gridExtra)
library(jsonlite)

#------------------------------------------------------------------------ 
# LOAD DATA
#------------------------------------------------------------------------ 

df_5 <- read.csv("backend/data/df_5.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

df_6b <- read.csv("backend/data/df_6b.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

df_4 <- read.csv("backend/data/df_4.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

df_3 <- read.csv("backend/data/df_3.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

df_5b <- read.csv("backend/data/df_5b.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

df_3a <- read.csv("backend/data/df_3a.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins"))),
         month = month(enter_time)) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

# Filter by red/white slots, and adding the inter-arrival time column 
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

#-------------------------------------------------------------------------------
# DURATION FUNCTION PARAMS
#-------------------------------------------------------------------------------

# Function to get required simulation function for duration
# Inputs: Choice of carpark, season/non-season parking lots, and month
# Function takes input in the form (str, str, str)
# Eg: get_sim_function_duration("3", "red", "Jul") returns the duration 
# for a car parked at season lots in carpark 3 in July
# Returns "Carpark Not Found" if carpark input by user is not valid
get_duration_val <- function(cp, slot_type, month){
  df_name <- paste0("df_", cp)
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_number <- match(month, months)
  df <- get(df_name) %>% filter(slot == slot_type, month == month_number)
  # If no data for a particular month, can take median over all months
  if (nrow(df) == 0){
    df <- get(df_name) %>% filter(slot == slot_type)
  }
  return(median(df$du_val, na.rm = T))
}

# Initialize the nested list
duration_params <- list()

# Loop through the combinations and store values in the nested list
for (m in c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) {
  duration_params[[m]] <- list()
  
  for (s in c("red", "white")) {
    duration_params[[m]][[s]] <- list()
    
    for (c in c("3", "3a", "4", "5", "5b", "6b")) {
      duration_params[[m]][[s]][[c]] <- get_duration_val(c, s, m)
    }
  }
}

# Convert the list to JSON
json_data <- toJSON(duration_params)

# Write JSON to a file
writeLines(json_data, "backend/params/duration_params.json")

#-------------------------------------------------------------------------------
# ARRIVAL FUNCTION PARAMS
#-------------------------------------------------------------------------------


#  # Function to retrieve the inter-arrival times based on the arrival rate expected by user
get_arrival_val <- function(cp, slot_type, month) {
  months_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_num <- match(month, months_vec)
  dfname <- paste0(slot_type, "_", cp)
  if (exists(dfname)) {
    # Special Case 1
    if (cp == "3" & slot_type == "white" & month == "Jul") {
      return(c("unif", 0, 50))
      # Special Case 2
    } else if (cp == "3" & slot_type == "white" & month == "Dec") {
      return(c("unif", 0, 100))
    } else {
      res <- get(dfname) %>% filter(month == month_num)
      if (nrow(res) == 0){
        res <- get(dfname)
      }
      fit <- fitdist(res$iat, "lomax", start = list(scale = 1, shape3.q = 1))
      scale_para <- as.numeric(summary(fit)$estimate[1])
      shape_para <- as.numeric(summary(fit)$estimate[2])
      # If no data for a particular month, can take median over all months
      return(c("lomax", scale_para, shape_para))
    }
  } else {
    return("Carpark Not Found")
  }
}

# Initialize the nested list
arrival_params <- list()

# Loop through the combinations and store values in the nested list
for (m in c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) {
  arrival_params[[m]] <- list()
  
  for (s in c("red", "white")) {
    arrival_params[[m]][[s]] <- list()
    
    for (c in c("3", "3a", "4", "5", "5b", "6b")) {
      arrival_params[[m]][[s]][[c]] <- get_arrival_val(c, s, m)
    }
  }
}

# Convert the list to JSON
json_data <- toJSON(arrival_params)

# Write JSON to a file
writeLines(json_data, "backend/params/arrival_params.json")
