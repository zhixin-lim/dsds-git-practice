library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(simmer)
library(simmer.plot)
library(VGAM)
library(fitdistrplus)
library(gridExtra)

#------------------------------------------------------------------------ 
# LOAD DATA
#------------------------------------------------------------------------ 

df_5 <- read.csv("../data/df_5.csv") %>%
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

df_6b <- read.csv("../data/df_6b.csv") %>%
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

df_4 <- read.csv("../data/df_4.csv") %>%
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

df_3 <- read.csv("../data/df_3.csv") %>%
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

df_5b <- read.csv("../data/df_5b.csv") %>%
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

df_3a <- read.csv("../data/df_3a.csv") %>%
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


#------------------------------------------------------------------------ 
# HELPING FUNCTIONS
#------------------------------------------------------------------------ 

# Define a function to convert distances to probabilities: Lower (but != 0 prob for far away cars)
# Can incorporate current occupancy as well?
distance_to_probability <- function(distances, cp_sheltered) {
  # mean(x) added as a constant term to avoid probabilities to shrink to 0
  probabilities <- apply(distances, 2, function(x) max(x) + mean(x) - x)
  # If current lot non-sheltered and other lot sheltered, the cost will be very high, so penalize such cases depneding on percentage of sheltered lots
  for (i in 1:6){
    if (cp_sheltered[i] == 0){
      for (j in 1:6){
        probabilities[j, i] = probabilities[j, i] - cp_sheltered[j]/100*mean(distances[,i])
      }
    }
  }
  # Cannot park at same lot
  diag(probabilities) <- 0 
  # Normalize the probability values so sum = 0
  probabilities <- apply(probabilities, 2, function(x) x/sum(x))  # Normalize probabilities to sum to 1
  return(probabilities)
}
prob_to_carpark_fn <- function(code, probabilities_matrix){
  cur_s = sample(1:6, 1, prob = probabilities_matrix[,code])
  if (cur_s == 1){return(car_3)}
  if (cur_s == 2){return(car_3a)}
  if (cur_s == 3){return(car_4)}
  if (cur_s == 4){return(car_5)}
  if (cur_s == 5){return(car_5b)}
  if (cur_s == 6){return(car_6b)}
}

# Function to gte vector of all months in simulation period along with times
get_month_data <- function(month_st, month_en) {
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  indices <- match(c(month_st, month_en), month_names)
  abbreviated_months <- month_names[indices[1]:indices[2]]
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  minutes_in_month <- days_in_month[indices[1]:indices[2]] * 24 * 60
  return(list(abbreviated_months = abbreviated_months, minutes_in_month = minutes_in_month))
}

# Function to get required simulation function for duration
# Inputs: Choice of carpark, season/non-season parking lots, and month
# Function takes input in the form (str, str, str)
# Eg: get_sim_function_duration("3", "red", "Jul") returns the duration 
# for a car parked at season lots in carpark 3 in July
# Returns "Carpark Not Found" if carpark input by user is not valid

get_sim_function_duration <- function(cp, slot_type, month){
  df_name <- paste0("df_", cp)
  if (exists(df_name)) {
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month_number <- match(month, months)
    df <- get(df_name) %>% filter(slot == slot_type, month == month_number)
    # If no data for a particular month, can take median over all months
    if (nrow(df) == 0){
      df <- get(df_name) %>% filter(slot == slot_type)
    }
    return(median(df$du_val, na.rm = T))
  }
  else {
    return("Carpark Not Found")
  }
}

# Function to get required simulation function for arrival
# This function requires the red_3 to white_6b dataframes in du_val_analysis.R to run
# Takes in input in the form (str, str, str)
# Eg: get_sim_function_duration("3", "red", "Jul") returns the inter-arrival time 
# between cars wanting to park at season lots in carpark 3 in July
# If time is 16:00:00 and get_sim_function_arrival returns 3.5, it means the next car
# is simulated to enter the carpark at 16:03:30
# Returns "Carpark Not Found" if carpark input by user is not valid

get_sim_function_arrival <- function(cp, slot_type, month) {
  months_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_num <- match(month, months_vec)
  dfname <- paste0(slot_type, "_", cp)
  if (exists(dfname)) {
    # Special Case 1
    if (cp == "3" & slot_type == "white" & month == "Jul") {
      return(function() runif(1, 0, 50))
      # Special Case 2
    } else if (cp == "3" & slot_type == "white" & month == "Dec") {
      return(function() runif(1, 0, 100))
    } else {
      res <- get(dfname) %>% filter(month == month_num)
      if (nrow(res) == 0){
        res <- get(dfname)
      }
      fit <- fitdist(res$iat, "lomax", start = list(scale = 1, shape3.q = 1))
      scale_para <- as.numeric(summary(fit)$estimate[1])
      shape_para <- as.numeric(summary(fit)$estimate[2])
      # If no data for a particular month, can take median over all months
      return(function() rlomax(1, scale = scale_para, shape3.q = shape_para))
    }
  } else {
    return("Carpark Not Found")
  }
}

#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Multi carpark
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#initial setup
cp_list <- c("3", "3a", "4", "5", "5b", "6b")
car_3 <- trajectory("car_3")
car_3a <- trajectory("car_3a")
car_4 <- trajectory("car_4")
car_5 <- trajectory("car_5")
car_5b <- trajectory("car_5b")
car_6b <- trajectory("car_6b")

#------------------------------------------------------------------------ 
# Function to simulate car park system
simulate_car_parks_once_slot_m <- function(cp_list, cp_state, cp_capacity,
                                           cp_sheltered, cp_red_perc, slot_type = "red", month,
                                           arrival_rate, simulation_time, distance_matrix) {
  cp_red_count <- round(cp_red_perc/100*cp_capacity, 0)
  cp_white_count <- cp_capacity - cp_red_count
  probabilities_matrix <- distance_to_probability(distance_matrix, cp_sheltered)
  
  # need to change the timeout function to fit the distribution for the stay duration
  car_3 <- trajectory("car_3") %>%
    renege_in(10, prob_to_carpark_fn("3", probabilities_matrix)) %>%
    seize("car_park_3") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("3", slot_type, month)) %>%
    release("car_park_3")
  
  car_3a<- trajectory("car_3a") %>%
    renege_in(10, prob_to_carpark_fn("3a", probabilities_matrix))%>%
    seize("car_park_3a") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("3a", slot_type, month)) %>%
    release("car_park_3a")
  
  car_4<- trajectory("car_4") %>%
    renege_in(10,prob_to_carpark_fn("4", probabilities_matrix))%>%
    seize("car_park_4") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("4", slot_type, month)) %>%
    release("car_park_4")
  
  car_5<- trajectory("car_5") %>%
    renege_in(10, prob_to_carpark_fn("5", probabilities_matrix))%>%
    seize("car_park_5") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("5", slot_type, month)) %>%
    release("car_park_5")
  
  car_5b<- trajectory("car_5b") %>%
    renege_in(10,prob_to_carpark_fn("5b", probabilities_matrix))%>%
    seize("car_park_5b") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("5b", slot_type, month)) %>%
    release("car_park_5b")
  
  car_6b<- trajectory("car_6b") %>%
    renege_in(10,prob_to_carpark_fn("6b", probabilities_matrix))%>%
    seize("car_park_6b") %>%
    renege_abort()%>%
    timeout(get_sim_function_duration("6b", slot_type, month)) %>%
    release("car_park_6b")
  
  env <- simmer()
  
  for (i in 1:6) {
    if (cp_state[i]=="closed"){
      env %>% add_resource(paste0("car_park_", cp_list[i]), capacity = 0)
    }
    else{
      env %>% add_resource(paste0("car_park_", cp_list[i]), 
                           capacity = ifelse(slot_type == "red", cp_red_count[i], cp_white_count[i]))
    }
  }
  
  # If no red/white slot, don't add generator
  if ((slot_type == "red" & cp_red_perc[1] != 0) | 
      (slot_type == "white" & cp_red_perc[1] != 100)){
    env %>% add_generator("car_generator_3", car_3, 
                          get_sim_function_arrival("3", slot_type, month))
  }
  if ((slot_type == "red" & cp_red_perc[2] != 0) | 
      (slot_type == "white" & cp_red_perc[2] != 100)){
    env %>% add_generator("car_generator_3a", car_3a, 
                          get_sim_function_arrival("3a", slot_type, month))
  }
  if ((slot_type == "red" & cp_red_perc[3] != 0) | 
      (slot_type == "white" & cp_red_perc[3] != 100)){
    env %>% add_generator("car_generator_4", car_4, 
                          get_sim_function_arrival("4", slot_type, month))
  }
  if ((slot_type == "red" & cp_red_perc[4] != 0) | 
      (slot_type == "white" & cp_red_perc[4] != 100)){
    env %>% add_generator("car_generator_5", car_5, 
                          get_sim_function_arrival("5", slot_type, month))
  }
  if ((slot_type == "red" & cp_red_perc[5] != 0) | 
      (slot_type == "white" & cp_red_perc[5] != 100)){
    env %>% add_generator("car_generator_5b", car_5b, 
                          get_sim_function_arrival("5b", slot_type, month))
  }
  if ((slot_type == "red" & cp_red_perc[6] != 0) | 
      (slot_type == "white" & cp_red_perc[6] != 100)){
    env %>% add_generator("car_generator_6b", car_6b, 
                          get_sim_function_arrival("6b", slot_type, month))
  }
  out <- run(env, until = simulation_time)
  return(out)
}

simulate_car_parks_once_m <- function(cp_list, cp_state, cp_capacity, cp_sheltered, 
                                      cp_red_perc, month, arrival_rate, 
                                      simulation_time, distance_matrix){
  red_res <- simulate_car_parks_once_slot_m(cp_list, cp_state, cp_capacity,
                                            cp_sheltered, cp_red_perc, slot_type = "red", month, 
                                            arrival_rate, simulation_time, distance_matrix)
  white_res <- simulate_car_parks_once_slot_m(cp_list, cp_state, cp_capacity,
                                              cp_sheltered, cp_red_perc, slot_type = "white", month, 
                                              arrival_rate, simulation_time, distance_matrix)
  return(list(red_res, white_res))
}
##Everything up to here is the same as DES_simple_single.R script##

#function to let the simulation run n numbers of times
run_sim_n_times_m <- function(n = 20, 
                              cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                              cp_state = c("open", "open", "open", "open", "open", "open"), 
                              cp_capacity = c(243, 67, 116, 70, 32, 173), 
                              cp_sheltered = c(100, 0, 0, 0, 0, 100),
                              cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                              arrival_rate = "historical",
                              month_st = "Jan", month_en = "Apr",
                              distance_matrix = matrix(c(
                                0, 500, 1000, 150, 200, 250,
                                500, 0, 800, 1300, 180, 230,
                                1000, 800, 0, 600, 1100, 160,
                                150, 1300, 600, 0, 500, 1000,
                                200, 180, 1100, 500, 0, 500,
                                250, 230, 160, 1000, 500, 0
                              ), nrow = 6, byrow = TRUE, 
                              dimnames = list(cp_list, cp_list))){
  
  unclosed_lots <-  paste0("car_park_", cp_list)[which(cp_state != "closed")]
  unclosed_lots_red <- paste0("car_park_", cp_list)[which(cp_state != "closed" & cp_red_perc != 0)]
  unclosed_lots_white <- paste0("car_park_", cp_list)[which(cp_state != "closed" & cp_red_perc != 100)]
  month_data <- get_month_data(month_st, month_en)
  df_main <- NULL
  mean_eachsim_red_max_occ <- numeric(n)
  std_devs_eachsim_red_max_occ <- numeric(n)
  mean_eachsim_white_max_occ <- numeric(n)
  std_devs_eachsim_white_max_occ <- numeric(n)
  mean_eachsim_red_mean_occ <- numeric(n)
  std_devs_eachsim_red_mean_occ <- numeric(n)
  mean_eachsim_white_mean_occ <- numeric(n)
  std_devs_eachsim_white_mean_occ <- numeric(n)
  for (i in 1:n){
    for (mon in 1:length(month_data[[1]])){
      results<-simulate_car_parks_once_m(cp_list, cp_state, cp_capacity, cp_sheltered,
                                         cp_red_perc, month_data[[1]][mon], arrival_rate, 
                                         month_data[[2]][mon], distance_matrix)
      #I get the max number of cars of each slot type in each cp
      summarized_table_red <- get_mon_resources(results[[1]]) %>% 
        filter(resource %in% unclosed_lots_red) %>%
        group_by(resource) %>%
        summarize(red_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                  red_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T))
      summarized_table_white<-get_mon_resources(results[[2]]) %>%
        filter(resource %in% unclosed_lots_white) %>%
        group_by(resource) %>%
        summarize(white_mean_parking = mean(system, na.rm = T)/mean(capacity, na.rm = T),
                  white_max_parking = max(system, na.rm = T)/mean(capacity, na.rm = T))
      mean_eachsim_red <- as.numeric(summarized_table_red %>% 
                                       summarize(average_max_parking=mean(red_max_parking, na.rm = T),
                                                 average_mean_parking=mean(red_mean_parking, na.rm = T)))
      mean_eachsim_red_max_occ[i] <- mean_eachsim_red[1]
      mean_eachsim_red_mean_occ[i] <- mean_eachsim_red[2]
      std_devs_eachsim_red <- as.numeric(summarized_table_red %>% 
                                           summarize(sd_max_parking=sd(red_max_parking, na.rm = T),
                                                     sd_mean_parking=sd(red_mean_parking, na.rm = T)))
      std_devs_eachsim_red_max_occ[i] <- std_devs_eachsim_red[1]
      std_devs_eachsim_red_mean_occ[i] <- std_devs_eachsim_red[2]
      mean_eachsim_white <- as.numeric(summarized_table_white %>% 
                                         summarize(average_max_parking=mean(white_max_parking, na.rm = T),
                                                   average_mean_parking=mean(white_mean_parking, na.rm = T)))
      mean_eachsim_white_max_occ[i] <- mean_eachsim_white[1]
      mean_eachsim_white_mean_occ[i] <- mean_eachsim_white[2]
      std_devs_eachsim_white <- as.numeric(summarized_table_white %>% 
                                             summarize(sd_max_parking=sd(white_max_parking, na.rm = T),
                                                       sd_mean_parking=sd(white_mean_parking, na.rm = T)))
      std_devs_eachsim_white_max_occ[i] <- std_devs_eachsim_white[1]
      std_devs_eachsim_white_mean_occ[i] <- std_devs_eachsim_white[2]
    }
  }
  return(list(mean_eachsim_red_max_occ = mean_eachsim_red_max_occ,
              std_devs_eachsim_red_max_occ = std_devs_eachsim_red_max_occ,
              mean_eachsim_white_max_occ = mean_eachsim_white_max_occ,
              std_devs_eachsim_white_max_occ = std_devs_eachsim_white_max_occ,
              mean_eachsim_red_mean_occ = mean_eachsim_red_mean_occ,
              std_devs_eachsim_red_mean_occ = std_devs_eachsim_red_mean_occ,
              mean_eachsim_white_mean_occ = mean_eachsim_white_mean_occ,
              std_devs_eachsim_white_mean_occ = std_devs_eachsim_white_mean_occ))
}

n_sims <- seq(10, 50, by = 10) #change the number to test
means_max_occ_red <- numeric(length(n_sims))
std_devs_max_occ_red <- numeric(length(n_sims))
means_max_occ_white <- numeric(length(n_sims))
std_devs_max_occ_white <- numeric(length(n_sims))
means_mean_occ_red <- numeric(length(n_sims))
std_devs_mean_occ_red <- numeric(length(n_sims))
means_mean_occ_white <- numeric(length(n_sims))
std_devs_mean_occ_white <- numeric(length(n_sims))

for (i in seq_along(n_sims)) {
  sim_res<-run_sim_n_times_m(i, cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                           cp_state = c("open", "open", "open", "open", "open", "open"), 
                           cp_capacity = c(243, 67, 116, 70, 32, 173), 
                           cp_sheltered = c(100, 0, 0, 0, 0, 100),
                           cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                           arrival_rate = "historical",
                           month_st = "Jan", month_en = "Dec",
                           distance_matrix = matrix(c(
                             0, 500, 1000, 150, 200, 250,
                             500, 0, 800, 1300, 180, 230,
                             1000, 800, 0, 600, 1100, 160,
                             150, 1300, 600, 0, 500, 1000,
                             200, 180, 1100, 500, 0, 500,
                             250, 230, 160, 1000, 500, 0
                           ), nrow = 6, byrow = TRUE, 
                           dimnames = list(cp_list, cp_list)))
  means_max_occ_red[i] <- sim_res$mean_eachsim_red_max_occ %>% mean(., na.rm = T)
  std_devs_max_occ_red[i] <- sim_res$std_devs_eachsim_red_max_occ %>% mean(., na.rm = T)
  means_max_occ_white[i] <- sim_res$mean_eachsim_white_max_occ %>% mean(., na.rm = T)
  std_devs_max_occ_white[i] <- sim_res$std_devs_eachsim_white_max_occ %>% mean(., na.rm = T)
  means_mean_occ_red[i] <- sim_res$mean_eachsim_red_mean_occ %>% mean(., na.rm = T)
  std_devs_mean_occ_red[i] <- sim_res$std_devs_eachsim_red_mean_occ %>% mean(., na.rm = T)
  means_mean_occ_white[i] <- sim_res$mean_eachsim_white_mean_occ %>% mean(., na.rm = T)
  std_devs_mean_occ_white[i] <- sim_res$std_devs_eachsim_white_mean_occ %>% mean(., na.rm = T)
}

# T distribution assumption CI (Alt: Non parametric CI)
# Calculate confidence interval width for max_occ
conf_int_width_red_max_occ <- qt(0.975, n_sims - 1) * (std_devs_max_occ_red / sqrt(n_sims))
conf_int_width_white_max_occ <- qt(0.975, n_sims - 1) * (std_devs_max_occ_white / sqrt(n_sims))

# Calculate confidence interval width for mean_occ
conf_int_width_red_mean_occ <- qt(0.975, n_sims - 1) * (std_devs_mean_occ_red / sqrt(n_sims))
conf_int_width_white_mean_occ <- qt(0.975, n_sims - 1) * (std_devs_mean_occ_white / sqrt(n_sims))

# Single visual: for condifdence intervals of red and white slots for mean and max occupancies
max_occ <- data.frame(red = conf_int_width_red_max_occ, white = conf_int_width_white_max_occ, n = n_sims) %>%
  pivot_longer(red:white, names_to = "slot", values_to = "width") %>%
  mutate(type = "Max Occupancy")
mean_occ <- data.frame(red = conf_int_width_red_mean_occ, white = conf_int_width_white_mean_occ, n = n_sims) %>%
  pivot_longer(red:white, names_to = "slot", values_to = "width") %>%
  mutate(type = "Mean Occupancy")

rbind(max_occ, mean_occ) %>% 
  ggplot() +
  geom_line(aes(x = n, y = width, color = slot)) +
  facet_wrap(type~.) +
  labs(x = "Number of simulations", y = "Confidence Interval Width", color = "Slot") +
  theme_bw()


#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Draft for simulating cp closure: Single carpark
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#initial setup
car_3 <- trajectory("car_3")
car_3a <- trajectory("car_3a")
car_4 <- trajectory("car_4")
car_5 <- trajectory("car_5")
car_5b <- trajectory("car_5b")
car_6b <- trajectory("car_6b")

#------------------------------------------------------------------------ 
# Need to run these every time a new simulation

# Function to simulate car park system
# cp_index denotes carpark index from 1:6 mapping to c("3", "3a", "4", "5", "5b", "6b")
simulate_car_parks_once_slot_s <- function(cp_index, cp_capacity, cp_red_perc, 
                                           slot_type = "red", month, 
                                           arrival_rate, simulation_time) {
  cp_red_count <- round(cp_red_perc/100*cp_capacity, 0)
  cp_white_count <- cp_capacity - cp_red_count
  
  car_3 <- trajectory("car_3") %>%
    seize("car_park_3") %>%
    timeout(get_sim_function_duration("3", slot_type, month)) %>%
    release("car_park_3")
  
  car_3a <- trajectory("car_3a") %>%
    seize("car_park_3a") %>%
    timeout(get_sim_function_duration("3a", slot_type, month)) %>%
    release("car_park_3a")
  
  car_4 <- trajectory("car_4") %>%
    seize("car_park_4") %>%
    timeout(get_sim_function_duration("4", slot_type, month)) %>%
    release("car_park_4")
  
  car_5 <- trajectory("car_5") %>%
    seize("car_park_5") %>%
    timeout(get_sim_function_duration("5", slot_type, month)) %>%
    release("car_park_5")
  
  car_5b <- trajectory("car_5b") %>%
    seize("car_park_5b") %>%
    timeout(get_sim_function_duration("5b", slot_type, month)) %>%
    release("car_park_5b")
  
  car_6b <- trajectory("car_6b") %>%
    seize("car_park_6b") %>%
    timeout(get_sim_function_duration("6b", slot_type, month)) %>%
    release("car_park_6b")
  
  env <- simmer()
  
  env %>% add_resource(paste0("car_park_", cp_list[cp_index]), 
                       capacity = ifelse(slot_type == "red", cp_red_count, cp_white_count))
  if (cp_index == 1){
    env %>% add_generator("car_generator_3", car_3, 
                          get_sim_function_arrival("3", slot_type, month))
  } else if (cp_index == 2){
    env %>% add_generator("car_generator_3a", car_3a, 
                          get_sim_function_arrival("3a", slot_type, month))
  } else if (cp_index == 3){
    env %>% add_generator("car_generator_4", car_4, 
                          get_sim_function_arrival("4", slot_type, month))
  } else if (cp_index == 4){
    env %>% add_generator("car_generator_5", car_5, 
                          get_sim_function_arrival("5", slot_type, month))
  } else if (cp_index == 5){
    env %>% add_generator("car_generator_5b", car_5b, 
                          get_sim_function_arrival("5b", slot_type, month))
  } else {
    env %>% add_generator("car_generator_6b", car_6b, 
                          get_sim_function_arrival("6b", slot_type, month))
  }
  
  out <- run(env, until = simulation_time)
  return(out)
}

simulate_car_parks_once_s <- function(cp_index, cp_capacity, cp_red_perc, month, 
                                      arrival_rate, simulation_time){
  if (cp_red_perc != 0){
    red_res <- simulate_car_parks_once_slot_s(cp_index, cp_capacity, cp_red_perc, 
                                              slot_type = "red", month, arrival_rate, simulation_time)
  } else {red_res <- NULL}
  
  if (cp_red_perc != 100){
    white_res <- simulate_car_parks_once_slot_s(cp_index, cp_capacity, cp_red_perc, 
                                                slot_type = "white", month, arrival_rate, simulation_time)
  } else {white_res <- NULL}
  
  return(list(red_res, white_res))
}


#function to let the simulation run n numbers of times
run_sim_n_times_s <- function(n = 20, 
                              cp_index = 1,
                              cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                              cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                              arrival_rate = "historical",
                              month_st = "Jan", month_en = "Dec",
                              simulation_time = 100000){
  df_main <- NULL
  month_data <- get_month_data(month_st, month_en)
  mean_eachsim_red_max_occ <- numeric(n)
  mean_eachsim_white_max_occ <- numeric(n)
  mean_eachsim_red_mean_occ <- numeric(n)
  mean_eachsim_white_mean_occ <- numeric(n)
  std_devs_eachsim_red <- numeric(n)
  std_devs_eachsim_white <- numeric(n)
  
  for (i in 1:n){
    for (mon in 1:length(month_data[[1]])){
      results<-simulate_car_parks_once_s(cp_index, cp_capacity, 
                                         cp_red_perc, month_data[[1]][mon], arrival_rate, month_data[[2]][mon])
      if (!is.null(results[[1]])){
        summarized_table_red <- get_mon_resources(results[[1]]) %>% 
          summarize(red_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                    red_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T),
                    red_sd_parking = sd(system, na.rm = T)/mean(capacity, na.rm = T))
      } else {summarized_table_red <- data.frame(red_mean_parking = NA)}
      if (!is.null(results[[2]])) {
        summarized_table_white<-get_mon_resources(results[[2]]) %>% 
          summarize(white_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                    white_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T),
                    white_sd_parking = sd(system, na.rm = T)/mean(capacity, na.rm = T))
      }  else {summarized_table_white <- data.frame(white_mean_parking = NA)}
    }
    mean_eachsim_red <- as.numeric(summarized_table_red)
    mean_eachsim_red_max_occ[i] <- mean_eachsim_red[1]
    mean_eachsim_red_mean_occ[i] <- mean_eachsim_red[2]
    std_devs_eachsim_red[i] <- mean_eachsim_red[3]
    mean_eachsim_white <- as.numeric(summarized_table_white)
    mean_eachsim_white_max_occ[i] <- mean_eachsim_white[1]
    mean_eachsim_white_mean_occ[i] <- mean_eachsim_white[2]
    std_devs_eachsim_white[i] <- mean_eachsim_white[3]
  }
  return(list(mean_eachsim_red_max_occ = mean_eachsim_red_max_occ,
              mean_eachsim_white_max_occ = mean_eachsim_white_max_occ,
              mean_eachsim_red_mean_occ = mean_eachsim_red_mean_occ,
              mean_eachsim_white_mean_occ = mean_eachsim_white_mean_occ,
              std_devs_eachsim_red = std_devs_eachsim_red,
              std_devs_eachsim_white = std_devs_eachsim_white))
}

n_sims <- seq(10, 50, by = 10) #change the number to test
means_max_occ_red <- numeric(length(n_sims))
means_max_occ_white <- numeric(length(n_sims))
means_mean_occ_red <- numeric(length(n_sims))
means_mean_occ_white <- numeric(length(n_sims))
std_devs_red <- numeric(length(n_sims))
std_devs_white <- numeric(length(n_sims))
cp_index=1

for (i in seq_along(n_sims)) {
  sim_res<-run_sim_n_times_s(i, 
                           cp_index = 1,
                           cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                           cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                           arrival_rate = "historical",
                           month_st="Jan", month_en = "Dec",
                           simulation_time = 100000)
  means_max_occ_red[i] <- sim_res$mean_eachsim_red_max_occ %>% mean(., na.rm = T)
  means_max_occ_white[i] <- sim_res$mean_eachsim_white_max_occ %>% mean(., na.rm = T)
  means_mean_occ_red[i] <- sim_res$mean_eachsim_red_mean_occ %>% mean(., na.rm = T)
  means_mean_occ_white[i] <- sim_res$mean_eachsim_white_mean_occ %>% mean(., na.rm = T)
  std_devs_red[i] <- sim_res$std_devs_eachsim_red %>% mean(., na.rm = T)
  std_devs_white[i] <- sim_res$std_devs_eachsim_white %>% mean(., na.rm = T)
}

# T distribution assumption CI (Alt: Non parametric CI)

conf_int_width_red <- qt(0.975, n_sims - 1) * (std_devs_red / sqrt(n_sims))
conf_int_width_white <- qt(0.975, n_sims - 1) * (std_devs_white / sqrt(n_sims))

# Single visual: for condifdence intervals of red and white slots for mean and max occupancies
data.frame(red = conf_int_width_red, white = conf_int_width_white, n = n_sims) %>%
  pivot_longer(red:white, names_to = "slot", values_to = "width") %>%
  ggplot() +
  geom_line(aes(x = n, y = width, color = slot)) +
  labs(x = "Number of simulations", y = "Confidence Interval Width", color = "Slot") +
  theme_bw()
