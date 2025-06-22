library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(simmer)
library(simmer.plot)
library(VGAM)
library(fitdistrplus)

#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Draft for simulating cp closure
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
# User Inputs
#------------------------------------------------------------------------

# The inputs given by the user will alter the list below which will then be fitted into the simulation
# The list of all carparks
cp_list <- c("3", "3a", "4", "5", "5b", "6b")
# Current capacity of all carparks (user should be able to modify this)
cp_capacity <- c(243, 67, 116, 70, 32, 173) # USER CAN MODIFY!
# % of red lots for each carpark
cp_red_perc <- c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451) # USER CAN MODIFY!

month <- "all" # can be one of "all" or "Jan", "Feb", .... "Dec" # USER CAN MODIFY!
weekday <- "all" # can be one of "all", "weekend", "weekday" # USER CAN MODIFY!
time <- "all" # can be one of "all", "morning (6 am - 12 noon)", "afternoon (12 noon - 4pm)", "evening (4 pm - 9pm)", "night (9 pm - 6 am)" # USER CAN MODIFY!
arrival_rate <- "historical" # can be historical or a numerical input from user (in terms of arrivals per minute) # USER CAN MODIFY!
simulation_time <- 1000 # how many mins to run simulation for
num_simulations <- 10 # How many simulations to run

#------------------------------------------------------------------------ 
# HELPING FUNCTIONS
#------------------------------------------------------------------------ 

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
    return(median(df$du_val))
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
      return(runif(1, 0, 50))
      # Special Case 2
    } else if (cp == "3" & slot_type == "white" & month == "Dec") {
      return(runif(1, 0, 100))
    } else {
      res <- get(dfname) %>% filter(month == month_num)
      fit <- fitdist(res$iat, "lomax", start = list(scale = 1, shape3.q = 1))
      scale_para <- as.numeric(summary(fit)$estimate[1])
      shape_para <- as.numeric(summary(fit)$estimate[2])
      return(rlomax(1, scale = scale_para, shape3.q = shape_para))
    }
  } else {
    return("Carpark Not Found")
  }
}
#------------------------------------------------------------------------ 
# Need to run these every time a new simulation

# Function to simulate car park system
# cp_index denotes carpark index from 1:6 mapping to c("3", "3a", "4", "5", "5b", "6b")
simulate_car_parks_once_slot <- function(cp_index, cp_capacity, cp_red_perc, 
                                         slot_type = "red", month, weekday, 
                                         time, arrival_rate, simulation_time) {
  cp_red_count <- round(cp_red_perc/100*cp_capacity, 0)
  cp_white_count <- cp_capacity - cp_red_count
  
  car_3 <- trajectory("car_3") %>%
    seize("car_park_3") %>%
    timeout(get_sim_function_duration("3", slot_type, month, weekday, time)) %>%
    release("car_park_3")
  
  car_3a <- trajectory("car_3a") %>%
    seize("car_park_3a") %>%
    timeout(get_sim_function_duration("3a", slot_type, month, weekday, time)) %>%
    release("car_park_3a")
  
  car_4 <- trajectory("car_4") %>%
    seize("car_park_4") %>%
    timeout(get_sim_function_duration("4", slot_type, month, weekday, time)) %>%
    release("car_park_4")
  
  car_5 <- trajectory("car_5") %>%
    seize("car_park_5") %>%
    timeout(get_sim_function_duration("5", slot_type, month, weekday, time)) %>%
    release("car_park_5")
  
  car_5b <- trajectory("car_5b") %>%
    seize("car_park_5b") %>%
    timeout(get_sim_function_duration("5b", slot_type, month, weekday, time)) %>%
    release("car_park_5b")
  
  car_6b <- trajectory("car_6b") %>%
    seize("car_park_6b") %>%
    timeout(get_sim_function_duration("6b", slot_type, month, weekday, time)) %>%
    release("car_park_6b")
  
  env <- simmer()
  
  env %>% add_resource(paste0("car_park_", cp_list[cp_index]), 
          capacity = ifelse(slot_type == "red", cp_red_count, cp_white_count))
  if (cp_index == 1){
  env %>% add_generator("car_generator_3", car_3, 
                        get_sim_function_arrival("3", slot_type, month, weekday, time, arrival_rate))
  } else if (cp_index == 2){
  env %>% add_generator("car_generator_3a", car_3a, 
                        get_sim_function_arrival("3a", slot_type, month, weekday, time, arrival_rate))
  } else if (cp_index == 3){
  env %>% add_generator("car_generator_4", car_4, 
                        get_sim_function_arrival("4", slot_type, month, weekday, time, arrival_rate))
  } else if (cp_index == 4){
  env %>% add_generator("car_generator_5", car_5, 
                        get_sim_function_arrival("5", slot_type, month, weekday, time, arrival_rate))
  } else if (cp_index == 5){
  env %>% add_generator("car_generator_5b", car_5b, 
                        get_sim_function_arrival("5b", slot_type, month, weekday, time, arrival_rate))
  } else {
  env %>% add_generator("car_generator_6b", car_6b, 
                        get_sim_function_arrival("6b", slot_type, month, weekday, time, arrival_rate))
  }
  
  out <- run(env, until = simulation_time)
  return(out)
}

simulate_car_parks_once <- function(cp_index = 1,
                                    cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                                    cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                                    month = "all",
                                    weekday = "all",
                                    time = "all",
                                    arrival_rate = "historical",
                                    simulation_time = 1000){
  red_res <- simulate_car_parks_once_slot(cp_index, cp_capacity, cp_red_perc, 
                                          slot_type = "red", month, weekday, 
                                          time, arrival_rate, simulation_time)
  white_res <- simulate_car_parks_once_slot(cp_index, cp_capacity, cp_red_perc, 
                                            slot_type = "white", month, weekday, 
                                            time, arrival_rate, simulation_time)
  return(list(red_res, white_res))
}


#simulate the environment
out <- simulate_car_parks_once(3)

# Red
r1 <- get_mon_arrivals(out[[1]])
r2 <- get_mon_resources(out[[1]])
plot(r2, metric = "utilization")
plot(r2, metric = "usage")
plot(r2, metric = "usage", steps = T)
plot(r1, metric = "activity_time")
plot(r1, metric = "waiting_time")
plot(r1, metric = "flow_time")

# White
r1 <- get_mon_arrivals(out[[2]])
r2 <- get_mon_resources(out[[2]])
plot(r2, metric = "utilization")
plot(r2, metric = "usage")
plot(r2, metric = "usage", steps = T)
plot(r1, metric = "activity_time")
plot(r1, metric = "waiting_time")
plot(r1, metric = "flow_time")


