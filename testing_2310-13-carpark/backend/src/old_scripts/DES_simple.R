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
# The status of all carparks
cp_state <- c("closed", "open", "open", "open", "open", "open") # USER CAN MODIFY!
# Current capacity of all carparks (user should be able to modify this)
cp_capacity <- c(243, 67, 116, 70, 32, 173) # USER CAN MODIFY!
# Percentage of sheltered lots for each carpark:
cp_sheltered <- c(100, 0, 0, 0, 0, 100) # USER CAN MODIFY!
# % of red lots for each carpark
cp_red_perc <- c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451) # USER CAN MODIFY!

month <- "all" # can be one of "all" or "Jan", "Feb", .... "Dec" # USER CAN MODIFY!
weekday <- "all" # can be one of "all", "weekend", "weekday" # USER CAN MODIFY!
time <- "all" # can be one of "all", "morning (6 am - 12 noon)", "afternoon (12 noon - 4pm)", "evening (4 pm - 9pm)", "night (9 pm - 6 am)" # USER CAN MODIFY!
arrival_rate <- "historical" # can be historical or a numerical input from user (in terms of arrivals per minute) # USER CAN MODIFY!
simulation_time <- 1000 # how many mins to run simulation for
num_simulations <- 10 # How many simulations to run

# Define the distance matrix (replace this with your actual distance matrix) [A symmetric matrix with value on diagonals = 0]
# USER CAN MODIFY!
distance_matrix <- matrix(c(
  # "3", "3a", "4", "5", "5b", "6b"
  0, 500, 1000, 150, 200, 250,
  500, 0, 800, 1300, 180, 230,
  1000, 800, 0, 600, 1100, 160,
  150, 1300, 600, 0, 500, 1000,
  200, 180, 1100, 500, 0, 500,
  250, 230, 160, 1000, 500, 0
), nrow = 6, byrow = TRUE)



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

# Function to get required simulation function for duration
# Inputs: Choice of carpark, season/non-season parking lots, and month
# Function takes input in the form (str, str, str)
# Eg: get_sim_function_duration("3", "red", "Jul") returns the duration 
# for a car parked at season lots in carpark 3 in July
# Returns "Carpark Not Found" if carpark input by user is not valid

get_sim_function_duration <- function(cp, slot_type, month) {
  df_name <- paste0("df_", cp)
  problem_months <- c("Jan", "Feb", "Mar")
  if (exists(df_name)) {
    # Handling the issue of no data for Carpark 5 from Jan-Mar
    if (cp == "5" & month %in% problem_months) {
      return("No data available")
    } 
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
  problem_months <- c("Jan", "Feb", "Mar")
  dfname <- paste0(slot_type, "_", cp)
  if (exists(dfname)) {
    # Handle the missing data for Carpark 5 from Jan-Mar
    if (cp == "5" & month %in% problem_months) {
      return("No data available")
      # Special Case 1
    } else if (cp == "3" & slot_type == "white" & month == "Jul") {
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

# Function to simulate arrival rate one time
# This function simulate hourly arrival rate (60min)
# Time period can be changed by altering the condition in the while statement
get_arrival_rate_once <- function(cp, slot_type, month) {
  t <- 0
  num_arrivals <- 0
  # This is to generate hourly arrival rate (60min)
  while (t < 60) {
    iat_function <- get_sim_function_arrival(cp, slot_type, month) 
    iat <- as.numeric(iat_function())
    t <- t + iat
    num_arrivals <- num_arrivals + 1
  }
  return(num_arrivals)
}

simulate_arrival_rate <- function(cp, slot_type, month) {
  simulated_arrival_rate <- sapply(1:100, function(x) get_arrival_rate_once(cp, slot_type, month))
  return(round(mean(simulated_arrival_rate),0))
}

#------------------------------------------------------------------------ 
# Need to run these every time a new simulation

# Function to simulate car park system
simulate_car_parks_once_slot <- function(cp_list, cp_state, cp_capacity,
        cp_sheltered, cp_red_perc, slot_type = "red", month, weekday, 
        time, arrival_rate, simulation_time, distance_matrix) {
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
  
  env %>% add_generator("car_generator_3", car_3, 
                        get_sim_function_arrival("3", slot_type, month))
  env %>% add_generator("car_generator_3a", car_3a, 
                        get_sim_function_arrival("3a", slot_type, month))
  env %>% add_generator("car_generator_4", car_4, 
                        get_sim_function_arrival("4", slot_type, month))
  env %>% add_generator("car_generator_5", car_5, 
                        get_sim_function_arrival("5", slot_type, month))
  env %>% add_generator("car_generator_5b", car_5b, 
                        get_sim_function_arrival("5b", slot_type, month))
  env %>% add_generator("car_generator_6b", car_6b, 
                        get_sim_function_arrival("6b", slot_type, month))
  
  out <- run(env, until = simulation_time)
  return(out)
}

simulate_car_parks_once <- function(cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                                    cp_state = c("open", "open", "open", "open", "open", "open"), 
                                    cp_capacity = c(243, 67, 116, 70, 32, 173), 
                                    cp_sheltered = c(100, 0, 0, 0, 0, 100),
                                    cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                                    month = "all",
                                    weekday = "all",
                                    time = "all",
                                    arrival_rate = "historical",
                                    simulation_time = 1000,
                                    distance_matrix = matrix(c(
                                      0, 500, 1000, 150, 200, 250,
                                      500, 0, 800, 1300, 180, 230,
                                      1000, 800, 0, 600, 1100, 160,
                                      150, 1300, 600, 0, 500, 1000,
                                      200, 180, 1100, 500, 0, 500,
                                      250, 230, 160, 1000, 500, 0
                                    ), nrow = 6, byrow = TRUE, 
                                    dimnames = list(cp_list, cp_list))){
  red_res <- simulate_car_parks_once_slot(cp_list, cp_state, cp_capacity,
                cp_sheltered, cp_red_perc, slot_type = "red", month, weekday, 
                time, arrival_rate, simulation_time, distance_matrix)
  white_res <- simulate_car_parks_once_slot(cp_list, cp_state, cp_capacity,
                cp_sheltered, cp_red_perc, slot_type = "white", month, weekday,
                time, arrival_rate, simulation_time, distance_matrix)
  return(list(red_res, white_res))
}
  

#simulate the environment
out <- simulate_car_parks_once()

# Red
r1 <- get_mon_arrivals(out[[1]])
r2 <- get_mon_resources(out[[1]])
unclosed_lots <- paste0("car_park_", cp_list)[which(cp_state != "closed" & cp_red_perc != 0)]
plot(r2, metric = "utilization", unclosed_lots)
plot(r2, metric = "usage", unclosed_lots)
plot(r2, metric = "usage", unclosed_lots, steps = T)
plot(r1, metric = "activity_time")
plot(r1, metric = "waiting_time")
plot(r1, metric = "flow_time")

# White
r1 <- get_mon_arrivals(out[[2]])
r2 <- get_mon_resources(out[[2]])
unclosed_lots <- paste0("car_park_", cp_list)[which(cp_state != "closed" & cp_red_perc != 100)]
plot(r2, metric = "utilization", unclosed_lots)
plot(r2, metric = "usage", unclosed_lots)
plot(r2, metric = "usage", unclosed_lots, steps = T)
plot(r1, metric = "activity_time")
plot(r1, metric = "waiting_time")
plot(r1, metric = "flow_time")


