library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(VGAM)
library(fitdistrplus)
library(gridExtra)
library(simmer)
library(simmer.plot)
library(parallel)

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

prob_to_carpark_fn <- function(code, probabilities_matrix, slot_type,cp_state,slot_count){
  cur_s = sample(1:6, 1, prob = probabilities_matrix[,code])
  if (cur_s == 1 && slot_type == "red" && cp_state[1]!="closed" && slot_count[1]!=0){return("car_park_3_red")}
  else if (cur_s == 1 && slot_type == "white" && cp_state[1]!="closed" && slot_count[1]!=0){return("car_park_3_white")}
  else if (cur_s == 2 && slot_type == "red" && cp_state[2]!="closed" && slot_count[2]!=0){return("car_park_3a_red")}
  else if (cur_s == 2 && slot_type == "white" && cp_state[2]!="closed" && slot_count[2]!=0){return("car_park_3a_white")}
  else if (cur_s == 3 && slot_type == "red" && cp_state[3]!="closed" && slot_count[3]!=0){return("car_park_4_red")}
  else if (cur_s == 3 && slot_type == "white" && cp_state[3]!="closed" && slot_count[3]!=0){return("car_park_4_white")}
  else if (cur_s == 4 && slot_type == "red" && cp_state[4]!="closed" && slot_count[4]!=0){return("car_park_5_red")}
  else if (cur_s == 4 && slot_type == "white" && cp_state[4]!="closed" && slot_count[4]!=0){return("car_park_5_white")}
  else if (cur_s == 5 && slot_type == "red" && cp_state[5]!="closed" && slot_count[5]!=0){return("car_park_5b_red")}
  else if (cur_s == 5 && slot_type == "white" && cp_state[5]!="closed" && slot_count[5]!=0){return("car_park_5b_white")}
  else if (cur_s == 6 && slot_type == "red" && cp_state[6]!="closed" && slot_count[6]!=0){return("car_park_6b_red")}
  else if (cur_s == 6 && slot_type == "white" && cp_state[6]!="closed" && slot_count[6]!=0){return("car_park_6b_white")}
  else{return(prob_to_carpark_fn(code, probabilities_matrix, slot_type,cp_state,slot_count))}
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

#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Multi carpark
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#initial setup
cp_list <- c("3", "3a", "4", "5", "5b", "6b")

#------------------------------------------------------------------------ 
# Function to simulate car park system
simulate_car_parks_once_month_m <- function(cp_list, cp_state, cp_capacity,
                                           cp_sheltered, cp_red_perc, month,
                                           arrival_rate, simulation_time, distance_matrix) {
  cp_red_count <- round(cp_red_perc/100*cp_capacity, 0)
  cp_white_count <- cp_capacity - cp_red_count
  probabilities_matrix <- distance_to_probability(distance_matrix, cp_sheltered)
  
  car_trajectory_red <- function(car_id,cp_state,cp_red_count) {
    
    trajectory(paste0("car_", car_id, "_red")) %>%
      renege_in(function() ifelse(get_capacity(env, paste0("car_park_", car_id, "_red"))==0,0,1), out=trajectory("to_white")%>%
                  renege_in(function() ifelse(get_capacity(env, paste0("car_park_", car_id, "_white"))==0,0,10), out=trajectory("another_carpark_red")%>%
                              select(resource=function()prob_to_carpark_fn(car_id, probabilities_matrix, "red",cp_state,cp_red_count))%>%
                              seize_selected()%>%
                              timeout(get_sim_function_duration(car_id, "red", month))%>%
                              release_selected()) %>%
                  seize(paste0("car_park_", car_id, "_white"))%>%
                  renege_abort()%>%
                  timeout(get_sim_function_duration(car_id, "red", month))%>%
                  release(paste0("car_park_", car_id, "_white"))) %>%
      seize(paste0("car_park_", car_id, "_red")) %>%
      renege_abort() %>%
      timeout(get_sim_function_duration(car_id, "red", month)) %>%
      release(paste0("car_park_", car_id, "_red"))
  }
  car_trajectory_white <- function(car_id,cp_state,cp_white_count) {
    
    trajectory(paste0("car_", car_id, "_white")) %>%
      renege_in(function() ifelse(get_capacity(env, paste0("car_park_", car_id, "_white"))==0,0,10), out=trajectory("another_carpark_white")%>%
                  select(resource=function()prob_to_carpark_fn(car_id, probabilities_matrix, "white",cp_state,cp_white_count))%>%
                  seize_selected()%>%
                  timeout(get_sim_function_duration(car_id, "white", month))%>%
                  release_selected()) %>%
      seize(paste0("car_park_", car_id, "_white")) %>%
      renege_abort() %>%
      timeout(get_sim_function_duration(car_id, "white", month)) %>%
      release(paste0("car_park_", car_id, "_white"))
  }
  
  env <- simmer()
  
  for (i in 1:6) {
    if (cp_state[i]=="closed") {
      env %>% add_resource(paste0("car_park_", cp_list[i],"_red"), capacity = 0)
      env %>% add_resource(paste0("car_park_", cp_list[i],"_white"), capacity = 0)
    } else {
      env %>% add_resource(paste0("car_park_", cp_list[i],"_red"), 
                           capacity = cp_red_count[i])
      env %>% add_resource(paste0("car_park_", cp_list[i],"_white"), 
                           capacity = cp_white_count[i])
    }
  }
  
  for (i in 1:6) {
    if (cp_red_perc[i] != 0) {
      env %>% add_generator(paste0("car_generator_", cp_list[i], "_red"), 
                            car_trajectory_red(cp_list[i],cp_state, cp_red_count), 
                            get_sim_function_arrival(cp_list[i], "red", month, arrival_rate))
    }
    if (cp_red_perc[i] != 100) {
      env %>% add_generator(paste0("car_generator_", cp_list[i], "_white"),
                             car_trajectory_white(cp_list[i],cp_state, cp_white_count),
                             get_sim_function_arrival(cp_list[i], "white", month, arrival_rate))
    }
  }
  
  out <- run(env, until = simulation_time)
  return(out)
}

run_sim_m_once <- function(sim = 1, # indicates the simulation number
                              cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                              cp_state = c("open", "open", "open", "open", "open", "open"), 
                              cp_capacity = c(243, 67, 116, 70, 32, 173), 
                              cp_sheltered = c(100, 0, 0, 0, 0, 100),
                              cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                              arrival_rate = 1,
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
  unclosed_lots_red <- paste0("car_park_", cp_list, "_red")[which(cp_state != "closed" & cp_red_perc != 0)]
  unclosed_lots_white <- paste0("car_park_", cp_list, "_white")[which(cp_state != "closed" & cp_red_perc != 100)]
  month_data <- get_month_data(month_st, month_en)
  df_main <- NULL
  
  for (mon in 1:length(month_data[[1]])){
    results<-simulate_car_parks_once_month_m(cp_list, cp_state, cp_capacity, cp_sheltered,
                                            cp_red_perc, month_data[[1]][mon], arrival_rate, 
                                            month_data[[2]][mon], distance_matrix)
    #I get the max number of cars of each slot type in each cp
    summarized_table_red <- get_mon_resources(results) %>% 
      filter(resource %in% unclosed_lots_red) %>%
      group_by(resource) %>%
      summarize(red_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                red_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T),
                sd_red=sd(server,na.rm=T)) %>%
      mutate(resource=sub("_red", "", resource))
    summarized_table_white<-get_mon_resources(results) %>%
      filter(resource %in% unclosed_lots_white) %>%
      group_by(resource) %>%
      summarize(white_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                white_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T),
                sd_white=sd(server,na.rm=T)) %>%
      mutate(resource=sub("_white", "", resource))
    cur_df <- data.frame(resource = unclosed_lots) %>% left_join(summarized_table_red, by = "resource") %>%
      left_join(summarized_table_white, by = "resource") %>% 
      replace(is.na(.), 0) %>%
      mutate(mon = month_data[[1]][mon], sim = sim)
    df_main <- rbind(df_main, cur_df)
  }
  return(list(df_main))
}

# Paralleize function to use mclapply for multiple sims
run_sim_n_times_m <- function(n = 20, 
                                     cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                                     cp_state = c("open", "open", "open", "open", "open", "open"), 
                                     cp_capacity = c(243, 67, 116, 70, 32, 173), 
                                     cp_sheltered = c(100, 0, 0, 0, 0, 100),
                                     cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                                     arrival_rate = 1,
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
  unclosed_lots_red <- paste0("car_park_", cp_list, "_red")[which(cp_state != "closed" & cp_red_perc != 0)]
  unclosed_lots_white <- paste0("car_park_", cp_list, "_white")[which(cp_state != "closed" & cp_red_perc != 100)]
  month_data <- get_month_data(month_st, month_en)
  
  # Use mclapply to parallelize the simulations 
  # Windows systems don't work
  if (tolower(Sys.info()["sysname"]) == "windows"){
    results_list <- mclapply(1:n, function(i) {
      out <- run_sim_m_once(i, cp_list, cp_state, cp_capacity, cp_sheltered,
                                    cp_red_perc, arrival_rate, month_st, month_en, distance_matrix)
      return(out)
    }, mc.cores = 1)
  } else {
    results_list <- mclapply(1:n, function(i) {
      out <- run_sim_m_once(i, cp_list, cp_state, cp_capacity, cp_sheltered,
                                    cp_red_perc, arrival_rate, month_st, month_en, distance_matrix)
      return(out)
    }, mc.cores = parallel::detectCores() - 2)
  }
  # Extract results from the list
  df_main <- do.call(rbind, lapply(results_list, function(result) result[[1]]))
  plist_red_util <- plist_white_util <- plist_red_use <- plist_white_use <- list()
  df_utils_red <- df_utils_white <- NULL
  for (mon in 1:length(month_data[[1]])){
    results <- simulate_car_parks_once_month_m(cp_list, cp_state, cp_capacity, cp_sheltered,
                                              cp_red_perc, month_data[[1]][mon], arrival_rate, 
                                              month_data[[2]][mon], distance_matrix)
    # Red
    r2 <- get_mon_resources(results) %>% filter(resource %in% unclosed_lots_red)
    p_red1 <- plot(r2, metric = "utilization", unclosed_lots_red) + labs(title = month_data[[1]][mon])
    plist_red_util <- append(plist_red_util, list(p_red1))
    p_red2 <- plot(r2, metric = "usage", unclosed_lots_red, steps = F) + labs(title = month_data[[1]][mon])
    plist_red_use <- append(plist_red_use, list(p_red2))
    cur_df <- get_mon_resources(results) %>% filter(resource %in% unclosed_lots_red)
    cur_df$mon = mon
    df_utils_red <- rbind(df_utils_red, cur_df)
    
    # White
    r2 <- get_mon_resources(results) %>% filter(resource %in% unclosed_lots_white)
    p_white1 <- plot(r2, metric = "utilization", unclosed_lots_white) + labs(title = month_data[[1]][mon])
    plist_white_util <- append(plist_white_util, list(p_white1))
    p_white2 <- plot(r2, metric = "usage", unclosed_lots_white, steps = F) + labs(title = month_data[[1]][mon])
    plist_white_use <- append(plist_white_use, list(p_white2))
    cur_df <- get_mon_resources(results) %>% filter(resource %in% unclosed_lots_white)
    cur_df$mon = mon
    df_utils_white <- rbind(df_utils_white, cur_df)
  }
  
  return(list(df_main, plist_red_util, plist_white_util, plist_red_use, plist_white_use, 
              df_utils_red, df_utils_white))
}



#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Single carpark
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#------------------------------------------------------------------------ 
# Need to run these every time a new simulation

# Function to simulate car park system
# cp_index denotes carpark index from 1:6 mapping to c("3", "3a", "4", "5", "5b", "6b")
simulate_car_parks_once_month_s <- function(cp_index, cp_capacity, cp_red_perc,
                                           month, arrival_rate, simulation_time) {
  cp_red_count <- round(cp_red_perc/100*cp_capacity, 0)
  cp_white_count <- cp_capacity - cp_red_count
  
  car_trajectory_red <- function(car_id) {
    trajectory(paste0("car_", car_id, "_red")) %>%
      renege_in(1, out=trajectory()%>%
                  seize(paste0("car_park_", car_id, "_white"))%>%
                  renege_abort()%>%
                  timeout(get_sim_function_duration(car_id, "red", month))%>%
                  release(paste0("car_park_", car_id, "_white"))) %>%
      seize(paste0("car_park_", car_id, "_red")) %>%
      renege_abort() %>%
      timeout(get_sim_function_duration(car_id, "red", month)) %>%
      release(paste0("car_park_", car_id, "_red"))
  }
  
  car_trajectory_white <- function(car_id) {
    trajectory(paste0("car_", car_id, "_white")) %>%
      seize(paste0("car_park_", car_id, "_white")) %>%
      timeout(get_sim_function_duration(car_id, "white", month)) %>%
      release(paste0("car_park_", car_id, "_white"))
  }
  
  env <- simmer()
  
  env %>% add_resource(paste0("car_park_", cp_list[cp_index], "_red"), 
                       capacity = cp_red_count)
  env %>% add_resource(paste0("car_park_", cp_list[cp_index], "_white"), 
                       capacity = cp_white_count)
  
  if (cp_index == 1) {
    env %>% add_generator("car_generator_3_red", car_trajectory_red("3"), 
                          get_sim_function_arrival("3", "red", month, arrival_rate))
    env %>% add_generator("car_generator_3_white", car_trajectory_white("3"), 
                          get_sim_function_arrival("3", "white", month, arrival_rate))
  } else if (cp_index == 2) {
    env %>% add_generator("car_generator_3a_red", car_trajectory_red("3a"), 
                          get_sim_function_arrival("3a", "red", month, arrival_rate))
    env %>% add_generator("car_generator_3a_white", car_trajectory_white("3a"), 
                          get_sim_function_arrival("3a", "white", month, arrival_rate))
  } else if (cp_index == 3) {
    env %>% add_generator("car_generator_4_red", car_trajectory_red("4"), 
                          get_sim_function_arrival("4", "red", month, arrival_rate))
    env %>% add_generator("car_generator_4_white", car_trajectory_white("4"), 
                          get_sim_function_arrival("4", "white", month, arrival_rate))
  } else if (cp_index == 4) {
    env %>% add_generator("car_generator_5_red", car_trajectory_red("5"), 
                          get_sim_function_arrival("5", "red", month, arrival_rate))
    env %>% add_generator("car_generator_5_white", car_trajectory_white("5"), 
                          get_sim_function_arrival("5", "white", month, arrival_rate))
  } else if (cp_index == 5) {
    env %>% add_generator("car_generator_5b_red", car_trajectory_red("5b"), 
                          get_sim_function_arrival("5b", "red", month, arrival_rate))
    env %>% add_generator("car_generator_5b_white", car_trajectory_white("5b"), 
                          get_sim_function_arrival("5b", "white", month, arrival_rate))
  } else {
    env %>% add_generator("car_generator_6b_red", car_trajectory_red("6b"), 
                          get_sim_function_arrival("6b", "red", month, arrival_rate))
    env %>% add_generator("car_generator_6b_white", car_trajectory_white("6b"), 
                          get_sim_function_arrival("6b", "white", month, arrival_rate))
  }
  
  out <- run(env, until = simulation_time)
  return(out)
}

#function to let the simulation run once
run_sim_s_once <- function(sim = 1, # to indicate simulation number,
                                   cp_index = 1,
                                   cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                                   cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                                   arrival_rate = 1,
                                   month_st = "Jan", month_en = "Apr"){
  
  df_main <- NULL
  month_data <- get_month_data(month_st, month_en)
  red <- paste0("car_park_", cp_list[cp_index], "_red")
  white <- paste0("car_park_", cp_list[cp_index], "_white")
  for (mon in 1:length(month_data[[1]])){
    results<-simulate_car_parks_once_month_s(cp_index, cp_capacity, 
                                            cp_red_perc, month_data[[1]][mon], arrival_rate, month_data[[2]][mon])
    if (exists(red, results$resources)){
      summarized_table_red <- get_mon_resources(results) %>% 
        filter(resource %in% red) %>%
        summarize(red_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                  red_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T))
    } else {summarized_table_red <- data.frame(red_mean_parking = NA)}
    if (exists(white, results$resources)) {
      summarized_table_white<-get_mon_resources(results) %>% 
        filter(resource %in% white) %>%
        summarize(white_mean_parking = mean(server, na.rm = T)/mean(capacity, na.rm = T),
                  white_max_parking = max(server, na.rm = T)/mean(capacity, na.rm = T))
    }  else {summarized_table_white <- data.frame(white_mean_parking = NA)}
    cur_df <- cbind(summarized_table_red, summarized_table_white) %>% 
      replace(is.na(.), 0) %>%
      mutate(mon = month_data[[1]][mon], sim = sim)
    df_main <- rbind(df_main, cur_df)
  }
  return(list(df_main))
}

# Paralleize function to use mclapply for multiple sims
run_sim_n_times_s <- function(n = 20, 
                              cp_index = 1,
                              cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                              cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                              arrival_rate = 1,
                              month_st = "Jan", month_en = "Apr"){
  
  month_data <- get_month_data(month_st, month_en)
  red <- paste0("car_park_", cp_list[cp_index], "_red")
  white <- paste0("car_park_", cp_list[cp_index], "_white")
  
  # Use mclapply to parallelize the simulations
  if (tolower(Sys.info()["sysname"]) == "windows"){
    results_list <- mclapply(1:n, function(i) {
      out <- run_sim_s_once(i, cp_index, cp_capacity, cp_red_perc, 
                                    arrival_rate, month_st, month_en)
      return(out)
    }, mc.cores = 1)
  } else {
    results_list <- mclapply(1:n, function(i) {
      out <- run_sim_s_once(i, cp_index, cp_capacity, cp_red_perc, 
                                    arrival_rate, month_st, month_en)
      return(out)
    }, mc.cores = parallel::detectCores() - 2)
  }
  
  # Extract results from the list
  df_main <- do.call(rbind, lapply(results_list, function(result) result[[1]]))
  
  # Plot for simulations:
  plist_red_use <- plist_white_use <- list()
  df_utils_red <- df_utils_white <- NULL
  for (mon in 1:length(month_data[[1]])){
    results <- simulate_car_parks_once_month_s(cp_index, cp_capacity, 
                                              cp_red_perc, month_data[[1]][mon], arrival_rate, month_data[[2]][mon])
    if (exists(red, results$resources)){
      # Red
      r2 <- get_mon_resources(results) %>% filter(resource %in% red)
      p_red2 <- plot(r2, metric = "usage", steps = F) + labs(title = month_data[[1]][mon])
      plist_red_use <- append(plist_red_use, list(p_red2))
      cur_df <- get_mon_resources(results) %>% filter(resource %in% red)
      cur_df$mon = mon
      df_utils_red <- rbind(df_utils_red, cur_df)
    } else {plist_red_use <- list(NULL)}
    if (exists(white, results$resources)) {
      # White
      r2 <- get_mon_resources(results) %>% filter(resource %in% white)
      p_white2 <- plot(r2, metric = "usage", steps = F) + labs(title = month_data[[1]][mon])
      plist_white_use <- append(plist_white_use, list(p_white2)) 
      cur_df <- get_mon_resources(results) %>% filter(resource %in% white)
      cur_df$mon = mon
      df_utils_white <- rbind(df_utils_white, cur_df)
    }  else {plist_white_use <- list(NULL)}
  }
  
  return(list(df_main, plist_red_use, plist_white_use, df_utils_red, df_utils_white))
}
