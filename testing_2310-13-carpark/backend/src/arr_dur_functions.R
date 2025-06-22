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
library(jsonlite)

#-------------------------------------------------------------------------------
# DURATION FUNCTION 
#-------------------------------------------------------------------------------

# Load the JSON file
duration_params <- fromJSON("backend/params/duration_params.json")

# Function to retrieve value from the loaded data
get_sim_function_duration <- function(cp, slot_type, month) {
  if (month %in% names(duration_params) &&
      slot_type %in% names(duration_params[[month]]) &&
      cp %in% names(duration_params[[month]][[slot_type]])) {
    return(duration_params[[month]][[slot_type]][[cp]])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
# ARRIVAL FUNCTION 
#-------------------------------------------------------------------------------

# Load the JSON file
arrival_params <- fromJSON("backend/params/arrival_params.json")

# Function to retrieve the inter-arrival times based on the arrival rate expected by user
get_sim_function_arrival <- function(cp, slot_type, month, arrival_rate) {
  if (month %in% names(arrival_params) &&
      slot_type %in% names(arrival_params[[month]]) &&
      cp %in% names(arrival_params[[month]][[slot_type]])) {
    params <- arrival_params[[month]][[slot_type]][[cp]]
    if (params[1] == "unif"){
      return(return(function() runif(1, as.numeric(params[2]), as.numeric(params[3]))/arrival_rate))
    } else {
      gen_f <- function(){
        iat <- rlomax(1, as.numeric(params[2]), as.numeric(params[3]))/arrival_rate
        iat <- min(iat, 100)
        return(iat)
      }
      return(gen_f)
    }
  } else {
    return(NULL)
  }
}

# get_sim_function_arrival <- function(cp, slot_type, month, arrival_rate) {
#   months_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   month_num <- match(month, months_vec)
#   problem_months <- c("Jan", "Feb", "Mar")
#   dfname <- paste0(slot_type, "_", cp)
#   if (exists(dfname)) {
#     # Special Case 1
#     if (cp == "3" & slot_type == "white" & month == "Jul") {
#       return(function() runif(1, 0, 50)/arrival_rate)
#       # Special Case 2
#     } else if (cp == "3" & slot_type == "white" & month == "Dec") {
#       return(function() runif(1, 0, 100)/arrival_rate)
#       # Special Case 3
#     } else if (cp == "5" & month %in% problem_months) {
#       fit <- fitdist(get(dfname)$iat, "lomax", start = list(scale = 1, shape3.q = 1))
#       scale_para <- as.numeric(summary(fit)$estimate[1])
#       shape_para <- as.numeric(summary(fit)$estimate[2])
#       iat <- rlomax(1, scale_para, shape_para)
#       if (iat > median(get(dfname)$iat)) {
#         iat <- median(get(dfname)$iat)
#       }
#       return(function() iat/arrival_rate)
#     } else {
#       res <- get(dfname) %>% filter(month == month_num)
#       iat <- arrival_params[[month]][[slot_type]][[cp]]
#       if (iat > median(res$iat)) {
#         iat <- median(res$iat)
#       }
#       return(function() iat/arrival_rate)
#     }
#   }
# }

# Placeholder until the arrival function is correctly configured:
# get_sim_function_arrival <- function(cp, slot_type, month, arrival_rate) {
#   months_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   month_num <- match(month, months_vec)
#   problem_months <- c("Jan", "Feb", "Mar")
#   dfname <- paste0(slot_type, "_", cp)
#   if (exists(dfname)) {
#     # Handle the missing data for Carpark 5 from Jan-Mar
#     if (cp == "5" & month %in% problem_months) {
#       # In the case where the data does not contain certain months, fit the lomax distribution using the remaining months instead
#       fit <- fitdist(get(dfname)$iat, "lomax", start = list(scale = 1, shape3.q = 1))
#       scale_para <- as.numeric(summary(fit)$estimate[1])
#       shape_para <- as.numeric(summary(fit)$estimate[2])
#       return(function() rlomax(1, scale_para, shape_para)/arrival_rate)
#       # Special Case 1
#     } else if (cp == "3" & slot_type == "white" & month == "Jul") {
#       return(function()  runif(1, 0, 50)/arrival_rate)
#       # Special Case 2
#     } else if (cp == "3" & slot_type == "white" & month == "Dec") {
#       return(function()  runif(1, 0, 100)/arrival_rate)
#     } else {
#       res <- get(dfname) %>% filter(month == month_num)
#       fit <- fitdist(res$iat, "lomax", start = list(scale = 1, shape3.q = 1))
#       scale_para <- as.numeric(summary(fit)$estimate[1])
#       shape_para <- as.numeric(summary(fit)$estimate[2])
#       return(function()  rlomax(1, scale = scale_para, shape3.q = shape_para)/arrival_rate)
#     }
#   } 
# }
