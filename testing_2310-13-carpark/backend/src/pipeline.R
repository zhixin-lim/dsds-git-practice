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

old_duration_params <- fromJSON("backend/params/duration_params.json")
old_arrival_params <- fromJSON("backend/params/arrival_params.json")

# Initialize the nested list
duration_params_new <- list()

# Initialize the nested list
arrival_params_new <- list()

data_update_pipeline <- function(filepaths = c("backend/data/raw_Cp33a45b6b_a.csv", 
                                               "backend/data/raw_Cp5_a.csv")) {
  # Initialize an empty data frame to store the concatenated data
  df <- data.frame()
  
  # Loop through each file path and read the CSV, then bind it to df
  for (filepath in filepaths) {
    temp_df <- read.csv(filepath)
    df <- rbind(df, temp_df)
  }
  df <- df %>%
    # Standardize column names
    rename(enter_time = enter, exit_time = Exit) %>%
    # Correctly identify NAs for columns with _du
    mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), na_if, "\\N") %>%
    mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), function(x) ifelse(as.numeric(x) < 0, NA, x)) %>%
    # Correct date types
    mutate(enter_time = dmy_hm(enter_time), exit_time = dmy_hm(exit_time))
  pdf <- df %>% mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
                         as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
                       check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
    filter(check_col_1 != 1 | check_col_2 <= 0)
  
  # We remove above rows for now and restructure data into tidy format
  df_tidy <- df %>% 
    # Implement checks
    mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
             as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
           check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
    filter(!(check_col_1 != 1 | check_col_2 <= 0)) %>%
    # Remove check columns
    dplyr::select(-c(check_col_1, check_col_2)) %>%
    # Tidy dataframe: "hourly_du", "staff_du", "student_du", "esp_du"
    mutate(type = as.factor(case_when(
      !is.na(hourly_du) ~ "non-season",
      !is.na(staff_du) ~ "season (staff)",
      !is.na(student_du) ~ "season (student)",
      !is.na(esp_du) ~ "non-season (esp)"
    )), du_val = as.numeric(case_when(
      !is.na(hourly_du) ~ hourly_du,
      !is.na(staff_du) ~ staff_du,
      !is.na(student_du) ~ student_du,
      !is.na(esp_du) ~ esp_du
    ))) %>%
    # Remove redundant cols after tidying data
    dplyr::select(-c("hourly_du", "staff_du", "student_du", "esp_du")) %>%
    # If essential service provider, data is boolean (need to correct it by maybe actually 
    # calculating the correct time manually):
    mutate(du_val = ifelse(type == "esp", difftime(exit_time, enter_time, units = "mins"), du_val)) %>%
    # Drop duplicate rows
    distinct() %>%
    mutate(slot = case_when(
      type == "non-season" ~ "white",
      type == "season (staff)"  ~ "red",
      type == "season (student)"  ~ "white",
      type == "non-season (esp)"  ~ "white"
    ))
  
  # ensure du_val>10, IU!=0 and IU != "5228257a". Also ensure that date is in POSIXCT format
  df_tidy <- df_tidy %>%
    filter(du_val>10, IU!=0, IU != "5228257a") %>%
    mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
           exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
    arrange(enter_time)
  
  # partitioning (assuming that the IDs follow the historical order)
  df_3 <- df_tidy %>% filter(ExitId %in% c(82, 83))
  df_3a <- df_tidy %>% filter(ExitId == 161)
  df_4 <- df_tidy %>% filter(ExitId == 76)
  df_5 <- df_tidy %>% filter(ExitId == 70)
  df_5b <- df_tidy %>% filter(ExitId == 92)
  df_6b <- df_tidy %>% filter(ExitId == 48)
  

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
  
  get_duration_val <- function(cp, slot_type, month){
    df_name <- paste0("df_", cp)
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month_number <- match(month, months)
    df <- get(df_name) %>% filter(slot == slot_type, month == month_number)
    # If no data uploaded for current carpark, just use old values
    if (nrow(df) == 0){
      return(old_duration_params[[month]][[slot_type]][[cp]])
    }
    return(median(df$du_val, na.rm = T))
  }
  
  # Loop through the combinations and store values in the nested list
  for (m in c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) {
    duration_params_new[[m]] <- list()
    
    for (s in c("red", "white")) {
      duration_params_new[[m]][[s]] <- list()
      
      for (c in c("3", "3a", "4", "5", "5b", "6b")) {
        duration_params_new[[m]][[s]][[c]] <- get_duration_val(c, s, m)
      }
    }
  }
  
  # Convert the list to JSON
  json_data <- toJSON(duration_params_new)
  
  # Write JSON to a file
  writeLines(json_data, "../backend/params/duration_params.json")
  
  #  Function to retrieve the inter-arrival times based on the arrival rate expected by user
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
        # If no data uploaded for current carpark, just use old values
        if (nrow(res) == 0){
          return(old_arrival_params[[month]][[slot_type]][[cp]])
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
  
  # Loop through the combinations and store values in the nested list
  for (m in c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) {
    arrival_params_new[[m]] <- list()
    
    for (s in c("red", "white")) {
      arrival_params_new[[m]][[s]] <- list()
      
      for (c in c("3", "3a", "4", "5", "5b", "6b")) {
        arrival_params_new[[m]][[s]][[c]] <- get_arrival_val(c, s, m)
      }
    }
  }
  
  # Convert the list to JSON
  json_data <- toJSON(arrival_params_new)
  
  # Write JSON to a file
  writeLines(json_data, "../backend/params/arrival_params.json")
  
}

