library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)

# STEP 1: LOAD ----------

df1 <- read_csv("../data/Sample raw data format - duration_cp10_Apr2023_a.csv") # cp10
df2 <- read_csv("../data/raw_Cp5_a.csv") # cp5
df3 <- read_csv("../data/raw_Cp33a45b6b_a.csv") # cp3, 3a, 4, 5b, 6b

# STEP 2: INITIAL PREPROCESS -----------------

# Standardise data formats:
df1 <- df1 %>%
  # Add an exit id that is not already for any other carpark
  cbind(ExitId = setdiff(1:100, union(unique(df3$ExitId), unique(df2$ExitId)))[1], .) %>% 
  # Remove report time as not in other data frames
  select(-`report time`) %>%
  # Standardize column names
  rename(enter_time = `Enter Time`, exit_time = `Exit time`)

df2 <- df2 %>%
  # Standardize column names
  rename(enter_time = enter, exit_time = Exit) %>%
  # Correctly identify NAs for columns with _du
  mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), na_if, "\\N") %>%
  mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), function(x) ifelse(as.numeric(x) < 0, NA, x)) %>%
  # Correct date types
  mutate(enter_time = dmy_hm(enter_time), exit_time = dmy_hm(exit_time))

df3 <- df3 %>%
  # Standardize column names
  rename(enter_time = enter, exit_time = Exit) %>%
  # Correctly identify NAs for columns with _du
  mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), na_if, "\\N") %>%
  mutate_at(vars("hourly_du", "staff_du", "student_du", "esp_du"), function(x) ifelse(as.numeric(x) < 0, NA, x)) %>%
  # Correct date types
  mutate(enter_time = dmy_hm(enter_time), exit_time = dmy_hm(exit_time))

# STEP 3: DATA CHECKS ----------------

# 1. Ensure only/at least one of hourly duration cols is present
# 2. Ensure exit time is more than entry time

# df1
pdf1 <- df1 %>% mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
                         as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
                       check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(check_col_1 != 1 | check_col_2 <= 0)
# We remove above rows for now and restructure data into tidy format
df1_tidy <- df1 %>% 
  # Implement checks
  mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
           as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
         check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(!(check_col_1 != 1 | check_col_2 <= 0)) %>%
  # Remove check columns
  select(-c(check_col_1, check_col_2)) %>%
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
  select(-c("hourly_du", "staff_du", "student_du", "esp_du")) %>%
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
  
# df2
pdf2 <- df2 %>% mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
                 as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
                 check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(check_col_1 != 1 | check_col_2 <= 0)
# We remove above rows for now and restructure data into tidy format
df2_tidy <- df2 %>% 
  # Implement checks
  mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
           as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
         check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(!(check_col_1 != 1 | check_col_2 <= 0)) %>%
  # Remove check columns
  select(-c(check_col_1, check_col_2)) %>%
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
  select(-c("hourly_du", "staff_du", "student_du", "esp_du")) %>%
  # Drop duplicate rows
  distinct() %>%
  mutate(slot = case_when(
    type == "non-season" ~ "white",
    type == "season (staff)"  ~ "red",
    type == "season (student)"  ~ "white",
    type == "non-season (esp)"  ~ "white"
  ))

# df3
pdf3 <- df3 %>% mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
                         as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
                       check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(check_col_1 != 1 | check_col_2 <= 0)
# We remove above rows for now and restructure data into tidy format
df3_tidy <- df3 %>% 
  # Implement checks
  mutate(check_col_1 = as.numeric(!is.na(hourly_du)) + as.numeric(!is.na(staff_du)) + 
           as.numeric(!is.na(student_du)) + as.numeric(!is.na(esp_du)),
         check_col_2 = difftime(exit_time, enter_time, units = "hours")) %>%
  filter(!(check_col_1 != 1 | check_col_2 <= 0)) %>%
  # Remove check columns
  select(-c(check_col_1, check_col_2)) %>%
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
  select(-c("hourly_du", "staff_du", "student_du", "esp_du")) %>%
  # Drop duplicate rows
  distinct() %>%
  mutate(slot = case_when(
    type == "non-season" ~ "white",
    type == "season (staff)"  ~ "red",
    type == "season (student)"  ~ "white",
    type == "non-season (esp)"  ~ "white"
  ))

# Data calculation agreement
df1_tidy %>% mutate(t = difftime(exit_time, enter_time, units = "mins")) %>% 
  filter(abs(t - du_val) > 1) %>% nrow() #0
df2_tidy %>% mutate(t = difftime(exit_time, enter_time, units = "mins")) %>% 
  filter(abs(t - du_val) > 1) %>% nrow() #0
df3_tidy %>% mutate(t = difftime(exit_time, enter_time, units = "mins")) %>% 
  filter(abs(t - du_val) > 1) %>% nrow() #0


# PARTITION DATASETS -------------
df_10 <- df1_tidy
df_5 <- df2_tidy
df_6b <- df3_tidy %>% filter(ExitId %in% c(48, 52))
df_4 <- df3_tidy %>% filter(ExitId == 76)
df_3 <- df3_tidy %>% filter(ExitId %in% c(82, 83))
df_5b <- df3_tidy %>% filter(ExitId == 92)
df_3a <- df3_tidy %>% filter(ExitId == 161)

# SAVE DATAFRAMES:
write.csv(df_10, "../data/df_10.csv", row.names=FALSE)
write.csv(df_5, "../data/df_5.csv", row.names=FALSE)
write.csv(df_6b, "../data/df_6b.csv", row.names=FALSE)
write.csv(df_4, "../data/df_4.csv", row.names=FALSE)
write.csv(df_3, "../data/df_3.csv", row.names=FALSE)
write.csv(df_5b, "../data/df_5b.csv", row.names=FALSE)
write.csv(df_3a, "../data/df_3a.csv", row.names=FALSE)
