library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(simmer)
library(simmer.plot)

df_5 <- read.csv("../data/df_5.csv") %>%
  # Remove problematic IU
  filter(IU != "5228257a") %>%
  # Some rows don't contain time value, making analysis very difficult (since this number is very small, maybe 10 data points, we can simply remove these rows)
  filter(str_length(enter_time) == 19, str_length(exit_time) == 19) %>% 
  # Convert to proper date-time objects and updating the du_val column
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S"),
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
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
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
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
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
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
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
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
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
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
         du_val = as.numeric(floor(difftime(exit_time, enter_time, units = "mins")))) %>%
  # Only parked cars
  filter(du_val > 10) %>%
  # Arrange in proper order
  arrange(enter_time)

## CLOSURE IMPACT

# distance in meters
dcp5_cp4 = 400
dcp5_cp5b = 400
dcp5_cp3a = 450
dcp5_cp3 = 450
dcp5_cp6b = 800
dcp5_cp10 = 1400 # We cannot even use cp10 since we don't have observations before construction began

closure_cp5_date <- df_5 %>% 
  arrange(enter_time) %>% tail(1) %>% 
  pull(enter_time) %>% as.Date()

df_cp3_impact <- df_3 %>% mutate(date = as.Date(enter_time)) %>%
  group_by(date, slot) %>%
  count() %>% 
  ungroup() %>%
  mutate(constr = as.factor(ifelse(date > closure_cp5_date, 1, 0)))
ggplot(df_cp3_impact) +
  geom_density(aes(x = n, fill= constr)) +
  facet_wrap(slot~., scales = "free_y") +
  theme_bw()
df_cp3_impact_tab <- df_cp3_impact %>% group_by(slot, constr) %>% summarise(median_arr = median(n))
df_cp3_impact_tab$cp = "cp3"


df_cp3a_impact <- df_3a %>% mutate(date = as.Date(enter_time)) %>%
  group_by(date, slot) %>%
  count() %>% 
  ungroup() %>%
  mutate(constr = as.factor(ifelse(date > closure_cp5_date, 1, 0)))
ggplot(df_cp3a_impact) +
  geom_density(aes(x = n, fill= constr)) +
  facet_wrap(slot~., scales = "free_y") +
  theme_bw()
df_cp3a_impact_tab <- df_cp3a_impact %>% group_by(slot, constr) %>% summarise(median_arr = median(n))
df_cp3a_impact_tab$cp = "cp3a"


df_cp4_impact <- df_4 %>% mutate(date = as.Date(enter_time)) %>%
  group_by(date, slot) %>%
  count() %>% 
  ungroup() %>%
  mutate(constr = as.factor(ifelse(date > closure_cp5_date, 1, 0)))
ggplot(df_cp4_impact) +
  geom_density(aes(x = n, fill= constr)) +
  facet_wrap(slot~., scales = "free_y") +
  theme_bw()
df_cp4_impact_tab <- df_cp4_impact %>% group_by(slot, constr) %>% summarise(median_arr = median(n))
df_cp4_impact_tab$cp = "cp4"


df_cp5b_impact <- df_5b %>% mutate(date = as.Date(enter_time)) %>%
  group_by(date, slot) %>%
  count() %>% 
  ungroup() %>%
  mutate(constr = as.factor(ifelse(date > closure_cp5_date, 1, 0)))
ggplot(df_cp5b_impact) +
  geom_density(aes(x = n, fill= constr)) +
  facet_wrap(slot~., scales = "free_y") +
  theme_bw()
df_cp5b_impact_tab <- df_cp5b_impact %>% group_by(slot, constr) %>% summarise(median_arr = median(n))
df_cp5b_impact_tab$cp = "cp5b"


df_cp6b_impact <- df_6b %>% mutate(date = as.Date(enter_time)) %>%
  group_by(date, slot) %>%
  count() %>% 
  ungroup() %>%
  mutate(constr = as.factor(ifelse(date > closure_cp5_date, 1, 0)))
ggplot(df_cp6b_impact) +
  geom_density(aes(x = n, fill= constr)) +
  facet_wrap(slot~., scales = "free_y") +
  theme_bw()
df_cp6b_impact_tab <- df_cp6b_impact %>% group_by(slot, constr) %>% summarise(median_arr = median(n))
df_cp6b_impact_tab$cp = "cp6b"


df_overall_impact <- rbind(df_cp3_impact_tab, df_cp3a_impact_tab,
                           df_cp4_impact_tab, df_cp5b_impact_tab,
                           df_cp6b_impact_tab)

df_overall_impact %>% 
  pivot_wider(names_from = constr, values_from = median_arr) %>%
  mutate(diff = `1` - `0`,
         dist = case_when(
    cp=="cp3"~dcp5_cp3,
    cp=="cp3a"~dcp5_cp3a,
    cp=="cp4"~dcp5_cp4,
    cp=="cp5b"~dcp5_cp5b,
    cp=="cp6b"~dcp5_cp6b
  )) %>%
  mutate(cp = reorder(as.factor(cp), dist)) %>%
  ggplot() +
  geom_col(aes(x = cp, y = diff)) +
  facet_wrap(slot~.) +
  theme_bw() +
  labs(x = "Carparks (Ordered by increasing dist from CP5)", y = "Difference in Median",
       caption = "Median difference in total arrivals per day before and after CP5 closure")








# NON HOMOGENOUS POSSION

df <- df_3a %>% filter(hour(enter_time) %in% 12:19) %>%
  arrange(enter_time) %>% 
  mutate(iat = difftime(enter_time, lag(enter_time)),
         cd = day(enter_time), nd = lag(day(enter_time))) %>%
  filter(cd == nd)

plot_qq_poiss <- function(x, r){
  n <- length(x)
  p = seq(1/(n + 1), n/(n + 1), by = 1/(n + 1))
  tq = qexp(p, rate = r)
  sq <- sort(x)
  data.frame(tq, sq) %>%
    ggplot(aes(x = tq, y = sq)) +
    # Plot original points
    geom_point(size = 2.5, color = "blue") +
    # To plot the y = x line
    geom_abline(slope = 1, intercept = 0, linewidth = 0.8, linetype = 2) +
    # Plot styling [Optional]
    theme_minimal() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "QQ Plot") +
    theme(plot.title = element_text(hjust = 0.5))
}


# 1. Check for exponential distribution of inter arrival times

inter_arrival_times <- as.numeric(df$iat)

# input in separate plot(because fig margins are too large)
pdf("inter_arrival_times_output_plot.pdf")
plot(sort(inter_arrival_times))
dev.off()

plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))


# What happens if closure

res <- cumsum(inter_arrival_times)

# Simulated non-homogeneous Poisson data
lambda <- function(t) 1 + 0.5*t 
set.seed(123) 
t <- res
observed_counts <- sapply(t, function(t) rpois(1, lambda(t)))  # Simulated counts
expected_counts <- sapply(t, function(t) lambda(t))  # Calculate expected counts
chisq_test <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
print(chisq_test)


lambda <- function(t) 1 + t + t^2 + t^3
set.seed(123) 
t <- res
observed_counts <- sapply(t, function(t) rpois(1, lambda(t)))  # Simulated counts
expected_counts <- sapply(t, function(t) lambda(t))  # Calculate expected counts
chisq_test <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
print(chisq_test)

set.seed(123) 
t <- res
b0 <- seq(0, 10, 1)
b1 <- seq(0.5, 10, 1)
finres = rep(0, length(b0)*length(b1))
counter = 0
for (i0 in b0){
  for (i1 in b1){
    counter = counter + 1
    cur_p_vals = rep(0, 10)
    for (iter in 1:10){
      lambda <- function(t) i0 + i1*t
      observed_counts <- sapply(t, function(t) rpois(1, lambda(t)))  # Simulated counts
      expected_counts <- sapply(t, function(t) lambda(t))  # Calculate expected counts
      chisq_test <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
      cur_p_vals[iter] = chisq_test$p.value
    }
    finres[counter] = mean(cur_p_vals)
  }
}

finres
which.max(finres)

counter = 0
for (i0 in b0){
  for (i1 in b1){
    counter = counter + 1
    if (counter == 25){
      print(i0)
      print(i1)
    }
  }
}

lambda <- function(t) 1 + 0.5*t
t <- res
observed_counts <- sapply(t, function(t) rpois(1, lambda(t)))  # Simulated counts
expected_counts <- sapply(t, function(t) lambda(t))  # Calculate expected counts
chisq_test <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
print(chisq_test)

