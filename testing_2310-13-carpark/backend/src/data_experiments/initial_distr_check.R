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

# Simulation with poisson assumption test: -------------------

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
df_cur <- df_3 %>% arrange(as.POSIXct(enter_time)) %>% mutate(iat = as.numeric(difftime(
  as.POSIXct(enter_time), 
  as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat))
inter_arrival_times <- df_cur$iat

# input in separate plot(because fig margins are too large)
pdf("plots/inter_arrival_times_output_plot.pdf")
plot(sort(inter_arrival_times))
dev.off()

plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))

# 2. Hypothesis: Maybe outliers:
df_cur <- df_3 %>% 
  arrange(as.POSIXct(enter_time)) %>% 
  mutate(iat = as.numeric(difftime(
    as.POSIXct(enter_time), 
    as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat)) %>%
  filter(
    iat < quantile(iat, 0.75) + 1.5*IQR(iat),
    iat > quantile(iat, 0.25) - 1.5*IQR(iat))
inter_arrival_times <- df_cur$iat

# separate plot
pdf("plots/outlier_adjusted_inter_arrival_times_output_plot.pdf")
plot(sort(inter_arrival_times))
dev.off()
plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))

# 3. Hypothesis: Maybe type of day:
df_cur <- df_3 %>% 
  filter(weekdays(enter_time) %in% c("Sunday", "Saturday")) %>%
  arrange(as.POSIXct(enter_time)) %>% 
  mutate(iat = as.numeric(difftime(
    as.POSIXct(enter_time), 
    as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat)) %>%
  filter(
    iat < quantile(iat, 0.75) + 1.5*IQR(iat),
    iat > quantile(iat, 0.25) - 1.5*IQR(iat))
inter_arrival_times <- df_cur$iat

# separate plot
pdf("plots/day_adjusted_inter_arrival_times_output_plot.pdf")
plot(sort(inter_arrival_times))
dev.off()

plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))

# 3. Hypothesis: Maybe type of parker:
df_cur <- df_3 %>%
  filter(type == "non-season") %>% 
  arrange(as.POSIXct(enter_time)) %>% 
  mutate(iat = as.numeric(difftime(
    as.POSIXct(enter_time), 
    as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat)) %>% 
  filter(
    iat < quantile(iat, 0.75) + 1.5*IQR(iat),
    iat > quantile(iat, 0.25) - 1.5*IQR(iat))
inter_arrival_times <- df_cur$iat

# separate plot
pdf("plots/output_plot_by_non_season_parkers.pdf")
plot(sort(inter_arrival_times))
dev.off()
plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))

# 4. Hypothesis: Maybe type of slot
df_cur <- df_3 %>%
  filter(slot == "red") %>% 
  arrange(as.POSIXct(enter_time)) %>% 
  mutate(iat = as.numeric(difftime(
    as.POSIXct(enter_time), 
    as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat)) %>% 
  filter(
    iat < quantile(iat, 0.75) + 1.5*IQR(iat),
    iat > quantile(iat, 0.25) - 1.5*IQR(iat))
inter_arrival_times <- df_cur$iat

# separate plot
pdf("plots/output_plot_by slot.pdf")
plot(sort(inter_arrival_times))
dev.off()

plot_qq_poiss(inter_arrival_times, 1/mean(inter_arrival_times))

# Maybe something else entirely! Let's look at the distribution of arrivals dircetly and see if can figure out any patterns
# this part is creating a time series res and inputting it into cur_df to find the distribution
cur_df <- df_3 %>% 
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         issumbreak = month(enter_time) %in% 5:7,
         iswintbreak = month(enter_time) == 12) %>%
  arrange(as.POSIXct(enter_time)) %>% 
  mutate(iat = as.numeric(difftime(
    as.POSIXct(enter_time), 
    as.POSIXct(lag(enter_time)), units = "mins"))) %>% 
  filter(!is.na(iat)) 
res <- rep(0, nrow(cur_df))
for (i in 1:nrow(cur_df)) {
  enter_time <- as.numeric(difftime(cur_df$enter_time[i], cur_df$enter_time[1], units = "mins"))
  res[i] <- enter_time
}
cur_df$var = res

# First, look at the distribution across weekdays (with gray regions highlighting breaks)
ggplot(cur_df, aes(x = var, fill = slot)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red" = "red", "white" = "white")) +
  theme_bw() +
  facet_wrap(wkd~.) +
  # this part is a rectangle that shows the summer and the winter breaks respectively
  annotate("rect", xmin = min(cur_df[cur_df$issumbreak == T,]$var), 
           xmax = max(cur_df[cur_df$issumbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = min(cur_df[cur_df$iswintbreak == T,]$var), 
           xmax = max(cur_df[cur_df$iswintbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5)

# Next, look at the distribution on whether it was weekend or not
ggplot(cur_df, aes(x = var, fill = slot)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red" = "red", "white" = "white")) +
  theme_bw() +
  facet_wrap(~isweekend) +
  annotate("rect", xmin = min(cur_df[cur_df$issumbreak == T,]$var), 
           xmax = max(cur_df[cur_df$issumbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = min(cur_df[cur_df$iswintbreak == T,]$var), 
           xmax = max(cur_df[cur_df$iswintbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5)

# Next, look at the distribution  across day of the week
ggplot(cur_df, aes(x = var, fill = slot)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red" = "red", "white" = "white")) +
  theme_bw() +
  facet_wrap(type~.) +
  annotate("rect", xmin = min(cur_df[cur_df$issumbreak == T,]$var), 
           xmax = max(cur_df[cur_df$issumbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = min(cur_df[cur_df$iswintbreak == T,]$var), 
           xmax = max(cur_df[cur_df$iswintbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5)

# Maybe combine analysis for weekend with the type of the slot
ggplot(cur_df, aes(x = var, fill = isweekend)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(type~.) +
  annotate("rect", xmin = min(cur_df[cur_df$issumbreak == T,]$var), 
           xmax = max(cur_df[cur_df$issumbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = min(cur_df[cur_df$iswintbreak == T,]$var), 
           xmax = max(cur_df[cur_df$iswintbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5)

# Esp dominates on weekend, so cannot see anything clearly, let's filter them out1
cur_df %>% filter(type != "non-season (esp)") %>%
  ggplot(aes(x = var, fill = isweekend)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(type~.) +
  annotate("rect", xmin = min(cur_df[cur_df$issumbreak == T,]$var), 
           xmax = max(cur_df[cur_df$issumbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = min(cur_df[cur_df$iswintbreak == T,]$var), 
           xmax = max(cur_df[cur_df$iswintbreak == T,]$var), ymin = -Inf, ymax = Inf, alpha = 0.5)

