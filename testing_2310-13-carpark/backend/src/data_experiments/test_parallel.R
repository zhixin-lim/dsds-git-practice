setwd("../../")
source("backend/src/get_results.R")
source("backend/src/arr_dur_functions.R")


# Function to run simulations and measure time
run_and_measure_time <- function(n, parallel_version = FALSE, ...) {
  if (parallel_version) {
    start_time <- system.time({
      run_sim_n_times_m_parallel(n = n, ...)
    })[3]
  } else {
    start_time <- system.time({
      run_sim_n_times_m(n = n, ...)
    })[3]
  }
  
  return(start_time)
}

# Set parameters
n_values <- c(5, 10, 15, 20, 50)  # Choose different values of n
cp_list <- c("3", "3a", "4", "5", "5b", "6b")
cp_state <- c("open", "open", "closed", "open", "open", "open")
cp_capacity <- c(243, 67, 116, 70, 32, 173)
cp_sheltered <- c(100, 0, 0, 0, 0, 100)
cp_red_perc <- c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)
arrival_rate <- 1
month_st <- "Jan"
month_en <- "Apr"
distance_matrix <- matrix(c(
  0, 500, 1000, 150, 200, 250,
  500, 0, 800, 1300, 180, 230,
  1000, 800, 0, 600, 1100, 160,
  150, 1300, 600, 0, 500, 1000,
  200, 180, 1100, 500, 0, 500,
  250, 230, 160, 1000, 500, 0
), nrow = 6, byrow = TRUE, 
dimnames = list(cp_list, cp_list))

# Run simulations for different values of n
times_non_parallel <- sapply(n_values, function(n) run_and_measure_time(n, FALSE, 
                                                                        cp_list = cp_list,
                                                                        cp_state = cp_state,
                                                                        cp_capacity = cp_capacity,
                                                                        cp_sheltered = cp_sheltered,
                                                                        cp_red_perc = cp_red_perc,
                                                                        arrival_rate = arrival_rate,
                                                                        month_st = month_st,
                                                                        month_en = month_en,
                                                                        distance_matrix = distance_matrix))

times_parallel <- sapply(n_values, function(n) run_and_measure_time(n, TRUE, 
                                                                    cp_list = cp_list,
                                                                    cp_state = cp_state,
                                                                    cp_capacity = cp_capacity,
                                                                    cp_sheltered = cp_sheltered,
                                                                    cp_red_perc = cp_red_perc,
                                                                    arrival_rate = arrival_rate,
                                                                    month_st = month_st,
                                                                    month_en = month_en,
                                                                    distance_matrix = distance_matrix))

# Create a data frame for plotting
df_time_complexity <- data.frame(
  n = rep(n_values, times = 2),
  version = rep(c("Non-Parallel", "Parallel"), each = length(n_values)),
  time = c(times_non_parallel, times_parallel)
)

# Plot the time complexity
ggplot(df_time_complexity, aes(x = n, y = time, color = version)) +
  geom_line() +
  geom_point() +
  labs(title = "Time Complexity Comparison",
       x = "Number of Simulations (n)",
       y = "Execution Time (seconds)",
       color = "Version") +
  theme_minimal()
