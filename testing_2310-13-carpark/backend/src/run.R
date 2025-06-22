setwd("../../")
source("backend/src/get_results.R")
source("backend/src/arr_dur_functions.R")

simulation_results <- run_sim_n_times_m(n = 5, 
                         cp_list = c("3", "3a", "4", "5", "5b", "6b"), 
                         cp_state = c("open", "open", "closed", "open", "open", "open"), 
                         cp_capacity = c(243, 67, 116, 70, 32, 173), 
                         cp_sheltered = c(100, 0, 0, 0, 0, 100),
                         cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451),
                         arrival_rate = 1,
                         month_st = "Jan",
                         month_en = "Jul",
                         distance_matrix = matrix(c(
                           0, 500, 1000, 150, 200, 250,
                           500, 0, 800, 1300, 180, 230,
                           1000, 800, 0, 600, 1100, 160,
                           150, 1300, 600, 0, 500, 1000,
                           200, 180, 1100, 500, 0, 500,
                           250, 230, 160, 1000, 500, 0
                         ), nrow = 6, byrow = TRUE, 
                         dimnames = list(cp_list, cp_list)))

# Access the results from the simulation
df_main <- simulation_results[[1]]
plist_red_util <- simulation_results[[2]]
plist_white_util <- simulation_results[[3]]
plist_red_use <- simulation_results[[4]]
plist_white_use <- simulation_results[[5]]

df_main %>% group_by(resource, mon) %>% 
  summarise(red_mean_parking = mean(red_mean_parking, na.rm = T), 
            white_mean_parking = mean(white_mean_parking, na.rm = T),
            red_max_parking = mean(red_max_parking, na.rm = T), 
            white_max_parking = mean(white_max_parking, na.rm = T),
            mean_sd_red = mean(sd_red), mean_sd_white = mean(sd_white))

grid.arrange(grobs = lapply(plist_red_util, 
                            function(x) x + theme_bw() +
                              scale_x_discrete(labels = cp_list)))
grid.arrange(grobs = lapply(plist_white_util, 
                            function(x) x + theme_bw() +
                              scale_x_discrete(labels = cp_list)))

grid.arrange(grobs = lapply(plist_red_use, 
                            function(x) x + theme_bw() + guides(color = "none")))
grid.arrange(grobs = lapply(plist_white_use, 
                            function(x) x + theme_bw() + guides(color = "none")))

# Visualize all months together:

visualize_all_months <- function(df){
  red_df_viz <- df %>% 
    # Filter end points of simulations (as simulation restarts at every month)
    filter(time > 1000) %>% group_by(mon) %>% arrange(time, .by_group = T)
  mon_breaks <- red_df_viz %>% group_by(mon) %>% 
    slice_max(time) %>% ungroup() %>%
    mutate(lval = lag(time)) %>% replace_na(list(lval = 0)) %>%
    mutate(lval = cumsum(lval)) %>% dplyr::select(mon, lval)
  red_df_viz <- red_df_viz %>% left_join(mon_breaks, by = "mon") %>% mutate(time = time + lval)
  fin_plot <- ggplot(red_df_viz, aes(x = time)) +
    geom_line(aes(y = server), color = "blue") +
    geom_line(aes(y = capacity), color = "green") +
    geom_line(aes(y = queue), color = "red") +
    labs(title = "Resource Usage Over Time",
         x = "Time (minutes)",
         y = "Capacity") +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(resource~., scales = "free_y") 
  for (i in unique(mon_breaks$lval)){
    fin_plot <- fin_plot + geom_vline(xintercept = i, linetype = "dashed")
  }
  fin_plot <- fin_plot + 
    scale_x_continuous(breaks = unique(mon_breaks$lval), labels = month(unique(red_df_viz$mon), label = T)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 0.6)) +
    labs(subtitle = "Blue: System Usage, Red: Queue, Green: Capacity")
  return(fin_plot)
}

visualize_all_months(simulation_results[[6]])
visualize_all_months(simulation_results[[7]])


# SOME POSSIBLE VISUALIZATIONS THAT YOU CAN SHOW!

# Something else can show? Standard deviation of simulations for each carark across months?
car_parks <- unique(df_main$resource)
split_data <- split(df_main, df_main$resource)
for (car_park in car_parks) {
  data_subset <- split_data[[car_park]]
  p <- ggplot(data_subset, aes(x = sim, y = sd_red)) +
    geom_line(aes(color = "Red")) +
    geom_line(aes(x = sim, y = sd_white, color = "White")) +
    labs(title = paste("Standard Deviation of Slots -", car_park), x = "Simulation", y = "Standard Deviation") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, max(data_subset$sim), by = 1)) +
    scale_color_manual(values = c("Red" = "red", "White" = "blue")) +
    guides(color = guide_legend(title = "Server Type"))+
    facet_wrap(~mon)
  # Open a separate plot window for each car park
  print(p)
}

# Create the scatter plot for red STANDRD DEVIATIONS
ggplot(df_means, aes(x = mon, y = mean_sd_red, color = resource)) +
  geom_point() +
  labs(
    title = "Mean Standard Deviation of Red Slots by Car Park",
    x = "Month", y = "Mean Standard Deviation"
  ) +
  theme_minimal() +
  scale_color_discrete(name = "Car Park")

# scatter plot for white STANDARD DEVIATIONS
ggplot(df_means, aes(x = mon, y = mean_sd_white, color = resource)) +
  geom_point() +
  labs(
    title = "Mean Standard Deviation of White Slots by Car Park",
    x = "Month", y = "Mean Standard Deviation"
  ) +
  theme_minimal() +
  scale_color_discrete(name = "Car Park")

#----------------------

cp_index = 2
res <- run_sim_n_times_s(n = 20, 
                         cp_index = cp_index,
                         cp_capacity = c(243, 67, 116, 70, 32, 173)[cp_index], 
                         cp_red_perc = c(12.7572, 20.89552, 18.10345, 24.28571, 100, 75.14451)[cp_index],
                         arrival_rate = 1,
                         month_st = "Jan",
                         month_en = "Apr")

res[[1]] %>% summarise(red_mean_parking = mean(red_mean_parking, na.rm = T), 
                       white_mean_parking = mean(white_mean_parking, na.rm = T),
                       red_max_parking = mean(red_max_parking, na.rm = T), 
                       white_max_parking = mean(white_max_parking, na.rm = T))

grid.arrange(grobs = lapply(res[[2]], 
                            function(x) x + theme_bw() + guides(color = "none")),  ncol = 2, nrow = 2)
grid.arrange(grobs = lapply(res[[3]], 
                            function(x) x + theme_bw() + guides(color = "none")),  ncol = 2, nrow = 2)

visualize_all_months(res[[4]])
visualize_all_months(res[[5]])
