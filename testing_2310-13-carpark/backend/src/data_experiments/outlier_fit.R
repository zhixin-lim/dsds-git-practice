library(tidyverse)
library(lubridate)
library(splines)
library(ggplot2)

# Get the number of car arrivals for Red Slots at CP5
df5r <- read.csv("../data/df_5.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

# Identifying outliers using Cook's Distance
cd <- cooks.distance(lm(df5r$count ~ bs(df5r$week, degree = 4)))
# Removing outliers
ip5r <- which(cd > 1)
new_df5r <- df5r[-ip5r,]

# Fit spline to model arrival rates for Red slots at CP5
ggplot(new_df5r, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 4),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP5")


# Get the number of car arrivals for White Slots at CP5
df5w <- read.csv("../data/df_5.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>% 
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>%
  summarize(count = n()) 

# Identifying and removing outliers
cd2 <- cooks.distance(lm(df5w$count ~ bs(df5w$week, degree = 4)))
ip5w <- which(cd2 > 1)

#Fit degree-5 spline to model arrival rates for White slots at CP5
ggplot(df5w, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 4),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for White Slots for CP5")


# Carpark 5b Red Slots
df5br <- read.csv("../data/df_5b.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd3 <- cooks.distance(lm(df5br$count ~ bs(df5br$week, degree = 4)))
ip5br <- which(cd3 > 1)
new_df5br <- df5br[-ip5br,]

ggplot(new_df5br, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 4),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP5b")

# Carpark 5b White slots
df5bw <- read.csv("../data/df_5b.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd4 <- cooks.distance(lm(df5bw$count ~ bs(df5bw$week, degree = 4)))
ip5bw <- which(cd4 > 1)
new_df5bw <- df5bw[-ip5bw,]

ggplot(new_df5bw, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 4),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for White Slots for CP5b")

# Carpark 3 Red slots
df3r <- read.csv("../data/df_3.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd5 <- cooks.distance(lm(df3r$count ~ bs(df3r$week, degree = 5)))
ip3r <- which(cd5 > 1)
new_df3r <- df3r[-ip3r,]

ggplot(new_df3r, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP3")

# Carpark 3 White slots
df3w <- read.csv("../data/df_3.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd6 <- cooks.distance(lm(df3w$count ~ bs(df3w$week, degree = 5)))
ip3w <- which(cd6 > 1)

ggplot(df3w, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for White Slots for CP3")



# Carpark 3a Red Slots
df3ar <- read.csv("../data/df_3a.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd7 <- cooks.distance(lm(df3ar$count ~ bs(df3ar$week, degree = 4)))
ip3ar <- which(cd7 > 1)
new_df3ar <- df3ar[-ip3ar,]

ggplot(new_df3ar, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 4),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP3a")

# Carpark 3a White slots
df3aw <- read.csv("../data/df_3a.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd8 <- cooks.distance(lm(df3aw$count ~ bs(df3aw$week, degree = 5)))
ip3aw <- which(cd8 > 1)
new_df3aw <- df3aw[-ip3aw,]

ggplot(new_df3aw, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP3")



# Carpark 4 Red slots
df4r <- read.csv("../data/df_4.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 


cd9 <- cooks.distance(lm(df4r$count ~ bs(df4r$week, degree = 5)))
ip4r <- which(cd9 > 1)
new_df4r <- df4r[-ip4r,]

ggplot(new_df4r, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP4")

# Carpark 4 White slots
df4w <- read.csv("../data/df_4.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd10 <- cooks.distance(lm(df4w$count ~ bs(df4w$week, degree = 5)))
ip4w <- which(cd10 > 1)
new_df4w <- df4w[-ip4w,]

ggplot(new_df4w, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for White Slots for CP4")



# Carpark 6b Red slots
df6r <- read.csv("../data/df_6b.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "red") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd11 <- cooks.distance(lm(df6r$count ~ bs(df6r$week, degree = 5)))
ip6r <- which(cd11 > 1)
new_df6r <- df6r[-ip6r,]

ggplot(new_df6r, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 5),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for Red Slots for CP6b")

# Carpark 6b White slots
df6w <- read.csv("../data/df_6b.csv") %>%
  filter(du_val > 10, 
         str_length(enter_time) == 19, 
         str_length(exit_time) == 19,
         slot == "white") %>%
  mutate(enter_time = as.POSIXct(enter_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, format = "%Y-%m-%d %H:%M:%S", tz = "Singapore")) %>%
  arrange(enter_time) %>%
  group_by(week = floor_date(enter_time, unit = "week")) %>% 
  summarize(count = n()) 

cd12 <- cooks.distance(lm(df6w$count ~ bs(df6w$week, degree = 6)))
ip6w <- which(cd12 > 1)
new_df6w <- df6w[-ip6w,]

ggplot(new_df6w, aes(x = week, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 6),se = TRUE) +
  labs(x = "Weekly Arrival Rate", y = "Number of Arrivals") +
  ggtitle("Weekly Arrival Rate for White Slots for CP6b")




# Analysis of car arrivals for white slots, based on holiday and school period 
# Carpark 3
df_3 <- read.csv("../data/df_3.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat)) 

df_3$iat[1] <- 0

# Carpark 3 White lots
white3 <- df_3 %>% filter(slot == "white")

# Carpark 3 White lots usage during holiday period
white3h <- white3 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white3h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3 White lots usage during holiday period")

# Carpark 3 White lots usage during school term
white3s <- white3 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white3s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3 White lots usage during school period")



# Carpark 3a
df_3a <- read.csv("../data/df_3a.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat))

df_3a$iat[1] <- 0

# Carpark 3a White lots
white3a <- df_3a %>% filter(slot == "white")

# Carpark 3 White lots usage during holiday period
white3ah <- white3a %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white3ah, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3a White lots usage during holiday period")

# Carpark 3a White lots usage during school term
white3as <- white3a %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white3as, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3a White lots usage during school period")



# Carpark 4
df_4 <- read.csv("../data/df_4.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat)) 

df_4$iat[1] <- 0

# Carpark 4 White lots
white4 <- df_4 %>% filter(slot == "white")

# Carpark 4 White lots usage during holiday period
white4h <- white4 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white4h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 4 White lots usage during holiday period")

# Carpark 4 White lots usage during school term
white4s <- white4 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white4s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 4 White lots usage during school period")



# Carpark 5
df_5 <- read.csv("../data/df_5.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat)) 

df_5$iat[1] <- 0

# Carpark 5 White lots
white5 <- df_5 %>% filter(slot == "white")

# Carpark 5 White lots usage during holiday period
white5h <- white5 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white5h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5 White lots usage during holiday period")

# Carpark 5 White lots usage during school term
white5s <- white5 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white5s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5 White lots usage during school period")



# Carpark 5b
df_5b <- read.csv("../data/df_5b.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat)) 

df_5b$iat[1] <- 0

# Carpark 5b White lots
white5b <- df_5b %>% filter(slot == "white")

# Carpark 5b White lots usage during holiday period
white5bh <- white5b %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white5bh, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5b White lots usage during holiday period")

# Carpark 5b White lots usage during school term
white5bs <- white5b %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white5bs, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5b White lots usage during school period")



# Carpark 6b
df_6b <- read.csv("../data/df_6b.csv") %>%
  filter(du_val > 10,
         str_length(enter_time) == 19,
         str_length(exit_time) == 19) %>% 
  mutate(enter_time = as.POSIXct(enter_time, tz = "Singapore"),
         exit_time = as.POSIXct(exit_time, tz = "Singapore"),
         day = as.Date(enter_time, tz = "Singapore")) %>%
  arrange(enter_time) %>%
  mutate(iat = as.numeric(difftime(enter_time, lag(enter_time), units = "mins"))) %>%
  mutate(wkd = weekdays(enter_time), 
         isweekend = wkd %in% c("Saturday", "Sunday"),
         hols = month(enter_time) %in% c(5,6,7,12)) %>%
  filter(!is.na(iat)) %>%
  filter(iat < quantile(iat, 0.75) + 1.5*IQR(iat),
         iat > quantile(iat, 0.25) - 1.5*IQR(iat)) 

df_6b$iat[1] <- 0

# Carpark 6b White lots
white6b <- df_6b %>% filter(slot == "white")

# Carpark 6b White lots usage during holiday period
white6bh <- white6b %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(white6bh, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 6b White lots usage during holiday period")

# Carpark 6b White lots usage during school term
white6bs <- white6b %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(white6bs, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 6b White lots usage during school period")



# Analysis of car arrivals for red slots, based on holiday and school period 
# Carpark 3 Red lots
red3 <- df_3 %>% filter(slot == "red")

# Carpark 3 Red lots usage during holiday period
red3h <- red3 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red3h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3 Red lots usage during holiday period")

# Carpark 3 Red lots usage during school term
red3s <- red3 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red3s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3 Red lots usage during school period")


# Carpark 3a Red lots
red3a <- df_3a %>% filter(slot == "red")

# Carpark 3 Red lots usage during holiday period
red3ah <- red3a %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red3ah, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3a Red lots usage during holiday period")

# Carpark 3a Red lots usage during school term
red3as <- red3a %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red3as, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 3a Red lots usage during school period")


# Carpark 4 Red lots
red4 <- df_4 %>% filter(slot == "red")

# Carpark 4 Red lots usage during holiday period
red4h <- red4 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red4h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 4 Red lots usage during holiday period")

# Carpark 4 Red lots usage during school term
red4s <- red4 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red4s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 4 Red lots usage during school period")


# Carpark 5 Red lots
red5 <- df_5 %>% filter(slot == "red")

# Carpark 5 Red lots usage during holiday period
red5h <- red5 %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red5h, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5 Red lots usage during holiday period")

# Carpark 5 Red lots usage during school term
red5s <- red5 %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red5s, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5 Red lots usage during school period")


# Carpark 5b Red lots
red5b <- df_5b %>% filter(slot == "red")

# Carpark 5b Red lots usage during holiday period
red5bh <- red5b %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red5bh, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5b Red lots usage during holiday period")

# Carpark 5b Red lots usage during school term
red5bs <- red5b %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red5bs, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 5b Red lots usage during school period")



# Carpark 6b Red lots
red6b <- df_6b %>% filter(slot == "red")

# Carpark 6b Red lots usage during holiday period
red6bh <- red6b %>%
  filter(hols == TRUE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n())

ggplot(red6bh, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) + 
  geom_smooth(method = "lm") +
  ggtitle("Carpark 6b Red lots usage during holiday period")

# Carpark 6b Red lots usage during school term
red6bs <- red6b %>% filter(hols == FALSE) %>%
  group_by(day, wkd) %>%
  summarize(total_cars = n()) 

ggplot(red6bs, aes(x = day, y = total_cars)) +
  geom_point() +
  facet_wrap(~wkd) +
  geom_smooth(method = "lm") +
  ggtitle("Carpark 6b Red lots usage during school period")