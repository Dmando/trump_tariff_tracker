library(fredr)
library(readr)
library(lubridate)

# Manufacturing Employee Data

setwd("~/../Documents/Evaluating_Trump_Tariffs/trump_tariff_tracker/")

# Sourcing Manufacturing Data

source("Load_Fredr_Data.R")

Summary_Sheet <- read_csv("Summary_Sheet.csv")
Summary_Sheet[,3] <- NA

# Loading Goal One Data

goal_one_data <- read_csv("Data/goal_one_data.csv")
goal_one_data <- goal_one_data %>% 
  select(date, total_weighted_z) %>% 
  rename(goal_one_value = total_weighted_z)

# Loading Goal Two Data

goal_two_data <- read_csv("Data/DTWEXBGS.csv")
goal_two_data <- goal_two_data %>% 
  select(date, zscore_2yravg) %>% 
  rename(goal_two_value = zscore_2yravg)

# Loading Goal Three Data

goal_three_data <- read_csv("Data/goal_three_data.csv")
goal_three_data_depr <- goal_three_data %>% 
  select(date, No, Stable) %>% 
  rename(goal_three_value_negative = No) %>% 
  rename(goal_three_value_stable = Stable)

# Loading Goal Four Data

goal_four_data <- read_csv("Data/goal_four_data.csv")
goal_four_data <- goal_four_data %>% 
  select(date, total_weighted_z) %>% 
  rename(goal_four_value = total_weighted_z)

# Loading Goal Five Data

goal_five_data <- read_csv("Data/goal_five_data.csv")
goal_five_data <- goal_five_data %>% 
  select(date, total_weighted_z) %>% 
  rename(goal_five_value = total_weighted_z)

# Loading Goal Six Data

goal_six_data <- read_csv("Data/goal_six_data.csv")
goal_six_data <- goal_six_data %>% 
  select(date, zscore_2yravg) %>% 
  rename(goal_six_value = zscore_2yravg)

# Loading Goal Seven Data

goal_seven_data <- read_csv("Data/goal_seven_data.csv")
goal_seven_data <- goal_seven_data  %>% 
  select(date, zscore_2yravg) %>% 
  rename(goal_seven_value = zscore_2yravg)

# Loading Goal Eight Data

goal_eight_data <- read_csv("Data/goal_eight_data.csv")
goal_eight_data <- goal_eight_data  %>% 
  select(date, value) %>% 
  rename(goal_eight_value = value)

# Combining Data

all_goal_data <- goal_three_data_depr %>% 
  left_join(goal_one_data) %>% 
  left_join(goal_two_data) %>% 
  left_join(goal_four_data) %>% 
  left_join(goal_five_data) %>% 
  left_join(goal_six_data) %>% 
  left_join(goal_seven_data) %>% 
  left_join(goal_eight_data) %>% 
  drop_na()

all_goal_data <- all_goal_data %>% 
  mutate(goal_one_value = case_when(
    `goal_one_value` >= 1 ~ 2, # Positive
    `goal_one_value` < -1 ~ 0, # Negative
    `goal_one_value` < 1 ~ 1 # Stable
  )) %>% 
  mutate(goal_two_value = case_when(
    `goal_two_value` >= 1 ~ 2, # Positive
    `goal_two_value` < -1 ~ 0, # Negative
    `goal_two_value` < 1 ~ 1 # Stable
  )) %>% 
  mutate(goal_three_value = case_when(
    goal_three_value_negative + goal_three_value_stable < 50 ~ 2, # Positive
    goal_three_value_negative > 50 ~ 0, # Negative
    goal_three_value_negative + goal_three_value_stable > 50 ~ 1 # Stable
  )) %>%
  mutate(goal_four_value = case_when(
    `goal_four_value` >= 1 ~ 2, # Positive
    `goal_four_value` < -1 ~ 0, # Negative
    `goal_four_value` < 1 ~ 1 # Stable
  )) %>% 
  mutate(goal_five_value = case_when(
    `goal_five_value` >= 1 ~ 2, # Positive
    `goal_five_value` < -1 ~ 0, # Negative
    `goal_five_value` < 1 ~ 1 # Stable
  ))%>% 
  mutate(goal_six_value = case_when(
    `goal_six_value` >= 1 ~ 2, # Positive
    `goal_six_value` < -1 ~ 0, # Negative
    `goal_six_value` < 1 ~ 1 # Stable
  ))%>% 
  mutate(goal_seven_value = case_when(
    `goal_seven_value` >= 1 ~ 2, # Positive
    `goal_seven_value` < -1 ~ 0, # Negative
    `goal_seven_value` < 1 ~ 1 # Stable
  )) %>% 
  mutate(goal_eight_value = case_when(
    `goal_eight_value` >= 50 ~ 2, # Positive
    `goal_eight_value` < 10 ~ 0, # Negative
    `goal_eight_value` < 50 ~ 1 # Stable
  )) %>% 
  select(-goal_three_value_negative, -goal_three_value_stable) %>% 
  mutate(goal_score = (goal_one_value + goal_two_value + goal_three_value + goal_four_value + goal_five_value + goal_six_value + goal_seven_value + goal_eight_value)/16)

write_csv(all_goal_data, "./all_goal_data.csv")

# Summary Sheet Calculations

if (goal_one_data[nrow(goal_one_data), ncol(goal_one_data)] >= 1) {
  Summary_Sheet[1, 3] <- "Positive"
} else if (goal_one_data[nrow(goal_one_data), ncol(goal_one_data)] <= -1) {
  Summary_Sheet[1, 3] <- "Negative"
} else {
  Summary_Sheet[1, 3] <- "Stable"
}

if (goal_two_data[nrow(goal_two_data), ncol(goal_two_data)] >= 1) {
  Summary_Sheet[2, 3] <- "Positive"
} else if (goal_two_data[nrow(goal_two_data), ncol(goal_two_data)] <= -1) {
  Summary_Sheet[2, 3] <- "Negative"
} else {
  Summary_Sheet[2, 3] <- "Stable"
}

if (goal_three_data[nrow(goal_three_data), "No"] > 50) {
  Summary_Sheet[3, 3] <- "Negative"
} else if (goal_three_data[nrow(goal_three_data), "Stable"] >= 50) {
  Summary_Sheet[3, 3] <- "Stable"
} else if (goal_three_data[nrow(goal_three_data), "Full"] >= 50) {
  Summary_Sheet[3, 3] <- "Positive"
}

if (goal_four_data[nrow(goal_four_data), ncol(goal_four_data)] >= 1) {
  Summary_Sheet[4, 3] <- "Positive"
} else if (goal_four_data[nrow(goal_four_data), ncol(goal_four_data)] <= -1) {
  Summary_Sheet[4, 3] <- "Negative"
} else {
  Summary_Sheet[4, 3] <- "Stable"
}

if (goal_five_data[nrow(goal_five_data), ncol(goal_five_data)] >= 1) {
  Summary_Sheet[5, 3] <- "Positive"
} else if (goal_four_data[nrow(goal_five_data), ncol(goal_five_data)] <= -1) {
  Summary_Sheet[5, 3] <- "Negative"
} else {
  Summary_Sheet[5, 3] <- "Stable"
}

if (goal_six_data[nrow(goal_six_data), ncol(goal_six_data)] >= 1) {
  Summary_Sheet[6, 3] <- "Positive"
} else if (goal_six_data[nrow(goal_six_data), ncol(goal_six_data)] <= -1) {
  Summary_Sheet[6, 3] <- "Negative"
} else {
  Summary_Sheet[6, 3] <- "Stable"
}

if (goal_seven_data[nrow(goal_seven_data), ncol(goal_seven_data)] >= 1) {
  Summary_Sheet[7, 3] <- "Positive"
} else if (goal_seven_data[nrow(goal_seven_data), ncol(goal_seven_data)] <= -1) {
  Summary_Sheet[7, 3] <- "Negative"
} else {
  Summary_Sheet[7, 3] <- "Stable"
}

if (goal_eight_data[nrow(goal_eight_data), ncol(goal_eight_data)] >= 50) {
  Summary_Sheet[8, 3] <- "Positive"
} else if (goal_eight_data[nrow(goal_eight_data), ncol(goal_eight_data)] < 10) {
  Summary_Sheet[8, 3] <- "Negative"
} else {
  Summary_Sheet[8, 3] <- "Stable"
}


write_csv(Summary_Sheet, "./Summary_Sheet.csv")
