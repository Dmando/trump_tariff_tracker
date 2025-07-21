library(fredr)
library(readr)
library(lubridate)

# Manufacturing Employee Data

setwd("~/../Documents/Evaluating_Trump_Tariffs/trump_tariff_tracker/")

# Sourcing Manufacturing Data

source("Load_Fredr_Data.R")

Summary_Sheet <- read_csv("Summary_Sheet.csv")
Summary_Sheet[,3] <- NA

# Goal One

goal_one_data <- read_csv("Data/goal_one_data.csv")
goal_one_data <- goal_one_data %>% 
  drop_na() %>% 
  arrange(date)

if (goal_one_data[nrow(goal_one_data), ncol(goal_one_data)] >= 1) {
  Summary_Sheet[1, 3] <- "Positive"
} else if (goal_one_data[nrow(goal_one_data), ncol(goal_one_data)] <= -1) {
  Summary_Sheet[1, 3] <- "Negative"
} else {
  Summary_Sheet[1, 3] <- "Stable"
}

# Goal Two

goal_two_data <- read_csv("Data/DTWEXBGS.csv")
goal_two_data <- goal_two_data %>% 
  drop_na() %>% 
  arrange(date)

if (goal_two_data[nrow(goal_two_data), ncol(goal_two_data)] >= 1) {
  Summary_Sheet[2, 3] <- "Positive"
} else if (goal_two_data[nrow(goal_two_data), ncol(goal_two_data)] <= -1) {
  Summary_Sheet[2, 3] <- "Negative"
} else {
  Summary_Sheet[2, 3] <- "Stable"
}

# Goal Three

goal_three_data <- read_csv("Data/TTT_Countries.csv")
goal_three_data <- goal_three_data %>% 
  group_by(Adherence) %>% 
  summarise(Percentage = sum(`GDP Percentage`))

if (goal_three_data$Percentage[goal_three_data$Adherence == "No"] > 50) {
  Summary_Sheet[3, 3] <- "Negative"
} else if (goal_three_data$Percentage[goal_three_data$Adherence == "Partial"] + goal_three_data$Percentage[goal_three_data$Adherence == "Full"] >= 50) {
  Summary_Sheet[3, 3] <- "Stable"
} else if (goal_three_data$Percentage[goal_three_data$Adherence == "Full"] >= 50) {
  Summary_Sheet[3, 3] <- "Positive"
}

# Goal Four

goal_four_data <- read_csv("Data/goal_four_data.csv")
goal_four_data <- goal_four_data %>% 
  drop_na() %>% 
  arrange(date)

if (goal_four_data[nrow(goal_four_data), ncol(goal_four_data)] >= 1) {
  Summary_Sheet[4, 3] <- "Positive"
} else if (goal_four_data[nrow(goal_four_data), ncol(goal_four_data)] <= -1) {
  Summary_Sheet[4, 3] <- "Negative"
} else {
  Summary_Sheet[4, 3] <- "Stable"
}

# Goal Six

goal_six_data <- read_csv("Data/goal_six_data.csv")
goal_six_data <- goal_six_data %>% 
  drop_na() %>% 
  arrange(date)

if (goal_six_data[nrow(goal_six_data), ncol(goal_six_data)] >= 1) {
  Summary_Sheet[6, 3] <- "Positive"
} else if (goal_six_data[nrow(goal_six_data), ncol(goal_six_data)] <= -1) {
  Summary_Sheet[6, 3] <- "Negative"
} else {
  Summary_Sheet[6, 3] <- "Stable"
}

# Goal Seven

goal_seven_data <- read_csv("Data/goal_seven_data.csv")
goal_seven_data <- goal_seven_data %>% 
  drop_na() %>% 
  arrange(date)

if (goal_seven_data[nrow(goal_seven_data), ncol(goal_seven_data)] >= 1) {
  Summary_Sheet[7, 3] <- "Positive"
} else if (goal_seven_data[nrow(goal_seven_data), ncol(goal_seven_data)] <= -1) {
  Summary_Sheet[7, 3] <- "Negative"
} else {
  Summary_Sheet[7, 3] <- "Stable"
}

# Goal Eight

goal_eight_data <- read_csv("Data/goal_eight_data.csv")
goal_eight_data <- goal_eight_data %>% 
  drop_na() %>% 
  arrange(record_date)

if (goal_eight_data[nrow(goal_eight_data), ncol(goal_eight_data)] >= 50) {
  Summary_Sheet[8, 3] <- "Positive"
} else if (goal_eight_data[nrow(goal_eight_data), ncol(goal_eight_data)] < 10) {
  Summary_Sheet[8, 3] <- "Negative"
} else {
  Summary_Sheet[8, 3] <- "Stable"
}

write_csv(Summary_Sheet, "./Summary_Sheet.csv")
