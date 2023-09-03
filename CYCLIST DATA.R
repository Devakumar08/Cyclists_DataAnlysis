# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the first 200 rows of the dataset
data <- read.csv("C:/Users/Dev/Downloads/202005-divvy-tripdata/cycle.csv", stringsAsFactors = FALSE, nrows = 200)

# Convert date-time columns to POSIXct
data$started_at <- as.POSIXct(data$started_at, format = "%Y-%m-%d %H:%M:%S")
data$ended_at <- as.POSIXct(data$ended_at, format = "%Y-%m-%d %H:%M:%S")

# Calculate ride duration in minutes
data$duration_minutes <- as.numeric(difftime(data$ended_at, data$started_at, units = "mins"))

# Summary statistics
summary(data)

# Count of member vs. casual rides
ride_counts <- data %>%
  group_by(member_casual) %>%
  summarise(count = n())

# Display ride counts by user type
print(ride_counts)

# Data Visualization
ggplot(data, aes(x = member_casual, y = duration_minutes, fill = member_casual)) +
  geom_boxplot() +
  labs(
    title = "Ride Duration by User Type (First 200 Rows)",
    x = "User Type",
    y = "Duration (minutes)",
    fill = "User Type"
  ) +
  theme_minimal()

# Histogram of ride durations by user type
ggplot(data, aes(x = duration_minutes, fill = member_casual)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~member_casual, scales = "free") +
  labs(
    title = "Distribution of Ride Durations by User Type",
    x = "Duration (minutes)",
    y = "Frequency",
    fill = "User Type"
  ) +
  theme_minimal()

# Density plot of ride durations by user type
ggplot(data, aes(x = duration_minutes, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Ride Durations by User Type",
    x = "Duration (minutes)",
    fill = "User Type"
  ) +
  theme_minimal()

# Bar chart of user type counts
user_type_counts <- data %>%
  group_by(member_casual) %>%
  summarise(count = n())

ggplot(user_type_counts, aes(x = member_casual, y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(
    title = "User Type Counts",
    x = "User Type",
    y = "Count",
    fill = "User Type"
  ) +
  theme_minimal()

# Pie chart of user type distribution
library(ggplot2)
library(dplyr)

user_type_distribution <- data %>%
  group_by(member_casual) %>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count)) * 100)

ggplot(user_type_distribution, aes(x = "", y = percent, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "User Type Distribution",
    x = NULL,
    y = NULL,
    fill = "User Type"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

