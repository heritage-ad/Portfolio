# Load required libraries
library(tidyverse)

# Load and preview the daily activity dataset
daily_activity <- read_csv("dailyActivity_merged.csv")
head(daily_activity)

# Convert ActivityDate to Date format
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Calculate average daily steps
average_daily_steps <- mean(daily_activity$TotalSteps, na.rm = TRUE)
print(average_daily_steps)

# Plot daily step trends
ggplot(daily_activity, aes(x = ActivityDate, y = TotalSteps)) +
  geom_line(color = "blue") +
  labs(title = "Daily Steps Trend", x = "Date", y = "Total Steps") +
  theme_minimal()

# Load and process hourly steps dataset
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_steps <- hourly_steps %>%
  mutate(Hour = format(as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"), "%H")) %>%
  group_by(Hour) %>%
  summarize(AverageSteps = mean(StepTotal, na.rm = TRUE)) %>%
  arrange(desc(AverageSteps))

# Plot average hourly steps
ggplot(hourly_steps, aes(x = Hour, y = AverageSteps)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Average Steps by Hour of Day", x = "Hour of Day", y = "Average Steps") +
  theme_minimal()
