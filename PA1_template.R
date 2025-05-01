#Activity
# Load required libraries
library(dplyr)
library(ggplot2)
library(lattice)

# 1. Load the data
data_frame <- read.csv("C:/Users/USUARIO/Documents/repdata_data_activity/Activity.csv")
data_frame$date <- as.Date(data_frame$date)  # Convert 'date' to Date format

# 2. Initial summary and checking for missing values
str(data_frame)
missing_values <- sum(is.na(data_frame$steps))
print(paste("Number of missing values:", missing_values))

# 3. Calculate the average steps per interval
mean_steps_interval <- aggregate(steps ~ interval, data = data_frame, mean, na.rm = TRUE)

# 4. Impute missing values using the average per interval
data_frame_imputed <- merge(data_frame, mean_steps_interval, by = "interval", all.x = TRUE)
data_frame_imputed$steps <- ifelse(is.na(data_frame_imputed$steps.x), 
                                   data_frame_imputed$steps.y, 
                                   data_frame_imputed$steps.x)
data_frame_imputed <- data_frame_imputed[, c("steps", "date", "interval")]

# Verify there are no NA values remaining
sum(is.na(data_frame_imputed$steps))

# 5. Calculate the mean and median number of daily steps
steps_per_day_imputed <- aggregate(steps ~ date, data = data_frame_imputed, sum)
mean_steps_imputed <- mean(steps_per_day_imputed$steps)
median_steps_imputed <- median(steps_per_day_imputed$steps)
print(paste("Mean daily steps (imputed):", mean_steps_imputed))
print(paste("Median daily steps (imputed):", median_steps_imputed))

# 6. Create a histogram of daily imputed steps
hist(steps_per_day_imputed$steps, 
     main = "Histogram: Total Steps Per Day (Imputed)", 
     xlab = "Total Steps", 
     col = "green", 
     breaks = 10)

# 7. Plot the average daily activity pattern
mean_steps_interval <- aggregate(steps ~ interval, data = data_frame_imputed, mean)
plot(mean_steps_interval$interval, mean_steps_interval$steps, 
     type = "l", 
     main = "Average Daily Activity Pattern", 
     xlab = "Interval (5 Minutes)", 
     ylab = "Average Steps", 
     col = "blue")

# 8. Find the interval with the highest average steps
max_interval <- mean_steps_interval[which.max(mean_steps_interval$steps), ]
print(paste("Interval with the highest average steps:", max_interval$interval))

# 9. Analyze patterns between weekdays and weekends
data_frame_imputed$day_type <- ifelse(weekdays(data_frame_imputed$date) %in% c("sábado", "domingo"), 
                                      "weekend", 
                                      "weekday")
# Calculate average steps per interval and day type
steps_day_type <- aggregate(steps ~ interval + day_type, data = data_frame_imputed, mean)
# Plot patterns for weekdays vs weekends

xyplot(steps ~ interval | day_type, data = steps_day_type, 
       type = "l", layout = c(1, 2), 
       main = "Activity Patterns: Weekdays vs Weekends", 
       xlab = "Interval (5 Minutes)", 
       ylab = "Average Steps")