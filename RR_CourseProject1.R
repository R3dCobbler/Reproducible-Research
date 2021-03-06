## Reproducible Research. Course Project 1

## Loading and preprocessing the data

## 1. Load and examine the data

actdata <- read.csv("activity.csv", header = TRUE, sep = ",")
head(actdata)
summary(actdata)
str(actdata)

## 2. Convert date column to class: Date

actdata$date <- as.Date(actdata$date)

## 3. Load packages for data manipulation

## This task will require some manipulation and reshaping of the data in the original data set.
## For this I will use the reshape2 package by Hadley Wickham (http://had.co.nz/reshape/introduction.pdf)

library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)



## Q1. What is mean total number of steps taken per day?

## For this part of the assignment, you can ignore the missing values in the dataset.


## A. Process data using melt and cast function and remove NA values to format the data.
## This enables the possibility to calculate the total number of steps per day.

mdata <- melt(actdata, id.vars = "date", measure.vars = "steps", na.rm = TRUE)

actdata1 <- dcast(mdata, date ~ variable, sum)  # actdata1 is new data frame by date

## B. Make histogram of total steps taken per day

plot(actdata1$date, actdata1$steps,
     type = "h",
     main = "Total Steps per Day", 
     xlab = "Date", 
     ylab = "Number of steps per day",
     lwd = 4,
     col = "green")
abline(h = mean(actdata1$steps, na.rm = TRUE), lwd = 1)

## C. Calculate the mean and median of the total number of steps taken per day

summary(actdata1)

mean_steps <- mean(actdata1$steps)
mean_steps

med_steps <- median(actdata1$steps)
med_steps



## Q2. What is the average daily activity pattern?

## A. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

## Using same process, change date for interval and calculate average

mOrigData <- melt(actdata, id.vars = "interval", measure.vars = "steps", na.rm = TRUE)

actdata2 <- dcast(mOrigData, interval ~ variable, mean)  # actdata2 is a new data frame by interval

plot(actdata2$interval, actdata2$steps,
     type = "l",
     main = "Analysis of steps across the 5 minute intervals", 
     xlab = "Average number of steps taken", 
     ylab = "Average across all days",
     lwd = 2,
     col = "green")
abline(h = mean(actdata2$steps, na.rm = TRUE), lwd = 1)

## B. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_interval <- actdata2$interval[which(actdata2$steps == max(actdata2$steps))]
mean_steps_at_max_interval <- max(actdata2$steps)
max_interval
mean_steps_at_max_interval


## Q3. Imputing missing values

## Note that there are a number of days/intervals where there are missing values.
## The presence of missing days may introduce bias into some calculations or summaries of the data.

## A. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

missing_values <- sum(is.na(actdata))
missing_values

## B. Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## Strategy: Use the mean number of steps per 5 minute interval to capture the best estimate using common behaviour patterns

## C. Create a new dataset that is equal to the original dataset but with the missing data filled in

## Use previous data frame with average per interval to replace NA values

actdata3 <- actdata
dataNA <- is.na(actdata3$steps)

data_int <- tapply(actdata3$steps, actdata3$interval, mean, na.rm = TRUE, simplify = TRUE)
actdata3$steps[dataNA] <- data_int[as.character(actdata3$interval[dataNA])]

head(actdata3)

## Check there are no more NA values
sum(is.na(actdata3))

## D. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

mdata3 <- melt(actdata3, id.vars = "date", measure.vars = "steps", na.rm = FALSE)

steps_per_date <- dcast(mdata3, date ~ variable, sum)

plot(steps_per_date$date, steps_per_date$steps,
     type = "h",
     main = "Complete Total Steps per Day", 
     xlab = "Date", 
     ylab = "Number of steps per day",
     lwd = 4,
     col = "blue")
abline(h = mean(steps_per_date$steps), lwd = 1)

## Calculate the new mean and median of the total number of steps taken per day

summary(steps_per_date)

mean_complete_steps <- mean(steps_per_date$steps)
mean_complete_steps

med_complete_steps <- median(steps_per_date$steps)
med_complete_steps


## Do these values differ from the estimates from the first part of the assignment?

## No

## There is no significant difference in applying the 5 minute interval averages to the NA values.



## Q4. Are there differences in activity patterns between weekdays and weekends?
## For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

## A. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

actdata3 <- actdata3 %>%
        mutate(daytype = ifelse(weekdays(actdata3$date) == "Saturday" |
                                weekdays(actdata3$date) == "Sunday",
                                "Weekend", "Weekday" ))
head(actdata3)

## B. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

ggplot(actdata3, aes(x = interval, y = steps, color = daytype)) +
        geom_line() +
        labs(title = "Average Daily Steps per 5 Minute Interval",
             x = "Interval", y = "Number of Steps") +
        facet_wrap(~daytype, ncol = 1, nrow = 2)

## Codebook

## actdata = original activity data set
## actdata1 = original data set reshaped to show total steps per date. (NA values removed)
## actdata2 = original data set reshaped to show total steps per interval. (NA values removed)
## actdata3 = new activity data set with filled in missing values

























