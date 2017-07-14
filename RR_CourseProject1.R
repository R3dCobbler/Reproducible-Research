## Reproducible Research. Course Project 1

## Loading and preprocessing the data

## 1. Load and examine the data

data <- read.csv("activity.csv", header = TRUE, sep = ",")
head(data)
summary(data)
str(data)

## 2. Convert date column to class: Date

data$date <- as.Date(data$date)

## 3. Load packages for data manipulation
library(lubridate)
library(dplyr)
library(reshape2)


## Q1. What is mean total number of steps taken per day?

## For this part of the assignment, you can ignore the missing values in the dataset.


## A. Process data using melt and cast function and remove NA values to format the data.
## This enables the possibility to calculate the total number of steps per day.

mDateSteps <- melt(data, id.vars = "date", measure.vars = "steps", na.rm = TRUE)

cDateSteps <- dcast(mDateSteps, date ~ variable, sum)

## B. Make histogram of total steps taken per day

plot(cDateSteps$date, cDateSteps$steps,
     type = "h",
     main = "Total Steps per Day", 
     xlab = "Date", 
     ylab = "Number of steps per day",
     lwd = 4,
     col = "green")
abline(h = mean(cDateSteps$steps, na.rm = TRUE), lwd = 1)

## C. Calculate the mean and median of the total number of steps taken per day

summary(cDateSteps)

mean_steps <- mean(cDateSteps$steps)
mean_steps

med_steps <- median(cDateSteps$steps)
med_steps



## Q2. What is the average daily activity pattern?

## A. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

## Using same process, change date for interval and calculate average

mStepInt <- melt(data, id.vars = "interval", measure.vars = "steps", na.rm = TRUE)

cStepInt <- dcast(mStepInt, interval ~ variable, mean)

plot(cStepInt$interval, cStepInt$steps,
     type = "l",
     main = "Analysis of steps across the 5 minute intervals", 
     xlab = "Average number of steps taken", 
     ylab = "Average across all days",
     lwd = 2,
     col = "green")
abline(h = mean(cStepInt$steps, na.rm = TRUE), lwd = 1)

## B. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_interval <- cStepInt$interval[which(cStepInt$steps == max(cStepInt$steps))]
mean_steps_at_max_interval <- max(cStepInt$steps)
max_interval
mean_steps_at_max_interval


## Q3. Imputing missing values

## Note that there are a number of days/intervals where there are missing values.
## The presence of missing days may introduce bias into some calculations or summaries of the data.

## A. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

missing_values <- sum(is.na(data))
missing_values

## B. Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## Strategy: Use the mean number of steps per 5 minute interval to capture the best estimate using common behaviour patterns

## C. Create a new dataset that is equal to the original dataset but with the missing data filled in

## Merge original data set with cStepInt data set

data2 <- merge(data, cStepInt, by = "interval", sort = TRUE, suffixes = c(".data1", ".data2"))

## Identify NA values

navalues = which(is.na(data$steps))

## Replace NA values

data[navalues,"steps"] = data2[navalues, "steps.data2"]

## D. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

mCompleteData <- melt(data, id.vars = "date", measure.vars = "steps", na.rm = FALSE)

cCompleteData <- dcast(mCompleteData, date ~ variable, sum)

plot(cCompleteData$date, cCompleteData$steps,
     type = "h",
     main = "Complete Total Steps per Day", 
     xlab = "Date", 
     ylab = "Number of steps per day",
     lwd = 4,
     col = "blue")
abline(h = mean(cCompleteData$steps), lwd = 1)

## Calculate the new mean and median of the total number of steps taken per day

summary(cCompleteData)

mean_complete_steps <- mean(cCompleteData$steps)
mean_complete_steps

med_complete_steps <- median(cCompleteData$steps)
med_complete_steps


## Do these values differ from the estimates from the first part of the assignment?

## Yes

diffmean <- mean_complete_steps - mean_steps
diffmean

diffmed < med_complete_steps - med_steps
diffmed

















