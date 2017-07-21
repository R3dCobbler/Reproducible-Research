## Reproducible Research: Course Project 2
## Mick Sheahan
## July 2017

## DATA PROCESSING

## Load necessary library to open the bzip2 file
library(R.utils)

## Load and examine data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "Storm.bz2")
bunzip2("Storm.bz2", destname = "StormData.csv")

stormdata <- read.csv("StormData.csv", header = TRUE)

## Run basic functions to understand features and detemine the types of cleaning and transformation needed.

str(stormdata)
summary(stormdata)
head(stormdata)

## Strategy for Large Data Size
# Reduce the data frame to focus on the questions at hand.
# This would include event types, locations, and types of harm (fatalities, injuries) variables in Q1
# For Q2, we need to see variables also connected with economic factors (crop and property damage)

library(dplyr)

stormdata1 <- stormdata %>%
        select(STATE, COUNTY, COUNTYNAME, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
       
summary(stormdata1)  # Reduced data frame with relevant variables  
str(stormdata1)

# Check missing values
sum(is.na(stormdata1))

# Examine levels of the damage variables
levels(stormdata1$PROPDMGEXP)
levels(stormdata1$CROPDMGEXP)


## Strategy for Poor Data Quality
# To determine the monetary loss, the damage column ("CROPDMG" or "PROPDMG") should be multiplied by the exponential value in the corresponding "...DMGEXP" column.
# Estimates should be rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, 
# i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude include "h" for hundreds, “K” for thousands, “M” for millions, and “B” for billions.
# However, several entries contain other characters or numbers in the EXP columns.
# As the entries should contain a letter abbreviation to signify the amount, I will ignore all values in the PROPDMGEXP and CROPDMGEXP that do not contain them.


## Convert EXP columns to characters
stormdata1$PROPDMGEXP <- as.character(stormdata1$PROPDMGEXP)
stormdata1$CROPDMGEXP <- as.character(stormdata1$CROPDMGEXP)

str(stormdata1)

stormdata1[stormdata1$PROPDMGEXP %in% c("", "+", "-", "?"), "PROPDMGEXP"] <- "0"
stormdata1[stormdata1$PROPDMGEXP %in% c(=> "0" & <= "8"), "PROPDMGEXP"] <- "0"
stormdata1[stormdata1$CROPDMGEXP %in% c("", "+", "-", "?"), "CROPDMGEXP"] <- "0"
stormdata1[stormdata1$CROPDMGEXP %in% c(=> "0" & <= "8"), "CROPDMGEXP"] <- "0"



validEXP <- c("B", "h", "H", "K", "m", "M")

stormdataEXP <- stormdata1[stormdata1$PROPDMGEXP | stormdata1$CROPDMGEXP %in% validEXP, ]


stormdata2 <- stormdata1 %>%
        filter(stormdata1$PROPDMGEXP == stormdataP | stormdata1$CROPDMGEXP == stormdataC)



# Prepare data for plotting

top10fatal <- aggregate(stormdata1$FATALITIES, by = list(EVTYPE = stormdata1$EVTYPE), sum)
fatality <- fatality[order(fatality$x, decreasing = TRUE), ]

injury <- aggregate(stormdata1$INJURIES, by = list(EVTYPE = stormdata1$EVTYPE), sum)
injury <- injury[order(injury$x, decreasing = TRUE), ]

## Question 1. Across the US, which types of events are most harmful with respect to population health?

library(ggplot2)

ggplot(fatality, aes(x = EVTYPE, y = x) +
        geom_bar() +
        labs(title = "Number of fatalities caused by severe weather in the US since 1950",
             x = "Weather Event", y = "Number of fatalities") 
       





