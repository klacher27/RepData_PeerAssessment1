library(dplyr)
library(ggplot2)

## Read the data
df <- read.csv("activity.csv")

## Get min and max date range of 24 hour periods 
minDate <- as.character(min(as.Date(df$date)))
maxDate <- as.character(max(as.Date(df$date)))

## MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY
## Calculate total steps per day (ignores NA values)
dfSum <- aggregate(x=df$steps, by=list(df$date), FUN=sum)
## Rename aggregate data frame columns
dfSum <- rename(dfSum, date=Group.1, steps=x)

## Part 1 - Make a histogram of the total number of steps taken each day
## Histogram of total steps taken each day
## Construct the plot
g <- ggplot(dfSum, aes(steps))

## Histogram of total steps frequency
g <- g + geom_histogram(binwidth=1000, colour = "blue", fill = "black")

## Add main label
g <- g + ggtitle("Frequency of Total Steps Taken in a Day")

## Add x and y axis labels
g <- g + xlab("Total Steps Taken in a Day (1000 step increments)")
g <- g + ylab("Frequency (count of occurrences")

## Display histogram
print(g)

## Part 2 - Calculate mean and median of total number of steps taken per day
meanSteps <- mean(dfSum$steps, na.rm=TRUE)
medianSteps <- median(dfSum$steps, na.rm=TRUE)
sprintf("Mean total steps per day: %.1f", meanSteps)
sprintf("Median total steps per day: %.1f", medianSteps)

## AVERAGE DAILY ACTIVITY PATTERN
dfADAP <- aggregate(x=df$steps, by=list(as.factor(df$interval)), FUN=mean, na.rm=TRUE, na.action=omit)
dfADAP <- rename(dfADAP, interval=Group.1, avgSteps=x)
dfADAP <- mutate(dfADAP, hour=paste('0000', as.character(dfADAP$interval), sep=""))
dfADAP$hour <- substr(dfADAP$hour, nchar(dfADAP$hour)-3, nchar(dfADAP$hour))
dfADAP$hour <- as.numeric(substr(dfADAP$hour, 1, 2)) + as.numeric(substr(dfADAP$hour, 3, 4)) / 60.0

## Part 1 - Line Plot
## Construct the plot
g <- ggplot(dfADAP, aes(hour, avgSteps))

## Line plot
g <- g + geom_line(aes(group=1), colour="blue", size=1)

g <- g + scale_x_continuous(breaks = seq(0, 24, by=1))

## Add vertical intercept for interval with peak activity
maxSteps <- filter(dfADAP, avgSteps == max(avgSteps) )
g <- g + geom_vline(xintercept = maxSteps$hour, colour="red")

## Add main title
## Add main label
maintitle1 <- "Average Number of Steps measured at 5 minute Intervals in 24 hour periods"
maintitle2 <- paste("between",minDate,"and",maxDate)
title <- paste(maintitle1,maintitle2)
g <- g + ggtitle(title)

## Add x and y axis labels
g <- g + xlab("Hour in Day (plotted at 5 minute intervals)")
g <- g + ylab("Average Number of Steps")

## Display plot
print(g)

## Part 2 -  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
sprintf("5-minute interval with max number of steps (averaged over all days): %s", maxSteps$interval)

## Imputing missing values
## Part 1 -  Calculate and report the total number of missing values in the dataset 
##           (i.e. the total number of rows with 홽홰s)
totalNAs <- sum(is.na(df$steps))
sprintf("Total number of NA values in dataset: %.0f", totalNAs)

## Part 2 - Devise a strategy for ﬁlling in all of the missing values in the dataset. 
##          The strategy does not need to be sophisticated. For example, you
##          could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Part 3 - Create a new dataset that is equal to the original dataset but with the missing data ﬁlled in.
## 
## Copy original dataset
cdf <- df
## Add a column with interval as a factor
cdf <- mutate(cdf, finterval=as.factor(interval))
## Fill in missing value with mean average steps from corresponding 5 minute interval
cdf$steps[is.na(cdf$steps)] <- dfADAP$avgSteps[match(cdf$finterval[is.na(cdf$steps)],dfADAP$interval)]

## Part 4 - Now plot a new histogram of the new complete data
## Calculate total steps per day
cdfSum <- aggregate(x=cdf$steps, by=list(cdf$date), FUN=sum)
## Rename aggregate data frame columns
cdfSum <- rename(cdfSum, date=Group.1, steps=x)

## Part 1 - Make a histogram of the total number of steps taken each day
## Histogram of total steps taken each day
## Construct the plot
g <- ggplot(cdfSum, aes(steps))

## Histogram of total steps frequency
g <- g + geom_histogram(binwidth=1000, colour = "blue", fill = "black")

## Add main label
g <- g + ggtitle("Frequency of Total Steps Taken in a Day")

## Add x and y axis labels
g <- g + xlab("Total Steps in a Day (1000 step increments)")
g <- g + ylab("Frequency (count of occurrences)")

## Display plot
print(g)

## Part 2 - Calculate mean and median of total number of steps taken per day
cmeanSteps <- mean(cdfSum$steps)
cmedianSteps <- median(cdfSum$steps)
sprintf("Mean total steps per day from complete dataset: %.1f", cmeanSteps)
sprintf("Median total steps per day from complete dataset: %.1f", cmedianSteps)

sprintf("Mean total steps per day from original dataset: %.1f", meanSteps)
sprintf("Median total steps per day from original dataset: %.1f", medianSteps)

## Are there diﬀerences in activity patterns between weekdays and weekends?
## Part 1 - Create a new factor variable in the dataset with two levels – “weekday”
##          and “weekend” indicating whether a given date is a weekday or weekend day.
cdf <- mutate(cdf, isWeekend=weekdays(as.Date(cdf$date)) %in% c("Saturday", "Sunday"))
cdf <- mutate(cdf, dayType=as.factor(ifelse(cdf$isWeekend,"Weekend","Weekday")))

## Part 2 - Make a panel plot containing a time series plot (i.e.type = "l") of the
##          5-minute interval (x-axis) and the average number of steps taken, averaged
##          across all weekday days or weekend days.
dfWADAP <- aggregate(x=cdf$steps, by=list(cdf$finterval,cdf$dayType), FUN=mean)
dfWADAP <- rename(dfWADAP, interval=Group.1, dayType=Group.2, avgSteps=x)
dfWADAP <- mutate(dfWADAP, hour=paste('0000', as.character(dfWADAP$interval), sep=""))
dfWADAP$hour <- substr(dfWADAP$hour, nchar(dfWADAP$hour)-3, nchar(dfWADAP$hour))
dfWADAP$hour <- as.numeric(substr(dfWADAP$hour, 1, 2)) + as.numeric(substr(dfWADAP$hour, 3, 4)) / 60.0

## Construct the two panel line plot
g <- ggplot(dfWADAP, aes(hour, avgSteps))

g <- g + facet_grid(dayType ~ .) + facet_wrap( ~dayType, ncol=1)

## Line plot
g <- g + geom_line(aes(group=1), colour="blue", size=1)

g <- g + scale_x_continuous(breaks = seq(0, 24, by=1))

# Add vertical intercept for interval with peak activity
weekdaySteps <- filter(dfWADAP, dayType=="Weekday")
weekendSteps <- filter(dfWADAP, dayType=="Weekend")
maxWeekdaySteps <- filter(weekdaySteps, avgSteps==max(avgSteps))
maxWeekendSteps <- filter(weekendSteps, avgSteps==max(avgSteps))
maxSteps <- rbind(maxWeekdaySteps, maxWeekendSteps)
g <- g + geom_vline(aes(xintercept = maxSteps$hour), maxSteps, colour="red")

## Add main label
maintitle1 <- "Average Number of Steps measured at 5 minute Intervals in 24 hour periods"
maintitle2 <- paste("between",minDate,"and",maxDate)
title <- paste(maintitle1,maintitle2)
g <- g + ggtitle(title)

## Add x and y axis labels
g <- g + xlab("Hour in Day (plotted at 5 minute intervals)")
g <- g + ylab("Average Number of Steps")

## Display plot
print(g)

maxSteps
sprintf("Weekday 5-minute interval with max number of steps (averaged over all days): %s", maxWeekdaySteps$interval)
sprintf("Weekend 5-minute interval with max number of steps (averaged over all days): %s", maxWeekendSteps$interval)
