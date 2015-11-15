# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Assuming the data set is in the current directory, unzip and load with `read.csv`. Convert the column containing dates into the `Date` format.

```r
system("unzip activity.zip")
rawData <- read.csv("activity.csv")
rawData$date <- as.Date(as.character(rawData$date))
```


## What is mean total number of steps taken per day?
First, compute the total number of steps per day. Two methods are presented below.

```r
# Method 1
total_per_day <- tapply(rawData$steps, rawData$date, sum)
# Method 2
library(plyr)
df <- data.frame(date = rawData$date, steps = rawData$steps)
total_per_day <- ddply(df, .(date), summarize, sum = sum(steps))
```
Plot a histogram of the total number of steps taken each day, to get an idea about the distribution of the data. Notice that the distribution is not symmetric and we expect the median to be slightly lower than the mean.

```r
hist(total_per_day$sum,xlab = "Total steps per day", main = "Histogram of Total Number of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Finally, calculate the mean and median of the total number of steps taken per day.

```r
mean(total_per_day$sum, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_per_day$sum, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
For this question, we use `ddply' to calculate the number of steps corresponding for each 5-min interval, averaged across all days in the data set. The first few rows of data are shown.

```r
avgSteps <- ddply(rawData, .(interval), summarize, avgSteps = mean(steps, na.rm =TRUE))
head(avgSteps)
```

```
##   interval  avgSteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
Construct a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). As expected, the average number of steps is lower for intervals lower than 500 (5am), and beyond 2000 (8pm), when the person was either asleep or relaxing at home.  

```r
plot(avgSteps$interval,avgSteps$avgSteps, type = "l", xlab = "Interval Number", ylab = "Average number of steps",main = "Number of steps, averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Finally, determine the 5-min interval, on average across all the days in the dataset, which contains the maximum number of steps. As a bonus, plot it on the time series plot.

```r
maxInterval <- avgSteps$interval[which.max(avgSteps$avgSteps)]
print(maxInterval)
```

```
## [1] 835
```

```r
plot(avgSteps$interval,avgSteps$avgSteps, type = "l", xlab = "Interval Number", ylab = "Average number of steps",main = "Number of steps, averaged across all days")
par(new = T)
points(maxInterval, max(avgSteps$avgSteps), col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

Notice the peak occurs between 8am and 9am (835 means 8:35am); the person wearing the fitness tracker likely exercises in the morning.

## Imputing missing values
First, calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs): 

```r
totalNA <- sum(is.na(rawData))
print(totalNA)
```

```
## [1] 2304
```

The imputation of the missing data is done by replacing the missing values for the number of steps with the mean for that 5-minute interval across all days, since it is already available. Data set *myData* is created, equal to the original dataset but with the missing data filled in.

```r
locateMissing <- which(is.na(rawData$steps))
ll <- levels(as.factor(rawData$interval))
myData <- rawData
for(ii in 1:totalNA) {
    # Find the position of the interval (index in the interval list ll) that corresponds to the missing 
    # data and retrieve the corresponding
    # average from the avgSteps vector.
    whichInterval <- which( ll == rawData$interval[locateMissing[ii]])
    myData$steps[locateMissing[ii]] <- avgSteps$avgSteps[whichInterval] 
}
```
    
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Notice that the distribution appears closer to being symmetric than previously; the value of the median now equals that of the mean. The impact of imputing missing data on the estimates of the total daily number of steps depends on how the imputation is done.

```r
total_per_day_impute_NA<- tapply(myData$steps, myData$date, sum)
hist(total_per_day_impute_NA,xlab = "Total steps per day", main = "Histogram of Total No. of Steps/Day After Imputation")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(total_per_day_impute_NA)
```

```
## [1] 10766.19
```

```r
median(total_per_day_impute_NA)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
To differentiate between weekdays and weekends, first create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
myData$day <- ifelse(weekdays(myData$date) %in% c("Saturday", "Sunday"), "weekend","weekday")
myData <- transform(myData, day = factor(day))
head(myData)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
myAvgSteps <- ddply(myData, .(interval, day), summarize, avgSteps = mean(steps))
library(lattice)
xyplot(avgSteps ~ interval | day, data = myAvgSteps, layout = c(1, 2), type ="l", 
       xlab = "Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

Notice there are differences between the two patterns, as expected: there are more local maxima throughout the day during weekends, indicating a higher activity level, as the person is not bound to an office chair.
