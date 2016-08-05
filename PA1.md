# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("~/activity.csv")
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

## What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

```r
table1 <- group_by(activity, date) %>% summarise(steps1 = sum(steps))
```

2.  Make a histogram of the total number of steps taken each day

```r
barplot(table1$steps1, main = "Historam of steps by date", las = 1)
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3 . Calculate and report the mean and median of the total number of steps taken per day

```r
mean(table1$steps1, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(table1$steps1, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
table2 <- group_by(activity, interval) %>% summarise(average = mean(steps, na.rm = TRUE))
plot(table2$interval, table2$average, type = "l", las = 1, main = "Average steps by time", xlab = "Time every 5 min", ylab = "Average steps")
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
filter(table2, average == max(average))
```

```
## # A tibble: 1 x 2
##   interval  average
##      <int>    <dbl>
## 1      835 206.1698
```

## Imputing missing values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# I will use the mean for the 5 min
```

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
activity1 <- merge(activity, steps.interval, by = "interval", suffixes = c("",".y"))
nas <- is.na(activity1$steps)
activity1$steps[nas] <- activity1$steps.y[nas]
activity1 <- activity1[, c(1:3)]
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
table3 <- group_by(activity1, date) %>% summarise(steps1 = sum(steps))
barplot(table3$steps1, main = "Historam of steps by date", las = 1)
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(table3$steps1, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(table3$steps1, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
# there is no impact on the average by day but the median has singly increased. 
```

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
Weekdaytipe <- mutate(activity1, day = wday(date, label = TRUE))
Weekdaytipe <- mutate(Weekdaytipe, category = day == c("Sat", "Sun"))
Weekdaytipe <- mutate(Weekdaytipe, category = replace(Weekdaytipe$category, Weekdaytipe$category == "TRUE","Weekend")) 
Weekdaytipe <- mutate(Weekdaytipe, category = replace(Weekdaytipe$category, Weekdaytipe$category == "FALSE","Weekday"))
```

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
Weekdays <- filter(Weekdaytipe, category == "Weekday") %>% group_by(interval) %>% summarise(average = mean(steps))%>% mutate(category = "Weekday") 
Weekends <- filter(Weekdaytipe, category == "Weekend") %>% group_by(interval) %>% summarise(average = mean(steps))%>% mutate(category = "Weekend") 
par(mfrow=c(2,1))
plot(Weekdays$interval,Weekdays$average, type = "l", main = "Weekdays", xlab = "", ylab = "")
plot(Weekends$interval,Weekends$average, type = "l", main = "Weekends", xlab = "Interval", ylab = "Average Steps")
```

![](PA1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
