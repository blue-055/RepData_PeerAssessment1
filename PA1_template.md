---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


``` r
unzip(zipfile="activity.zip")
```

```
## Warning in unzip(zipfile = "activity.zip"): error 1 in extracting from zip file
```

``` r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
part 1 of question 1 :Calculate the total number of steps taken per day

``` r
library(ggplot2)
totalsteps <- with(data, tapply(steps, date, sum, na.rm = TRUE))
```
part 2 Making a histogram of the total number of steps taken each day

``` r
hist(totalsteps, xlab = "Steps", main = "Total number of steps per day", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
final part: Calculate and report the mean and median of the total number of steps taken per day


``` r
Meansteps <- mean(totalsteps, na.rm = T) 
Mediansteps <- median(totalsteps, na.rm = T)
Mediansteps
```

```
## [1] 10395
```

``` r
Meansteps
```

```
## [1] 9354.23
```
the mean steps from the code above is approximately 9354 and the median is 10395 


## What is the average daily activity pattern?
Make a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
avgsteps <- aggregate(steps ~ interval , data, mean, na.rm = T)
plot(x= avgsteps$interval, y= avgsteps$steps, type = "l", main = "Average daily activity", xlab = "5-min Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
avgsteps[which.max(avgsteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
the interval is 835, the steps is 206.17

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA NAstart color red, start verbatim, NA, end verbatim, end color reds)

``` r
sum(is.na(data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* the strategy I am going to use is the the mean  that 5-minute interval

Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
datanew_filled <- data
datanew_filled[is.na(datanew_filled)] <- avgsteps$steps
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


``` r
totalsteps_filled <- with(datanew_filled, tapply(steps, date, sum))
hist(totalsteps_filled, main = "Total number of steps taken each day", xlab = "Steps", col = "blue")
```

![](PA1_template_files/figure-html/making graph-1.png)<!-- -->

corrected mean 


``` r
Meansteps_new <- mean(totalsteps_filled)
Mediansteps_new <- median(totalsteps_filled)
options(scipen = 999)
```
the new mean after the missing data was corrected for is approximately 10766 and the new corrected median is 10766
The new values differ from the values values in the first part as they both have increased


``` r
Meansteps_new 
```

```
## [1] 10766.19
```

``` r
Mediansteps_new
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
datanew_filled$weekdays <- weekdays(as.Date(datanew_filled$date))
datanew_filled$daytype <- ifelse(datanew_filled$weekdays %in% c("Saturday","Sunday"), "Weekend", "Weekday")
datanew_filled$daytype <- as.factor(datanew_filled$daytype)
```
Make a panel plot containing a time series plot (i.e. type = "l" type = "l"start color red, start verbatim, type = "l", end verbatim, end color red) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
library(ggplot2)
avgstepsnew <- aggregate(steps ~ interval + daytype , datanew_filled, mean)
ggplot(avgstepsnew, aes(x= interval, y= steps, color = daytype )) + geom_line()+ facet_wrap(daytype~., nrow = 2) + labs(title = "AVerage activity patterns Weekdays vs Weekends", x = "Interval", y= "Number of steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
