---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


RepData PeerAssessment1
=======================
  
Loading and preprocessing the data
----------------------------------
  
* Load the data
  

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

* Process/transform the data (if necessary) into a format suitable for your analysis


```r
steps_day <- aggregate(steps~date,data=activity,sum,na.rm=TRUE)
steps_interval <- aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
```

What is mean total number of steps taken per day?
-------------------------------------------------
  
* Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

* Calculate and report the mean and median total number of steps taken per day

```r
mean(steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day$steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?
-------------------------------------------

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(steps~interval, data=steps_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_interval[which.max(steps_interval$steps),]$interval
```

```
## [1] 835
```

Imputing missing values
-----------------------

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy is to impute with the floor(mean) of the steps in the interval

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_imputed <- activity
imputes <- vector()
for(i in 1:nrow(activity_imputed)){
  if(is.na(activity_imputed[i,]$steps)) {
    activity_imputed[i,]$steps <- floor(steps_interval[steps_interval$interval==activity_imputed[i,]$interval,]$steps[1])
    imputes <- rbind(imputes, 1)
  } else {
    imputes <- rbind(imputes, 0)
  }
}
activity_imputed <- cbind(activity_imputed, imputed=imputes)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_day_imputed <- aggregate(steps~date,data=activity_imputed,sum,na.rm=TRUE)
hist(steps_day_imputed$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(steps_day_imputed$steps)
```

```
## [1] 10749.77
```

```r
median(steps_day_imputed$steps)
```

```
## [1] 10641
```
The values has been decreased as now the dataset has more lower values. Nevertheless, the frequencies in the histogram are higher due to more available data points.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity_imputed$day <- ifelse(as.POSIXlt(as.Date(activity_imputed$date))$wday%%6==0,"weekend","weekday")
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
steps_interval_day <- aggregate(steps~interval+day,activity_imputed,mean)
xyplot(steps~interval|day, data=steps_interval_day, aspect=1/2, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
