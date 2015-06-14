# Reproducible Research: Peer Assessment 1


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.1.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```


---
Loading and preprocessing the data
---



```r
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
activitydata <- activity[ with (activity, { !(is.na(steps)) } ), ]
```

Calculate and report the mean and median of the total number of steps taken per day

```r
steps_by_interval <- aggregate(steps ~ interval, activitydata, mean)

# create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#What is mean total number of steps taken per day?



```r
group_by_day <- group_by(activitydata, date)
total_steps_by_day <- summarise(group_by_day, total = sum(steps))
hist(total_steps_by_day$total, main="Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(total_steps_by_day$total, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_by_day$total, na.rm=TRUE)
```

```
## [1] 10765
```
The mean and median are respectively 10766 and 10765 steps taken per day. 

#What is the average daily activity pattern?

```r
# preprocessing data for plot
steps_by_interval <- aggregate(steps ~ interval, activitydata, mean)
# create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The plot shows a peek around the 800th interval.


```r
steps_by_interval$interval[which.max(steps_by_interval$steps)]
```

```
## [1] 835
```

The peak is attained for the 835th interval.


```r
max(steps_by_interval$steps)
```

```
## [1] 206.1698
```

The peak is 206 steps.

#Imputing missing values

Total number of missing values in the dataset with NA:


```r
sum(is.na(activity))
```

```
## [1] 2304
```

As a strategy for filling in the missing values in the dataset, will be using the mean of the specific 5-minute interval for those observation that is missing.


```r
imputed_activitydata <- activity
for (i in 1:nrow(imputed_activitydata)) {
    if (is.na(imputed_activitydata$steps[i])) {
        interval_value <- imputed_activitydata$interval[i]
        steps_value <- steps_by_interval[
            steps_by_interval$interval == interval_value,]
        imputed_activitydata$steps[i] <- steps_value$steps
    }
}
```


```r
# calculate  total number of steps taken each day
imputed_steps_by_day <- aggregate(steps ~ date, imputed_activitydata, sum)
head(imputed_steps_by_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

I would say there is minimum impact as  I used the average for a given interval, it pulled the averages closer to the inserted average value.



```r
mean(imputed_steps_by_day$steps)
```

```
## [1] 10766.19
```

```r
median(imputed_steps_by_day$steps)
```

```
## [1] 10766.19
```

Mean remains the same and median values are higher after imputing missing data.


#Are there differences in activity patterns between weekdays and weekends?


```r
##Are there differences in activity patterns between weekdays and weekends?

imputed_activitydata['type_of_day'] <- weekdays(as.Date(imputed_activitydata$date))
imputed_activitydata$type_of_day[imputed_activitydata$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
imputed_activitydata$type_of_day[imputed_activitydata$type_of_day != "weekend"] <- "weekday"

# convert type_of_day from character to factor
imputed_activitydata$type_of_day <- as.factor(imputed_activitydata$type_of_day)

# calculate average steps by interval across all days
imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, imputed_activitydata, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
    facet_wrap(~ type_of_day, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

From the graph, i guess there is more activities during week-end time.

