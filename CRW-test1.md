

```r
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```

##**Reproducible Research: Peer Assessment 1**

*Charles Wylie   7/16/2016*

####**Loading and preprocessing the data. Show any code that is needed to:**

#####**1. Load the data (i.e. read.csv())**
#####**2. Process/transform the data (if necessary) into a format suitable for your analysis:**
  
  
  

```r
setwd('~/R/coursera/Module 5 - Reproducible Research/Week 2')
activity <- read.csv("activity.csv", colClasses = c('integer', 'Date', 'numeric'))
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
  
  
  
####**What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset.**

#####**1. Make a histogram of the total number of steps taken each day:**

  

```r
activityByDate <- aggregate(steps ~ date, activity, sum)
head(activityByDate)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
library(ggplot2)

theme_set(theme_gray(base_size = 14))
g <- ggplot(activityByDate, aes(steps)) +
    geom_histogram(binwidth = 750, fill = 'cornflowerblue', color = 'black') +
    labs(x = 'Steps Counted per Day') +
    labs(y = 'Number of Days') +
    labs(title = 'Histogram - Total Steps per Day') +
    theme(plot.margin = unit(c(1, 2, 1, 0.6), 'cm')) + # top, right, bottom, left
    theme(plot.title = element_text(margin = margin(b = 0.6, unit = 'cm'))) +
    theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
    theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
    theme(axis.text.x = element_text(size=13)) +
    theme(axis.text.y = element_text(size=13))
print(g)
```

![](CRW-test1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


#####**2. Calculate and report the mean and median total number of steps taken per day:**



```r
mean(activityByDate$steps)
```

```
## [1] 10766.19
```

```r
median(activityByDate$steps)
```

```
## [1] 10765
```


####**What is the average daily activity pattern?**

#####**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**



```r
activityByInterval <- aggregate(steps ~ interval, activity, mean)
head(activityByInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
theme_set(theme_gray(base_size = 14))
g <- ggplot(activityByInterval, aes(x=interval, y=steps, group=1)) +
    geom_line(color = 'cornflowerblue', size=1) +
    labs(x = 'Interval Number (= Time of Day)') +
    labs(y = 'Steps') +
    labs(title = 'Time Series Plot - Average Number of Steps per Interval') +
    theme(plot.margin = unit(c(1, 2, 1, 0.6), 'cm')) + # top, right, bottom, left
    theme(plot.title = element_text(margin = margin(b = 0.6, unit = 'cm'))) +
    theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
    theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
    theme(axis.text.x = element_text(size=13)) +
    theme(axis.text.y = element_text(size=13))
print(g)
```

![](CRW-test1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


#####**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**



```r
activityByInterval[which.max(activityByInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


#### **Imputing missing values. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.**

#####**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**



```r
sum(is.na(activity))
```

```
## [1] 2304
```


#####**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**


*My strategy is to use the mean of the 5-minute interval, across all the non-NA days in the dataset.*


#####**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**



```r
## Join the previously computed mean of each interval to the original dataset:
activity$avg_steps <- activityByInterval$steps
head(activity)
```

```
##   steps       date interval avg_steps
## 1    NA 2012-10-01        0 1.7169811
## 2    NA 2012-10-01        5 0.3396226
## 3    NA 2012-10-01       10 0.1320755
## 4    NA 2012-10-01       15 0.1509434
## 5    NA 2012-10-01       20 0.0754717
## 6    NA 2012-10-01       25 2.0943396
```

```r
## Replace NA entries with interval mean and delete the joined field "avg_steps":
activityImputed = transform(activity, steps = ifelse(is.na(steps), avg_steps, steps))
activityImputed <- subset(activityImputed, select=steps:interval)
head(activityImputed)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


#####**4. a) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.**



```r
activityImputedByDate <- aggregate(steps ~ date, activityImputed, sum)
head(activityImputedByDate)
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
theme_set(theme_gray(base_size = 14))
g <- ggplot(activityImputedByDate, aes(steps)) +
    geom_histogram(binwidth = 750, fill = 'cornflowerblue', color = 'black') +
    labs(x = 'Steps Counted per Day') +
    labs(y = 'Number of Days per Step Count') +
    labs(title = 'Histogram - Total Steps per Day - Missing Data Imputed') +
    theme(plot.margin = unit(c(1, 2, 1, 0.6), 'cm')) + # top, right, bottom, left
    theme(plot.title = element_text(margin = margin(b = 0.6, unit = 'cm'))) +
    theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
    theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
    theme(axis.text.x = element_text(size=13)) +
    theme(axis.text.y = element_text(size=13))
print(g)
```

![](CRW-test1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


#####**4. b) Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**



```r
## Original mean with missing data:
mean(activityByDate$steps)
```

```
## [1] 10766.19
```

```r
## Recalculated mean with imputed data:
mean(activityImputedByDate$steps)
```

```
## [1] 10766.19
```


*There is no change in the recalculated mean.* 



```r
## Original median with missing data:
median(activityByDate$steps)
```

```
## [1] 10765
```

```r
## Recalculated median with imputed data:
median(activityImputedByDate$steps)
```

```
## [1] 10766.19
```


*There is a very slight change (+1 step per day) in the recalculated median.*


####**Are there differences in activity patterns between weekdays and weekends? For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.**

#####**1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**



```r
activityImputed$day <- weekdays(activityImputed$date, abbreviate = TRUE)
activityImputed$weekday <- as.factor(ifelse(activityImputed$day %in% c('Sat', 'Sun'), 'weekend', 'weekday'))

head(activityImputed)
```

```
##       steps       date interval day weekday
## 1 1.7169811 2012-10-01        0 Mon weekday
## 2 0.3396226 2012-10-01        5 Mon weekday
## 3 0.1320755 2012-10-01       10 Mon weekday
## 4 0.1509434 2012-10-01       15 Mon weekday
## 5 0.0754717 2012-10-01       20 Mon weekday
## 6 2.0943396 2012-10-01       25 Mon weekday
```

```r
str(activityImputed)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Mon" "Mon" "Mon" "Mon" ...
##  $ weekday : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


#####**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**



```r
activityImputedAverage <- aggregate(steps ~ interval + weekday, activityImputed, mean)

library(lattice)

xyplot(steps ~ interval | weekday, data = activityImputedAverage, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps", main=list("Time Series Plot - Average Steps per Interval - Weekday vs Weekend",cex=0.9))
```

![](CRW-test1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

