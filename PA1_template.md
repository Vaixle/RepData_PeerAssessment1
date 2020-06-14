---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
activity <- read.csv("activity.csv")
activityNoNa<- activity[!is.na(activity$steps), ]
```


## What is mean total number of steps taken per day?

```r
daySteps<-aggregate(activityNoNa[c("steps")], list(date = activityNoNa$date), sum)
hist(daySteps$steps, xlab = "Number of Steps Taken per Day", main = "Total Steps Taken per Day vs Frequncy")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanSteps<-mean(daySteps$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps<-median(daySteps$steps)
medianSteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
intervalSteps <- aggregate(steps ~ interval, data=activityNoNa, FUN=mean)
plot(x=intervalSteps$interval, y=intervalSteps$steps, type="l", xlab="Interval", ylab="Avg Steps Taken", main="Avg Steps Taken per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxIntervalSteps<-intervalSteps[intervalSteps$steps == max(intervalSteps$steps), 1]
maxIntervalSteps
```

```
## [1] 835
```


## Imputing missing values

```r
sumNarows<-sum(is.na(activity$steps))
sumNarows
```

```
## [1] 2304
```

```r
activityFill <- activity
activityFill$steps <- ifelse(is.na(activityFill$steps) == TRUE, intervalSteps[intervalSteps$interval == activityFill$interval, "steps"], activityFill$steps)
dayStepsFill <- aggregate(steps ~ date, data=activityFill, FUN=sum)
hist(dayStepsFill$steps , xlab = "Number of Steps Taken per Day (Filled)", main = "Total Steps Taken per Day vs Frequncy (Filled)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
meanStepsFill = mean(dayStepsFill$steps)
meanStepsFill
```

```
## [1] 10766.19
```

```r
medianStepsFill = median(dayStepsFill$steps)
medianStepsFill
```

```
## [1] 10765.59
```

```r
data.frame("Mean Steps BF" = meanSteps, "Median Steps BF" = medianSteps, "Mean Steps AF" = meanStepsFill, "Median Steps AF" = medianStepsFill)
```

```
##   Mean.Steps.BF Median.Steps.BF Mean.Steps.AF Median.Steps.AF
## 1      10766.19           10765      10766.19        10765.59
```


## Are there differences in activity patterns between weekdays and weekends?

```r
activityFill$dayOfWeek <- sapply(activityFill$date, FUN=function(dateVal) {
      dateVal <- strptime(dateVal, format="%Y-%m-%d")
      
      if (weekdays(dateVal) %in% c("понедельник","вторник","среда","четверг","пятница")){
            return("weekday")
      }
      else{
            return("weekend")
      }
})

activityFill$dayOfWeek <- as.factor(activityFill$dayOfWeek)

avgDayStepsFill <- aggregate(steps~interval+dayOfWeek, data=activityFill, FUN=mean)

ggplot(data=avgDayStepsFill) + 
      geom_line(aes(x=interval, y=steps)) + 
      facet_grid(dayOfWeek ~ .) +
      labs(x="Interval", y="Steps", title="Avg Steps by Intervals, Weekdays vs Weekends (Filled)") + 
      theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
