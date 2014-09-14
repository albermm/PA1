Reproducible Research: Peer Assessment 1
========================================
---
> Author: **Alberto Martin**
---

This document will show and reproduce the code used to complete the Coursera Assignment
on Reproducible Research


Let's load some libraries that will be used along the analysis:


```r
        library(data.table)
        library(xtable)
        library(plyr)
        library(ggplot2) 
```


***

## Loading and preprocessing the data

Let's define the url from which to download the data and the directory and files
in which it has to be loaded.


```r
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        filezip <- "activity.zip"
        dir <- "./RepData_PeerAssessment1"
        path <- paste(dir, "/", filezip, sep = "")
        file <- paste(dir, "/", "activity.csv", sep = "")
```

Then, download the file and unzip it into the directory defined


```r
        download.file(url, destfile = path, method="curl")
        unzip(path, exdir = dir)
```

Last step of this process is to read the data into a csv file and transform fields to 
make them date and factor respectively. 


```r
        data <-read.csv(file, header=TRUE)
        data$date <-as.Date(data$date, format= "%Y-%m-%d")
        data$interval <- factor(data$interval)
```
Now we are all set to commence with the analysis


***

## What is mean total number of steps taken per day?

The first step is to aggregate the total number of steps for each day, we will do
this using "aggregate":


```r
stepsperday <- aggregate(steps ~ date, data, sum)
colnames(stepsperday) <- c("date", "steps")
```

And display the result in an histogram


```r
ggplot(stepsperday, aes(x = steps)) + geom_histogram(fill = "green", colour = "white", binwidth = 700) + 
        labs(title = "Total Steps Taken per Day", x = "Number of Steps per Day", 
        y = "Number of days") + theme_bw() + 
        geom_vline(aes(xintercept=mean(steps, na.rm=T)),   
                   color="blue", linetype="dashed", size=0.5)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



Mean and Median of steps per day:  


```r
mean(stepsperday$steps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsperday$steps, na.rm = TRUE)
```

```
## [1] 10765
```



***

## What is the average daily activity pattern?

We need to aggregate the steps by 5 mins intervals and change "interval" to integer 
for plotting


```r
steps.interval <- aggregate(data$steps, by = list(interval = data$interval), 
                                FUN = mean, na.rm = TRUE)

steps.interval$interval <- as.integer(levels(steps.interval$interval)[steps.interval$interval])

colnames(steps.interval) <- c("interval", "steps")

ggplot(steps.interval, aes(x = interval, y = steps)) + 
        geom_line(colour="green") + 
        labs(title = "Average number of steps", x = "Interval", 
        y = "Number of steps") + 
        theme_bw()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



In order to find the 5-minute interval which contains the maximum number of steps:


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```


***

## Imputing missing values

Calculate and report the total number of missing values in the data set


```r
sum(is.na(data))
```

```
## [1] 2304
```

Create a new variable in the main data set by merging it with step mean values
And then assign the original value if it is not missing or the replacement value if it is missing. 


```r
datacompleted <- merge(data, steps.interval, by = "interval", suffixes = c("",".prima"))
nas <- is.na(datacompleted$steps)
datacompleted$steps[nas] <- datacompleted$steps.prima[nas]
datacompleted <- datacompleted[, c(1:3)]


completedsteps_per_day <- aggregate(steps ~ date, datacompleted, sum)

ggplot(completedsteps_per_day, aes(x = steps)) + geom_histogram(fill = "green",colour = "white", binwidth = 700) + 
        labs(title = "Steps Taken per Day (Completed cases)", 
        x = "Number of Steps") + theme_bw()
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

To calculate the mean and median:


```r
mean(completedsteps_per_day$steps)
```

```
## [1] 10766
```

```r
median(completedsteps_per_day$steps)
```

```
## [1] 10766
```

Mean after populate missing values is: **10766.19**, the same than before.
Whereas the median was: **10765** with missing values and now it is:**10766.19**


***

## Are there differences in activity patterns between weekdays and weekends?
1 Create a new factor variable "typeofday" indicating the two possible levels, weekday or 
weekend.


```r
datacompleted$day <- as.factor(weekdays(datacompleted$date))
typeofday <- function(day) {
        if ((day) %in% c("Saturday", "Sunday")) {
                "weekend"
        } else {
                "weekday"
        }
}
datacompleted$typeofday <- as.factor(sapply(datacompleted$day, typeofday))
```


2 Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
stepsperdaycompleted <- ddply(datacompleted, c("interval", "typeofday"), summarise,
                                       mean = round(mean(steps)))

ggplot(stepsperdaycompleted, aes(x=interval, y=mean)) + 
        geom_line(aes(group = typeofday, colour=typeofday)) +
        facet_wrap( ~ typeofday, ncol=1) +
        labs(x = "Interval", y = "Number of steps") + 
        theme_bw() +
        theme(legend.position="none")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

