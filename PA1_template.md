---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Load required packages

```r
library(lattice)
library(ggplot2)
library(lubridate)
library(xtable)
```

## Loading and preprocessing the data
We load dataset from zipped file with unz(), then look at its structure using str().

```r
data <- read.table(unz("./activity.zip", "activity.csv"), header=TRUE, sep=",", na.strings="NA")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Next, convert the data column using ymd() from package lubridate, and check out the revised structure.

```r
data$date <- ymd(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Great. Our 'data' column is now in standard POSIXct format, so that R will understand the data with respect to dates and not some string of characters/numbers.

## What is mean total number of steps taken per day?
Try a histogram plot to get a preliminary understand of the dataset. Plot total steps taken each day.

```r
plotstepday <- with(data, aggregate(steps, list(date), sum))
hist(plotstepday$x, main="Histogram of Steps Taken Each Day", xlab="Total Number Each Day")
```

![plot of chunk plotstepday](figure/plotstepday-1.png) 

Find the mean and median of steps taken each day:

```r
Mean   <-  mean(plotstepday$x, na.rm=TRUE)
Median <-  median(plotstepday$x, na.rm=TRUE)
```

The mean number of steps taken per day is **10766** while the median is **10765**

## What is the average daily activity pattern?

```r
intval <- data[as.character(data$date)=="2012-10-01",]$interval
intval_avg <- with(data, aggregate(steps, list(interval), mean, na.rm=TRUE))
plot(intval, intval_avg$x, "l", col="blue", main="Steps per 5-min Interval", xlab="Interval", ylab="Steps", lwd=2)
```

![plot of chunk plotts](figure/plotts-1.png) 

From the plot, we see that steps taken are generally lower in the beginning and end of the entire recorded acitvity. There are some peaks, which could be due to a switch to a brisk from slow walk. 


```r
maxval <- max(intval_avg$x)
max_intval <- intval[which.max(intval_avg$x)]
```

The 5-min interval containing the maximum number of steps is the **835** interval at a maximum of **206.1698113** steps.

## Imputing missing values

We noted that there were several NA values in the dataset. Let's count now many are there in the entire dataset.

```r
totalnas <- sum(is.na(data$steps))
datasetlen <- length(data$steps)
```

Out of **17568** entries in the entire dataset, there are **2304** NA entries, that is quite a bit.

## Are there differences in activity patterns between weekdays and weekends?
