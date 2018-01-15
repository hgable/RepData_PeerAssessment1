---
output: 
  html_document: 
    keep_md: yes
---
Course 5 Week 2 Assignment

1. Set working directory, read and import the data, get per day steps

```r
setwd("C:/Users/hagable/Documents/Training/DataScience")
data <- read.csv("activity.csv")
head(data)
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
dataByDay <- aggregate(steps ~ date, sum, data=data)
```

2. What is the mean and median total number of steps taken per day?

```r
mean(dataByDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
hist(dataByDay$steps)
```

![](PA1_template_20180115_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
median(dataByDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

3. What is the average daily activity pattern? Which interval contains the max number of steps?

```r
dataByTime <- aggregate(steps ~ interval, mean, data=data)
plot(dataByTime$interval, dataByTime$steps, type ="l")
```

![](PA1_template_20180115_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
which.max(dataByTime$steps)
```

```
## [1] 104
```

```r
dataByTime[which.max(dataByTime$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

4. Find the number of NAs, replace them with the average number of steps, and recalculate mean and median.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
data$steps[is.na(data$steps)] <- mean(data$steps, na.rm = TRUE)
NoNAs <- data
sum(is.na(NoNAs))
```

```
## [1] 0
```

```r
NoNAsByDay <- aggregate(steps ~ date, sum, data=NoNAs)
mean(NoNAsByDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
hist(NoNAsByDay$steps)
```

![](PA1_template_20180115_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
median(NoNAsByDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
-The mean does not change, as we replaced missing value with the average value. However, the median increased slightly. 

5. Are there differences in activity patterns between weekdays and weekends?
-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data.filled = data
for (i in 1:nrow(data.filled)) {
    if (is.na(data.filled[i, "steps"])) {
        interval = as.character(data.filled[i, "interval"])
        data.filled[i, "steps"] = dataByTime[interval]
    }
}

data.filled$weekday = weekdays(as.Date(data.filled$date, format = "%Y-%m-%d"))
data.filled$weekday.type = factor(ifelse(data.filled$weekday == "Sunday" | data.filled$weekday == 
    "Saturday", "weekend", "weekday"), levels = c("weekday", "weekend"))

data.filled.weekdays = data.filled[data.filled$weekday.type == "weekday", ]
data.filled.weekend = data.filled[data.filled$weekday.type == "weekend", ]
avg.steps.per.interval.weekdays = sapply(split(data.filled.weekdays$steps, data.filled.weekdays$interval), 
    mean)
avg.steps.per.interval.weekend = sapply(split(data.filled.weekend$steps, data.filled.weekend$interval), 
    mean)

DoPlot = function() {
    par(mfrow = c(2, 1), mar = c(4, 5, 2, 2))
    plot(avg.steps.per.interval.weekend, type = "l", yaxt = "n", ylim = c(0, 
        250), xlim = c(0, 300), main = "weekend", ylab = "", xlab = "Interval")
    axis(side = 4, at = seq(0, 250, 50), labels = seq(0, 250, 50))
    plot(avg.steps.per.interval.weekdays, type = "l", ylim = c(0, 250), xlim = c(0, 
        300), main = "weekday", ylab = "", xlab = "Interval")
    par(mfrow = c(1, 1), mar = c(3, 2, 2, 2))
    mtext(text = "Number of steps", side = 2)
    par(mar = c(5, 5, 5, 2))
}

DoPlot()
```

![](PA1_template_20180115_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

