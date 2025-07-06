---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


``` r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.5.1
```

## Loading and preprocessing the data


``` r
dir("repdata_data_activity")
```

```
## [1] "activity.csv"
```

``` r
activity.ori <- read.csv("repdata_data_activity/activity.csv")
activity_clean <-na.omit(activity.ori)
activity <- activity_clean[!activity_clean$steps == 0,]
```


## What is mean total number of steps taken per day?


``` r
df <- aggregate(steps ~ date, data = na.omit(activity.ori), FUN = sum, na.rm = T) #na.omit(activity.ori)
hist(df$steps,
     breaks = 20,  # Adjust number of bins as needed
     col = "lightblue",
     main = "Histogram of Total Steps Taken Per Day",
     xlab = "Total Number of Steps",
     ylab = "Frequency (Number of Days)",
     border = "white")

# Add mean and median lines
abline(v = mean(df$steps), col = "red", lwd = 2, lty = 1)
abline(v = median(df$steps), col = "blue", lwd = 2, lty = 2)


# Add legend
legend("topright", 
       legend = c(paste("Mean:", round(mean(df$steps))),
                 paste("Median:", round(median(df$steps)))),
       col = c("red", "blue"), lty = 2, lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?

``` r
interval_steps <-  data.frame(tapply(activity_clean$steps, activity_clean$interval, mean))
interval_steps<- data.frame(interval_steps)
colnames(interval_steps) <- "steps"
interval_steps$interval <-rownames(interval_steps)

# Create time series plot
plot(interval_steps$interval, interval_steps$steps, 
     type = "l",
     xlab = "5 minute Interval", 
     ylab = "Average Steps",
     main = "Daily Average Activity Pattern",
     col = "red",
     lwd = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Imputing missing values

``` r
interval_steps <- aggregate(steps ~ interval   , data = activity_clean, FUN = mean, na.rm = TRUE)
data_na_impute <- merge(activity.ori[is.na(activity.ori$steps),-1],interval_steps , by = "interval")

library(dplyr)
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

``` r
activity_imputed <- rbind(data_na_impute, activity_clean) %>% arrange(date, interval)

str(activity_imputed)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```


``` r
ds2 <- aggregate( steps ~ date, data = activity_imputed, FUN = sum, na.rm = TRUE)

# Create histogram with proper binning and labels
hist(ds2$steps,
     breaks = 20,  # Adjust number of bins as needed
     col = "lightblue",
     main = "Histogram of Average Steps Taken Per Day",
     xlab = "Total Number of Steps",
     ylab = "Frequency (Number of Days)",
     border = "white")

# Add mean and median lines
abline(v = mean(ds2$steps), col = "red", lwd = 2, lty = 1)
abline(v = median(ds2$steps), col = "blue", lwd = 2, lty = 2)


# Add legend
legend("topright", 
       legend = c(paste("Mean:", round(mean(ds2$steps))),
                 paste("Median:", round(median(ds2$steps)))),
       col = c("red", "blue"), lty = 2, lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


``` r
activity.ori$weekdays <- weekdays(as.Date(activity.ori$date))
activity.ori$day_type <- "Weekday"
activity.ori[activity.ori$weekdays %in% c("Saturday", "Sunday"),]$day_type <- "weekend"
```



``` r
original_mean  <- mean(df$steps)
imputed_mean <- mean(ds2$steps)

original_median <- median(df$steps)
imputed_median <- median(ds2$steps)

comparison <- data.frame(
  Metric = c("Mean", "Median"),
  Original_data = c(original_mean, original_median),
  Imputed_data = c(imputed_mean, imputed_median),
  Difference = c(imputed_mean - original_mean, imputed_median - original_median)
)

print(comparison)
```

```
##   Metric Original_data Imputed_data Difference
## 1   Mean      10766.19     10766.19   0.000000
## 2 Median      10765.00     10766.19   1.188679
```


## Are there differences in activity patterns between weekdays and weekends?

``` r
# Create panel plot by day type
ggplot(activity.ori, aes(x = interval, y = steps)) +
  geom_line(stat = "summary", fun = mean, color = "steelblue") +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Activity Patterns: Weekdays vs Weekends",
       x = "5-minute Interval",
       y = "Average Number of Steps") +
  theme_minimal()
```

```
## Warning: Removed 2304 rows containing non-finite outside the scale range
## (`stat_summary()`).
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
