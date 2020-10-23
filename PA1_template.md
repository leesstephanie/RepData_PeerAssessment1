Reproducible Research First Project
===================================

First, I load the packages I would need

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Next, I download the zip file and then unzip it.

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                  "repdata_data_activity.zip")
    unzip("repdata_data_activity.zip")

Loading and preprocessing the data
----------------------------------

    data = read.csv("activity.csv")
    print(str(data))

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ## NULL

    print(summary(data))

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

As we can see here, the variable `steps` is the only one which has NA.
We are going to work on that later. Now I am going to answer the first
question.

What is mean total number of steps taken per day?
-------------------------------------------------

### Make a histogram of the total number of steps taken each day

    one = data %>% 
            group_by(date) %>%
            summarise(total = sum(steps, na.rm=TRUE))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    hist(one$total, breaks = 10, xlim = c(0,25000), xlab = "total steps",
         main = "Total number of steps taken per day", col="skyblue")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

The frequency can be interpreted as the total days in which a range of
total number of steps occur. The total frequency is 61. In the code, I
set the `breaks` to be 10, so the first bar is for all total number of
steps between 0 and 2000, the second bar is for all total number of
steps between 2000 and 4000, and so on. Missing values are included in
the leftmost bar.

### Calculate and report the mean and median total number of steps taken per day

    mean(one$total, na.rm = TRUE)

    ## [1] 9354.23

    median(one$total, na.rm = TRUE)

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

### Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    two <- data %>% group_by(interval) %>% summarize(avg=mean(steps, na.rm=T))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    axislbl <- seq(0,23,2)

    with(two, plot(interval, avg, type= "l", ylab="average across all days", 
                   xlab = "hour interval", xaxt="n",
                   main="Average number of steps taken on each interval across all days"))
    axis(1, at = axislbl*100, labels=as.character(axislbl))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    avgInt <- data %>% group_by(interval) %>% summarize(average = mean(steps, na.rm = T))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    avgInt$interval[which.max(avgInt$average)]

    ## [1] 835

So the interval 835, which means on 08.35 am, contains the maximum
number of steps.

Imputing missing values
-----------------------

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    nrow(data) - sum(complete.cases(data))

    ## [1] 2304

There are 2304 rows with NAs in the dataset. This number matches the
total NAs in `data$steps`.

### Devise a strategy for filling in all of the missing values in the dataset

My strategy is to fill missing values with either 0 or the values from
`avgInt$average`. I put 0 as one of the possible values because 0 occurs
so often in other days which there is no NAs. `avgInt$average` is the
number of steps for each interval, averaged across all days. I set seed
before doing that calculation so I could get the same data set for the
rest of the questions.

    filler <- function(ds){
      datafull <- ds
      set.seed(130)

      for (i in 1:nrow(datafull)){
        if (is.na(datafull$steps[i])){
          datafull$steps[i] <- sample(c(0, avgInt$average), 1, T)
        }
      }
      datafull
    }

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

This is the implementation of my strategy in the previous section to
create a new data set with no missing values.

    data_noNA <- filler(data)
    summary(data_noNA)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.46   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 27.30   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##                   (Other)   :15840

Now we don’t see the summary for total NAs, which means the NAs have
gone.

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    three = data_noNA %>% 
            group_by(date) %>%
            summarise(total = sum(steps, na.rm=TRUE))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    hist(three$total, breaks = 10, xlim = c(0,25000), xlab = "total steps",
         main = "Total number of steps taken per day", col="green")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    mean(three$total, na.rm = TRUE)

    ## [1] 10788.11

    median(three$total, na.rm = TRUE)

    ## [1] 10765

The histogram looks pretty similar, except for the leftmost bar. In the
original histogram, there are 10 days in which the total steps is less
than 2000. However, in the new histogram, there are less than 5 days in
which the total steps is less than 2000. The peak in both histograms
seems to be different. Let’s verify that.

    sum(one$total>10000 & one$total<12000) #data set with missing values

    ## [1] 16

    sum(three$total>10000 & three$total<12000) #data set with missing values filled in

    ## [1] 24

Indeed, we see more days with total number of steps fall in the range
between 10 to 12 thousand. The mean and median of total steps for both
data sets are slightly different. Both the mean and the median for
`data_noNA` are higher than those of `data`.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    data_noNA$date <- as.Date(data_noNA$date)
    data_noNA$daycats <- with(data_noNA,
                              ifelse(weekdays(date)=="Saturday"|weekdays(date)=="Sunday",
                                     "weekend", "weekday"))
    data_noNA$daycats <- as.factor(data_noNA$daycats)
    summary(data_noNA)

    ##      steps             date               interval         daycats     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5                  
    ##  Mean   : 37.46   Mean   :2012-10-31   Mean   :1177.5                  
    ##  3rd Qu.: 27.30   3rd Qu.:2012-11-15   3rd Qu.:1766.2                  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0

### Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Firstly, I am going to make the data set for the plot.

    four <- data_noNA %>% 
            group_by(daycats, interval) %>% 
            summarize(avg = mean(steps, na.rm = T))

    ## `summarise()` regrouping output by 'daycats' (override with `.groups` argument)

To make a plot like in the assignment page, I need a `lattice` package.
After loading the package, we can start plotting right away.

    library(lattice)
    xyplot(four$avg~four$interval|daycats, data = four, layout=c(1,2), type="l",
           xlab = "interval", ylab = "average number of steps", 
           main = "Average number of steps on 5-minute interval, \naveraged across all weekdays or weekend")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)
