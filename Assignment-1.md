Assignment 1 (Reproducible Results)
===================================

### Loading and Preprocessing the Data

We will load the data.

    data <- read.csv("activity.csv", header=TRUE)

### What is the mean total number of steps taken per day?

1.  First we calculate the total number of steps taken per day
2.  Then, we plot the histogram of the total number of steps taken each
    day

<!-- -->

    total_steps <- aggregate(steps~date,data,FUN=sum,na.rm=TRUE)
    hist(total_steps$steps,col="blue")

![](Assignment-1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Lastly we will calculate and report the mean and median of the total
    number of steps taken per day.

<!-- -->

    mean(total_steps$steps)

    ## [1] 10766.19

    median(total_steps$steps)

    ## [1] 10765

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

<!-- -->

    interval_steps <- aggregate(steps~interval,data=data,FUN=sum,na.rm=TRUE)
    plot(steps~interval,data=interval_steps,type="l",col="red")

![](Assignment-1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    interval_steps[which.max(interval_steps$steps),]$interval

    ## [1] 835

### Inputing Missing Values

1.  We calculate the total number of missing NAs

<!-- -->

    missingvalues <- sum(is.na(data$steps))
    missingvalues

    ## [1] 2304

1.  Next, we devised a stratgey to fill in the missing values. We will
    be using the mean of the daily steps. We store this is new\_input.

<!-- -->

    input_steps <- interval_steps$steps[match(data$interval,interval_steps$interval)]

1.  We create a new data set with all the NA values filled in.

<!-- -->

    new_input <- transform(data,steps = ifelse(is.na(data$steps),yes=input_steps,no=data$steps))

1.  Now, we create the new histogram to compare to the old histogram. We
    also compute the mean and median of the total number of steps taken

<!-- -->

    total_new_input <- aggregate(steps~date,data = new_input,FUN=sum)
    names(total_new_input) <-c("date","steps")
    hist(total_new_input$steps,col ="blue")

![](Assignment-1_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    mean(total_new_input$steps)

    ## [1] 84188.07

    median(total_new_input$steps)

    ## [1] 11458

From the results, we see that
