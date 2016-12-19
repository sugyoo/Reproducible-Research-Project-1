# Reproducible Research: Peer-graded Assignment: Course Project 1

```{r, echo=FALSE, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
library(scales)
library(Hmisc)
```

## 1. Code for reading in the dataset and/or processing the data
```{r, results='markup', warning=TRUE, message=TRUE}
if(!file.exists('./activity.csv')){
    unzip('./activity.zip')
}
activityData <- read.csv('./activity.csv')
```

-----

## 2. Histogram of the total number of steps taken each day
```{r}
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

## 3. Mean and median number of steps taken each day
```{r}
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
* Mean: `r stepsByDayMean`
* Median:  `r stepsByDayMedian`

-----

## 4. Time series plot of the average number of steps taken
```{r}
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
```{r}
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

* Most Steps at: `r timeMostSteps`

----

## 6. Code to describe and show a strategy for imputing missing data



```{r}
numMissingValues <- length(which(is.na(activityData$steps)))
```

* Number of missing values: `r numMissingValues`

```{r}
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```


## 7. Histogram of the total number of steps taken each day after missing values are imputed
```{r}
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

```{r}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```
* Mean (Imputed): `r stepsByDayMeanImputed`
* Median (Imputed):  `r stepsByDayMedianImputed`

----

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

