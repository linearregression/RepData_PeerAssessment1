---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1
For background of project, please refer to [README.md](https://github.com/linearregression/RepData_PeerAssessment1/blob/master/README.md)

## Download data and unzip locally

```r
zipData <- './activity.zip'
if(!file.exists("./activity.zip")) {
   print('Downloading activity.zip ....')   
   download.file(url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', destfile=zipData, method='curl')
}
if(!file.exists("./activity.csv")) {
   print('Unzip activity.zip ....')  
   unzip(zipfile=zipData, overwrite=TRUE)
}
```
## Loading and preprocessing the data
Load data from csv

```r
Sys.setlocale("LC_TIME", "English")
```

```
## Warning: OS reports request to set locale to "English" cannot be honored
```

```
## [1] ""
```

```r
fitdata<- read.csv(file='activity.csv', header=TRUE, stringsAsFactor=FALSE, na.strings = "NA", colClasses=c("integer", "Date", "integer"))
```
## What is mean total number of steps taken per day?

Frequency distribution of daily total of steps taken. Missing data dropped.


```r
stepsFreq <- aggregate(fitdata$steps, by=list(fitdata$date), sum, na.rm=TRUE)
colnames(stepsFreq) <- c('date','sum')
hist(stepsFreq$sum, xlab='Daily Steps taken', ylab='Occurence', main='Frequency distribution of daily total number of steps', col='cyan')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 
In additional to the histogram above, group steps by date to vizualize pattern by date. Missing data dropped.

```r
library(plyr)
library(dplyr)
library(ggplot2)
#fitdata <-na.omit(fitdata)
sumfitdata <- ddply(fitdata,~date, summarise, stepssum=sum(steps, na.rm=TRUE)) 
p <- ggplot(data = sumfitdata, mapping = aes(x=date, y=stepssum)) +
     xlab('Date') +
     ylab('Total Steps') +
     ggtitle('Total Number Steps per Date')
p2 <- p +layer(geom = 'histogram', geom_params = list(fill='green',color = 'steelblue'), stat = 'identity', stat_params = list(binwidth=5))
print(p2)
```

![plot of chunk stepsum](figure/stepsum.png) 
 - Mean number of steps taken per day:

```r
oldmean <- mean(sumfitdata$stepssum)
```
 - Median number of steps taken per day:


```r
oldmedian <- median(sumfitdata$stepssum)
```

## What is the average daily activity pattern

Mean number of steps per group by intervals across all days are calculated.
Maximum value of such is also calculated. 

Following is a frequency distribution of Mean number steps per 5 min interval:


```r
library(plyr)
library(dplyr)
library(ggplot2)
activitydata <- ddply(fitdata,c('interval'), summarise, 
     mean=mean(steps, na.rm=TRUE)) 
p <- ggplot(data = activitydata, mapping = aes(x=interval, y=mean)) +
     xlab('Interval (min)') +
     ylab('Average Steps') +
     ggtitle('Daily Activity Pattern') 
p <- p+layer(geom="line", stat="identity", geom_params = list(color='red'))
print(p)
```

![plot of chunk dailyactvity](figure/dailyactvity.png) 

Interval where Max Mean happens and the corresponding max mean:


```r
# Find index on interval axis has the max mean number of steps
index<- which.max(activitydata$mean)
# Interval
activitydata$interval[index]
```

```
## [1] 835
```

```r
# Max Mean Steps
activitydata$mean[index]
```

```
## [1] 206.2
```


## Imputing missing values

Total number of missing data:


```r
sum(!complete.cases(fitdata))
```

```
## [1] 2304
```
Strategy to replace missing data. 
Try out replace missing data with mean and median per corresponding interval, then compare changes.
Since mean is senstive to extreme values, try out median.


```r
fitdata2<- read.csv(file='activity.csv', header=TRUE, stringsAsFactor=FALSE, na.strings = "NA", colClasses=c("integer", "Date", "integer"))
# Calculate mean and median number of steps per interval

stepmeanbyinterval<- aggregate(steps~interval, data=fitdata2,FUN=mean)
stepsmedianbyinterval <- aggregate(steps~interval, data=fitdata2,FUN=median)
# Replace NA data with mean
fitdata2ByMean <- fitdata2 %>% group_by(interval) %>% mutate(steps= replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))

stepsFreqByMean <- aggregate(fitdata2ByMean$steps, by=list(fitdata2ByMean$date), sum, na.rm=TRUE)
colnames(stepsFreqByMean) <- c('date','sum')
hist(stepsFreqByMean$sum, xlab='Daily Steps taken', ylab='Occurence', main='Replacement of Missing Value as Mean', col='red')
```

![plot of chunk meanReplacement](figure/meanReplacement1.png) 

```r
meanbymean <- mean(stepsFreqByMean$sum)
medianbymean <- median(stepsFreqByMean$sum)
#Difference vs data ignoring NA
meanbymean - oldmean
```

```
## [1] 1412
```

```r
medianbymean - oldmedian
```

```
## [1] 371.2
```

```r
#Try this again with Replacemen with Median per that interval
fitdata2ByMedian <- fitdata2 %>% group_by(interval) %>% mutate(steps= replace(steps, is.na(steps), median(steps, na.rm=TRUE)))
stepsFreqByMedian <- aggregate(fitdata2ByMedian$steps, by=list(fitdata2ByMedian$date), sum, na.rm=TRUE)
colnames(stepsFreqByMedian) <- c('date','sum')
hist(stepsFreqByMedian$sum, xlab='Daily Steps taken', ylab='Occurence', main='Replacement of Missing Value as Median', col='yellow')
```

![plot of chunk meanReplacement](figure/meanReplacement2.png) 

```r
meanbymedian <-mean(stepsFreqByMedian$sum)
medianbymedian <- median(stepsFreqByMedian$sum)
#Difference vs data ignoring NA
meanbymedian - oldmean
```

```
## [1] 149.6
```

```r
medianbymedian - oldmedian
```

```
## [1] 0
```
Effect no much difference.


## Are there differences in activity patterns between weekdays and weekends?


```r
library(lubridate)
Sys.setlocale("LC_TIME", "English")
```

```
## Warning: OS reports request to set locale to "English" cannot be honored
```

```
## [1] ""
```

```r
fitdata<-na.omit(fitdata)
#Index of rows with weekdays and weekendsrespectively
fitdays <- weekdays(as.Date(fitdata$date))
weekdays <- (fitdays == 'Saturday') | (fitdays == 'Sunday')
weekends <- !weekdays

weekdaydata <- ddply(weekdays,c('interval', 'date'), summarise, mean=mean(steps))
```

```
## Error: missing value where TRUE/FALSE needed
```

```r
weekenddata <- ddply(weekend,c('interval', 'date'), summarise, mean=mean(steps))
```

```
## Error: object 'weekend' not found
```

```r
data2.weekday <- data2[data2$weekday == "weekday", ]
```

```
## Error: object 'data2' not found
```

```r
data2.weekend <- data2[data2$weekday == "weekend", ]
```

```
## Error: object 'data2' not found
```

```r
par(mfrow=c(2,1))
plot(x=timeHM_formatter(ActivityByInterval.weekday$Hour_Minute),
     y=ActivityByInterval.weekday$Avg_Steps, 
     type="l", 
     main="Weekday Activities", 
     xlab="5 Minute Daily Intervals", ylab="Mean Steps", 
     ylim=c(0,250))
```

```
## Error: could not find function "timeHM_formatter"
```

```r
plot(x=timeHM_formatter(ActivityByInterval.weekend$Hour_Minute),
     y=ActivityByInterval.weekend$Avg_Steps, 
     type="l",
     main="Weekend Activites", 
     xlab="5 Minute Daily Intervals", ylab="Mean Steps",
     ylim=c(0,250))
```

```
## Error: could not find function "timeHM_formatter"
```

