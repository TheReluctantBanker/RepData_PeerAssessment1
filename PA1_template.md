---
output:
  html_document:
    keep_md: yes
---
##Reproducible Research Project1

###Loading the dataset and initial inspection

```r
#dataset located in the working directory
activity.data=read.csv(file="activity.csv")
#inspecting and initial checks in the dataset
dim(activity.data)
```

```
## [1] 17568     3
```

```r
names(activity.data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity.data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

###transforming data

```r
#transforming the date variable from factor type to date type
activity.data$date=as.Date(activity.data$date,format="%Y-%m-%d")
```
###total steps each day

```r
#question1:What is mean total number of steps taken per day?
#missing values are ignored for this analysis
total.data=aggregate(activity.data$steps,by=list(activity.data$date),FUN=sum,na.rm=TRUE)

names(total.data)=c("date","total.steps")
#plotting a histogram of total.steps
hist(total.data$total.steps,main="Distribution of total steps each day",xlab=" total steps")
```

![plot of chunk total_steps](figure/total_steps-1.png) 

```r
mean.total.steps=mean(total.data$total.steps)
median.total.steps=median(total.data$total.steps)
print(paste0("mean of total steps is  ", round(mean.total.steps,2)))
```

```
## [1] "mean of total steps is  9354.23"
```

```r
print(paste0("median of total steps is  ", median.total.steps))
```

```
## [1] "median of total steps is  10395"
```

###analysing daily activity

```r
mean.each.interval=aggregate(activity.data$steps,by=list(activity.data$interval),FUN=mean,na.rm=TRUE)
names(mean.each.interval)=c("interval","average.steps")
plot(mean.each.interval$interval,mean.each.interval$average.steps,main="average steps in each interval",xlab="interval",ylab="average no of steps",type="l")
```

![plot of chunk daily_activity](figure/daily_activity-1.png) 

```r
print(paste0("the 5 minute interval having the maximum of average steps is ", mean.each.interval$interval[which.max(mean.each.interval$average.steps)], " th interval"))
```

```
## [1] "the 5 minute interval having the maximum of average steps is 835 th interval"
```

###imputing missing values

```r
#reporting missing values
print("Variable steps has missing values. Other variables do not have missing values")
```

```
## [1] "Variable steps has missing values. Other variables do not have missing values"
```

```r
num.missing=sum(is.na(activity.data$steps))
print(paste0("variable steps has " , num.missing, "  missing values"))
```

```
## [1] "variable steps has 2304  missing values"
```

```r
#imputing missing values
#data missing for an interval in a particular day is imputed with the median for the day
mean.data=aggregate(activity.data$steps,by=list(activity.data$date),FUN=mean,na.rm=TRUE)
names(mean.data)=c("date","mean.value")

#there are some dates for which all intervals have missing observations.
#such observations need to be imputed with the grand mean, i.e., mean of the total dataset
#other observations are imputed with the mean for that particular day

grand.mean=mean(mean.data$mean.value,na.rm=TRUE)
for (i in 1:nrow(mean.data))
  {
  if(is.nan(mean.data[i,2]))
    {
    mean.data[i,2] = grand.mean
    }
  }

#doing the imputation here by looping through the dataset, whereever encounter
# missing value for steps, replace it with median for that day

for (i in 1:nrow(activity.data))
    
  {
   if (!is.na(activity.data[i,1]))
 {
}
 else
   {
   activity.data[i,1]=mean.data$mean.value[mean.data$date==activity.data[i,2]]
} 
   
}
#imputing of missing values done
#summary of the dataset with imputed values
summary(activity.data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
imputed.total.data=aggregate(activity.data$steps,by=list(activity.data$date),FUN=sum,na.rm=TRUE)
names(imputed.total.data)=c("date","imputed.value")
hist(imputed.total.data$imputed.value,main="using imputed data", xlab="total steps")
```

![plot of chunk imputing](figure/imputing-1.png) 

```r
new.mean=mean(imputed.total.data$imputed.value)
new.median=median(imputed.total.data$imputed.value)
print(round(new.mean,2))
```

```
## [1] 10766.19
```

```r
print(new.median)
```

```
## [1] 10766.19
```

```r
print("Values obtained after imputation are different from the earlier values. The values obtained after imputation give a higher mean and higher median")
```

```
## [1] "Values obtained after imputation are different from the earlier values. The values obtained after imputation give a higher mean and higher median"
```

```r
print("distribution of the data has become almost normal")
```

```
## [1] "distribution of the data has become almost normal"
```

###comparing activity on weekend versus weekdays

```r
# are there difference in activity patterns between weekdays and weekends
# dataset with imputed values will be used here
#using weekdays() function to create new factor variable for whether weekday or weekend
activity.data$dayofweek=weekdays(activity.data$date)

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#determining rows with weekdays and weekends -using logical indexing
weekday.data=activity.data[(activity.data$dayofweek=="Monday"|activity.data$dayofweek=="Tuesday"|activity.data$dayofweek=="Wednesday"|activity.data$dayofweek=="Thursday"|activity.data$dayofweek=="Friday"),]
                           
weekend.data=activity.data[(activity.data$dayofweek=="Saturday"|activity.data$dayofweek=="Sunday"),] 

#obtaining the average for each interval - in the weekday data and weekend data
weekday.average=aggregate(weekday.data$steps,by=list(weekday.data$interval),FUN=mean,na.rm=TRUE)
weekend.average=aggregate(weekend.data$steps,by=list(weekend.data$interval),FUN=mean,na.rm=TRUE)

#renaming column names of the aggregated dataframes obtained above
names(weekday.average)=c("interval","average.value")
names(weekend.average)=c("interval","average.value")

#adding final column - flag ( weekday and weekend)
weekday.average$flag="weekday"
weekend.average$flag="weekend"

#rbinding the above two datasets to get a consolidated dataset
final.data=rbind(weekday.average,weekend.average)

#drawing panel plot using lattice package
library(lattice)
par(mfrow=c(2,1))
xyplot(average.value~interval |flag,data=final.data,type="l",layout=c(1,2),xlab="interval",ylab="Number of steps")
```

![plot of chunk comparing](figure/comparing-1.png) 

```r
print("there is a difference in the activity level between weekend and weekdays")
```

```
## [1] "there is a difference in the activity level between weekend and weekdays"
```

```r
print("weekend shows a consistently higher activity level compared to weekdays")
```

```
## [1] "weekend shows a consistently higher activity level compared to weekdays"
```

```r
print("It implies there is greater physical activity/movement during weekends")
```

```
## [1] "It implies there is greater physical activity/movement during weekends"
```
