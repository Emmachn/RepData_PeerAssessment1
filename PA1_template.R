setwd('/Users/emmasun/Desktop/data science/reproducible research')
library(ggplot2)
#######loading data######
rawdata <- read.csv("activity.csv")
######What is mean total number of steps taken per day?######
total_steps <- tapply(rawdata$steps, rawdata$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken per day")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)


#######What is the average daily activity pattern?######
Aver <- aggregate(x=list(steps=rawdata$steps), by=list(interval=rawdata$interval), FUN=mean, na.rm=TRUE)
ggplot(data=Aver, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken")
Aver[which.max(Aver$steps),]


######Imputing missing values#####
missing <- is.na(rawdata$steps)
#####the total number of missing####
table(missing)
#####Replacemissing values withmean value of its 5-minute interval#####
subti_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (Aver[Aver$interval==interval, "steps"])
  return(filled)
}
filled_data <- rawdata
filled_data$steps <- mapply(subti_value, filled_data$steps, filled_data$interval)


####the impact of imputing missing data on the estimates of the total daily number of steps?#####
total.steps <- tapply(filled_data$steps, filled_data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps)
median(total_steps)


#####Are there differences in activity patterns between weekdays and weekends?#####
weekday_or_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled_data$date <- as.Date(filled_data$date)
filled_data$day <- sapply(filled_data$date, FUN=weekday_or_weekend)


Aver <- aggregate(steps ~ interval + day, data=filled_data, mean)
ggplot(Aver, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")

