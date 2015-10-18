
# install libraries
install.packages("lattice")
install.packages("knitr")
install.packages("data.table")
install.packages("ggplot2")

# start libraries
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

library(data.table)
library(ggplot2)
library(lattice)

# set my working directory (usb)
setwd("F:/github/RepData_PeerAssessment1")

# read data
rdata <- read.csv('activity/activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))

rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
str(rdata)

steps_per_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)

ggplot(steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "green", binwidth = 1000) + 
  labs(title="Histogram of Steps per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Total)") + theme_bw() 

steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_mean

steps_median <- median(steps_per_day$steps, na.rm=TRUE)
steps_median

steps_per_interval <- aggregate(rdata$steps, 
                                by = list(interval = rdata$interval),
                                FUN=mean, na.rm=TRUE)

steps_per_interval$interval <- 
  as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
  geom_line(color="orange", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
  theme_bw()

max_interval <- steps_per_interval[which.max(  
  steps_per_interval$steps),]
max_interval

missing_vals <- sum(is.na(rdata$steps))

na_fill <- function(data, pervalue) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}

rdata_fill <- data.frame(  
  steps = na_fill(rdata, steps_per_interval),  
  date = rdata$date,  
  interval = rdata$interval)
str(rdata_fill)

sum(is.na(rdata_fill$steps))

fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

ggplot(fill_steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_mean_fill

steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill

rdata$dow <-  ifelse(as.POSIXlt(rdata_fill$date)$wday %in% c(0,6), 'weekend', 'weekday')

steps_by_interval_i <- aggregate(steps ~ interval + dow, rdata, mean)


xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, 
       main="Average Steps per Day by Interval",
       xlab="Interval",
       ylab="Steps",
       layout=c(1,2),
       type="l")
