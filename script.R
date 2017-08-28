# ========================Loading and Reading Data=======================
unzip(zipfile = "activity.zip")
activity_mon_data <- read.csv('activity.csv')
#=================================================================================

#--------------Histogram of Total steps Taken each day----------------------------
steps_by_date <- split(activity_mon_data$steps, activity_mon_data$date)
total_steps_by_date <- sapply(steps_by_date, sum, na.rm = TRUE)
library(ggplot2)
qplot(total_steps_by_date,binwidth=1000)
dev.copy(png, "plot1.png")
dev.off()
#---------------------------------------------------------------------------------

#-------------Mean and median number of steps taken each day----------------------
mean_steps <- mean(total_steps_by_date, na.rm = TRUE)
median_steps <- median(total_steps_by_date, na.rm =TRUE)
#---------------------------------------------------------------------------------

#-----------Time series plot of the average number of steps taken-----------------
averages <- aggregate(x=list(steps = activity_mon_data$steps), by=list(interval = activity_mon_data$interval),
                      FUN=mean, na.rm=TRUE)
g <- ggplot(averages, aes(interval,steps))
g + 
  geom_line() +
  ggtitle("Time series plot of the average number of steps taken")
dev.copy(png, "plot2.png")
dev.off()
#---------------------------------------------------------------------------------

#The 5-minute interval that, on average, contains the maximum number of steps-----
maximum <- max(as.integer(averages$steps))
int_max <- averages$interval[as.integer(averages$steps) == maximum]
#---------------------------------------------------------------------------------

#----Code to describe and show a strategy for imputing missing data---------------
missing <- is.na(activity_mon_data$steps)
total_missing <- sum(missing)
total_missing
#Replacing missing data with average at that interval
i<-1
imputed_data <- activity_mon_data
while(i<= nrow(imputed_data)){
  if(is.na(imputed_data[i,1]))
  {
    interval_missing <- imputed_data[i,3]
    imputed_data[i,1] <- averages[averages$interval == interval_missing,2]
  }
  i <- i + 1
}
steps_by_date <- split(imputed_data$steps, imputed_data$date)
imp_total_steps <- sapply(steps_by_date, sum, na.rm = TRUE)
par(mfrow = c(1,2), mar = c(2,2,2,2))
hist(imp_total_steps, main = "Histogram for imputed data",xlab = "Total steps")
hist(total_steps_by_date, main = "Histogram for data",xlab = "Total steps")
imp_mean_steps <- mean(imp_total_steps, na.rm = TRUE)
imp_median_steps <- median(imp_total_steps, na.rm =TRUE)
dev.copy(png, "plot3.png")
dev.off()
#---------------------------------------------------------------------------------

#Panel plot comparing the average number of steps taken per 5-minute interval-----
#----------------across weekdays and weekend--------------------------------------
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday",  "Friday")
imputed_data$dow = as.factor(ifelse(is.element(
                          weekdays(as.Date(imputed_data$date)),
                                                          weekdays),
                                    "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, 
       main="Average Steps per Day by Interval",
       xlab="Interval",
       ylab="Steps",
       layout=c(1,2), 
       type="l")
dev.copy(png, "plot4.png")
dev.off()
