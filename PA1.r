# Loading and preprocessing the data
setwd("~\\GitHub\\RepData_PeerAssessment1")
unzip("activity.zip")
a = read.csv("activity.csv", stringsAsFactors = FALSE)
a$date=as.Date(a$date, format = "%Y-%m-%d")
summary(a)
file.remove("activity.csv")

# What is mean total number of steps taken per day?
b = sapply(split(a$steps,a$date),sum, na.rm = TRUE)
hist(b, main = "Histogram of steps", xlab = "Number of steps per day")
mean(b)
median(b)

# What is the average daily activity pattern?
cc = sapply(split(a$steps,a$interval),mean, na.rm = TRUE)
plot(names(cc), cc, type = "l", xlab = "Interval in the day", ylab = "Average steps per interval", main = "Daily step pattern")
sort(cc, decreasing = TRUE)[1]

# Imputing missing values
sum(!complete.cases(a))
d = a
for (i in 1:nrow(d))
{
	if (is.na(d[i,1]))
	{
		dd = subset(a,a$interval == d$interval[i])
		d[i,1]=mean(dd$steps, na.rm = TRUE)
	}
}
summary(d)
b = sapply(split(d$steps,d$date),sum)
hist(b, main = "Histogram of steps", xlab = "Number of steps per day")
mean(b)
median(b)

# Are there differences in activity patterns between weekdays and weekends?
d$weekday = !(weekdays(d$date)=="Saturday" | weekdays(d$date)=="Sunday")
d$weekday = factor(d$weekday, labels = c("weekend", "weekday"))
str(d)
par(mfcol = 1:2)
e = split(d,d$weekday)
c1 = sapply(split(e$weekend$steps,e$weekend$interval),mean)
plot(names(c1), c1, type = "l", xlab = "Interval in the day", ylab = "Average steps per interval", main = "Weekend step pattern")
c2 = sapply(split(e$weekday$steps,e$weekday$interval),mean)
plot(names(c2), c2, type = "l", xlab = "Interval in the day", ylab = "Average steps per interval", main = "Weekday step pattern")