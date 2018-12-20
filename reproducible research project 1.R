#Reproducible Research Week 2 project 1

#download the data

#download the Data Set and Unzip it #
filename<-"repdata%2Fdata%2Factivity.zip"
fileurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists(filename)){
  download.file(fileurl, filename)
}
unzip(zipfile = filename)

#read data into csv

activitydata<-read.csv("activity.csv")

#use aggregate to find the sum of steps per day and store result as a new data frame called stepsperday

stepsperday<-aggregate(steps~date,data=activitydata,sum)

#make histogram of steps per day

with(stepsperday,hist(steps, main= "Histogram of Steps Per Day", xlab="Steps Per Day" ))

#Calculate the Mean Steps per day

Meanstepsperday<-mean(stepsperday$steps)

#report mean steps per day

print(paste("The mean steps per day are", Meanstepsperday))

#Calculate the Median Steps per day

Medianstepsperday<-median(stepsperday$steps)

print(paste("The Median steps per day are",Medianstepsperday))

#make time series plot of average steps per 5 minute interval

stepsperinterval<-aggregate(steps~interval,data=activitydata,mean)

#rename the "steps" column to "mean.steps"

names(stepsperinterval)[names(stepsperinterval)=='steps']<-'mean.steps'

#plot mean steps vs interval

with(stepsperinterval,plot(interval,mean.steps, type="l", xlab = "5 minute interval", ylab="Average Steps Per Interval", main="Average Steps Per Interval Over a 5-Day Period"))

#find interval with maximum number of steps on average day

maxsteps<-stepsperinterval[which(stepsperinterval$mean.steps==max(stepsperinterval$mean.steps)),]

print(paste("The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps,",maxsteps[1,2],", is interval number",maxsteps[1,1]))

#Deterimine the number of missing values for the dataset

activityNAs<-sum(is.na(activitydata$steps))

#report the number of missing values for the dataset

print(paste("The number of missing values is ",activityNAs))

#replace NAs with average steps

###looking into using ddply function for this here's the ddply from someone I reviewed 
# mean_intervals <- ddply(activity_data_tidy, .(interval), summarise, mean_steps = mean(steps))
# new_activity_data <- activity_data
# for (i in 1:nrow(new_activity_data)) {
#   if (is.na(new_activity_data$steps[i])) {
#     interval_value <- new_activity_data$interval[i]
#     new_steps_value <- mean_intervals[mean_intervals$interval == interval_value,]
#     new_activity_data$steps[i] <- new_steps_value$mean_steps
#   }
# }
# sum(is.na(new_activity_data))

splitactivity<-split(activitydata,activitydata$interval)


# I'm going to use the NA2mean function from G. Grothendieck here https://stackoverflow.com/questions/25835643/replace-missing-values-with-column-mean

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))


#use a for loop to apply the Na2mean to all of the data frames in the split activitydata dataframe 


suppressWarnings(for ( i in 1:length(splitactivity)){
  splitactivity[[i]] <- lapply(splitactivity[[i]], NA2mean)
  splitactivity[[i]] <- as.data.frame(splitactivity[[i]])
}
)

#use do.call to "unsplit" the splitactivity data set and store in a new variable, activitydataNAfree

activitydataNAfree<-do.call("rbind",splitactivity)

#find steps per day in activitydataNAfree
stepsperdayNAfree<-aggregate(steps~date,data=activitydataNAfree,sum)

#make histogram of steps per day without NAs

with(stepsperdayNAfree,hist(steps, main= "Histogram of Steps Per Day", xlab="Steps Per Day" ))

#Calculate the Mean Steps per day

MeanstepsperdayNAfree<-mean(stepsperdayNAfree$steps)

#report mean steps per day

print(paste("The mean steps per day are", MeanstepsperdayNAfree))

#Calculate the Median Steps per day

MedianstepsperdayNAfree<-median(stepsperdayNAfree$steps)

print(paste("The Median steps per day are",MedianstepsperdayNAfree))

#Report difference between mean and median calculated with and without NAs included

differenceinMeans<-MeanstepsperdayNAfree-Meanstepsperday

differenceinMedians<-MedianstepsperdayNAfree-Medianstepsperday

print(paste("When NAs aren't included in the data set the difference steps per day is", differenceinMeans, ", and the difference between median steps per day is", differenceinMedians))

#Are there differences between weekdays and weekends


#convert dates to days of the week

activitydataNAfree$date<-weekdays(as.Date(activitydataNAfree$date))

#convert weekdays to either weekend or weekday

activitydataNAfree$date<-lapply(activitydataNAfree$date, function(x) 
  if (x == "Saturday"| x=="Sunday"){
  x <- "weekend"
  }else {
  x<- "weekday"
  }
)
    
#convert character variable into factor variable (using lapply turned the column into a list so I need to use unlist to convert it to a vector)

activitydataNAfree$date<-as.factor(unlist(activitydataNAfree$date))

#Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

weekendvsweekdayaveragestepsperinterval<-aggregate(data=activitydataNAfree,.~date+interval, mean)

qplot(interval,steps,data=weekendvsweekdayaveragestepsperinterval,facets =. ~date, main = "Average Steps Per Interval on week days and weekday days", ylab="Average Number of Steps per Interval", xlab="5-minute interval number",geom=c("point","line"))
