
#I loaded the Summer_Sports_Experience data into variable "sports"
library(readr)
sports <- read_csv("Summer_Sports_Experience.csv")
View(sports)

#I changed the name of my columns into simpler names and I used dplyr to do so.
library(dplyr)
sports<-sports%>%
rename(
borough="Borough Location",
park="Park Location",
games="Sports Played",
start="Week Start Date",
end="Week End Date",
sun="Sunday's Attendance",
mon="Monday's Attendance",
tue="Tuesday's Attendance",
wed="Wednesday's Attendance",
Thu="Thursday's Attendance",
fri="Friday's Attendance",
sat="Saturday's Attendance",
tot_attend="Attendance Sum"
)

# This part is an update of "MILESTONE 1", based on my professors feedback I was able to make a barplot,
# and include all park locations where sport activities where held, and I  but gave a title and axis labels.
x <- barplot(table(sports$park), xaxt="n", main="Park Location", ylab="Frequency Played",xlab="Names of Park Location")
labs <- paste(names(table(sports$park)), ".")
text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45)

# Scatterplot of Friday Attendance (x axis) vx. Total Weekly Attendance (y axis),
# and I gave a title and axis labels to my scatterplot.
plot(tot_attend ~ fri, data = sports, main= "Scatterplot of Friday attendance v. Total Weekly Attendance", xlab="Friday attendance", ylab="Total Attendance")


# Here I calculated the correlation between friday attendance and total attendance.
cor(sports$tot_attend, sports$fri)

------------------------------------------------------------------------------------------------------
# Here i Computed the 95% confidence interval for the mean of the tot_attendance,
# when no people partecipate on sport activities on Friday's. 

  
# Here I  created a new data frame including only fradys where no people have partecipated in sport activities
nopeople<-subset(sports, fri==0)

# I checked my new data
nopeople

# Here I computed my sample mean
xbar<- mean(nopeople$tot_attend)
xbar

# Store the sample size in n, I founded on the dataframe 151x13 
n<-151

# Here I computed a t-value
t<- -qt(0.025,n-1)
t

# I also comuted a sample standard deviation
s<-sd(nopeople$tot_attend)
s

# Finally I computed the lower and upper bounds of the 95% confidence interval
xbar + t*s/sqrt(n)
xbar - t*s/sqrt(n)

# I also computed the z-interval
z<-qnorm(0.975)
z
xbar - z*s/sqrt(n)
xbar + z*s/sqrt(n)

--------------------------------------------------------------------------------
#Here I found the interval by using all data, including also the days on Friday where no one partecipated in sport activities.

#I computed the mean  
xbar<-mean(sports$tot_attend)
xbar

# I used n the number of all rows of my data 309x13
n<-309

#I computate the t-value
t<- -qt(0.025,n-1)
t

#And I computed the Standard Deviation
s<-sd(sports$tot_attend)
s

#I found the interval
xbar + t*s/sqrt(n)
xbar - t*s/sqrt(n)

#Finally, I also found the z-intervals
z<-qnorm(0.975)
z
xbar - z*s/sqrt(n)
xbar + z*s/sqrt(n)


