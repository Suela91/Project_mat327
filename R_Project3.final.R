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
# I checked if I did the changes right
sports

# Here I computed the linear regression model
attendace.lm<-lm(fri~tot_attend, data=sports)

# and I displayed the coefficients
attendace.lm

# I plotted a sccater plot for of the tot_attend(x) v. fri(y)
plot(sports$tot_attend, sports$fri, xlab="# of total attendance", ylab="# of Monday attendance", main="Sport Attendance")

# and I added a line to my scatter plot 
abline(attendace.lm,lwd=2,col="red")

#Here I ploted a histogram of the residuals
hist(resid(attendace.lm), xlab = "Residual", ylab = "Frequency", main = "Residuals for Sport Attendance Linear Model")

# then I tried to compute the true value of the total attendance
plot(sports$tot_attend, resid(attendace.lm), xlab = "Actual # of Attendance", ylab = "Residuals", main="Residuals for tot_attend ~ Mon")

#and again I added a line to my plot.
abline(h=0)

# I used summary to find the R-squared and look other important information
summary(attendace.lm)

-------------------------------------------------------------------------------------------------
# Here I computed a t.test to compute my hypothesis test of Monday attendance v. Friday attendance
t.test(sports$mon, sports$fri)

#again I used t.test to compute another hypothesis test comparing Friday attendance with Saturday attendance. 
t.test(sports$fri, sports$sat)