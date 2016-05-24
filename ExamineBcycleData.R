
# how many stations are there?
# which stations have the most rides?
# how long is a typical ride?
# what does the total # rides vs time look like?
# what is the average daily pattern of rides?
# is the # rides correlated with weather?

rm(list=ls())
setwd("/Users/Andy/Bcycle/")
library(ggplot2)
library(lubridate)

# url for the Bcycle data (2015)
#url<-"https://denver.bcycle.com/docs/librariesprovider34/default-document-library/2015denverbcycletripdata_public.xlsx?sfvrsn=2"

# Download file
#download.file(url,"Bcyc2015.xlsx")

# Read into R
bcyc<-read.csv("Bcyc2015.csv")

# Quick look at the data
dim(bcyc)
head(bcyc)
str(bcyc)

# Check format of dates and time
class(bcyc$Checkout.Date)
class(bcyc$Checkout.Time)

# Add a new column of class Posixct with date/time combined
bcyc$dt_chkout<-as.POSIXct( strptime(paste(bcyc$Checkout.Date,bcyc$Checkout.Time),"%m/%d/%y %H:%M:%S"))
bcyc$dt_ret<-as.POSIXct( strptime(paste(bcyc$Return.Date,bcyc$Return.Time),"%m/%d/%y %H:%M:%S"))

# Add a months variable
bcyc$month <- months(bcyc$dt_chkout)

# Month list
month_list <- c("January","February","March","April","May","June","July","August","September","October","November","December")

# Calculate the total # rides by month
tot_rides_month <- vector(mode='numeric',length=12)
for (i in seq_along(month_list)){
        a<-which(bcyc$month==month_list[i])
        tot_rides_month[i] <- length(a)
}

# Make a new dataframe w/ monthy rides
bcyc_monthly <- data.frame(rides=tot_rides_month,month=month_list,monthID=1:12)

plot(1:12,tot_rides_month,xlab="Month",ylab="Total Rides",main="Total Rides by Month: 2015",type="l")

#library(ggplot2)
qplot(1:12,tot_rides_month,geom=c("point","line","smooth"),xlab="Month",ylab="Total Rides",main="Total Rides by Month: 2015")

# so rides peak in aug/sept


## Get weather data for Denver

# daily for 1 year
url<-"https://www.wunderground.com/history/airport/KDEN/2015/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2015&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"

download.file(url,"DenWeather2015.csv")

wea<-read.csv("DenWeather2015.csv")
wea$MST <- as.Date(wea$MST,"%Y-%m-%d")
wea$month <- months(wea$MST)

# in Precip "T" is trace I think; change to zero for analysis
idT<-which(wea$PrecipitationIn=="T")
wea$PrecipitationIn[idT]<-"0.00"
wea$PrecipitationIn <- as.numeric(as.character(wea$PrecipitationIn))

# compute mean weather values per month
maxtemp_mean_month <- vector(mode='numeric',length=12)
mintemp_mean_month <- vector(mode='numeric',length=12)
precip_mean_month <- vector(mode='numeric',length=12)
maxwind_mean_month <- vector(mode='numeric',length=12)

for (i in seq_along(month_list)){
        a<-which(wea$month==month_list[i])
        maxtemp_mean_month[i] <- mean(wea$Max.TemperatureF[a])
        mintemp_mean_month[i] <- mean(wea$Min.TemperatureF[a],na.rm = TRUE)
        precip_mean_month[i] <- mean(wea$PrecipitationIn[a],na.rm = TRUE)
        maxwind_mean_month[i] <- mean(wea$Max.Wind.SpeedMPH[a],na.rm = TRUE)
}

# Make a data frame w/ monthly values
W_mon=data.frame(precip=precip_mean_month,maxtemp=maxtemp_mean_month,mintemp=mintemp_mean_month,maxwind=maxwind_mean_month,monthID=1:12,month=month_list)

qplot(W_mon$monthID,W_mon$maxwind,geom=c("point","smooth"))
qplot(W_mon$monthID,W_mon$precip,geom=c("point","smooth"))

qplot(W_mon$maxtemp,tot_rides_month,geom=c("point"))
qplot(W_mon$precip,tot_rides_month,geom=c("point"))
qplot(W_mon$maxwind,tot_rides_month,geom=c("point"))






# Now do same analysis, but split up by user pass type

# Make a list of the unique pass types in the dataset
uniq_pass_types <- unique(bcyc$Membership.Type)

# I'll separate into annual and non-annual types
# so I want types that start with "Annual"
ia<-grep("Annual",uniq_pass_types)
iday<-grep("24-hour",uniq_pass_types)

bcyc_annual=subset(bcyc,Membership.Type %in% uniq_pass_types[ia])
bcyc_day=subset(bcyc,Membership.Type %in% uniq_pass_types[iday])

# Now let's compute the monthly total rides again for these two groups separately 


tot_rides_month_annual <- vector(mode='numeric',length=12)
tot_rides_month_day <- vector(mode='numeric',length=12)
for (i in seq_along(month_list)){
        a<-which(bcyc_annual$month==month_list[i])
        tot_rides_month_annual[i] <- length(a)
        rm(a)
        
        a<-which(bcyc_day$month==month_list[i])
        tot_rides_month_day[i] <- length(a)
}

par(mfrow=c(2,1))
plot(1:12,tot_rides_month_annual/sum(tot_rides_month),xlab="Month",ylab="Total Rides",main="Annual Passes",type="l")
plot(1:12,tot_rides_month_day/sum(tot_rides_month_day),xlab="Month",ylab="Total Rides",main="24 Hr Pases",type="l")

par(mfrow=c(2,1))
plot(maxtemp_mean_month,tot_rides_month_annual)
#qplot(maxtemp_mean_month,tot_rides_month_annual)
#qplot(maxtemp_mean_month,tot_rides_month_annual)
plot(maxtemp_mean_month,tot_rides_month_day)


# The 24hr passes peak a little earlier (August), while annual passes peak in September. Both are strongly correlated with the max temp.

## What if we look at the daily patterns?

library(lubridate)
bcyc$yday <- yday(bcyc$dt_chkout)
uniq_yday <- sort(unique(bcyc$yday))

# Make an empty vector to store results in
tot_rides_daily <- vector(mode="numeric",length=length(uniq_yday))

for (i in seq_along(uniq_yday)) {
        rm(a)
        a<-which(bcyc$yday==uniq_yday[i])
        tot_rides_daily[i] <- length(a)
}

#plot(uniq_yday,tot_rides_daily,xlab="Yearday",ylab="Total Rides",main="Daily #total rides")

qplot(uniq_yday,tot_rides_daily,xlab="Yearday",ylab="Total Rides",main="Daily total rides",geom=c("point","smooth"))

# similar pattern, but there is more variability in the first 110 days. 
# would be cool to overlay monthly values on above plot

# Plot daily temperature
plot(yday(wea$MST),wea$Max.TemperatureF)

qplot(wea$Max.TemperatureF,tot_rides_daily,xlab="Max Temp",ylab="Total Rides",geom=c("point","smooth"))

qplot(wea$Max.TemperatureF,tot_rides_daily,xlab="Max Temp",ylab="Total Rides",geom=c("point","smooth"))

# seems to be a cutoff around 30 below that temp the total rides don't vary much?

qplot(wea$PrecipitationIn,tot_rides_daily,xlab="Precip",ylab="Total Rides",geom=c("point"))

qplot(wea$Max.Wind.SpeedMPH,tot_rides_daily,xlab="Max Wind",ylab="Total Rides",geom=c("point"))

## Try to make histogram of ride durations, for different ranges of temp

ig <- which(bcyc$Duration..Minutes.<150 )
hist(bcyc$Duration..Minutes.[ig],main="Trip Duration (<150mins)",xlab="Minutes")

# Need to add the daily temp to each bcyc ride
for (i in seq_along(yday(wea$MST))){
        ig<-which(bcyc$yday == yday(wea$MST)[i])
        bcyc$temp[ig]<-wea$Max.TemperatureF[i]
}

qplot(bcyc$yday,bcyc$temp)


tlim<-60
par(mfrow=c(1,3))
ig <- which(bcyc$temp<30 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")
median(bcyc$Duration..Minutes.[ig])
mean(bcyc$Duration..Minutes.[ig])

ig <- which(bcyc$temp>30 & bcyc$temp<60 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")
median(bcyc$Duration..Minutes.[ig])
mean(bcyc$Duration..Minutes.[ig])

ig <- which( bcyc$temp>60 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")
median(bcyc$Duration..Minutes.[ig])
mean(bcyc$Duration..Minutes.[ig])


# compute total # rides by station
tot_rides_sta <- tapply(a$)

# how many bikes are there?
uniq_bikes <- unique(a$Bike)
length(uniq_bikes)

# compute total rides by bike
tot_rides_bike <- tapply(a$)


boxplot(a$Duration..Minutes.)
ig <- which(bcyc$Duration..Minutes.<200)
hist(bcyc$Duration..Minutes.[ig],main="Trip Duration (mins)",xlab="Minutes")
boxplot(a$Duration..Minutes.[ig])
hist(bcyc$Duration..Minutes.[ig],main="Trip Duration (mins)",xlab="Minutes")
#rug(a$Duration..Minutes.[ig])

dt<-as.POSIXct( strptime(paste(a$Checkout.Date,a$Checkout.Time),"%m/%d/%y %H:%M:%S"))

# convert time from factor class
hist(dt)

# add a column of which day it is
a$day <- weekdays(dt)
hist(a$day)
b<-table(a$day)
plot(b/1000,ylab="Total Rides /1e3",xlab="day",main="Total Rides by Days")




# how many stations are there?
uniq_stat <- unique(a$Checkout.Kiosk)
length(uniq_stat)

