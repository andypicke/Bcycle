# How Does Weather Affect Denver Bcycle Usage?
Andy Pickering  
Aug 1, 2017  

## Introduction


## Bcycle Data
- <https://github.com/andypicke/Bcycle>


```r
rm(list=ls())
#setwd("/Users/Andy/Bcycle/")
library(ggplot2)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
```


First read in the data for 2015, which I have downloaded already from <https://denver.bcycle.com/company>. Note: I tried to read in the xlsx file using the 'xlsx' package, but it didn't work. Instead I just opened excel and saved the file as a csv.


```r
bcyc<-read.csv("data/Bcyc2015.csv")
head(bcyc)
```

```
##   User.s.Program User.ID   Zip              Membership.Type Bike
## 1 Denver B-cycle  253201 80202      Annual (Denver B-cycle)  212
## 2 Denver B-cycle  120679 80209      Annual (Denver B-cycle)    9
## 3 Denver B-cycle 1027135 60439      Annual (Denver B-cycle)  322
## 4 Denver B-cycle  986934 80203 Annual Plus (Denver B-cycle)  482
## 5 Denver B-cycle  130156 80204      Annual (Denver B-cycle)  466
## 6 Denver B-cycle 1051678 80211     24-hour (Denver B-cycle)  611
##   Checkout.Date Checkout.Time      Checkout.Kiosk Return.Date Return.Time
## 1      12/31/15   11:51:00 PM        32nd & Pecos    12/31/15 11:57:00 PM
## 2      12/31/15   11:29:00 PM     18th & Arapahoe    12/31/15 11:35:00 PM
## 3      12/31/15   10:50:00 PM     16th & Broadway    12/31/15 10:59:00 PM
## 4      12/31/15   10:41:00 PM 22nd & Pennsylvania    12/31/15 10:49:00 PM
## 5      12/31/15    9:38:00 PM      9th & Santa Fe    12/31/15  9:48:00 PM
## 6      12/31/15    9:18:00 PM 16th & Little Raven    12/31/15  9:32:00 PM
##        Return.Kiosk Duration..Minutes.
## 1    15th & Delgany                  6
## 2   25th & Lawrence                  6
## 3       17th & Race                  9
## 4   33rd & Arapahoe                  8
## 5    1st & Broadway                 10
## 6 Broadway & Walnut                 14
```


How many rides are contained in this dataset?

```r
nr<-nrow(bcyc)
nr
```

```
## [1] 363002
```



```r
str(bcyc)
```

```
## 'data.frame':	363002 obs. of  12 variables:
##  $ User.s.Program    : Factor w/ 18 levels "ArborBike","Austin B-cycle",..: 6 6 6 6 6 6 6 6 7 3 ...
##  $ User.ID           : int  253201 120679 1027135 986934 130156 1051678 313863 395197 253997 254005 ...
##  $ Zip               : Factor w/ 7820 levels "","0","1","10000",..: 5767 5774 4044 5768 5769 5777 5770 5796 5777 5777 ...
##  $ Membership.Type   : Factor w/ 26 levels "24-hour (Denver B-cycle)",..: 6 6 6 21 6 1 21 6 7 26 ...
##  $ Bike              : Factor w/ 743 levels "10","100","101",..: 105 697 208 360 346 489 103 479 74 446 ...
##  $ Checkout.Date     : Factor w/ 365 levels "1/1/15","1/10/15",..: 117 117 117 117 117 117 117 117 117 117 ...
##  $ Checkout.Time     : Factor w/ 1144 levels "1:00:00 PM","1:01:00 PM",..: 284 240 162 144 1102 1062 1050 1048 1046 1046 ...
##  $ Checkout.Kiosk    : Factor w/ 87 levels "10th & Osage",..: 50 34 22 43 60 23 45 38 23 23 ...
##  $ Return.Date       : Factor w/ 366 levels "1/1/15","1/1/16",..: 118 118 118 118 118 118 118 2 118 118 ...
##  $ Return.Time       : Factor w/ 1323 levels "1:00:00 AM","1:00:00 PM",..: 351 307 235 215 1301 1269 1237 1038 1231 1231 ...
##  $ Return.Kiosk      : Factor w/ 92 levels "10th & Osage",..: 20 45 32 51 39 63 51 38 24 24 ...
##  $ Duration..Minutes.: int  6 6 9 8 10 14 4 626 3 3 ...
```




```r
# add a new column of class Posixct with date/time comined
bcyc$dt_chkout<-as.POSIXct( strptime(paste(bcyc$Checkout.Date,bcyc$Checkout.Time),"%m/%d/%y %H:%M:%S"))
bcyc$dt_ret<-as.POSIXct( strptime(paste(bcyc$Return.Date,bcyc$Return.Time),"%m/%d/%y %H:%M:%S"))

bcyc$month <- month(bcyc$dt_chkout)
```



```r
bcyc %>% group_by(month) %>% count() %>% ggplot(aes(x=month,y=n)) +geom_point() + geom_line() + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

```
## Warning: Removed 1 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](BcycleDenver_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



## Weather Data
So we can see that the total rides peaks around August, and is lowest around December. This is probably related to the weather, let's get some weather data and check this out. I'm using data downloaded from <https://www.wunderground.com>.


```r
# Daily weather data for 2015
url<-"https://www.wunderground.com/history/airport/KDEN/2015/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2015&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"

download.file(url,"DenWeather2015.csv")
wea<-read.csv("DenWeather2015.csv")

wea$MST <- as.Date(wea$MST,"%Y-%m-%d")
wea$month <- month(wea$MS)

# in Precip "T" is trace I think; change to zero for analysis
idT<-which(wea$PrecipitationIn=="T")
wea$PrecipitationIn[idT]<-"0.00"
wea$PrecipitationIn <- as.numeric(as.character(wea$PrecipitationIn))

#wea %>% mutate(month = month(MST))

head(wea)
```

```
##          MST Max.TemperatureF Mean.TemperatureF Min.TemperatureF
## 1 2015-01-01               26                16                5
## 2 2015-01-02               35                23               11
## 3 2015-01-03               35                15               -5
## 4 2015-01-04               36                13              -10
## 5 2015-01-05               56                26               -5
## 6 2015-01-06               49                35               20
##   Max.Dew.PointF MeanDew.PointF Min.DewpointF Max.Humidity Mean.Humidity
## 1             19              9            -8           92            67
## 2             22             14             9           96            79
## 3             25             11            -7           92            77
## 4             13              2           -13           91            65
## 5             34             20            -6           95            67
## 6             36             29            19           92            73
##   Min.Humidity Max.Sea.Level.PressureIn Mean.Sea.Level.PressureIn
## 1           42                    30.22                     30.13
## 2           61                    30.17                     30.02
## 3           61                    30.40                     30.05
## 4           38                    30.51                     30.42
## 5           38                    30.53                     30.17
## 6           53                    30.64                     30.35
##   Min.Sea.Level.PressureIn Max.VisibilityMiles Mean.VisibilityMiles
## 1                    29.99                  10                    6
## 2                    29.79                  10                   10
## 3                    29.79                  10                    6
## 4                    30.20                  10                   10
## 5                    29.99                  10                    9
## 6                    30.22                  10                    8
##   Min.VisibilityMiles Max.Wind.SpeedMPH Mean.Wind.SpeedMPH
## 1                   0                15                  8
## 2                   7                17                 10
## 3                   0                32                 12
## 4                   6                15                  7
## 5                   4                37                 15
## 6                   0                30                  9
##   Max.Gust.SpeedMPH PrecipitationIn CloudCover   Events
## 1                18            0.08          5 Fog-Snow
## 2                22            0.00          2         
## 3                37            0.08          6 Fog-Snow
## 4                19            0.00          5         
## 5                46            0.00          5         
## 6                39            0.00          6 Fog-Snow
##   WindDirDegrees.br... month
## 1            230<br />     1
## 2            203<br />     1
## 3             47<br />     1
## 4            221<br />     1
## 5            278<br />     1
## 6             81<br />     1
```



```r
wea %>% group_by(month) %>% summarise(Tavg = mean(`Mean.TemperatureF`)) %>% ggplot(aes(x=month,y=Tavg)) + geom_point() + geom_line() + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

![](BcycleDenver_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Comparison of weather and Bcycle data 
- The MaxTemp seasonal cycle looks very similar to the month ride totals. Let's make some scatterplots to better see the correlation between weather variables and the number of rides.



### Monthly



```r
bcyc_monthly <- bcyc %>% group_by(month) %>% count()
wea_monthly  <- wea %>% group_by(month) %>% summarise(tavg=mean(`Mean.TemperatureF`,na.rm=TRUE))
month_merge  <- merge(bcyc_monthly,wea_monthly) 
ggplot(month_merge,aes(x=tavg,y=n))+geom_point() + geom_smooth(method = "lm")
```

![](BcycleDenver_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



### Daily


```r
bcyc$yday <- yday(bcyc$dt_chkout)
wea$yday <- yday(wea$MST)

bcyc %>% group_by(yday) %>%count()%>% ggplot(aes(yday,n)) + geom_point() + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

```
## Warning: Removed 1 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](BcycleDenver_files/figure-html/Compute Daily Rides-1.png)<!-- -->

```r
wea %>% ggplot(aes(yday,`Mean.TemperatureF`)) + geom_point() + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

![](BcycleDenver_files/figure-html/Compute Daily Rides-2.png)<!-- -->



At the daily level, the seasonal pattern is the same but there is a lot more variability, especially in the winter/spring.  
  

### Rides vs Temperature 

```r
bcyc_daily <- bcyc %>% group_by(yday) %>% count()
yday_merge <- merge(bcyc_daily,wea) %>% select(yday,n,`Mean.TemperatureF`)
yday_merge %>% ggplot(aes(Mean.TemperatureF,n)) + geom_point() +  geom_smooth(col='red') + geom_smooth(method = "lm")
```

```
## `geom_smooth()` using method = 'loess'
```

![](BcycleDenver_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


### Ride duration vs temperature.


```r
# Need to add the daily temp to each bcyc ride
for (i in seq_along(yday(wea$MST))){
        ig<-which(bcyc$yday == yday(wea$MST)[i])
        bcyc$temp[ig]<-wea$Max.TemperatureF[i]
}


tlim<-40
par(mfrow=c(1,3))
ig <- which(bcyc$temp<30 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig],main="temp<30",xlab="Duration (min)")
rug(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")

ig <- which(bcyc$temp>30 & bcyc$temp<60 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig],main="30<temp<60",xlab="Duration (min)")
rug(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")

ig <- which( bcyc$temp>60 & bcyc$Duration..Minutes.<tlim)
hist(bcyc$Duration..Minutes.[ig],main="temp>60",xlab="Duration (min)")
rug(bcyc$Duration..Minutes.[ig])
abline(v=median(bcyc$Duration..Minutes.[ig]),col="red")
abline(v=mean(bcyc$Duration..Minutes.[ig]),col="blue")
```

![](BcycleDenver_files/figure-html/Ride Durations for different temp ranges-1.png)<!-- -->


## Conclusions:  
- The total number of Denver Bcycle rides has a strong seasonal cycle, peaking around August and minimum around January.  
- The total number of Denver Bcycle rides per month is strongly correlated with the monthly mean of max temperatures.  
- Below about 30 deg and above 80 deg, the number of rides is less dependent on further decreasing(increasing) temperature.  
- The mean and median ride durations tend to be larger for increasing temperatures.  

##  Follow-up Questions:  
- Do all years look the same?  
- Does the relationship between weather and rides look different for different types of passes (ie annual vs 24 hour)?  
- Is there a stronger correlation with precip on shorter timescales (hourly?)?

