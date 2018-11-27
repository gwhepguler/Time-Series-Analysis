# R-Pipeline-Forecasting-TimeSeries
# Title: Visualize + Analyze data --Household Electric Submeters
#
# Last update: 2018.07.27
# Updated by : Greg Hepguler

# File: R-Pipeline-Forecasting-TimeSeries_upd0727.R

###############
# Project Notes
###############

# FORECASTING TASKS:

# TSLM Forecast
# Decompose
# HoltWinters

###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory #  ?getwd  # >> get help
# MyDir <- getwd()     
getwd()

# set working directory
# setwd(MyDir)
MyDir <- "D:/GH14/DATA-SCIENCE/RProjects"   # Set working directory #
setwd(MyDir)
dir()

################
# Load packages
################

# R Packages for Time Series Tutorials ------------------------------------------
#
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("zoo")
install.packages("tidyr")
install.packages("plotly")
install.packages("ggfortify")

install.packages("forecast")

# load library -----------------------------------------
library(ggplot2)
library(zoo)

library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(tidyr)
library(plotly)
library(ggfortify)

library(forecast)

###############
# Load dataset 
###############

hhpwr <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(hhpwr)
str(hhpwr)

##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")

class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "America/New_York"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%d/%m/%Y")

str(hhpwrDT)

##------- Change data types---------##

# Note: Understand the difference between as.numeric(as.character()) and as.numeric()

hhpwrDT$Global_active_power <- as.numeric(as.character(hhpwr$Global_active_power))
hhpwrDT$Global_reactive_power <- as.numeric(as.character(hhpwr$Global_reactive_power))
hhpwrDT$Voltage <- as.numeric(as.character(hhpwr$Voltage))
hhpwrDT$Global_intensity <- as.numeric(as.character(hhpwr$Global_intensity))
hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))
str(hhpwrDT)


## ------ Evaluate NA values ----------##

# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Count the number of values = NA
sum(is.na(hhpwrDT$Sub_metering_1)) # Review any metadata with dataset

###############
# TSLM Forecast
###############

# FORECAST (1) ------------------------------

# create subset for 3 years
# Total kWh per ESM3 by year 

esm3 <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(hhpwrDT$Sub_metering_3, Year==2007 | Year==2008 | Year==2009) %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

esm3
# A tibble: 3 x 3
# Year   SM3 DateTime           
# <dbl> <dbl> <dttm>             
# 1 2007. 3023. 2007-01-01 09:19:00
# 2 2008. 3179. 2008-01-01 00:00:00
# 3 2009. 3557. 2009-01-01 06:10:00

# Create TS object with SubMeter3
esmts <- ts(esm3$SM3, frequency=1, start=c(2007,1))

# autoplot(esmts)
# plot.ts(esmts)

# Fit lm and Forecast
# Apply time series linear regression to the sub-meter 3 ts object 
fitesm3 <- tslm(esmts ~ trend)
summary(fitesm3)

fcastesm3 <- forecast(fitesm3, h=2)
fcastesm3
# SM3
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2010       3786.881 3278.680 4295.082 1688.773 5884.989
# 2011       4053.869 3381.582 4726.156 1278.333 6829.405

## Plot the forecast for sub-meter 3. 
plot(fcastesm3, col=c("blue", "red"), plot.type = "s", ylab="kwh", xlab="Year", 
     main="YEARLY ESM3 Data 2007 - 2009 and Forecast 2010 - 2011")


# FORECAST (2) ------------------------------

esmfc <- hhpwrDT %>%
      mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
      mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
      filter(Date > "2006-12-31" & Date < "2010-11-01") %>%  # Nov-10 is incomplete 
      group_by(Year, Month) %>%  # Group data by Month
      summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
           DateTime = first(DateTime) )  # To verify date of first instance
esmfc
# A tibble: 46 x 4
# Groups:   Year [?]
# Year Month   SM3 DateTime           
# <dbl> <dbl> <dbl> <dttm>             
# 1 2007.    1.  330. 2007-01-01 09:19:00
# 2 2007.    2.  270. 2007-02-01 05:37:00
# 3 2007.    3.  290. 2007-03-01 05:09:00
# 4 2007.    4.  190. 2007-04-01 00:00:00
# 5 2007.    5.  229. 2007-05-01 05:53:00
# 6 2007.    6.  189. 2007-06-01 06:16:00
# 7 2007.    7.  155. 2007-07-01 00:00:00
# 8 2007.    8.  225. 2007-08-01 04:23:00
# 9 2007.    9.  226. 2007-09-01 06:47:00
# 10 2007.  10.  256. 2007-10-01 00:00:00
# ... with 36 more rows

# tail(esmfc, 10)
tail(esmfc)
# A tibble: 6 x 4
# Groups:   Year [1]
# Year Month   SM3 DateTime           
# <dbl> <dbl> <dbl> <dttm>             
# 1 2010.    5.  365. 2010-05-01 00:00:00
# 2 2010.    6.  307. 2010-06-01 00:01:00
# 3 2010.    7.  193. 2010-07-01 00:00:00
# 4 2010.    8.  160. 2010-08-01 00:00:00
# 5 2010.    9.  258. 2010-09-01 00:00:00
# 6 2010.   10.  316. 2010-10-01 00:01:00

# Create TS object with SubMeter3
timesm <- ts(esmfc$SM3, frequency=12, start=c(2007,1), end=c(2010,10))
timesm
# Jan     Feb     Mar     Apr     May     Jun     Jul     Aug     Sep     Oct     Nov     Dec
# 2007 329.578 270.274 290.361 189.503 229.448 188.851 154.815 225.442 226.375 256.080 299.690 362.423
# 2008 312.175 255.918 279.542 295.678 290.620 290.103 227.228  79.665 284.282 275.887 280.615 307.346
# 2009 329.606 296.166 328.697 307.840 311.048 259.969 187.936 192.064 296.547 327.505 335.529 383.909
# 2010 395.913 411.714 324.312 336.091 364.625 306.934 192.912 160.189 257.861 315.669 

# Fit lm and Forecast
# Apply time series linear regression to the sub-meter 3 ts object 
fitsm3 <- tslm(timesm ~ trend + season)
summary(fitsm3)

fcastsm3 <- forecast(fitsm3, h=14)
fcastsm3

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Nov 2010       351.1559 292.2140 410.0978 259.4569 442.8550
# Dec 2010       397.1039 338.1620 456.0458 305.4049 488.8030
# Jan 2011       399.1654 341.1026 457.2283 308.8340 489.4969
# Feb 2011       365.8654 307.8026 423.9283 275.5340 456.1969
# Mar 2011       363.0754 305.0126 421.1383 272.7440 453.4069
# ....
# Sep 2011       323.6137 265.5508 381.6765 233.2823 413.9451
# Oct 2011       351.1327 293.0698 409.1955 260.8013 441.4641
# Nov 2011       374.0949 313.2450 434.9448 279.4275 468.7623
# Dec 2011       420.0429 359.1930 480.8928 325.3755 514.7103

## Plot the forecast for sub-meter 3. 
plot(fcastsm3, col=c("green", "red"), plot.type = "s", ylab="kwh", xlab="Year", 
     main="MONTHLY ESM3 Data: Jan-07 - Oct-10 & FORECAST: Nov-10 - Dec-11")

###########
# Decompose
###########

# TASK 1 ----------- Decompose Sub-meter 3 into trend, seasonal and remainder -------------
SM3comps <- decompose(timesm)
SM3comps

## Plot decomposed sub-meter 3 
plot(SM3comps)

## Check summary statistics for decomposed sub-meter 3 
summary(SM3comps)

# Length Class  Mode     
# x        46     ts     numeric  
# seasonal 46     ts     numeric  
# trend    46     ts     numeric  
# random   46     ts     numeric  
# figure   12     -none- numeric  
# type      1     -none- character

# TASK 2 ----------- Subset Feb-10 kWh per hour -------------------------------------------

SS0210 <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
  mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
  mutate(Hour= hour(DateTime)) %>%      # Add col Hour using dplyr and lubridate functions
  filter(hhpwrDT$Sub_metering_3, (Year==2010 & Month == 2) ) %>%
  group_by(Day, Hour) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime) )  # To verify date of first instance

SS0210
# A tibble: 672 x 4
# Groups:   Day [?]
# Day  Hour    SM3 DateTime           
# <int> <int>  <dbl> <dttm>             
# 1     1     0 0.0410 2010-02-01 00:00:00
# 2     1     1 0.0410 2010-02-01 01:00:00
# 3     1     2 0.306  2010-02-01 02:00:00
# 4     1     3 0.431  2010-02-01 03:00:00
# 5     1     4 0.0410 2010-02-01 04:00:00
# 6     1     5 0.0410 2010-02-01 05:00:00
# 7     1     6 0.407  2010-02-01 06:00:00
# 8     1     7 1.15   2010-02-01 07:00:00
# 9     1     8 1.12   2010-02-01 08:00:00
# 10    1     9 1.11   2010-02-01 09:00:00
# ... with 662 more rows

tail (SS0210)
# A tibble: 6 x 4
# Groups:   Day [1]
# Day  Hour    SM3 DateTime           
# <int> <int>  <dbl> <dttm>             
# 1    28    18 0.0380 2010-02-28 18:01:00
# 2    28    19 0.0390 2010-02-28 19:00:00
# 3    28    20 0.0730 2010-02-28 20:00:00
# 4    28    21 0.0390 2010-02-28 21:00:00
# 5    28    22 0.0390 2010-02-28 22:01:00
# 6    28    23 0.0390 2010-02-28 23:00:00

Hoursm <- ts(SS0210$SM3, frequency = 24)

Hoursm
head(Hoursm)
# Time Series:
# Start = c(1, 1) 
# End = c(2, 3) 
# Frequency = 3 
# [1] 0.041 0.041 0.306 0.431 0.041 0.041

tail(Hoursm)
# Time Series:
# Start = c(670, 2) 
# End = c(672, 1) 
# Frequency = 3 
# [1] 0.039 0.040 0.038 0.039 0.073 0.039

fitcomps2 <- decompose(Hoursm)
fitcomps2

# Check summary statistics for decomposed sub-meter 3 
summary(fitcomps2)

# Length Class  Mode     
# x        672    ts     numeric  
# seasonal 672    ts     numeric  
# trend    672    ts     numeric  
# random   672    ts     numeric  
# figure    24    -none- numeric  
# type       1    -none- character

#plot all 28*24 hours
plot(fitcomps2)  

###########
# HW
###########

# Create subset >> kWh by day for each season, win, Spr, Smr, Fal 2010 -----------------

WinSub <- hhpwrDT %>%
      mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
      mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
      mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
      filter(hhpwrDT$Sub_metering_3, (Year == 2009 & Month == 12) | (Year==2010 & Month == 1) |  
             (Year==2010 & Month == 2)) %>%
      group_by(Year, Month, Day) %>%  # Group data by Day
      summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime) )  # To verify date of first instance

SprSub <- hhpwrDT %>%
      mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
      mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
      mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
      mutate(Qtr = quarter(DateTime)) %>%
      filter(hhpwrDT$Sub_metering_3, (Year==2010) & (Month == 3 | Month == 4 | Month == 5)) %>%
      group_by(Year, Month, Day) %>%  # Group data by Day
      summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
                DateTime = first(DateTime) )  # To verify date of first instance

SmrSub <- hhpwrDT %>%
      mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
      mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
      mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
      mutate(Qtr = quarter(DateTime)) %>%
      filter(hhpwrDT$Sub_metering_3, (Year==2010) & (Month == 6 | Month == 7 | Month == 8)) %>%
      group_by(Year, Month, Day) %>%  # Group data by Day
      summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
                DateTime = first(DateTime) )  # To verify date of first instance

FalSub <- hhpwrDT %>%
      mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
      mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
      mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
      mutate(Qtr = quarter(DateTime)) %>%
      filter(hhpwrDT$Sub_metering_3, (Year==2010) & (Month == 9 | Month == 10 | Month == 11)) %>%
      group_by(Year, Month, Day) %>%  # Group data by Day
      summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
                DateTime = first(DateTime) )  # To verify date of first instance

# Check subset results ------------------------------------------------------------

WinSub
# A tibble: 89 x 5
# Groups:   Year, Month [?]
# Year Month   Day   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dttm>             
# 1 2009.   12.     1 10.8  2009-12-01 00:00:00
# 2 2009.   12.     2 12.9  2009-12-02 00:00:00
# 3 2009.   12.     3  9.91 2009-12-03 00:01:00
# 4 2009.   12.     4  9.13 2009-12-04 00:00:00
# 5 2009.   12.     5 13.3  2009-12-05 00:01:00
# 6 2009.   12.     6 12.4  2009-12-06 00:00:00
# 7 2009.   12.     7 10.3  2009-12-07 00:00:00
# 8 2009.   12.     8 15.4  2009-12-08 00:00:00
# 9 2009.   12.     9  7.97 2009-12-09 00:00:00
#10 2009.   12.    10 15.4  2009-12-10 00:01:00
# ... with 79 more rows

SprSub
# A tibble: 92 x 5
# Groups:   Year, Month [?]
# Year Month   Day   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dttm>             
# 1 2010.    3.     1  7.12 2010-03-01 00:00:00
# 2 2010.    3.     2  3.60 2010-03-02 00:01:00
# 3 2010.    3.     3  7.22 2010-03-03 00:01:00
# 4 2010.    3.     4  2.93 2010-03-04 00:01:00
# 5 2010.    3.     5  5.92 2010-03-05 00:00:00
# 6 2010.    3.     6  3.97 2010-03-06 00:00:00
# 7 2010.    3.     7  3.68 2010-03-07 00:00:00
# ...
#10 2010.    3.    10 14.9  2010-03-10 00:00:00
# ... with 82 more rows

SmrSub
# A tibble: 88 x 5
# Groups:   Year, Month [?]
# Year Month   Day   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dttm>             
# 1 2010.    6.     1 11.9  2010-06-01 00:01:00
# 2 2010.    6.     2  8.45 2010-06-02 00:00:00
# 3 2010.    6.     3 12.3  2010-06-03 00:01:00
# 4 2010.    6.     4 12.5  2010-06-04 00:00:00
# ... 
# 9 2010.    6.     9  8.26 2010-06-09 00:01:00
#10 2010.    6.    10 10.9  2010-06-10 00:00:00
# ... with 78 more rows

FalSub
# A tibble: 85 x 5
# Groups:   Year, Month [?]
# Year Month   Day   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dttm>             
# 1 2010.    9.     1 14.0  2010-09-01 00:00:00
# 2 2010.    9.     2 11.8  2010-09-02 00:01:00
# 3 2010.    9.     3  9.75 2010-09-03 00:01:00
# 4 2010.    9.     4 13.2  2010-09-04 00:01:00
# .... 
# 9 2010.    9.     9  7.02 2010-09-09 00:01:00
#10 2010.    9.    10 10.8  2010-09-10 00:01:00
# ... with 75 more rows

tail(FalSub)
# A tibble: 6 x 5
# Groups:   Year, Month [1]
# Year Month   Day   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dttm>             
# 1 2010.   11.    21  4.78 2010-11-21 02:00:00
# 2 2010.   11.    22 10.1  2010-11-22 06:43:00
# 3 2010.   11.    23  7.61 2010-11-23 06:46:00
# 4 2010.   11.    24 12.2  2010-11-24 02:48:00
# 5 2010.   11.    25  5.07 2010-11-25 04:55:00
# 6 2010.   11.    26  9.99 2010-11-26 06:43:00


# Create TS objects for SubMeter3  -------------------------------------------------

Win <- ts(WinSub$SM3, start = 1, end = 90, freq = 7)
Win
Spr <- ts(SprSub$SM3, start = 1, end = 90, freq = 7)
Spr
Smr <- ts(SmrSub$SM3, start = 1, end = 90, freq = 7)
Smr
Fal <- ts(FalSub$SM3, start = 1, end = 90, freq = 7)
Fal

# Fit lm ---------------------------------------------------------------------------
Winfit <- tslm(Win ~ trend)
summary(Winfit)
Sprfit <- tslm(Spr ~ trend)
summary(Sprfit)
Smrfit <- tslm(Smr ~ trend)
summary(Smrfit)
Falfit <- tslm(Fal ~ trend)
summary(Falfit)

# Forecast -------------------------------------------------------------------------

Winfct <- forecast(Winfit, h=210)
Winfct

Sprfct <- forecast(Sprfit, h=210)
Sprfct

Smrfct <- forecast(Smrfit, h=210)
Smrfct

Falfct <- forecast(Falfit, h=210)
Falfct

# Plot fit and forecast objects ------------------------------------------------------

plot(Winfct, col=c("red", "red"), plot.type = "s", ylab="Kwh", xlab="Date", main="Win Forecast (lm)")
plot(Sprfct, col="green", plot.type = "s", ylab="Kwh", xlab="Date", main="Spr Forecast (lm)")
plot(Smrfct, col="blue", plot.type = "s", ylab="Kwh", xlab="Date", main="Smr Forecast (lm)")
plot(Falfct, col="black", plot.type = "s", ylab="Kwh", xlab="Date", main="Fal Forecast (lm)")

# Decompose --------------------------------------------------------------------------

compWin <- decompose(Win)
summary(compWin)
# Length Class  Mode     
# x        268    ts     numeric  
# seasonal 268    ts     numeric  
# trend    268    ts     numeric  
# random   268    ts     numeric  
# figure     3    -none- numeric  
# type       1    -none- character
plot(compWin)

compSpr <- decompose(Spr)
summary(compSpr)
# Length Class  Mode     
# x        268    ts     numeric  
# seasonal 268    ts     numeric  
# trend    268    ts     numeric  
# random   268    ts     numeric  
# figure     3    -none- numeric  
# type       1    -none- character
plot(compSpr)

compSmr <- decompose(Smr)
summary(compSmr)
# Length Class  Mode     
# x        268    ts     numeric  
# seasonal 268    ts     numeric  
# trend    268    ts     numeric  
# random   268    ts     numeric  
# figure     3    -none- numeric  
# type       1    -none- character
plot(compSmr)

compFal <- decompose(Fal)
summary(compFal)
# Length Class  Mode     
# x        268    ts     numeric  
# seasonal 268    ts     numeric  
# trend    268    ts     numeric  
# random   268    ts     numeric  
# figure     3    -none- numeric  
# type       1    -none- character
plot(compFal)

# Remove Seasonal Components --------------------------------------------------------

WinAdj <- Win - compWin$seasonal
SprAdj <- Spr - compSpr$seasonal
SmrAdj <- Smr - compSmr$seasonal
FalAdj <- Fal - compFal$seasonal

# HoltWinters ------------------------------------------------------------------------

# HoltWinters Simple Exponential Smoothing   ----

Win_HWfor <- HoltWinters(WinAdj, beta=FALSE, gamma=FALSE)
Win_HWfor           # List variable
plot(Win_HWfor)
Win_HWfor$fitted    # HW forecast are stored in a named element of list variable called “fitted”
Win_HWfor$SSE       # the sum of squared errors for the in-sample forecast errors
# [1] 9038.294

Spr_HWfor <- HoltWinters(SprAdj, beta=FALSE, gamma=FALSE)
Spr_HWfor
plot(Spr_HWfor)
Spr_HWfor$fitted
Spr_HWfor$SSE       # the sum of squared errors for the in-sample forecast errors
# [1] 7864.381

Smr_HWfor <- HoltWinters(SmrAdj, beta=FALSE, gamma=FALSE)
Smr_HWfor
plot(Smr_HWfor)
Smr_HWfor$fitted
Smr_HWfor$SSE       # the sum of squared errors for the in-sample forecast errors
# [1] 2786.25

Fal_HWfor <- HoltWinters(FalAdj, beta=FALSE, gamma=FALSE)
Fal_HWfor
plot(Fal_HWfor)
Fal_HWfor$fitted
Fal_HWfor$SSE       # the sum of squared errors for the in-sample forecast errors
#[1]  6566.432

# HoltWinters Forecast and Plots  -----------------------------------------------

Win_HWF <- forecast(Win_HWfor, h=210)
Win_HWF
plot(Win_HWF, col=c("blue", "red"), plot.type = "s", ylab="Kwh", xlab="Days - Sub-meter 3", main="Win HW Forecast")

Spr_HWF <- forecast(Spr_HWfor, h=210)
Spr_HWF
plot(Spr_HWF, col=c("blue", "red"), plot.type = "s", ylab="Kwh", xlab="Days - Sub-meter 3", main="Spr HW Forecast")

Smr_HWF <- forecast(Smr_HWfor, h=210)
Smr_HWF
plot(Smr_HWF, col=c("blue", "red"), plot.type = "s", ylab="Kwh", xlab="Days - Sub-meter 3", main="Smr HW Forecast")

Fal_HWF <- forecast(Fal_HWfor, h=210)
Fal_HWF
plot(Fal_HWF, col=c("blue", "red"), plot.type = "s", ylab="Kwh", xlab="Days - Sub-meter 3", main="Fal HW Forecast")

# Ljung-Box test ------------------------------------

Box.test(Win_HWF$residuals, lag=20, type="Ljung-Box")
# 	Box-Ljung test
# data:  Win_HWF$residuals
# X-squared = 133.49, df = 20, p-value < 2.2e-16

Box.test(Spr_HWF$residuals, lag=20, type="Ljung-Box")
# Box-Ljung test
# data:  Spr_HWF$residuals
# X-squared = 196.89, df = 20, p-value < 2.2e-16

Box.test(Smr_HWF$residuals, lag=20, type="Ljung-Box")
# Box-Ljung test
# data:  Smr_HWF$residuals
# X-squared = 118.02, df = 20, p-value = 6.661e-16

Box.test(Fal_HWF$residuals, lag=20, type="Ljung-Box")
# Box-Ljung test
# data:  Fal_HWF$residuals
# X-squared = 71.233, df = 20, p-value = 1.144e-07


