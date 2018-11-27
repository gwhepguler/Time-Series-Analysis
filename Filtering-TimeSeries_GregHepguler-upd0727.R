# R-Pipeline-Filtering-TimeSeries
# Title: Visualize + Analyze Energy Data --Household Electric Submeters
#
# Last update: 2018.07.26
# Updated by : Greg Hepguler

# File: R-Pipeline-Filtering-TimeSeries_GregHepguler_upd0714-v2.R

###############
# Project Notes
###############

####--- Filtering ---#
#  - 1. Create a subset that shows the total annual consumption (kWh) for each submeter over the Jan-07 thru Dec-09 period. 
#  - 2. Create a subset that shows the average daily consumption (kWh) for each submeter by weekday for the winter seasons
#       (Dec-Feb) over Jan-07 thru Oct-10 period. This subset should only have 7 values (one for each weekday - 
#       it reflects the typical usage per weekday during the winter season). 
#  - 3. Create a subset that shows the average hourly kWh used for each hour of the day during January 2010. 
#       This subset should only have 24 values (it reflects the typical usage per hour of day during Jan-10). 
#  - 4. Create a subset that shows consumption at 12:00pm (noon) for the 1st day of each month for 2009. 

####--- Plot ---#
#  Note: Use Plotly (or ggplot2). To create the plots, you may need to convert the subsets to long format.
#  - 1. Create a multi-variate side-by-side column plot for the annual ds above (#1) showing all submeters. 
#  - 2. Ditto for (#2,3,4) except create a line chart.
    
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
install.packages("scales")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("reshape2")
install.packages("zoo")
install.packages("tidyr")
install.packages("plotly")
install.packages("mime")

# R Packages for Time Series 01/03 Tutorial --------------------------------------
install.packages("ggmap")
install.packages("devtools")

# load library -----------------------------------------
library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(reshape2)
library(zoo)

library(ggmap)
library(devtools)

require(lubridate) # work with dates
require(dplyr)     # data manipulation (filter, summarize, mutate)
require(tidyr)
library(plotly)
library(mime)

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


## -------- Save pre-processed dataset --------##

# write.csv(hhpwrDT, file="Submeter-Filtering-DataFrame.csv")

#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): filter for time interval (Yr/Mo/Day) using lubridate w/in dplyr mutate() to create col.
# 2. dplyr::filter(): select cols to filter by; full ds + col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select desired time interval to subset
# 4. dplyr::summarize(): select which vars and any calculations for the vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 

# Note: For time intervals less than a year, you may need to create a main dataset for that time interval 
# (e.g., month, week, day), and then subset this main time interval subset to get the final results you need. 


#################
## DS ANNUAL (F1)
#################

# MAIN DS
# Total kWh per SM by year 

yr.sum.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009)  %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

yr.sum.wide
# Year   SM1   SM2   SM3 DateTime           
# <dbl> <dbl> <dbl> <dbl> <dttm>             
#  1 2007.  643.  854. 3023. 2007-01-01 00:00:00
#  2 2008.  584.  662. 3179. 2008-01-01 00:00:00
#  3 2009.  593.  592. 3557. 2009-01-01 00:00:00

names(yr.sum.wide)
class(yr.sum.wide)  

plot_ly(yr.sum.wide, x = ~yr.sum.wide$Year, y = ~yr.sum.wide$SM1, name = 'subm1', type = 'bar') %>%
  add_trace(y = ~yr.sum.wide$SM2,  name = 'subm2') %>%
  add_trace(y = ~yr.sum.wide$SM3,  name = 'subm3') %>%
  layout(title = " Yearly Power Consumption",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kilowatt-hours)"))

##################
## Weekly (F2)
##################

SMWeekDay <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
  mutate(WeekDay = weekdays(DateTime)) %>% 
  filter((Year == 2007 | Year == 2008 | Year == 2009 | Year == 2010 ) & 
           (Month == 1 | Month == 2 | Month == 12)) %>%  
  group_by(WeekDay) %>%  # Group data by Weekday
  summarize(SM1 = round(sum(Sub_metering_1 / 1000 /47, na.rm = TRUE),3), 
            SM2 = round(sum(Sub_metering_2 / 1000 /47, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000 /47, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

SMWeekDay$WeekDay <- factor(SMWeekDay$WeekDay , levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday", "Saturday"))
SMWeekDay <- SMWeekDay[order(SMWeekDay$WeekDay), ]
SMWeekDay
# A tibble: 7 x 5
# WeekDay     SM1   SM2   SM3 DateTime           
# <fct>     <dbl> <dbl> <dbl> <dttm>             
# 1 Sunday     3.08 3.18  10.00 2007-01-07 00:00:00
# 2 Monday     1.19 0.817 11.0  2007-01-01 00:00:00
# 3 Tuesday    1.12 2.07  11.3  2007-01-02 00:00:00
# 4 Wednesday  1.79 2.97  10.7  2007-01-03 00:00:00
# 5 Thursday   1.22 0.889 10.8  2007-01-04 00:00:00
# 6 Friday     1.31 1.43  11.4  2007-01-05 00:00:00
# 7 Saturday   3.22 2.96  12.6  2007-01-06 00:00:00
  
plot_ly(SMWeekDay, x = ~SMWeekDay$WeekDay, y = ~SMWeekDay$SM1, name = 'subm1', type = 'scatter',  
         mode = 'l') %>%
  add_trace(y = ~SMWeekDay$SM2, name = 'subm2', mode = 'l') %>%
  add_trace(y = ~SMWeekDay$SM3, name = 'subm3', mode = 'l') %>%
  layout(title = "Average Power Consumption on Week Days Dec - Jan",
         xaxis = list(title = "Week Day"),
         yaxis = list (title = "Power (kWh)"))

#####################
## Jan 2010 Hourly F3
#####################

Hourly_Jan2010 <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%     # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime)) %>%   # Add col Month using dplyr and lubridate functions
  mutate(Day = day(DateTime)) %>%       # Add col Day using dplyr and lubridate functions
  mutate(Hour= hour(DateTime)) %>%      # Add col Hour using dplyr and lubridate functions
  filter(Year==2010 & Month == 1 ) %>%
  group_by(Hour) %>%  # Group data by Hour
  summarize(SM1 = round(sum(Sub_metering_1 / 1000 /31, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000 /31, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000 /31, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Hourly_Jan2010
# A tibble: 24 x 5
#  Hour    SM1     SM2    SM3 DateTime           
#  <int>  <dbl>   <dbl>  <dbl> <dttm>             
#  1     0 0.0480 0.0170  0.382  2010-01-01 00:00:00
#  2     1 0.     0.0140  0.284  2010-01-01 01:00:00
#  3     2 0.     0.0190  0.193  2010-01-01 02:00:00
#  4     3 0.     0.0180  0.107  2010-01-01 03:00:00
#  5     4 0.     0.0170  0.0830 2010-01-01 04:00:00
#  6     5 0.     0.00900 0.0630 2010-01-01 05:00:00
#  7     6 0.     0.0110  0.362  2010-01-01 06:00:00
#  8     7 0.112  0.0120  0.774  2010-01-01 07:00:00
#  9     8 0.0470 0.0610  0.846  2010-01-01 08:00:00
#  10     9 0.0680 0.0720  0.892  2010-01-01 09:00:00
# ... with 14 more rows

Hourly_Jan2010[10:24,]

plot_ly(Hourly_Jan2010, x = ~Hour, y = ~Hourly_Jan2010$SM1, type = 'scatter', name = 'subm1', mode = 'lines') %>%
  add_trace(y = ~Hourly_Jan2010$SM2, name = 'subm2', mode = 'lines') %>%
  add_trace(y = ~Hourly_Jan2010$SM3, name = 'subm3', mode = 'lines') %>%
  layout(title = "Power Consumption Jan 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

######################
## 1st day of month F4
######################

hhpwrDT$Sub_metering_1[is.na(hhpwrDT$Sub_metering_1)] <- 0
hhpwrDT$Sub_metering_2[is.na(hhpwrDT$Sub_metering_2)] <- 0
hhpwrDT$Sub_metering_3[is.na(hhpwrDT$Sub_metering_3)] <- 0

smf <- hhpwrDT %>%
  mutate(Year  = year(DateTime))   %>%      # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime))  %>%     # Add col Month using dplyr and lubridate functions
  mutate(Day   = day(DateTime))    %>%      # Add col Day using dplyr and lubridate functions
  mutate(Hour  = hour(DateTime))   %>%      # Add col Hour using dplyr and lubridate functions
  mutate(Min   = minute(DateTime)) %>%    # Add col Min using dplyr and lubridate functions
  filter(Date == "2009-01-01" & Time == "12:00:00"  |
           Date == "2009-02-01" & Time == "12:00:00"  |
           Date == "2009-03-01" & Time == "12:00:00"  |
           Date == "2009-04-01" & Time == "12:00:00"  |
           Date == "2009-05-01" & Time == "12:00:00"  |
           Date == "2009-06-01" & Time == "12:00:00"  |
           Date == "2009-07-01" & Time == "12:00:00"  |
           Date == "2009-08-01" & Time == "12:00:00"  |
           Date == "2009-09-01" & Time == "12:00:00"  |
           Date == "2009-10-01" & Time == "12:00:00"  |
           Date == "2009-11-01" & Time == "12:00:00"  |
           Date == "2009-12-01" & Time == "12:00:00"  )    %>%
  group_by(Year, Month) %>%  # Group data by Time at Noon
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

smf
# A tibble: 12 x 6
# Groups:   Year [?]
#  Year Month     SM1     SM2     SM3 DateTime           
#  <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dttm>             
#  1 2009.    1. 0.      0.      0.      2009-01-01 12:00:00
#  2 2009.    2. 0.0380  0.      0.      2009-02-01 12:00:00
#  3 2009.    3. 0.00100 0.00200 0.0180  2009-03-01 12:00:00
#  4 2009.    4. 0.      0.      0.      2009-04-01 12:00:00
#  5 2009.    5. 0.      0.0390  0.00100 2009-05-01 12:00:00
#  6 2009.    6. 0.0390  0.      0.0180  2009-06-01 12:00:00
#  7 2009.    7. 0.      0.      0.00100 2009-07-01 12:00:00
#  8 2009.    8. 0.      0.      0.0180  2009-08-01 12:00:00
#  9 2009.    9. 0.      0.      0.      2009-09-01 12:00:00
#  10 2009.  10. 0.      0.      0.      2009-10-01 12:00:00
#  11 2009.  11. 0.      0.      0.0180  2009-11-01 12:00:00
#  12 2009.  12. 0.      0.      0.0190  2009-12-01 12:00:00

plot_ly(smf, x = ~smf$DateTime, y = ~smf$SM1, name = 'subm1', type = 'scatter', mode = 'points') %>%
  add_trace(y = ~smf$SM2, name = 'subm2', mode = 'points') %>%
  add_trace(y = ~smf$SM3, name = 'subm3', mode = 'points') %>%
  layout(title = "Power Consumption first day of month 2009",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)"))
