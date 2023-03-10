################################################################################
## daynight_presence.R
##
## Will Oestreich
################################################################################
## This script runs of the automated daily resolution processing for cachalot
## click presence/absence. This includes daily presence/absence and daily night 
## and day presence/absence. Also calculates rates of click detection by solar 
## elevation category (day, night dusk/dawn).
## 
## The input detection files here are from a BLED implemented in Raven Pro 
## v1.6.3: [1400-4000Hz, 1/4/4 frames, 70% min occ,  5.0 SNR, Block Size 2, 
## Hop Size 0.496, Percentile 20.0]
################################################################################

##### packages #####
library(tidyverse)
library(plyr)
library(suncalc)
library(R.matlab)
library(lubridate)

##### prepare files and parameters before looping through test #####
## clear variables
rm(list=ls())

## hydrophone lat, lon for sunrise/sunset calcs
hlat <- 36.7125
hlon <- -121.1868

## set minimum number of repetitions to classify as a cachalot
r <- 6  # this was optimized using script "daily_performance.R"

## set degree of rounding for inter-click-interval calculations
ro <- 0.25

## set maximum and minimum separation in seconds to consider sequence to be cachalot
minsep <- 0.5
maxsep <- 2.0

## list detection files
files <- list.files(path="data/BLED", pattern="*.txt", full.names=TRUE, recursive=TRUE)

## prepare an daily presence/absence data frame, including daily y/n ("yn") and day, night columns
presence <- data.frame(matrix(NA, nrow = length(files), ncol = 9))
colnames(presence) <- c("date","yn","day","night","dd","rate","d_rate","n_rate","dd_rate")
presence$date <- as.Date(presence$date)

## night, dusk/dawn, and day recording time counts (in hours)
rectime <- readMat("data/recording_time.mat")
D <- rectime$D
ct <- D[[2]]
ct_n <- ct[44,,1]/12/60
ct_dd <- ct[44,,2]/12/60
ct_d <- ct[44,,3]/12/60
dnum <- D[[5]]
secs <- (dnum - 719529)*86400
dts <- as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', tz = 'UTC'), format = '%Y-%m-%d %H:%M', tz = 'UTC', usetz = FALSE), tz = 'UTC')

##### loop through files #####
for (f in 1:length(files)) {
#for (f in 40) {
  #### preparatory steps
  ## extract date from detection file name
  presence$date[f] <- as.Date(substr(files[f],21,28),"%Y%m%d")
  print(as.Date(substr(files[f],21,28),"%Y%m%d"))
  
  ## load in BLED
  bled1 <- read.csv(files[f], sep="\t", header=T)
  colnames(bled1) <- c("Selection","View","Channel","Begin","End","LowFreq","HighFreq","Occupancy")
  
  ## filter out waveform rows (no double counting)
  bled1 <- bled1 %>% filter(View == "Spectrogram 1")
  bled1$diff <- NA
  
  #### daily-resolution processing
  ## find difference (rounded) between consecutive detections in this day
  bled1$time <- as.POSIXct(presence$date[f]) + bled1$Begin
  attr(bled1$time,"tzone") <- "UTC"
  bled1$diff[- 1] <- diff(bled1$Begin)  # difference (in seconds) between detections
  bled1$diff[bled1$diff < 0.4] <- NA # assign NA for differences that are shorter than cachalot click sequences. w/o this, some differences (e.g. 0.38) that are very short could be rounded up to 0.5, and counted as a cachalot click sequence
  bled1$ICI <- bled1$diff # store the actual ICI (non-rounded) for subsequent analysis as well
  bled1$diff <- round_any(bled1$diff, ro) # round to nearest ro (set above)
  
  ## this method identifies not just counts of cadences, but consecutive sequences of a cadence
  t1 <- rle(bled1$diff)
  seq1 <- rep(t1$lengths >= r, times = t1$lengths)
  cadence1 <- cbind(bled1$diff,seq1)
  cadence1 <- as.data.frame(cadence1)
  rownames(cadence1) <- NULL
  colnames(cadence1) <- c("diff","seq")
  bled1$seq <- cadence1$seq
  bled1 <- bled1 %>% filter(diff >= minsep & diff <= maxsep)
  
  if (length(bled1$Selection) > 0) {
    ## get solar elevation for day/night processing
    sun <- getSunlightPosition(date = bled1$time, 
                                     lat = hlat,
                                     lon = hlon,
                                     keep = "altitude")
    bled1$sol <- sun$altitude
    bled1$soldeg <- bled1$sol*180/pi
    
    ## get index of date for recording time info
    x <- which(dts == as.Date(substr(files[f],21,28),"%Y%m%d"))
    

    #### is there a clear cachalot cadence in this 24-hour day? 
    test1 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE)
    if (length(test1$seq) >= 1) {
      presence$yn[f] <- 1
      presence$rate[f] <- length(test1$seq)/(ct_d[x] + ct_n[x] + ct_dd[x])
    }
    else {
      presence$yn[f] <- 0
      presence$rate[f] <- 0
    }
    
    #### is there a clear cachalot cadence in daytime hours? 
    test2 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg > 0)
    if (length(test2$seq) >= 1) {
      presence$day[f] <- 1
      presence$d_rate[f] <- length(test2$seq)/ct_d[x]
    }
    else {
      presence$day[f] <- 0
      presence$d_rate[f] <- 0
    }
    
    #### is there a clear cachalot cadence in nighttime hours? 
    test3 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg < -12)
    if (length(test3$seq) >= 1) {
      presence$night[f] <- 1
      presence$n_rate[f] <- length(test3$seq)/ct_n[x]
    }
    else {
      presence$night[f] <- 0
      presence$n_rate[f] <- 0
    }
    
    #### is there a clear cachalot cadence in dusk/dawn hours? 
    test4 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg >= -12 & soldeg <= 0)
    if (length(test4$seq) >= 1) {
      presence$dd[f] <- 1
      presence$dd_rate[f] <- length(test4$seq)/ct_dd[x]
    }
    else {
      presence$dd[f] <- 0
      presence$dd_rate[f] <- 0
    }
    
    #### store away all clicks id'd in sequences for later analysis
    if (f == 1) {
      clicks <- data.frame(matrix(NA, nrow = length(bled1$Selection), ncol = length(bled1)))
      colnames(clicks) <- colnames(bled1)
      clicks <- bled1 %>% filter(seq == 1)
    }
    else {
      newclicks <- bled1 %>% filter(seq == 1)
      clicks <- rbind(clicks,newclicks)
    }
  } else {
    presence$yn[f] <- 0
    presence$rate[f] <- 0
    presence$day[f] <- 0
    presence$d_rate[f] <- 0
    presence$night[f] <- 0
    presence$n_rate[f] <- 0
    presence$dd[f] <- 0
    presence$dd_rate[f] <- 0
  }
}

##### now calculate normalized click detection rates by solar elevation category
clicks$soldeg <- clicks$sol*180/pi

##### store year and month information for later time-series analysis
presence$month <- month(presence$date)
presence$year <- year(presence$date)

##### calculate yearly % recording days present
annual_perc <- data.frame(matrix(NA, nrow = 8, ncol = 2))
colnames(annual_perc) <- c("year","perc")
for (yr in 2015:2022) {
  subyr <- presence %>% filter(year == yr)
  annual_perc$perc[yr-2014] <- (sum(subyr$yn)/length(subyr$yn))*100
  annual_perc$year[yr-2014] <- yr
}
late_perc <- data.frame(matrix(NA, nrow = 8, ncol = 2))
colnames(late_perc) <- c("year","perc")
for (yr in 2015:2021) {
  subyr <- presence %>% filter(year == yr & month > 7)
  late_perc$perc[yr-2014] <- (sum(subyr$yn)/length(subyr$yn))*100
  late_perc$year[yr-2014] <- yr
}


##### save presence/absence, click solar elevation, annual perc information to files
write.csv(presence, file = "outputs/files/presence.csv")
clicks$year <- year(clicks$time)
clicks$month <- month(clicks$time)
clicks_soldeg <- clicks %>% select(soldeg,year,month,time,diff)
write.csv(clicks_soldeg,"outputs/files/clicks_soldeg.csv",row.names = FALSE)
write.csv(annual_perc,"outputs/files/annual_perc.csv",row.names = FALSE)
write.csv(late_perc,"outputs/files/late_perc.csv",row.names = FALSE)

##### day vs. night calcs normalized by recording time 
ct_n <- ct_n[which(dts > as.POSIXct("2015-07-31") & dts < as.POSIXct("2023-01-01"))]
ct_dd <- ct_dd[which(dts > as.POSIXct("2015-07-31") & dts < as.POSIXct("2023-01-01"))]
ct_d <- ct_d[which(dts > as.POSIXct("2015-07-31") & dts < as.POSIXct("2023-01-01"))]

##### Save presences and rates by solar elevation category
day_night_dd <- data.frame(matrix(NA, nrow = 3, ncol = 3))
colnames(day_night_dd) <- c("sol_category","rate","totaldays")

# night
day_night_dd$sol_category[1] <- "night"
day_night_dd$rate[1] <- length(clicks$soldeg[clicks$soldeg < -12])/sum(ct_n)
day_night_dd$totaldays[1] <- sum(presence$night==1)

# dusk/dawn
day_night_dd$sol_category[2] <- "dusk/dawn"
day_night_dd$rate[2] <- length(clicks$soldeg[clicks$soldeg >= -12 & clicks$soldeg <= 0])/sum(ct_dd)
day_night_dd$totaldays[2] <- sum(presence$dd==1)

# day
day_night_dd$sol_category[3] <- "day"
day_night_dd$rate[3] <- length(clicks$soldeg[clicks$soldeg > 0])/sum(ct_d)
day_night_dd$totaldays[3] <- sum(presence$day==1)

# save
write.csv(day_night_dd, file = "outputs/files/day_night_dd_stats.csv")


##### day vs. night calcs normalized by recording time, by year
rectime <- readMat("data/recording_time.mat")
D <- rectime$D
ct <- D[[2]]
## night, dusk/dawn, and day recording time counts (in hours)
rec <- as.data.frame(matrix(NA, nrow = length(ct[44,,1]), ncol = 0))
rec$ct_n <- ct[44,,1]/12/60
rec$ct_dd <- ct[44,,2]/12/60
rec$ct_d <- ct[44,,3]/12/60
dnum <- D[[5]]
secs <- (dnum - 719529)*86400
rec$date <- as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', tz = 'UTC'), format = '%Y-%m-%d %H:%M', tz = 'UTC', usetz = FALSE), tz = 'UTC')
rec$year <- year(rec$date)
rec$month <- month(rec$date)

day_night_dd_yearly <- data.frame(matrix(NA, nrow = 8, ncol = 5))
colnames(day_night_dd_yearly) <- c("year","n_rate","d_rate","dd_rate","ratio")
for (y in 2015:2022){
  rec_curr <- rec %>% filter(year == y)
  clicks_curr <- clicks %>% filter(year == y)
  
  day_night_dd_yearly$year[y-2014] <- y
  day_night_dd_yearly$n_rate[y-2014] <- length(clicks_curr$soldeg[clicks_curr$soldeg < -12])/sum(rec_curr$ct_n)
  day_night_dd_yearly$d_rate[y-2014] <- length(clicks_curr$soldeg[clicks_curr$soldeg > 0])/sum(rec_curr$ct_d)
  day_night_dd_yearly$dd_rate[y-2014] <- length(clicks_curr$soldeg[clicks_curr$soldeg >= -12 & clicks_curr$soldeg <= 0])/sum(rec_curr$ct_dd)
}
day_night_dd_yearly$ratio <- day_night_dd_yearly$d_rate/day_night_dd_yearly$n_rate
write.csv(day_night_dd_yearly, file = "outputs/files/day_night_dd_yearly.csv",row.names = FALSE)

##### Monthly percent presence and diel ratio
yy <- rep(c(2015,2016,2017,2018,2019,2020,2021,2022),each=12)
yy <- yy[8:96]
mm <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=8)
mm <- mm[8:96]

monthly <- data.frame(matrix(NA, nrow = length(yy), ncol = 7))
colnames(monthly) <- c("year","month","perc","n_rate","d_rate","dd_rate","ratio")
for (i in 1:length(yy)) {
  mo <- presence %>% filter(month == mm[i], year == yy[i])
  monthly$year[i] <- yy[i]
  monthly$month[i] <- mm[i]
  monthly$perc[i] <- (sum(mo$yn)/length(mo$yn))*100
  
  rec_curr <- rec %>% filter(year == yy[i], month == mm[i])
  clicks_curr <- clicks %>% filter(year == yy[i], month == mm[i])
  monthly$n_rate[i] <- length(clicks_curr$soldeg[clicks_curr$soldeg < -12])/sum(rec_curr$ct_n)
  monthly$d_rate[i] <- length(clicks_curr$soldeg[clicks_curr$soldeg > 0])/sum(rec_curr$ct_d)
  monthly$dd_rate[i] <- length(clicks_curr$soldeg[clicks_curr$soldeg >= -12 & clicks_curr$soldeg <= 0])/sum(rec_curr$ct_dd)
  monthly$ratio[i] <- monthly$d_rate[i]/monthly$n_rate[i]
}

write.csv(monthly, file = "outputs/files/monthly.csv",row.names = FALSE)


