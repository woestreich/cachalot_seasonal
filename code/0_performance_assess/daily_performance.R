################################################################################
## daily_performance.R
## 
## Will Oestreich
################################################################################
## Testing of automated method to discern cachalot clicks from false
## positives in band limited energy detector (BLED) detections using 
## inter-click-interval. This approach uses 1 BLED, searching each for sequences
## of detections with a constant (rounded) inter-click-interval. It further 
## constrains the search by only considering inter-click-intervals typical of 
## cachalot clicks (~0.5-2 seconds).
##
## In this script, manual audit results for 24 randomly-selected days (1 from 
## each month of 2016 and 2020) are compared to automated results. Manual audits 
## were conducted by WKO in Raven Pro v1.6.3 w/ brightness 30, contrast 65, FFT 
## window size 256 (95% overlap), and listening at max volume in Raven.
##
## This script generates false positive and false negative rates at daily 
## resolution. Different BLEDs and "r" values (number of detection repeats at 
## constant rounded interval) can be assessed here.
################################################################################

##### packages #####
library(tidyverse)
library(plyr)

##### clear variables #####
rm(list=ls())

## manual audit files
man_2016 <- read.csv("data/manual_validation/2016_test/manual_audit_daily.csv")
man_2020 <- read.csv("data/manual_validation/2020_test/manual_audit_daily.csv")
man_2022 <- read.csv("data/manual_validation/2022_sighting/manual_audit_daily.csv")
audit <- rbind(man_2016,man_2020,man_2022)
audit$month <- seq(1,26)
## BLED test files
f2016 <- list.files(path="data/manual_validation/2016_test/detections/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
f2020 <- list.files(path="data/manual_validation/2020_test/detections/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
f2022 <- list.files(path="data/manual_validation/2022_sighting/detections/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
files <- c(f2016,f2020,f2022)

# performance dataframe
perf <- data.frame(matrix(NA, nrow = 8, ncol = 7))
colnames(perf) <- c("r","day_accuracy","day_balanced_accuracy","day_precision","day_recall","day_tn_rate","day_fp_rate")

## set degree of rounding for inter-click-interval calculations
ro <- 0.25

## set maximum and minimum separation in seconds to consider sequence to be cachalot
minsep <- 0.5
maxsep <- 2.0

##### loop through r (repetition) values #####
for (r in 3:10) {
  print(r)
  ## prepare a daily presence data frame
  presence <- data.frame(matrix(NA, nrow = length(files), ncol = 2))
  colnames(presence) <- c("month","yn")
  
  ## prepare an daily performance data frame
  performance_day <- data.frame(matrix(0, nrow = length(files), ncol = 5))
  colnames(performance_day) <- c("month","tp","tn","fp","fn")
  
  ##### loop through files #####
  for (f in 1:length(files)) {
    ## load in BLED
    bled <- read.csv(files[f], sep="\t", header=T)
    colnames(bled) <- c("Selection","View","Channel","Begin","End","LowFreq","HighFreq","Occupancy")
    
    ## filter out waveform rows (no double counting)
    bled <- bled %>% filter(View == "Spectrogram 1")
    
    diff1 <- diff(bled$Begin) # difference (in seconds) between detections
    diff1[diff1 < 0.4] <- NA # assign NA for differences that are shorter than cachalot click sequences. w/o this, some differences (e.g. 0.38) that are very short could be rounded up to 0.5, and counted as a cachalot click sequence
    diff2 <- round_any(diff1, ro) # round to nearest ro (set above)
      
    ## this method identifies not just counts of cadences, but consecutive sequences of a cadence
    t1 <- rle(diff2)
    seq1 <- rep(t1$lengths >= r, times = t1$lengths)
    cadence1 <- cbind(diff2,seq1)
    cadence1 <- as.data.frame(cadence1)
    rownames(cadence1) <- NULL
    colnames(cadence1) <- c("diff","seq")
    
    ## is there a clear cachalot cadence in this day? 
    test1 <- cadence1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE)
    if (length(test1$seq) >= 1) {
      presence$yn[f] <- 1
      presence$month[f] <- f
    }
    else {
      presence$yn[f] <- 0
      presence$month[f] <- f
    }
    
    ## assess performance at daily resolution
    performance_day$month[f] <- f
    presence_curr <- presence %>% filter(month == f)
    audit_curr <- audit %>% filter(month == f)
    if (sum(presence_curr$yn) > 0 & sum(audit_curr$yn_audit) > 0) {
      performance_day$tp[f] <- 1
    } else if (sum(presence_curr$yn) == 0 & sum(audit_curr$yn_audit) == 0) {
      performance_day$tn[f] <- 1
    } else if (sum(presence_curr$yn) > 0 & sum(audit_curr$yn_audit) == 0) {
      performance_day$fp[f] <- 1
    } else if (sum(presence_curr$yn) == 0 & sum(audit_curr$yn_audit) > 0) {
      performance_day$fn[f] <- 1
    }
  }
  
  ##### store in appropriate data frame ####
  x <- r - 2
  perf$r[x] <- r
  perf$day_precision[x] <- sum(performance_day$tp)/(sum(performance_day$tp) + sum(performance_day$fp))
  perf$day_recall[x] <- sum(performance_day$tp)/(sum(performance_day$tp) + sum(performance_day$fn))
  perf$day_tn_rate[x] <- sum(performance_day$tn)/(sum(performance_day$tn) + sum(performance_day$fp))
  perf$day_fp_rate[x] <- 1 - perf$day_tn_rate[x]
  perf$day_accuracy[x] <- (sum(performance_day$tp) + sum(performance_day$tn))/length(performance_day$month)
  perf$day_balanced_accuracy[x] <- (perf$day_recall[x] + perf$day_tn_rate[x])/2
}

################################################################################
## Performance results plotting 
################################################################################
library(patchwork)
##### Daily performance #####
tiff("outputs/figures/performance.tiff",units="in", width=10,height=6,res=300)
p1 <- ggplot(perf, aes(x=r, y=day_balanced_accuracy)) + geom_line() +
  labs(x = "Number of repetitions required (r)", y = "Balanced accuracy") +
  ylim(c(0,1)) +
  ggtitle("Daily resolution, comparisons to manual audit")
p2 <- ggplot(perf, aes(x=r, y=day_fp_rate)) + geom_line() +
  labs(x = "Number of repetitions required (r)", y = "False positive rate") +
  ylim(c(0,1))

p1/p2
dev.off()



