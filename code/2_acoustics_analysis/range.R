library(ncdf4)
library(lubridate)
library(zoo)
library(patchwork)
library(tidyr)
library(ggplot2)
library(dplyr)
## clear variables
rm(list=ls())

## detections
presence <- read.csv("outputs/files/acoustic_outputs/presence.csv")
## calculate monthly % presence
yy <- rep(c(2015,2016,2017,2018,2019,2020,2021,2022),each=12)
yy <- yy[8:96]
mm <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=8)
mm <- mm[8:96]
allmonths <- data.frame(matrix(NA, nrow = length(yy), ncol = 4))
colnames(allmonths) <- c("year","month","perc","ICImean")
for (i in 1:length(yy)) {
  mo <- presence %>% filter(month == mm[i], year == yy[i])
  allmonths$year[i] <- yy[i]
  allmonths$month[i] <- mm[i]
  allmonths$perc[i] <- (sum(mo$yn)/length(mo$yn))*100
}

pa <- ggplot(allmonths, aes(x = as.factor(month), y = perc)) + geom_boxplot() +
  ylab("Recording days with foraging\nsperm whale present (%)") +
  annotate("text", x = 1, y = 95, label = "A", fontface = "bold") +
  ylim(c(0,100)) +
  xlab("")


##### ambient noise #####
load("data/detection_range/ambient_noise/MARS-CachalotBandNoise.RData")
pb <- ggplot(D, aes(x=as.factor(month),y=noise)) + geom_boxplot() +
  ylab(bquote(atop("Daily mean spectrum level 1.4-4 kHz", paste("(dB re 1 ", mu, "Pa"^2, "Hz"^-1, ")")))) +
  xlab("") +
  annotate("text", x = 1, y = 82, label = "B", fontface = "bold") 

##### acoustic propagation loss modeling detection #####
jan100 <- nc_open("../cachalot_writing/propagation_modeling/Jan_2016_2022_0100m")
jul100 <- nc_open("../cachalot_writing/propagation_modeling/Jul_2016_2022_0100m")
jan500 <- nc_open("../cachalot_writing/propagation_modeling/Jan_2016_2022_0500m")
jul500 <- nc_open("../cachalot_writing/propagation_modeling/Jul_2016_2022_0500m")
jan1k <- nc_open("../cachalot_writing/propagation_modeling/Jan_2016_2022_1000m")
jul1k <- nc_open("../cachalot_writing/propagation_modeling/Jul_2016_2022_1000m")

df <- data.frame(matrix(0,361,7))
colnames(df) <- c("bearing","jan100","jul100","jan500","jul500","jan1k","jul1k")
df$bearing <- seq(1,361)
df$jan100 <- ncvar_get(jan100, 'listening_range_5dBdetectionSNR')
df$jul100 <- ncvar_get(jul100, 'listening_range_5dBdetectionSNR')
df$jan500 <- ncvar_get(jan500, 'listening_range_5dBdetectionSNR')
df$jul500 <- ncvar_get(jul500, 'listening_range_5dBdetectionSNR')
df$jan1k <- ncvar_get(jan1k, 'listening_range_5dBdetectionSNR')
df$jul1k <- ncvar_get(jul1k, 'listening_range_5dBdetectionSNR')

p1 <- ggplot(df, aes(bearing,jan100)) + geom_point(color="red") + 
  geom_point(data=df, aes(bearing,jul100)) +
  ylab("range (m)") + 
  ggtitle("100m source depth") +
  ylim(c(0,180000)) +
  annotate("text", label = "January", x = 30, y = 170000) +
  annotate("text", label = "July", x = 20, y = 140000) +
  geom_point(x = 5, y = 170000, color = "red") +
  geom_point(x = 5, y = 140000, color = "black")

p2 <- ggplot(df, aes(bearing,jan500)) + geom_point(color="red") + 
  geom_point(data=df, aes(bearing,jul500)) +
  ylab("range (m)") + 
  ggtitle("500m source depth") +
  ylim(c(0,180000))

p3 <- ggplot(df, aes(bearing,jan1k)) + geom_point(color="red") + 
  geom_point(data=df, aes(bearing,jul1k)) +
  ylab("range (m)") + 
  ggtitle("1000m source depth") +
  ylim(c(0,180000))

p1/p2/p3

df2 <- data.frame(matrix(NA,16,4))
colnames(df2) <- c("month","depth","mean","sd")
x <- df %>% filter(bearing > 153 & bearing < 312)
df2$month[1:3] <- c(1,1,1)
df2$month[7:16] <- c(2,3,4,5,6,8,9,10,11,12)
df2$depth[1:3] <- c(100,500,1000)
df2$month[4:6] <- c(7,7,7)
df2$depth[4:6] <- c(100,500,1000)
df2$mean[1] <- mean(x$jan100)
df2$mean[2] <- mean(x$jan500)
df2$mean[3] <- mean(x$jan1k)
df2$mean[4] <- mean(x$jul100)
df2$mean[5] <- mean(x$jul500)
df2$mean[6] <- mean(x$jul1k)
df2$sd[1] <- sd(x$jan100)
df2$sd[2] <- sd(x$jan500)
df2$sd[3] <- sd(x$jan1k)
df2$sd[4] <- sd(x$jul100)
df2$sd[5] <- sd(x$jul500)
df2$sd[6] <- sd(x$jul1k)

df2$mean <- df2$mean/1000
df2$sd <- df2$sd/1000

pc <- ggplot(df2, aes(x=as.factor(month), y=mean, ymin=mean-sd, ymax=mean+sd)) + 
  geom_pointrange(aes(color=as.factor(depth)), position=position_dodge(width=0.8)) +
  scale_color_manual(name = "Source depth (m)", values = c("#1b9e77","#d95f02","#7570b3"), breaks = c("100", "500", "1000")) +
  annotate("text", x = 1, y = 160, label = "C", fontface = "bold") +
  ylab("Detection range (km)") +
  xlab("Month")

tiff("outputs/figures/listening.tiff",units="in", width=8,height=8,res=300)
pa/pb/pc
dev.off()

