## Create figures for seasonal and interannual patterns in click detection
## (Figure 2, Figure S5)

## clear variables
rm(list=ls())
## packages
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)

## load data files
presence <- read.csv("outputs/files/acoustic_outputs/presence.csv")
annual_perc <- read.csv("outputs/files/acoustic_outputs/annual_perc.csv")

## calculate monthly % presence
yy <- rep(c(2015,2016,2017,2018,2019,2020,2021,2022),each=12)
yy <- yy[8:96]
mm <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=8)
mm <- mm[8:96]
allmonths <- data.frame(matrix(NA, nrow = length(yy), ncol = 3))
colnames(allmonths) <- c("year","month","perc")
for (i in 1:length(yy)) {
  mo <- presence %>% filter(month == mm[i], year == yy[i])
  allmonths$year[i] <- yy[i]
  allmonths$month[i] <- mm[i]
  allmonths$perc[i] <- (sum(mo$yn)/length(mo$yn))*100
}

## calculations for interannual visualization
annual_perc$date <- as.Date(paste(as.character(annual_perc$year),"-07-01",sep = ""))
# running mean
wsize = 3
allmonths$date <- as.Date(paste(as.character(allmonths$year),as.character(allmonths$month),"15",sep = "-"))
allmonths$perc_rm <- rollapply(allmonths$perc,wsize,mean,fill=NA,na.rm = TRUE)

## prepare SSTA dataframe
# ssta <- read.csv("data/CCC_SSTA_monthly.csv")
# ssta$dt <- as.Date(dmy(ssta$dt))

## month-to-month comparisons of % presence distribution
pvs1 <- matrix(NA,12,1)
s1 <- allmonths %>% filter(month == 1) # peak month
for (m in 2:12) {
  s2 <- allmonths %>% filter(month == m)
  t <- t.test(s1$perc,s2$perc)
  pvs1[m] <- t$p.value
}
stars1 <- as.data.frame(matrix(NA,4,2))
colnames(stars1) <- c("y","x")
stars1$y <- 5
stars1$x <- which(pvs1 < 0.05)

pvs2 <- matrix(NA,12,1)
s7 <- allmonths %>% filter(month == 7) # trough month
for (m in c(1:6,8:12)) {
  s2 <- allmonths %>% filter(month == m)
  t <- t.test(s7$perc,s2$perc)
  pvs2[m] <- t$p.value
}
stars2 <- as.data.frame(matrix(NA,6,2))
colnames(stars2) <- c("y","x")
stars2$y <- 5
stars2$x <- which(pvs2 < 0.05)

##### PLOT
pa <- ggplot(allmonths, aes(x = as.factor(month), y = perc)) + geom_boxplot(fill = "#a1dab4") +
  stat_summary(geom = "point", fun = "mean", col = "black", size = 2, shape = 24, fill = "black") + 
  ylim(c(0,100)) +
  xlab("Month") +
  ylab("Recording days with foraging\nsperm whale present (%)") +
  geom_text(data = stars1, aes(x=x,y=y,label="*"),size=7) +
  geom_text(data = stars2, aes(x=x,y=y,label="**"),size=7) +
  geom_vline(aes(xintercept=1.5), linetype="dotted") +
  geom_vline(aes(xintercept=2.5), linetype="dotted") +
  geom_vline(aes(xintercept=3.5), linetype="dotted") +
  geom_vline(aes(xintercept=4.5), linetype="dotted") +
  geom_vline(aes(xintercept=5.5), linetype="dotted") +
  geom_vline(aes(xintercept=6.5), linetype="dotted") +
  geom_vline(aes(xintercept=7.5), linetype="dotted") +
  geom_vline(aes(xintercept=8.5), linetype="dotted") +
  geom_vline(aes(xintercept=9.5), linetype="dotted") +
  geom_vline(aes(xintercept=10.5), linetype="dotted") +
  geom_vline(aes(xintercept=11.5), linetype="dotted") +
  annotate("text", label = "Jan", x = 1, y = 0, size = 5) +
  annotate("text", label = "Feb", x = 2, y = 0, size = 5) +
  annotate("text", label = "Mar", x = 3, y = 0, size = 5) +
  annotate("text", label = "Apr", x = 4, y = 0, size = 5) +
  annotate("text", label = "May", x = 5, y = 0, size = 5) +
  annotate("text", label = "Jun", x = 6, y = 0, size = 5) +
  annotate("text", label = "Jul", x = 7, y = 0, size = 5) +
  annotate("text", label = "Aug", x = 8, y = 0, size = 5) +
  annotate("text", label = "Sep", x = 9, y = 0, size = 5) +
  annotate("text", label = "Oct", x = 10, y = 0, size = 5) +
  annotate("text", label = "Nov", x = 11, y = 0, size = 5) +
  annotate("text", label = "Dec", x = 12, y = 0, size = 5) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))

pb <- ggplot(allmonths, aes(x=date,y=perc_rm)) + geom_line(color="#a1dab4",size=1) +
  geom_line(data=annual_perc,aes(x=date,y=perc),size=1) + 
  geom_point(data=annual_perc,aes(x=date,y=perc),color="black", fill="#a1dab4", shape=24, size = 5) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 20, size = 5) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 20, size = 5) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 20, size = 5) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 20, size = 5) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 20, size = 5) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 20, size = 5) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 20, size = 5) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 20, size = 5) +
  scale_x_continuous(limits = c(as.Date("2015-01-01"),as.Date("2022-12-31")), expand = c(0,0)) +
  ylim(c(20,80)) +
  xlab("") +
  ylab("Recording days with foraging\nsperm whale present (%)") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(), text = element_text(size = 14))

tiff("outputs/figures/Fig2.tiff",units="in", width=8,height=3,res=300)
pa
dev.off()

tiff("outputs/figures/Fig4.tiff",units="in", width=8,height=3,res=300)
pb
dev.off()

  
##### ICI supplemental figure #####
clicks <- read.csv("outputs/files/acoustic_outputs/clicks_soldeg.csv")

tiff("outputs/figures/ICI.tiff",units="in", width=6,height=2,res=300)
ggplot(clicks, aes(x = as.factor(month), y = ICI)) + 
  geom_violin(draw_quantiles = 0.5) +
  stat_summary(geom = "point", fun = "mean", col = "black", size = 2, shape = 1, fill = "black") + 
  ylab("Inter-click-interval") +
  xlab("Month") 
dev.off()

# ICIclim <- data.frame(matrix(0,12,3))
# colnames(ICIclim) <- c("month","mn","s")
# for (m in 1:12) {
#   d <- clicks %>% filter(month == m)
#   ICIclim$month[m] <- m
#   ICIclim$mn[m] <- mean(d$ICI)
#   ICIclim$s[m] <- sd(d$ICI)
#   print(ggplot(d, aes(ICI)) + geom_histogram() + ggtitle(m))
# }
# ggplot(ICIclim, aes(x = month, y = mn, ymin = mn-s, ymax = mn+s)) + 
#   geom_pointrange() +
#   ylab("Inter-click-interval") +
#   xlab("Month") +
#   scale_x_continuous(breaks = seq(1,12))



