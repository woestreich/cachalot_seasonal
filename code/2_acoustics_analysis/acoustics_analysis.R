## Create figures for seasonal and interannual patterns in click detection
## (Figure 2, Figure S5)

## clear variables
rm(list=ls())
## packages
library(lubridate)
library(tidyr)
library(zoo)
library(dplyr)
library(mgcv)

## load data files
presence <- read.csv("outputs/files/acoustic_outputs/presence.csv")
annual_perc <- read.csv("outputs/files/acoustic_outputs/annual_perc.csv")
clicks <- read.csv("outputs/files/acoustic_outputs/clicks_soldeg.csv")

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
  mo_clicks <- clicks %>% filter(month == mm[i], year == yy[i])
  allmonths$ICImean[i] <- mean(mo_clicks$ICI)
}

## calculations for interannual visualization
annual_perc$date <- as.Date(paste(as.character(annual_perc$year),"-07-01",sep = ""))
# running mean
wsize = 3
allmonths$date <- as.Date(paste(as.character(allmonths$year),as.character(allmonths$month),"15",sep = "-"))
allmonths$perc_rm <- rollapply(allmonths$perc,wsize,mean,fill=NA,na.rm = TRUE)

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

##### GAM
allmonths$year <- as.factor(allmonths$year)
gam1 <- gam(formula = perc ~ s(month, bs="cc", k=12) + s(year, bs="re"), data = allmonths, method = "REML")
s <- summary(gam1)
tiff("outputs/figures/gam.tiff",units="in", width=4.5,height=4,res=300)
plot(gam1, select = 1, ylab="Presence relative likelihood",xlab="Month")
mtext(paste("Deviance explained =",round(s$dev.expl, 2)), side=3)
dev.off()

##### PLOT
pa <- ggplot(allmonths, aes(x=date,y=perc_rm)) + geom_line(color="#a1dab4",size=1) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 7, size = 5) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 7, size = 5) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 7, size = 5) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 7, size = 5) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 7, size = 5) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 7, size = 5) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 7, size = 5) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 7, size = 5) +
  annotate("text", label = "A", x = as.Date("2015-04-01"), y = 90, size = 5, fontface = "bold") +
  scale_x_continuous(limits = c(as.Date("2015-01-01"),as.Date("2022-12-31")), expand = c(0,0)) +
  ylim(c(0,100)) +
  xlab("") +
  ylab("Percent of recording days with\nforaging sperm whale present") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(), text = element_text(size = 14))

pb <- ggplot(allmonths, aes(x = as.factor(month), y = perc)) + geom_boxplot(fill = "#a1dab4") +
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
  annotate("text", label = "B", x = 0.8, y = 98, size = 5, fontface = "bold") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = pa$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90, size = 5) + 
  theme_void() +
  coord_cartesian(clip = "off")
pa$labels$y <- ""
pb$labels$y <- ""

layout <- "
A
B
B
"
pab <- pa/pb + plot_layout(design = layout)

layout <- "
ABBBBBBBBBBB
"

tiff("outputs/figures/Fig2_new.tiff",units="in", width=8,height=6,res=300)
p3 + pab + plot_layout(design = layout)
dev.off()



  
##### ICI supplemental figure #####
ICIclim <- data.frame(matrix(0,12,3))
colnames(ICIclim) <- c("month","mn","s")
for (m in 1:12) {
  d <- clicks %>% filter(month == m)
  ICIclim$month[m] <- m
  ICIclim$mn[m] <- mean(d$ICI)
  ICIclim$s[m] <- sd(d$ICI)
}

pa <- ggplot(ICIclim, aes(x = month, y = mn, ymin = mn-s, ymax = mn+s)) +
  geom_pointrange() +
  ylab("Mean\ninter-click-interval") +
  xlab("Month") +
  scale_x_continuous(breaks = seq(1,12)) +
  ylim(c(0.37,2)) +
  geom_hline(aes(yintercept=0.375), linetype="dashed") +
  geom_hline(aes(yintercept=2), linetype="dashed") +
  annotate("text", x = 1, y = 1.8, label = "A", fontface = 'bold') +
  theme(text = element_text(size = 10))

lm1 <- lm(allmonths$ICImean~allmonths$perc)
pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(pts) <- c("x","y")
pts$x <- c(min(allmonths$perc),max(allmonths$perc))
pts$y <- c(lm1$coefficients[1] + lm1$coefficients[2]*min(allmonths$perc),
                   lm1$coefficients[1] + lm1$coefficients[2]*max(allmonths$perc))

pb <- ggplot(allmonths, aes(x=perc, y=ICImean)) + geom_point() +
  geom_line(data=pts, aes(x=x,y=y), linetype = "dashed") +
  xlab("Monthly percent of recording days\nwith foraging sperm whale present") +
  ylab("Monthly mean\ninter-click-interval") +
  xlim(c(0,100)) +
  annotate("text", x = 75, y = 1.2, 
           label = paste('R^2 ==',round(summary(lm1)$r.squared, 2)), 
           size=3, parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 1.25, label = "B", fontface = 'bold') +
  theme(text = element_text(size = 10))

tiff("outputs/figures/ICI.tiff",units="in", width=6,height=2,res=300)
pa + pb
dev.off()

