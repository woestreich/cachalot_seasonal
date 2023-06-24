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
library(cetcolor)

## load data files
presence <- read.csv("outputs/files/acoustic_outputs/presence.csv")
presence["female"][is.na(presence["female"])] <- 0
annual_perc <- read.csv("outputs/files/acoustic_outputs/annual_perc.csv")
clicks <- read.csv("outputs/files/acoustic_outputs/clicks_soldeg.csv")
ici_stats <- read.csv("outputs/files/acoustic_outputs/ICI_stats.csv")

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
  allmonths$perc_fem[i] <- (sum(mo$female)/length(mo$female))*100
  mo_clicks <- clicks %>% filter(month == mm[i], year == yy[i])
  allmonths$ICImean[i] <- mean(mo_clicks$ICI)
  allmonths$ICIsd[i] <- sd(mo_clicks$ICI)
  small_ICI <- mo_clicks %>% filter(ICI <= 0.6)
  allmonths$small_prop[i] <- length(small_ICI$ICI)/length(mo_clicks$ICI)
}

## calculate percent of click sequences overall with ICI <= 0.6
clicks_fem <- clicks %>% filter(ICI <= 0.6)
prop_clicks_fem <- length(clicks_fem$ICI)/length(clicks$ICI)

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

##### ICI stats figure #####
pp <- ggplot(ici_stats, aes(x=ICI,y=pd_mean)) + 
  geom_rect(aes(xmin = 0.425, ymin=0, xmax=0.6, ymax = 0.075), color = '#3182bd', fill = 'white') +
  geom_rect(aes(xmin = 0.6, ymin=0, xmax=0.8, ymax = 0.075), color = '#9ecae1', fill = 'white') +
  geom_rect(aes(xmin = 0.8, ymin=0, xmax=2.125, ymax = 0.075), color = '#deebf7', fill = 'white') +
  geom_rect(aes(xmin = 0.425, ymin=0.075, xmax=0.6, ymax = 0.08), color = '#3182bd', fill = '#3182bd') +
  geom_rect(aes(xmin = 0.6, ymin=0.075, xmax=0.8, ymax = 0.08), color = '#9ecae1', fill = '#9ecae1') +
  geom_rect(aes(xmin = 0.8, ymin=0.075, xmax=2.125, ymax = 0.08), color = '#deebf7', fill = '#deebf7') +
  geom_line(size = 1) +
  geom_line(data = ici_stats, aes(x=ICI,y=pd_min), linetype = "dashed", size = 0.5) +
  geom_line(data = ici_stats, aes(x=ICI,y=pd_max), linetype = "dashed", size = 0.5) +
  xlab("Inter-click-interval (s)") +
  ylab("Density") +
  annotate("segment", x = 1.0, xend = 1.2, y = 0.062, yend = 0.062, linetype = "dashed", size =0.5) +
  annotate("segment", x = 1.0, xend = 1.2, y = 0.07, yend = 0.07, size = 1) +
  geom_rect(aes(xmin = 1.0, ymin=0.054-0.0025, xmax=1.2, ymax = 0.054+0.0025), color = '#3182bd', fill = '#3182bd') +
  geom_rect(aes(xmin = 1.0, ymin=0.046-0.0025, xmax=1.2, ymax = 0.046+0.0025), color = '#9ecae1', fill = '#9ecae1') +
  geom_rect(aes(xmin = 1.0, ymin=0.038-0.0025, xmax=1.2, ymax = 0.038+0.0025), color = '#deebf7', fill = '#deebf7') +
  annotate("text", x = 1.25, y = 0.07, label = "Mean monthly distribution", hjust = 0) +
  annotate("text", x = 1.25, y = 0.062, label = "Min & max monthly distribution", hjust = 0) +
  annotate("text", x = 1.25, y = 0.054, label = "Females & juveniles", hjust = 0) +
  annotate("text", x = 1.25, y = 0.046, label = "Mid-size (large females & juvenile males)", hjust = 0) +
  annotate("text", x = 1.25, y = 0.038, label = "Adult males", hjust = 0) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 14)) 
pp

tiff("outputs/figures/Fig3_ICI.tiff",units="in", width=7,height=3,res=300)
pp
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
  theme(text = element_text(size = 10))

tiff("outputs/figures/ICI_new.tiff",units="in", width=5,height=2,res=300)
pb
dev.off()


##### NPTZ (Figure 6) #####
## 160 - 180
nptz_sst <- read.csv("data/NPTZ_monthly_sst.csv")

# MONTHLY
colnames(nptz_sst) <- c("year","month","nptz_lat_sst")
nptz_sst$year <- as.factor(nptz_sst$year)
mmm <- allmonths %>% full_join(nptz_sst, by = c("year","month"))
lm_sst <- lm(mmm$perc~mmm$nptz_lat_sst)

# QUARTERLY
mq <- data.frame(matrix(NA, nrow = 29, ncol = 4))
colnames(mq) <- c("year","qt","perc","nptz")
mq$year <- c(2015,rep(2016,4),rep(2017,4),rep(2018,4),rep(2019,4),rep(2020,4),rep(2021,4),rep(2022,4))
mq$qt <- c(4,rep(c(1,2,3,4),7))
for (i in 1:length(mq$year)) {
  x <- allmonths %>% filter(year == mq$year[i] & month < (mq$qt[i]*3)+1 & month > (mq$qt[i]*3)-3)
  y <- nptz_sst %>% filter(year == mq$year[i] & month < (mq$qt[i]*3)+1 & month > (mq$qt[i]*3)-3)
  mq$perc[i] <- mean(x$perc)
  mq$nptz[i] <- mean(y$nptz_lat_sst)
}
lm_sst_qt <- lm(mq$perc ~ mq$nptz)

col <- cet_pal(12,'c2s')
pd <- ggplot(mmm, aes(x=nptz_lat_sst,y=perc)) + geom_point(aes(fill = as.factor(mmm$month)), shape = 21, color = "black") +
  scale_fill_manual(labels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                    values = col) +
  geom_smooth(method=lm , color="black", se=TRUE) +
  annotate("text", x = 41, y = 90, label = paste('R^2 ==',round(summary(lm_sst)$r.squared, 2)), size=4, parse = TRUE, hjust = 0) +
  annotate("text", x = 41, y = 82, label = 'p < 0.0001', size=4, hjust = 0) +
  xlab("North Pacific Transition Zone\nmonthly mean latitude (°N)") +
  ylab("Days with foraging\nsperm whale present (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         text = element_text(size = 12)) +
  guides(fill=guide_legend(ncol=2,title="Month")) 

pe <- ggplot(mq, aes(x=nptz,y=perc)) + geom_point(aes(fill = as.factor(mq$qt)), shape = 21, color = "black") +
  geom_smooth(method=lm , color="black", se=TRUE) +
  annotate("text", x = 41, y = 90, label = paste('R^2 ==',round(summary(lm_sst_qt)$r.squared, 2)), size=4, parse = TRUE, hjust = 0) +
  annotate("text", x = 41, y = 82, label = 'p < 0.01', size=4, hjust = 0) +
  xlab("North Pacific Transition Zone\nmonthly mean latitude (°N)") +
  ylab("Days with foraging\nsperm whale present (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12)) +
  guides(fill=guide_legend(ncol=2,title="Quarter")) 


tiff("outputs/figures/NPTZ_month_160.tiff",units="in", width=6,height=3.5,res=300)
pd
dev.off()


## 130 - 140
nptz_sst <- read.csv("data/NPTZ_monthly_sst_narrow.csv")

tiff("outputs/figures/NPTZ_quart_160.tiff",units="in", width=6,height=3.5,res=300)
pe
dev.off()

# MONTHLY
colnames(nptz_sst) <- c("year","month","nptz_lat_sst")
nptz_sst$year <- as.factor(nptz_sst$year)
mmm <- allmonths %>% full_join(nptz_sst, by = c("year","month"))
lm_sst <- lm(mmm$perc~mmm$nptz_lat_sst)

# QUARTERLY
mq <- data.frame(matrix(NA, nrow = 29, ncol = 4))
colnames(mq) <- c("year","qt","perc","nptz")
mq$year <- c(2015,rep(2016,4),rep(2017,4),rep(2018,4),rep(2019,4),rep(2020,4),rep(2021,4),rep(2022,4))
mq$qt <- c(4,rep(c(1,2,3,4),7))
for (i in 1:length(mq$year)) {
  x <- allmonths %>% filter(year == mq$year[i] & month < (mq$qt[i]*3)+1 & month > (mq$qt[i]*3)-3)
  y <- nptz_sst %>% filter(year == mq$year[i] & month < (mq$qt[i]*3)+1 & month > (mq$qt[i]*3)-3)
  mq$perc[i] <- mean(x$perc)
  mq$nptz[i] <- mean(y$nptz_lat_sst)
}
lm_sst_qt <- lm(mq$perc ~ mq$nptz)

col <- cet_pal(12,'c2s')
pd <- ggplot(mmm, aes(x=nptz_lat_sst,y=perc)) + geom_point(aes(fill = as.factor(mmm$month)), shape = 21, color = "black") +
  scale_fill_manual(labels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                    values = col) +
  geom_smooth(method=lm , color="black", se=TRUE) +
  annotate("text", x = 41, y = 90, label = paste('R^2 ==',round(summary(lm_sst)$r.squared, 2)), size=4, parse = TRUE, hjust = 0) +
  annotate("text", x = 41, y = 82, label = 'p < 0.0001', size=4, hjust = 0) +
  xlab("North Pacific Transition Zone\nmonthly mean latitude (°N)") +
  ylab("Days with foraging\nsperm whale present (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12)) +
  guides(fill=guide_legend(ncol=2,title="Month")) 

pe <- ggplot(mq, aes(x=nptz,y=perc)) + geom_point(aes(fill = as.factor(mq$qt)), shape = 21, color = "black") +
  geom_smooth(method=lm , color="black", se=TRUE) +
  annotate("text", x = 41, y = 90, label = paste('R^2 ==',round(summary(lm_sst_qt)$r.squared, 2)), size=4, parse = TRUE, hjust = 0) +
  annotate("text", x = 41, y = 82, label = 'p < 0.01', size=4, hjust = 0) +
  xlab("North Pacific Transition Zone\nmonthly mean latitude (°N)") +
  ylab("Days with foraging\nsperm whale present (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12)) +
  guides(fill=guide_legend(ncol=2,title="Quarter")) 
