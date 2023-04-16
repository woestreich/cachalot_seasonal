## simulation figure 2 (main text figure 4)

## packages
library(tidyr)
library(lubridate)
library(patchwork)
library(dplyr)
library(ggplot2)
## clear variables
rm(list=ls())

## load data files
presence <- read.csv("outputs/files/acoustic_outputs/presence.csv")
goa <- read.csv("data/goa_perc.csv")
t_h1perc <- read.csv("outputs/files/simulation_outputs/trackers_h1perc.csv")
t_h2perc <- read.csv("outputs/files/simulation_outputs/trackers_h2perc.csv")
m_h1perc <- read.csv("outputs/files/simulation_outputs/migrants_h1perc.csv")
m_h2perc <- read.csv("outputs/files/simulation_outputs/migrants_h2perc.csv")
p_h1perc <- read.csv("outputs/files/simulation_outputs/partial_h1perc.csv")
p_h2perc <- read.csv("outputs/files/simulation_outputs/partial_h2perc.csv")
n_h1perc <- read.csv("outputs/files/simulation_outputs/nomads_h1perc.csv")
n_h2perc <- read.csv("outputs/files/simulation_outputs/nomads_h2perc.csv")
# remove 1st year to minimize impacts of initial conditions
t_h1perc <- t_h1perc %>% filter(yr > 1)
t_h2perc <- t_h2perc %>% filter(yr > 1)
m_h1perc <- m_h1perc %>% filter(yr > 1)
m_h2perc <- m_h2perc %>% filter(yr > 1)
p_h1perc <- p_h1perc %>% filter(yr > 1)
p_h2perc <- p_h2perc %>% filter(yr > 1)
n_h1perc <- n_h1perc %>% filter(yr > 1)
n_h2perc <- n_h2perc %>% filter(yr > 1)


## calculate monthly % presence for CCCS
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

## climatology of means
mean_h1 <- data.frame(matrix(NA, nrow = 12, ncol = 6))
mean_h2 <- data.frame(matrix(NA, nrow = 12, ncol = 6))
colnames(mean_h1) <- c("month","emp","t","m","n","p")
colnames(mean_h2) <- c("month","emp","t","m","n","p")
for (i in 1:12) {
  # month
  mean_h1$month[i] <- i
  mean_h2$month[i] <- i
  
  # southern hydrophone
  e <- allmonths %>% filter((month == i))
  mean_h1$emp[i] <- mean(e$perc)
  t <- t_h1perc %>% filter((month == i))
  mean_h1$t[i] <- mean(t$perc)
  m <- m_h1perc %>% filter((month == i))
  mean_h1$m[i] <- mean(m$perc)
  n <- n_h1perc %>% filter((month == i))
  mean_h1$n[i] <- mean(n$perc)
  p <- p_h1perc %>% filter((month == i))
  mean_h1$p[i] <- mean(p$perc)
  
  # southern hydrophone
  e <- goa %>% filter((month == i))
  mean_h2$emp[i] <- mean(e$perc)
  t <- t_h2perc %>% filter((month == i))
  mean_h2$t[i] <- mean(t$perc)
  m <- m_h2perc %>% filter((month == i))
  mean_h2$m[i] <- mean(m$perc)
  n <- n_h2perc %>% filter((month == i))
  mean_h2$n[i] <- mean(n$perc)
  p <- p_h2perc %>% filter((month == i))
  mean_h2$p[i] <- mean(p$perc)
}

## RMSD (mean version)
resid1 <- (mean_h1$emp - mean_h1$t)^2
resid2 <- (mean_h2$emp - mean_h2$t)^2
b1 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (mean_h1$emp - mean_h1$n)^2
resid2 <- (mean_h2$emp - mean_h2$n)^2
c1 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (mean_h1$emp - mean_h1$m)^2
resid2 <- (mean_h2$emp - mean_h2$m)^2
d1 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (mean_h1$emp - mean_h1$p)^2
resid2 <- (mean_h2$emp - mean_h2$p)^2
e1 <- sqrt(sum(c(resid1,resid2))/24)

## climatology of medians
med_h1 <- data.frame(matrix(NA, nrow = 12, ncol = 6))
med_h2 <- data.frame(matrix(NA, nrow = 12, ncol = 6))
colnames(med_h1) <- c("month","emp","t","m","n","p")
colnames(med_h2) <- c("month","emp","t","m","n","p")
for (i in 1:12) {
  # month
  med_h1$month[i] <- i
  med_h2$month[i] <- i

  # southern hydrophone
  e <- allmonths %>% filter((month == i))
  med_h1$emp[i] <- median(e$perc)
  t <- t_h1perc %>% filter((month == i))
  med_h1$t[i] <- median(t$perc)
  m <- m_h1perc %>% filter((month == i))
  med_h1$m[i] <- median(m$perc)
  n <- n_h1perc %>% filter((month == i))
  med_h1$n[i] <- median(n$perc)
  p <- p_h1perc %>% filter((month == i))
  med_h1$p[i] <- median(p$perc)

  # southern hydrophone
  e <- goa %>% filter((month == i))
  med_h2$emp[i] <- median(e$perc)
  t <- t_h2perc %>% filter((month == i))
  med_h2$t[i] <- median(t$perc)
  m <- m_h2perc %>% filter((month == i))
  med_h2$m[i] <- median(m$perc)
  n <- n_h2perc %>% filter((month == i))
  med_h2$n[i] <- median(n$perc)
  p <- p_h2perc %>% filter((month == i))
  med_h2$p[i] <- median(p$perc)
}

## RMSD (median version)
resid1 <- (med_h1$emp - med_h1$t)^2
resid2 <- (med_h2$emp - med_h2$t)^2
b2 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (med_h1$emp - med_h1$n)^2
resid2 <- (med_h2$emp - med_h2$n)^2
c2 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (med_h1$emp - med_h1$m)^2
resid2 <- (med_h2$emp - med_h2$m)^2
d2 <- sqrt(sum(c(resid1,resid2))/24)

resid1 <- (med_h1$emp - med_h1$p)^2
resid2 <- (med_h2$emp - med_h2$p)^2
e2 <- sqrt(sum(c(resid1,resid2))/24)

##### Panel A: empirical data
p1 <- ggplot(goa, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))

p2 <- ggplot(allmonths, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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
  geom_hline(aes(yintercept=0)) +
  annotate("text", label = "Jan", x = 1, y = -8, size = 2.5) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 2.5) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 2.5) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 2.5) +
  annotate("text", label = "May", x = 5, y = -8, size = 2.5) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 2.5) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 2.5) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 2.5) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 2.5) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 2.5) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 2.5) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 2.5) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p1$labels$y <- ""
p2$labels$y <- ""
p12 <- (p1 / plot_spacer() / p2) + plot_layout(heights = c(4, -1.2 ,4))
p <- p3 + p12 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBB
AABBBBBBBBBBBB
AABBBBBBBBBBBB
"

tiff("outputs/figures/Fig4a.tiff",units="in", width=4.5,height=2.5,res=300)
p + plot_layout(design = layout)
dev.off()

##### Panel B: seasonal resource tracking
p1 <- ggplot(t_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p2 <- ggplot(t_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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
  geom_hline(aes(yintercept=0)) +
  annotate("text", label = "Jan", x = 1, y = -8, size = 2.5) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 2.5) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 2.5) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 2.5) +
  annotate("text", label = "May", x = 5, y = -8, size = 2.5) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 2.5) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 2.5) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 2.5) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 2.5) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 2.5) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 2.5) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 2.5) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p1$labels$y <- ""
p2$labels$y <- ""
p12 <- (p1 / plot_spacer() / p2) + plot_layout(heights = c(4, -1.2 ,4))
p <- p3 + p12 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBB
AABBBBBBBBBBBB
AABBBBBBBBBBBB
"

tiff("outputs/figures/Fig4b.tiff",units="in", width=4.5,height=2.5,res=300)
p + plot_layout(design = layout)
dev.off()

##### Panel C: nomadic resource tracking
p1 <- ggplot(n_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p2 <- ggplot(n_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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
  geom_hline(aes(yintercept=0)) +
  annotate("text", label = "Jan", x = 1, y = -8, size = 2.5) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 2.5) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 2.5) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 2.5) +
  annotate("text", label = "May", x = 5, y = -8, size = 2.5) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 2.5) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 2.5) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 2.5) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 2.5) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 2.5) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 2.5) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 2.5) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p1$labels$y <- ""
p2$labels$y <- ""
p12 <- (p1 / plot_spacer() / p2) + plot_layout(heights = c(4, -1.2 ,4))
p <- p3 + p12 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBB
AABBBBBBBBBBBB
AABBBBBBBBBBBB
"

tiff("outputs/figures/Fig4c.tiff",units="in", width=4.5,height=2.5,res=300)
p + plot_layout(design = layout)
dev.off()

##### Panel D: migration
p1 <- ggplot(m_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p2 <- ggplot(m_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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
  geom_hline(aes(yintercept=0)) +
  annotate("text", label = "Jan", x = 1, y = -8, size = 2.5) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 2.5) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 2.5) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 2.5) +
  annotate("text", label = "May", x = 5, y = -8, size = 2.5) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 2.5) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 2.5) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 2.5) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 2.5) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 2.5) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 2.5) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 2.5) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p1$labels$y <- ""
p2$labels$y <- ""
p12 <- (p1 / plot_spacer() / p2) + plot_layout(heights = c(4, -1.2 ,4))
p <- p3 + p12 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBB
AABBBBBBBBBBBB
AABBBBBBBBBBBB
"

tiff("outputs/figures/Fig4d.tiff",units="in", width=4.5,height=2.5,res=300)
p + plot_layout(design = layout)
dev.off()

##### Panel E: partial migration
p1 <- ggplot(p_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(0,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p2 <- ggplot(p_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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
  geom_hline(aes(yintercept=0)) +
  annotate("text", label = "Jan", x = 1, y = -8, size = 2.5) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 2.5) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 2.5) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 2.5) +
  annotate("text", label = "May", x = 5, y = -8, size = 2.5) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 2.5) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 2.5) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 2.5) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 2.5) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 2.5) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 2.5) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 2.5) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 14))

p3 <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p1$labels$y <- ""
p2$labels$y <- ""
p12 <- (p1 / plot_spacer() / p2) + plot_layout(heights = c(4, -1.2 ,4))
p <- p3 + p12 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBB
AABBBBBBBBBBBB
AABBBBBBBBBBBB
"

tiff("outputs/figures/Fig4e.tiff",units="in", width=4.5,height=2.5,res=300)
p + plot_layout(design = layout)
dev.off()
