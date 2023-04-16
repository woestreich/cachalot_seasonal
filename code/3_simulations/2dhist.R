## packages
library(tidyr)
library(lubridate)
library(patchwork)
library(dplyr)
library(ggplot2)
library(hexbin)
## clear variables
rm(list=ls())

n_lats <- read.csv("outputs/files/simulation_outputs/nomads_lats.csv")
n <- n_lats[,c("latitude","longitude")]
tiff("outputs/figures/2dhist_n.tiff",units="in", width=4.5,height=4,res=300)
hexbinplot(latitude~longitude, data=n, mincnt=0, maxcnt=800, main = "nomadic resource tracking")
dev.off()

t_lats <- read.csv("outputs/files/simulation_outputs/trackers_lats.csv")
t <- t_lats[,c("latitude","longitude")]
tiff("outputs/figures/2dhist_t.tiff",units="in", width=4.5,height=4,res=300)
hexbinplot(latitude~longitude, data=t, mincnt=0, maxcnt=800, main = "seasonal resource tracking")
dev.off()

m_lats <- read.csv("outputs/files/simulation_outputs/migrants_lats.csv")
m <- m_lats[,c("latitude","longitude")]
tiff("outputs/figures/2dhist_m.tiff",units="in", width=4.5,height=4,res=300)
hexbinplot(latitude~longitude, data=m, mincnt=0, maxcnt=800, main = "seasonal migration")
dev.off()

p_lats <- read.csv("outputs/files/simulation_outputs/partial_lats.csv")
p <- p_lats[,c("latitude","longitude")]
tiff("outputs/figures/2dhist_p.tiff",units="in", width=4.5,height=4,res=300)
hexbinplot(latitude~longitude, data=p, mincnt=0, maxcnt=800, main = "partial migration")
dev.off()



