## simulation figure 1 (main text figure 3)

## packages
library(tidyr)
library(lubridate)
library(patchwork)
library(dplyr)
library(ggplot2)
## clear variables
rm(list=ls())

## load data files
n_agentdf <- read.csv("outputs/files/simulation_outputs/nomads_agentdf.csv")
t_agentdf <- read.csv("outputs/files/simulation_outputs/trackers_agentdf.csv")
m_agentdf <- read.csv("outputs/files/simulation_outputs/migrants_agentdf.csv")
p_agentdf <- read.csv("outputs/files/simulation_outputs/partial_agentdf.csv")
n_lats <- read.csv("outputs/files/simulation_outputs/nomads_lats.csv")
t_lats <- read.csv("outputs/files/simulation_outputs/trackers_lats.csv")
m_lats <- read.csv("outputs/files/simulation_outputs/migrants_lats.csv")
p_lats <- read.csv("outputs/files/simulation_outputs/partial_lats.csv")
# remove 1st year to minimize impacts of initial conditions; store winter/summer
n_lats <- n_lats %>% filter(yr > 1)
n_lats_winter <- n_lats %>% filter(month == 1 | month == 12 | month == 2)
n_lats_summer <- n_lats %>% filter(month == 7 | month == 6 | month == 8)
t_lats <- t_lats %>% filter(yr > 1)
t_lats_winter <- t_lats %>% filter(month == 1 | month == 12 | month == 2)
t_lats_summer <- t_lats %>% filter(month == 7 | month == 6 | month == 8)
m_lats <- m_lats %>% filter(yr > 1)
m_lats_winter <- m_lats %>% filter(month == 1 | month == 12 | month == 2)
m_lats_summer <- m_lats %>% filter(month == 7 | month == 6 | month == 8)
p_lats <- p_lats %>% filter(yr > 1)
p_lats_winter <- p_lats %>% filter(month == 1 | month == 12 | month == 2)
p_lats_summer <- p_lats %>% filter(month == 7 | month == 6 | month == 8)

# hydrophone info
## hydrophone locations
h1_lat <- 5000
h1_lon <- 0
h2_lat <- 25000
h2_lon <- 0
detection_range <- 450

#####
# A: Seasonal resource tracking
#rand_ind <- round(runif(1, 1, 100))
rand_ind <- 77
ind <- t_agentdf %>% filter(id == rand_ind) 
ind_start <- ind %>% filter(step == 1)
ind_mid <- ind %>% filter(step == 183)
ind_end <- ind %>% filter(step == 365)
p1 <- ggplot(ind, aes(longitude,latitude,color=step)) + 
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#a1dab4", fill = "#a1dab4", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#2c7fb8", fill = "#2c7fb8", alpha=0.4)  +
  annotate("text", x = 2450, y = 29500, label = 'N', size = 7) +
  geom_segment(aes(x = 2450, y = 22500, xend = 2450, yend = 28000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_path()  +
  scale_color_gradient(low = '#bdbdbd', high = '#000000') +
  geom_point(data = ind_start, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4, stroke = 2) +
  geom_point(data = ind_mid, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4, stroke = 2) +
  geom_point(data = ind_end, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4, stroke = 2) +
  xlim(c(-2500,2500)) +
  ylim(c(min(n_agentdf$latitude),max(n_agentdf$latitude))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        text = element_text(size = 14))

p12 <- ggplot(t_lats_winter, aes(latitude)) + geom_density(bw=1200) + 
  geom_density(data = t_lats_summer, aes(latitude), color = "gray", fill = "gray", alpha = 0.5, bw=1200) + 
  annotate("rect", fill = "#a1dab4", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h1_lat - 450, xmax = h1_lat + 450) +
  annotate("rect", fill = "#2c7fb8", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h2_lat - 450, xmax = h2_lat + 450) +
  geom_vline(xintercept = h1_lat, color = "#a1dab4") +
  geom_vline(xintercept = h2_lat, color = "#2c7fb8") +
  xlim(c(100,29900)) +
  coord_flip() +
  ylab("Density") +
  xlab(expression("S  " %<-% "  Latitude  " %->% "  N")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())

layout <- "
AAAAAAAAABB
AAAAAAAAABB
AAAAAAAAABB
"

tiff("outputs/figures/2Fig3a.tiff",units="in", width=4.5,height=3,res=300)
p1 + p12 + plot_layout(design = layout)
dev.off()

#####
# B: Nomadic resource tracking
#rand_ind <- round(runif(1, 1, 100))
rand_ind <- 61
ind <- n_agentdf %>% filter(id == rand_ind) 
ind_start <- ind %>% filter(step == 1)
ind_mid <- ind %>% filter(step == 183)
ind_end <- ind %>% filter(step == 365)
p1 <- ggplot(ind, aes(longitude,latitude,color=step)) + 
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#a1dab4", fill = "#a1dab4", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#2c7fb8", fill = "#2c7fb8", alpha=0.4)  +
  annotate("text", x = 2450, y = 29500, label = 'N', size = 7) +
  geom_segment(aes(x = 2450, y = 22500, xend = 2450, yend = 28000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_path()  +
  scale_color_gradient(low = '#bdbdbd', high = '#000000') +
  geom_point(data = ind_start, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4, stroke = 2) +
  geom_point(data = ind_mid, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4, stroke = 2) +
  geom_point(data = ind_end, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4, stroke = 2) +
  xlim(c(-2500,2500)) +
  ylim(c(min(n_agentdf$latitude),max(n_agentdf$latitude))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        text = element_text(size = 14))

p12 <- ggplot(n_lats_winter, aes(latitude)) + geom_density(bw=1200) + 
  geom_density(data = n_lats_summer, aes(latitude), color = "gray", fill = "gray", alpha = 0.5, bw=1200) + 
  annotate("rect", fill = "#a1dab4", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h1_lat - 450, xmax = h1_lat + 450) +
  annotate("rect", fill = "#2c7fb8", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h2_lat - 450, xmax = h2_lat + 450) +
  geom_vline(xintercept = h1_lat, color = "#a1dab4") +
  geom_vline(xintercept = h2_lat, color = "#2c7fb8") +
  xlim(c(100,29900)) +
  coord_flip() +
  ylab("Density") +
  xlab(expression("S  " %<-% "  Latitude  " %->% "  N")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())

layout <- "
AAAAAAAAABB
AAAAAAAAABB
AAAAAAAAABB
"

tiff("outputs/figures/2Fig3b.tiff",units="in", width=4.5,height=3,res=300)
p1 + p12 + plot_layout(design = layout)
dev.off()

#####
# C: Seasonal migration
rand_ind <- 97
ind <- m_agentdf %>% filter(id == rand_ind) 
ind_start <- ind %>% filter(step == 1)
ind_mid <- ind %>% filter(step == 183)
ind_end <- ind %>% filter(step == 365)
p1 <- ggplot(ind, aes(longitude,latitude,color=step)) + 
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#a1dab4", fill = "#a1dab4", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#2c7fb8", fill = "#2c7fb8", alpha=0.4)  +
  annotate("text", x = 2450, y = 29500, label = 'N', size = 7) +
  geom_segment(aes(x = 2450, y = 22500, xend = 2450, yend = 28000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_path()  +
  scale_color_gradient(low = '#bdbdbd', high = '#000000') +
  geom_point(data = ind_start, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4, stroke = 2) +
  geom_point(data = ind_mid, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4, stroke = 2) +
  geom_point(data = ind_end, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4, stroke = 2) +
  xlim(c(-2500,2500)) +
  ylim(c(min(n_agentdf$latitude),max(n_agentdf$latitude))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        text = element_text(size = 14))

p12 <- ggplot(m_lats_winter, aes(latitude)) + geom_density(bw=1200) + 
  geom_density(data = m_lats_summer, aes(latitude), color = "gray", fill = "gray", alpha = 0.5, bw=1200) + 
  annotate("rect", fill = "#a1dab4", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h1_lat - 450, xmax = h1_lat + 450) +
  annotate("rect", fill = "#2c7fb8", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h2_lat - 450, xmax = h2_lat + 450) +
  geom_vline(xintercept = h1_lat, color = "#a1dab4") +
  geom_vline(xintercept = h2_lat, color = "#2c7fb8") +
  xlim(c(100,29900)) +
  coord_flip() +
  ylab("Density") +
  xlab(expression("S  " %<-% "  Latitude  " %->% "  N")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())

layout <- "
AAAAAAAAABB
AAAAAAAAABB
AAAAAAAAABB
"

tiff("outputs/figures/2Fig3c.tiff",units="in", width=4.5,height=3,res=300)
p1 + p12 + plot_layout(design = layout)
dev.off()

##### 
# D: Partial migration
rand_ind <- 42
rand_ind2 <- 79
ind <- p_agentdf %>% filter(id == rand_ind) 
ind_start <- ind %>% filter(step == 1)
ind_mid <- ind %>% filter(step == 183)
ind_end <- ind %>% filter(step == 365)
ind2 <- p_agentdf %>% filter(id == rand_ind2) 
ind_start2 <- ind2 %>% filter(step == 1)
ind_mid2 <- ind2 %>% filter(step == 183)
ind_end2 <- ind2 %>% filter(step == 365)
p1 <- ggplot(ind, aes(longitude,latitude,color=step)) + 
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#a1dab4", fill = "#a1dab4", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "#2c7fb8", fill = "#2c7fb8", alpha=0.4)  +
  annotate("text", x = 2450, y = 29500, label = 'N', size = 7) +
  geom_segment(aes(x = 2450, y = 22500, xend = 2450, yend = 28000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_path()  +
  geom_path(data=ind2, aes(longitude,latitude,color=step)) +
  scale_color_gradient(low = '#bdbdbd', high = '#000000') +
  geom_point(data = ind_start, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4, stroke = 2) +
  geom_point(data = ind_mid, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4, stroke = 2) +
  geom_point(data = ind_end, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4, stroke = 2) +
  geom_point(data = ind_start2, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4, stroke = 2) +
  geom_point(data = ind_mid2, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4, stroke = 2) +
  geom_point(data = ind_end2, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4, stroke = 2) +
  xlim(c(-2500,2500)) +
  ylim(c(min(n_agentdf$latitude),max(n_agentdf$latitude))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        text = element_text(size = 14))

p12 <- ggplot(p_lats_winter, aes(latitude)) + geom_density(bw=1200) + 
  geom_density(data = p_lats_summer, aes(latitude), color = "gray", fill = "gray", alpha = 0.5, bw=1200) + 
  annotate("rect", fill = "#a1dab4", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h1_lat - 450, xmax = h1_lat + 450) +
  annotate("rect", fill = "#2c7fb8", alpha = 0.4, 
           ymin = -Inf, ymax = Inf,
           xmin = h2_lat - 450, xmax = h2_lat + 450) +
  geom_vline(xintercept = h1_lat, color = "#a1dab4") +
  geom_vline(xintercept = h2_lat, color = "#2c7fb8") +
  xlim(c(100,29900)) +
  coord_flip() +
  ylab("Density") +
  xlab(expression("S  " %<-% "  Latitude  " %->% "  N")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())

layout <- "
AAAAAAAAABB
AAAAAAAAABB
AAAAAAAAABB
"

tiff("outputs/figures/2Fig3d.tiff",units="in", width=4.5,height=3,res=300)
p1 + p12 + plot_layout(design = layout)
dev.off()

