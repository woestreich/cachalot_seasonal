## simulation figures, 2nd version

## packages
library(tidyr)
library(ggplot2)
library(ggExtra)
library(lubridate)
## clear variables
rm(list=ls())

# hydrophone info
## hydrophone locations
h1_lat <- 5000
h1_lon <- 0
h2_lat <- 25000
h2_lon <- 0
detection_range <- 450

#################### Panel A simulated individual track ########################
n_agentdf <- read.csv("outputs/files/simulation_outputs/nomads_agentdf.csv")
#rand_ind <- round(runif(1,1,100))
rand_ind <- 81
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
n_lats <- read.csv("outputs/files/simulation_outputs/nomads_lats.csv")
# remove 1st year to minimize impacts of initial conditions
n_lats <- n_lats %>% filter(yr > 1)
n_lats_winter <- n_lats %>% filter(month == 1 | month == 12 | month == 2)
n_lats_summer <- n_lats %>% filter(month == 7 | month == 6 | month == 8)

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

tiff("outputs/figures/Fig3a.tiff",units="in", width=4.5,height=3,res=300)
p1 + p12 + plot_layout(design = layout)
dev.off()

#################### Panel B cartoon empirical results #########################
a <- as.data.frame(matrix(NA,12,2))
colnames(a) <- c("x","y")
a$x <- seq(1,12)
a$y <- 50
pa <- ggplot(a, aes(x,y)) + geom_point(color="white") +
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
  annotate("text", label = "Jan", x = 1, y = -8, size = 3) +
  annotate("text", label = "Feb", x = 2, y = -8, size = 3) +
  annotate("text", label = "Mar", x = 3, y = -8, size = 3) +
  annotate("text", label = "Apr", x = 4, y = -8, size = 3) +
  annotate("text", label = "May", x = 5, y = -8, size = 3) +
  annotate("text", label = "Jun", x = 6, y = -8, size = 3) +
  annotate("text", label = "Jul", x = 7, y = -8, size = 3) +
  annotate("text", label = "Aug", x = 8, y = -8, size = 3) +
  annotate("text", label = "Sep", x = 9, y = -8, size = 3) +
  annotate("text", label = "Oct", x = 10, y = -8, size = 3) +
  annotate("text", label = "Nov", x = 11, y = -8, size = 3) +
  annotate("text", label = "Dec", x = 12, y = -8, size = 3) +
  xlab("") +
  ylab("Percent of days with\nsperm whale present") +
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
tiff("outputs/figures/Fig3b.tiff",units="in", width=4.5,height=2.5,res=300)
pa
dev.off()


#################### NOMADIC RESOURCE TRACKING #################################
n_agentdf <- read.csv("outputs/files/simulation_outputs/nomads_agentdf.csv")
n_h1perc <- read.csv("outputs/files/simulation_outputs/nomads_h1perc.csv")
n_h2perc <- read.csv("outputs/files/simulation_outputs/nomads_h2perc.csv")
n_lats <- read.csv("outputs/files/simulation_outputs/nomads_lats.csv")
# remove 1st year to minimize impacts of initial conditions
n_lats <- n_lats %>% filter(yr > 1)
n_lats_winter <- n_lats %>% filter(month == 1 | month == 12 | month == 2)
n_lats_summer <- n_lats %>% filter(month == 7 | month == 6 | month == 8)

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

p3 <- ggplot(n_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p4 <- ggplot(n_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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

p5 <- ggplot(data.frame(l = p3$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p3$labels$y <- ""
p4$labels$y <- ""
p34 <- (p3 / plot_spacer() / p4) + plot_layout(heights = c(4, -1.2 ,4))
p345 <- p5 + p34 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
"

tiff("outputs/figures/Fig3_n.tiff",units="in", width=4.5,height=2.5,res=300)
p345 + p12 + plot_layout(design = layout)
dev.off()


#################### MIGRATORY RESOURCE TRACKING ###############################
t_agentdf <- read.csv("outputs/files/simulation_outputs/trackers_agentdf.csv")
t_h1perc <- read.csv("outputs/files/simulation_outputs/trackers_h1perc.csv")
t_h2perc <- read.csv("outputs/files/simulation_outputs/trackers_h2perc.csv")
t_lats <- read.csv("outputs/files/simulation_outputs/trackers_lats.csv")
# remove 1st year to minimize impacts of initial conditions
t_lats <- t_lats %>% filter(yr > 1)
t_lats_winter <- t_lats %>% filter(month == 1 | month == 12 | month == 2)
t_lats_summer <- t_lats %>% filter(month == 7 | month == 6 | month == 8)

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

p3 <- ggplot(t_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p4 <- ggplot(t_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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

p5 <- ggplot(data.frame(l = p3$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p3$labels$y <- ""
p4$labels$y <- ""
p34 <- (p3 / plot_spacer() / p4) + plot_layout(heights = c(4, -1.2 ,4))
p345 <- p5 + p34 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
"

tiff("outputs/figures/Fig3_t.tiff",units="in", width=4.5,height=2.5,res=300)
p345 + p12 + plot_layout(design = layout)
dev.off()


#################### "STRAIGHTLINE" MIGRATION BETWEEN DISTINCT HABITATS ########
m_agentdf <- read.csv("outputs/files/simulation_outputs/migrants_agentdf.csv")
m_h1perc <- read.csv("outputs/files/simulation_outputs/migrants_h1perc.csv")
m_h2perc <- read.csv("outputs/files/simulation_outputs/migrants_h2perc.csv")
m_lats <- read.csv("outputs/files/simulation_outputs/migrants_lats.csv")
# remove 1st year to minimize impacts of initial conditions
m_lats <- m_lats %>% filter(yr > 1)
m_lats_winter <- m_lats %>% filter(month == 1 | month == 12 | month == 2)
m_lats_summer <- m_lats %>% filter(month == 7 | month == 6 | month == 8)

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

p3 <- ggplot(m_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p4 <- ggplot(m_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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

p5 <- ggplot(data.frame(l = p3$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p3$labels$y <- ""
p4$labels$y <- ""
p34 <- (p3 / plot_spacer() / p4) + plot_layout(heights = c(4, -1.2 ,4))
p345 <- p5 + p34 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
"

tiff("outputs/figures/Fig3_m.tiff",units="in", width=4.5,height=2.5,res=300)
p345 + p12 + plot_layout(design = layout)
dev.off()

#################### PARTIAL MIGRATION #########################################
p_agentdf <- read.csv("outputs/files/simulation_outputs/partial_agentdf.csv")
p_h1perc <- read.csv("outputs/files/simulation_outputs/partial_h1perc.csv")
p_h2perc <- read.csv("outputs/files/simulation_outputs/partial_h2perc.csv")
p_lats <- read.csv("outputs/files/simulation_outputs/partial_lats.csv")
# remove 1st year to minimize impacts of initial conditions
p_lats <- p_lats %>% filter(yr > 1)
p_lats_winter <- p_lats %>% filter(month == 1 | month == 12 | month == 2)
p_lats_summer <- p_lats %>% filter(month == 7 | month == 6 | month == 8)

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

p3 <- ggplot(p_h2perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#2c7fb8") +
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
  ylim(c(-10,100)) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 14))
p4 <- ggplot(p_h1perc, aes(as.factor(month), perc)) + geom_boxplot(fill = "#a1dab4") +
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

p5 <- ggplot(data.frame(l = p3$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")
p3$labels$y <- ""
p4$labels$y <- ""
p34 <- (p3 / plot_spacer() / p4) + plot_layout(heights = c(4, -1.2 ,4))
p345 <- p5 + p34 + plot_layout(widths = c(1,20))

layout <- "
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
AABBBBBBBBBBBCC
"

tiff("outputs/figures/Fig3_p.tiff",units="in", width=4.5,height=2.5,res=300)
p345 + p12 + plot_layout(design = layout)
dev.off()

