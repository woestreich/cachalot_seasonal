

library(tidyverse)
library(lubridate)
library(patchwork)
rm(list = ls())

## years, individuals, days, and timesteps
n.years <- 1 # number years to simulate
n.ind <- 100 # number of individuals to simulate
n.days <- 365 # days in year
increment.days <- 60*60*24 # daily increment for each step 
sdt <- "2023-01-01 00:00:00" # start time for timekeeping

## probabilities defining movement
pr <- 0.1 # probability of behavioral state switching
pr_n <- c(seq(0.8, 0.2, length.out = 6),seq(0.2, 0.8, length.out = 6))

## hydrophone locations
h1_lat <- 5000
h1_lon <- 0
h2_lat <- 25000
h2_lon <- 0

## initial condition bounds
lon_min <- -200
lon_max <- 200
pr2a <- 0.2
pr2b <- 0.5
# south initial conditions
lat_min_s <- 0
lat_max_s <- 10000
# mid initial conditions
lat_min_m <- 10000
lat_max_m <- 20000
# north initial conditions
lat_min_n <- 20000
lat_max_n <- 30000

## arena boundaries
long_bound_max <- 2000
long_bound_min <- -2000
lat_bound_max <- 30000
lat_bound_min <- 0

## set up dfs for detection % by month at each hydrophone
h1_perc <- data.frame(matrix(0,12*n.years,3))
h2_perc <- data.frame(matrix(0,12*n.years,3))
colnames(h1_perc) <- c("yr","month","perc")
colnames(h2_perc) <- c("yr","month","perc")
detection_range <- 450

## set up df for keeping track of all agents' latitudes at each daily step of the simulation
lats.df <- data.frame(matrix(0,n.ind*n.days*n.years,4))
colnames(lats.df) <- c("latitude","longitude","month","yr")

##### LOOP THROUGH YEARS #####
for (y in 1:n.years) {
  ## placeholder to keep track of place for storing all agent latitudes over all time steps
  jj1 <- ((y-1)*n.ind*n.days)+1
  jj2 <- jj1-1+(n.days*n.ind)
  
  ## initialize behavioral state and agent position data frames for the given year
  state.df <- data.frame(matrix(0,n.ind*n.days,4))
  colnames(state.df) <- c("id","step","value","state")
  agent.df <- data.frame(matrix(0,n.ind*n.days,12))
  colnames(agent.df) <- c("yr","month","id","datetime","step","longitude","latitude","heading","dist_h1","dist_h2","x_change","y_change")
  class(agent.df$datetime) <- 'POSIXct'
  
  ##### LOOP THROUGH INDIVIDUALS FOR EACH YEAR #####
  for (i in 1:n.ind) {
    
    ##### LOOP THROUGH DAYS FOR EACH INDIVIDUAL #####
    for (d in 1:n.days) {
      ## progress tracker
      print(paste('year:',y,'|| individual:',i,'|| day:',d))
      ## index placeholder 
      k <- (i-1)*n.days + d
      
      ##### calculate agent information for the given day
      ## if day 1 of year 1:
      if (y == 1 & d == 1) {
        ## auxiliary information
        agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
        agent.df$yr[k] <- y
        agent.df$month[k] <- month(agent.df$datetime[k])
        agent.df$id[k] <- i
        agent.df$step[k] <- d
        ## behavioral state
        state.df$id[k] <- i
        state.df$step[k] <- d
        rand <- runif(1,0,1)
        if (rand >= 0.5) {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Forage'
        } else {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Search'
        }
        ## position
        agent.df$longitude[k] <- runif(1,lon_min,lon_max)
        x <- runif(1,0,1)
        if (x < pr2a) {
          agent.df$latitude[k] <- runif(1,lat_min_n,lat_max_n)
        } else if (x < pr2b & x > pr2a) {
          agent.df$latitude[k] <- runif(1,lat_min_m,lat_max_m)
        } else {
          agent.df$latitude[k] <- runif(1,lat_min_s,lat_max_s)
        }
        
        ## heading
        agent.df$heading[k] <- runif(1,0.0001,360)
        ## distances from hydrophones
        agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
        agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
        ## x_change and y_change
        agent.df$x_change[k] <- 0
        agent.df$y_change[k] <- 0
      } 
      
      ## if day 1 but year > 1:
      else if (y > 1 & d == 1) {
        ## get info from day 365 of the previous year for the present individual
        old.state <- state.lastyear %>% filter(id == i)
        old.agent <- agent.lastyear %>% filter(id == i)
        ## auxiliary information
        agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
        agent.df$yr[k] <- y
        agent.df$month[k] <- month(agent.df$datetime[k])
        agent.df$id[k] <- i
        agent.df$step[k] <- d
        ## behavioral state
        state.df$id[k] <- i
        state.df$step[k] <- d
        prev.state <- old.state$state[1]
        rand <- runif(1,0,1)
        if (rand >= pr && prev.state == 'Forage' ) {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Forage'
        } else if (rand < pr && prev.state == 'Forage') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Search'
        } else if (rand >= pr && prev.state == 'Search') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Search'
        } else if (rand < pr && prev.state == 'Search') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Forage'
        }
        ## position and heading
        if (state.df$state[k] == 'Forage') {
          step <- rgamma(1, shape = 2, rate = 2) * 100
          # Uniform distribution for direction during forage state
          agent.df$heading[k] <- runif(1,0.0001,360)
          # Determine the angle to perform trig
          angle <- 0 
          if (agent.df$heading[k] > 0 &&  agent.df$heading[k] <= 90) {
            angle <- (90 -  agent.df$heading[k])
          } else if (agent.df$heading[k] > 90 && agent.df$heading[k] <= 180) {
            angle <- (agent.df$heading[k] - 90)
          } else if (agent.df$heading[k] > 180 && agent.df$heading[k] <= 270) {
            angle <- (270 - agent.df$heading[k])
          } else {
            angle <- (agent.df$heading[k] - 270)
          }
          # Use sine to determine the movement in y (latitude)
          rad_y <- angle*0.0174532925
          y_change <- sin(rad_y)*step
          # Use cosine to determine the movement in x (longitude)
          rad_x <- angle*0.0174532925
          x_change <- cos(rad_x)*step
          ## x_change and y_change
          agent.df$x_change[k] <- x_change
          agent.df$y_change[k] <- y_change
          # Determine whether to add or subtract new value based on heading
          if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] > 0 && agent.df$heading[k] <= 90) {
            agent.df$latitude[k] <- old.agent$latitude[1] + y_change
          } else {
            agent.df$latitude[k] <- old.agent$latitude[1] - y_change
          }
          if (agent.df$heading[k] > 0 && agent.df$heading[k] <= 180) {
            agent.df$longitude[k] <- old.agent$longitude[1] + x_change
          } else {
            agent.df$longitude[k] <- old.agent$longitude[1] - x_change
          }
        } 
        else if (state.df$state[k] == 'Search') {
          #step <- runif(1,0,700)
          step <- runif(1,0,500)
          # Use N-S search heading probabilities for search behavioral state
          n_s <- runif(1,0,1)
          if (n_s < pr_n[agent.df$month[k]]) {
            agent.df$heading[k] <- rnorm(1,0,10)
            if (agent.df$heading[k] < 0) {
              agent.df$heading[k] <- 360 + agent.df$heading[k]
            }
          } else {
            agent.df$heading[k] <- rnorm(1,180,10)
          }
          # Determine the angle to perform trig
          angle <- 0 
          if (agent.df$heading[k] > 0 && agent.df$heading[k] <= 90) {
            angle <- (90 - agent.df$heading[k])
          } else if (agent.df$heading[k] > 90 && agent.df$heading[k] <= 180) {
            angle <- (agent.df$heading[k] - 90)
          } else if (agent.df$heading[k] > 180 && agent.df$heading[k] <= 270) {
            angle <- (270 - agent.df$heading[k])
          } else {
            angle <- (agent.df$heading[k] - 270)
          }
          # Use sine to determine the movement in y (latitude)
          rad_y <- angle*0.0174532925
          y_change <- sin(rad_y)*step
          # Use cosine to determine the movement in x (longitude)
          rad_x <- angle*0.0174532925
          x_change <- cos(rad_x)*step
          ## x_change and y_change
          agent.df$x_change[k] <- x_change
          agent.df$y_change[k] <- y_change
          # new positions
          if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
            agent.df$latitude[k] <- old.agent$latitude[1] + y_change
          } else {
            agent.df$latitude[k] <- old.agent$latitude[1] - y_change
          }
          if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
            agent.df$longitude[k] <- agent.df$longitude[1] + x_change
          } else {
            agent.df$longitude[k] <- agent.df$longitude[1] - x_change
          }
        }
        ## keep position within arena
        if (agent.df$longitude[k] > long_bound_max) {
          agent.df$longitude[k] <- long_bound_max
        } 
        else if (agent.df$longitude[k] < long_bound_min) {
          agent.df$longitude[k] <- long_bound_min
        } else {
          agent.df$longitude[k] <- agent.df$longitude[k]
        }
        if (agent.df$latitude[k] > lat_bound_max) {
          agent.df$latitude[k] <- lat_bound_max
        } 
        else if (agent.df$latitude[k] < lat_bound_min) {
          agent.df$latitude[k] <- lat_bound_min
        } 
        ## distances from hydrophones
        agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
        agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
      }
      
      ## for all other days:
      else {
        ## auxiliary information
        agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
        agent.df$yr[k] <- y
        agent.df$month[k] <- month(agent.df$datetime[k])
        agent.df$id[k] <- i
        agent.df$step[k] <- d
        ## behavioral state
        state.df$id[k] <- i
        state.df$step[k] <- d
        prev.state <- state.df$state[k-1]
        rand <- runif(1,0,1)
        if (rand >= pr && prev.state == 'Forage' ) {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Forage'
        } else if (rand < pr && prev.state == 'Forage') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Search'
        } else if (rand >= pr && prev.state == 'Search') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Search'
        } else if (rand < pr && prev.state == 'Search') {
          state.df$value[k] <- rand
          state.df$state[k] <- 'Forage'
        }
        ## position and heading
        if (state.df$state[k] == 'Forage') {
          step <- rgamma(1, shape = 2, rate = 2) * 100
          # Uniform distribution for direction during forage state
          agent.df$heading[k] <- runif(1,0.0001,360)
          # Determine the angle to perform trig
          angle <- 0 
          if (agent.df$heading[k] > 0 &&  agent.df$heading[k] <= 90) {
            angle <- (90 -  agent.df$heading[k])
          } else if (agent.df$heading[k] > 90 && agent.df$heading[k] <= 180) {
            angle <- (agent.df$heading[k] - 90)
          } else if (agent.df$heading[k] > 180 && agent.df$heading[k] <= 270) {
            angle <- (270 - agent.df$heading[k])
          } else {
            angle <- (agent.df$heading[k] - 270)
          }
          # Use sine to determine the movement in y (latitude)
          rad_y <- angle*0.0174532925
          y_change <- sin(rad_y)*step
          # Use cosine to determine the movement in x (longitude)
          rad_x <- angle*0.0174532925
          x_change <- cos(rad_x)*step
          ## x_change and y_change
          agent.df$x_change[k] <- x_change
          agent.df$y_change[k] <- y_change
          # Determine whether to add or subtract new value based on heading
          if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] > 0 && agent.df$heading[k] <= 90) {
            agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
          } else {
            agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
          }
          if (agent.df$heading[k] > 0 && agent.df$heading[k] <= 180) {
            agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
          } else {
            agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
          }
        } 
        else if (state.df$state[k] == 'Search') {
          step <- runif(1,0,500)
          # Use N-S search heading probabilities for search behavioral state
          n_s <- runif(1,0,1)
          if (n_s < pr_n[agent.df$month[k]]) {
            agent.df$heading[k] <- rnorm(1,0,10)
            if (agent.df$heading[k] < 0) {
              agent.df$heading[k] <- 360 + agent.df$heading[k]
            }
          } else {
            agent.df$heading[k] <- rnorm(1,180,10)
          }
          # Determine the angle to perform trig
          angle <- 0 
          if (agent.df$heading[k] > 0 && agent.df$heading[k] <= 90) {
            angle <- (90 - agent.df$heading[k])
          } else if (agent.df$heading[k] > 90 && agent.df$heading[k] <= 180) {
            angle <- (agent.df$heading[k] - 90)
          } else if (agent.df$heading[k] > 180 && agent.df$heading[k] <= 270) {
            angle <- (270 - agent.df$heading[k])
          } else {
            angle <- (agent.df$heading[k] - 270)
          }
          # Use sine to determine the movement in y (latitude)
          rad_y <- angle*0.0174532925
          y_change <- sin(rad_y)*step
          # Use cosine to determine the movement in x (longitude)
          rad_x <- angle*0.0174532925
          x_change <- cos(rad_x)*step
          ## x_change and y_change
          agent.df$x_change[k] <- x_change
          agent.df$y_change[k] <- y_change
          # new positions
          if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
            agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
          } else {
            agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
          }
          if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
            agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
          } else {
            agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
          }
        }
        ## keep position within arena
        if (agent.df$longitude[k] > long_bound_max) {
          agent.df$longitude[k] <- long_bound_max
        } 
        else if (agent.df$longitude[k] < long_bound_min) {
          agent.df$longitude[k] <- long_bound_min
        } 
        if (agent.df$latitude[k] > lat_bound_max) {
          agent.df$latitude[k] <- lat_bound_max
        } 
        else if (agent.df$latitude[k] < lat_bound_min) {
          agent.df$latitude[k] <- lat_bound_min
        } 
        ## distances from hydrophones
        agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
        agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
      }
    }
  }
  ## If final day of year and final individual: 
  ## (1) calculate % of days w/ clicks present at each hydrophone for each month of year
  ## (2) store each agent's information from day 365 for calculating day 1 info next year
  ## (3) clear agent.df and state.df dataframes (but keep for vis if final year of model run)
  if (d == n.days & i == n.ind) {
    print(paste('calculating monthly % presence for year',y))
    for (m in 1:12) {
      # index placeholder
      kk <- (y-1)*12 + m
      # filter to current month
      dm <- agent.df %>% filter(month == m & yr == y)
      # daily presence vs. absence dfs for each hydrophone in present month
      h1_pres <- data.frame(matrix(0,length(dm$step)/n.ind,1))
      colnames(h1_pres) <- "yn"
      h2_pres <- data.frame(matrix(0,length(dm$step)/n.ind,1))
      colnames(h2_pres) <- "yn"
      for (dd in min(dm$step):max(dm$step)) {
        dm_day <- dm %>% filter(step == dd)
        if (any(dm_day$dist_h1 < detection_range)) {
          h1_pres$yn[dd-min(dm$step)+1] <- 1
        } else {
          h1_pres$yn[dd-min(dm$step)+1] <- 0
        }
        if (any(dm_day$dist_h2 < detection_range)) {
          h2_pres$yn[dd-min(dm$step)+1] <- 1
        } else {
          h2_pres$yn[dd-min(dm$step)+1] <- 0
        }
      }
      # hydrophone 1 monthly values
      h1_perc$yr[kk] <- y
      h1_perc$month[kk] <- m
      h1_perc$perc[kk] <- (sum(h1_pres$yn)/length(h1_pres$yn))*100
      # hydrophone 2 monthly values
      h2_perc$yr[kk] <- y
      h2_perc$month[kk] <- m
      h2_perc$perc[kk] <- (sum(h2_pres$yn)/length(h2_pres$yn))*100
    }
    
    ## store lat & lon of all agents at each timestep
    lats.df$latitude[jj1:jj2] <- agent.df$latitude
    lats.df$longitude[jj1:jj2] <- agent.df$longitude
    lats.df$month[jj1:jj2] <- agent.df$month
    lats.df$yr[jj1:jj2] <- agent.df$yr
    
    ## store last year info and clear some variables 
    if (y < n.years) {
      state.lastyear <- state.df %>% filter(step == n.days)
      agent.lastyear <- agent.df %>% filter(step == n.days)
      rm(state.df, agent.df, h1_pres, h2_pres)
    }
  }
}


########### DIAGNOSTIC PLOTS ######################
ggplot(agent.df, aes(x=longitude, y = latitude)) +
  geom_path() +
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "red", fill = "red", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "blue", fill = "blue", alpha=0.4) 
for (m in 1:12) {
  a <- agent.df %>% filter(month == m)
  print(ggplot(a, aes(x=longitude, y = latitude)) +
          geom_point() +
          annotate("polygon",
                   x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
                   y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
                   color = "red", fill = "red", alpha=0.4) +
          annotate("polygon",
                   x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
                   y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
                   color = "blue", fill = "blue", alpha=0.4)  +
          xlim(c(-2500,2500)) +
          ylim(c(min(agent.df$latitude),max(agent.df$latitude))) +
          ggtitle(m))
}

# h1 <- h1_perc %>% filter(yr == 2)
# h2 <- h2_perc %>% filter(yr == 2)
# ggplot(h1, aes(x = month, y = perc)) + 
#   geom_point(color = "red") +
#   geom_point(data = h2, aes(x = month, y = perc), color = "blue")

h1 <- h1_perc %>% filter(yr > 1)
h2 <- h2_perc %>% filter(yr > 1)
pa <- ggplot(h1, aes(x = as.factor(month), y = perc)) + geom_boxplot(color = "red")
pb <- ggplot(h2, aes(x = as.factor(month), y = perc)) + geom_boxplot(color = "blue")
pa/pb

ggplot(agent.df, aes(x=step, y = latitude)) + geom_point(color = "black") +
  geom_hline(yintercept = h1_lat, color = "red") +
  geom_hline(yintercept = h2_lat, color = "blue") 

ggplot(agent.df, aes(x=step, y = longitude)) + geom_point()
ggplot(lats.df, aes(latitude)) + geom_histogram()
ggplot(lats.df, aes(longitude)) + geom_histogram()

rand_ind <- round(runif(1,1,n.ind))
#rand_ind <- 1
ind <- agent.df %>% filter(id == rand_ind) 
ind_start <- ind %>% filter(step == 1)
ind_mid <- ind %>% filter(step == 183)
ind_end <- ind %>% filter(step == 365)
ggplot(ind, aes(longitude,latitude,color=step)) + 
  annotate("polygon",
           x=h1_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h1_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "red", fill = "red", alpha=0.4) +
  annotate("polygon",
           x=h2_lon + detection_range*cos(seq(0,2*pi,length.out=100)),
           y=h2_lat + detection_range*sin(seq(0,2*pi,length.out=100)),
           color = "blue", fill = "blue", alpha=0.4)  +
  annotate("text", x = -2450, y = 29500, label = 'F', size = 7, fontface = "bold") +
  annotate("text", x = 2450, y = 29500, label = 'N', size = 7) +
  geom_segment(aes(x = 2450, y = 22500, xend = 2450, yend = 28000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_path()  +
  scale_color_gradient(low = '#bdbdbd', high = '#000000') +
  geom_point(data = ind_start, aes(longitude, latitude), shape = 21, color = 'black', fill = 'white', size = 4) +
  geom_point(data = ind_mid, aes(longitude, latitude), shape = 21, color = 'black', fill = 'gray', size = 4) +
  geom_point(data = ind_end, aes(longitude, latitude), shape = 21, color = 'black', fill = 'black', size = 4) +
  xlim(c(-2500,2500)) +
  ylim(c(min(agent.df$latitude),max(agent.df$latitude))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        text = element_text(size = 14))

##### save files for later formal visualization
write.csv(agent.df, file = "outputs/files/simulation_outputs/trackers_agentdf.csv",row.names = FALSE)
write.csv(h1_perc, file = "outputs/files/simulation_outputs/trackers_h1perc.csv",row.names = FALSE)
write.csv(h2_perc, file = "outputs/files/simulation_outputs/trackers_h2perc.csv",row.names = FALSE)
write.csv(lats.df, file = "outputs/files/simulation_outputs/trackers_lats.csv",row.names = FALSE)


