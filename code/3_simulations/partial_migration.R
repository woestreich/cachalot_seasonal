

library(tidyverse)
library(lubridate)
library(patchwork)
rm(list = ls())

## years, individuals, days, and timesteps
n.years <- 10 # number years to simulate
n.ind <- 100 # number of individuals to simulate
n.days <- 365 # days in year
increment.days <- 60*60*24 # daily increment for each step 
sdt <- "2023-01-01 00:00:00" # start time for timekeeping

## probabilities defining movement during nomadic periods
pr <- 0.1 # probability of behavioral state switching

## hydrophone locations
h1_lat <- 5000
h1_lon <- 0
h2_lat <- 25000
h2_lon <- 0

# ## south and north nomadic range boundaries
lon_min <- -2000
lon_max <- 2000
lat_min_s <- 0
lat_max_s <- 10000
lat_min_n <- 20000
lat_max_n <- 30000

## for nomadic individuals or resident foragers, there is no n-s tracking 
## behavior with longer step lengths, so latitudinal and longitudinal mvmt 
## magnitudes are scaled by the bounding domain 
xy_adjust <- (lat_max_s - lat_min_s)/(lon_max-lon_min) 

## set up dfs for detection % by month at each hydrophone
h1_perc <- data.frame(matrix(0,12*n.years,3))
h2_perc <- data.frame(matrix(0,12*n.years,3))
colnames(h1_perc) <- c("yr","month","perc")
colnames(h2_perc) <- c("yr","month","perc")
detection_range <- 450

## set up df for keeping track of all agents' latitudes at each daily step of the simulation
lats.df <- data.frame(matrix(0,n.ind*n.days*n.years,3))
colnames(lats.df) <- c("latitude","month","yr")

##### LOOP THROUGH YEARS #####
for (y in 1:n.years) {
  ## placeholder to keep track of place for storing all agent latitudes over all time steps
  jj1 <- ((y-1)*n.ind*n.days)+1
  jj2 <- jj1-1+(n.days*n.ind)
  
  ## initialize behavioral state and agent position data frames for the given year
  state.df <- data.frame(matrix(0,n.ind*n.days,4))
  colnames(state.df) <- c("id","step","value","state")
  agent.df <- data.frame(matrix(0,n.ind*n.days,11))
  colnames(agent.df) <- c("yr","month","id","datetime","step","longitude","latitude","heading","dist_h1","dist_h2","x_change")
  class(agent.df$datetime) <- 'POSIXct'
  
  ##### LOOP THROUGH INDIVIDUALS FOR EACH YEAR #####
  for (i in 1:n.ind) {
    
    ## assign migratory or resident strategy to 50% of individuals
    ## MIGRANTS
    if (i < ((n.ind/2)+1)) {
      strategy <- 'migrant'
      print(paste(i,'- migrant'))
      ## start the run for each individual by defining their migration transition timing for the given year
      ## timing and variability of migratory transitions
      print(paste('assigning behavioral states for ind',i,'/ year',y))
      rand1 <- rnorm(1,122,5)
      rand2 <- rand1 + rnorm(1,61,5)
      rand3 <- rand2 + rnorm(1,122,5)
      # migration step lengths
      migr_step_n <- (22000)/(rand2-rand1)
      migr_step_s <- (22000)/(rand2-rand1)
      for (d in 1:n.days) {
        ## index placeholder 
        k <- (i-1)*n.days + d
        if (d < rand1) {
          state.df$state[k] <- 'nomad_s'
        } else if (d >= rand1 && d < rand2) {
          state.df$state[k] <- 'migrate_n'
        } else if (d >= rand2 && d < rand3) {
          state.df$state[k] <- 'nomad_n'
        } else if (d >= rand3) {
          state.df$state[k] <- 'migrate_s'
        }  
        state.df$id[k] <- i
        state.df$step[k] <- d
      }
      
      ## now that behavioral states are assigned for each day, can proceed with positions
      for (d in 1:n.days) {
        ## progress tracker
        print(paste('year:',y,'|| individual:',i,'|| day:',d))
        ## index placeholder 
        k <- (i-1)*n.days + d
        
        ##### IF DAY 1 OF YEAR 1
        if (y == 1 & d == 1) {
          ## auxiliary information
          agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
          agent.df$yr[k] <- y
          agent.df$month[k] <- month(agent.df$datetime[k])
          agent.df$id[k] <- i
          agent.df$step[k] <- d
          ## each individual starts the year in the nomad_s state, so determine forage vs. search 50-50
          rand <- runif(1,0,1)
          if (rand >= 0.5) {
            curr.state <- 'Forage'
          } else {
            curr.state <- 'Search'
          }
          ## position
          agent.df$longitude[k] <- runif(1,lon_min,lon_max)
          agent.df$latitude[k] <- runif(1,lat_min_s,lat_max_s)
          agent.df$x_change[k] <- 0
          ## heading
          agent.df$heading[k] <- runif(1,0.0001,360)
          ## distances from hydrophones
          agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
          agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
          ## set previous state to current state for next step
          prev.state <- curr.state
        }
        
        ##### IF DAY 1, BUT YEAR > 1
        else if (y > 1 & d == 1) {
          ## get info from day 365 of the previous year for the present individual
          old.agent <- agent.lastyear %>% filter(id == i)
          ## auxiliary information
          agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
          agent.df$yr[k] <- y
          agent.df$month[k] <- month(agent.df$datetime[k])
          agent.df$id[k] <- i
          agent.df$step[k] <- d
          ## each individual starts the year in the nomad_s state, so determine forage vs. search 50-50
          rand <- runif(1,0,1)
          if (rand >= 0.5) {
            curr.state <- 'Forage'
          } else {
            curr.state <- 'Search'
          }
          if (curr.state == 'Forage') {
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
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
          else if (curr.state == 'Search') {
            step <- runif(1,0,300)
            #pick random direction for re-initiating search
            agent.df$heading[k] <- runif(1,0.0001,360)
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
            # new positions
            if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
              agent.df$longitude[k] <- old.agent$longitude[1] + x_change
            } else {
              agent.df$longitude[k] <- old.agent$longitude[1] - x_change
            }
            if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
              agent.df$latitude[k] <- old.agent$latitude[1] + y_change
            } else {
              agent.df$latitude[k] <- old.agent$latitude[1] - y_change
            }
          }
          ## keep position within arena
          if (agent.df$longitude[k] > lon_max) {
            agent.df$longitude[k] <- lon_max
          } 
          else if (agent.df$longitude[k] < lon_min) {
            agent.df$longitude[k] <- lon_min
          } 
          if (agent.df$latitude[k] > lat_max_s) {
            agent.df$latitude[k] <- lat_max_s
          } 
          else if (agent.df$latitude[k] < lat_min_s) {
            agent.df$latitude[k] <- lat_min_s
          } 
          ## distances from hydrophones
          agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
          agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
          ## store away current state as previous state for next step
          prev.state <- curr.state
        } 
        
        ##### ALL OTHER DAYS
        else {
          ## auxiliary information
          agent.df$datetime[k] <- as.POSIXct(strptime(sdt, "%Y-%m-%d %H:%M:%S") + (d-1)*increment.days)
          agent.df$yr[k] <- y
          agent.df$month[k] <- month(agent.df$datetime[k])
          agent.df$id[k] <- i
          agent.df$step[k] <- d
          
          ## CASE 1: nomadic in southern range
          if (state.df$state[k] == "nomad_s" & state.df$state[k-1] == "nomad_s") {
            # this is a nomad case, so establish if searching or foraging
            rand <- runif(1,0,1)
            if (rand >= pr && prev.state == 'Forage' ) {
              curr.state <- 'Forage'
            } else if (rand < pr && prev.state == 'Forage') {
              curr.state <- 'Search'
            } else if (rand >= pr && prev.state == 'Search') {
              curr.state <- 'Search'
            } else if (rand < pr && prev.state == 'Search') {
              curr.state <- 'Forage'
            }
            ## position and heading
            if (curr.state == 'Forage') {
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
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
            else if (curr.state == 'Search') {
              step <- runif(1,0,300)
              #pick random direction for re-initiating search
              if (curr.state == 'Search' & prev.state == 'Forage') {
                newhead <- runif(1,0.0001,360)
              }
              #if continuing search, use more directed movement until forage is found
              else if (curr.state == 'Search' & prev.state == 'Search') {
                newhead <- rnorm(1,agent.df$heading[k-1],60)
              }
              #keep heading between 0-360
              if (newhead >= 0 & newhead <= 360) {
                agent.df$heading[k] <- newhead
              } else if (newhead > 360) {
                agent.df$heading[k] <- newhead - 360
              } else if (newhead < 0) {
                agent.df$heading[k] <- 360 + newhead
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
              # new positions
              if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
                agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
              } else {
                agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
              }
              if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
                agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
              } else {
                agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
              }
            }
            ## keep position within arena
            if (agent.df$longitude[k] > lon_max) {
              agent.df$longitude[k] <- lon_max
            } 
            else if (agent.df$longitude[k] < lon_min) {
              agent.df$longitude[k] <- lon_min
            } 
            if (agent.df$latitude[k] > lat_max_s) {
              agent.df$latitude[k] <- lat_max_s
            } 
            else if (agent.df$latitude[k] < lat_min_s) {
              agent.df$latitude[k] <- lat_min_s
            } 
            ## distances from hydrophones
            agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
            agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
            ## store away current state as previous state for next step
            prev.state <- curr.state
          }
          
          ## CASE 2: nomadic in northern range
          if (state.df$state[k] == "nomad_n" & state.df$state[k-1] == "nomad_n") {
            # this is a nomad case, so establish if searching or foraging
            rand <- runif(1,0,1)
            if (rand >= pr && prev.state == 'Forage' ) {
              curr.state <- 'Forage'
            } else if (rand < pr && prev.state == 'Forage') {
              curr.state <- 'Search'
            } else if (rand >= pr && prev.state == 'Search') {
              curr.state <- 'Search'
            } else if (rand < pr && prev.state == 'Search') {
              curr.state <- 'Forage'
            }
            ## position and heading
            if (curr.state == 'Forage') {
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
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
            else if (curr.state == 'Search') {
              step <- runif(1,0,300)
              #pick random direction for re-initiating search
              if (curr.state == 'Search' & prev.state == 'Forage') {
                newhead <- runif(1,0.0001,360)
              }
              #if continuing search, use more directed movement until forage is found
              else if (curr.state == 'Search' & prev.state == 'Search') {
                newhead <- rnorm(1,agent.df$heading[k-1],60)
              }
              #keep heading between 0-360
              if (newhead >= 0 & newhead <= 360) {
                agent.df$heading[k] <- newhead
              } else if (newhead > 360) {
                agent.df$heading[k] <- newhead - 360
              } else if (newhead < 0) {
                agent.df$heading[k] <- 360 + newhead
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
              # new positions
              if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
                agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
              } else {
                agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
              }
              if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
                agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
              } else {
                agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
              }
            }
            ## keep position within arena
            if (agent.df$longitude[k] > lon_max) {
              agent.df$longitude[k] <- lon_max
            } 
            else if (agent.df$longitude[k] < lon_min) {
              agent.df$longitude[k] <- lon_min
            } 
            if (agent.df$latitude[k] > lat_max_n) {
              agent.df$latitude[k] <- lat_max_n
            } 
            else if (agent.df$latitude[k] < lat_min_n) {
              agent.df$latitude[k] <- lat_min_n
            } 
            ## distances from hydrophones
            agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
            agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
            ## store away current state as previous state for next step
            prev.state <- curr.state
          }
          
          ## CASE 3: transitioning from migrate_north to nomad_north
          if (state.df$state[k] == "nomad_n" & state.df$state[k-1] == "migrate_n") {
            # this is a nomad case, so establish if searching or foraging.
            # in this case, agent is coming from migration, so search vs. forage
            # is random, rather than influenced by previous state.
            rand <- runif(1,0,1)
            if (rand >= 0.5) {
              curr.state <- 'Forage'
            } else {
              curr.state <- 'Search'
            }
            ## position and heading
            if (curr.state == 'Forage') {
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
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
            else if (curr.state == 'Search') {
              step <- runif(1,0,300)
              #pick random direction for re-initiating search
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
              y_change <- sin(rad_y)*step*xy_adjust
              # Use cosine to determine the movement in x (longitude)
              rad_x <- angle*0.0174532925
              x_change <- cos(rad_x)*step
              agent.df$x_change[k] <- x_change
              # new positions
              if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
                agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
              } else {
                agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
              }
              if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
                agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
              } else {
                agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
              }
            }
            ## keep position within arena
            if (agent.df$longitude[k] > lon_max) {
              agent.df$longitude[k] <- lon_max
            } 
            else if (agent.df$longitude[k] < lon_min) {
              agent.df$longitude[k] <- lon_min
            } 
            if (agent.df$latitude[k] > lat_max_n) {
              agent.df$latitude[k] <- lat_max_n
            } 
            else if (agent.df$latitude[k] < lat_min_n) {
              agent.df$latitude[k] <- lat_min_n
            } 
            ## distances from hydrophones
            agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
            agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
            ## store away current state as previous state for next step
            prev.state <- curr.state
          }
          
          ## CASE 4: transitioning migrate_south to nomad_south
          ## This transition occurs on the 1st day of the year, so it is handled above
          
          ## CASE 5: migrating north
          if (state.df$state[k] == "migrate_n") {
            ## this is a migration case, so no need to make searching vs. foraging decision
            ## position and heading
            agent.df$heading[k] <- rnorm(1,0,5)
            if (agent.df$heading[k] < 0) {
              agent.df$heading[k] <- 360 + agent.df$heading[k]
            }
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
            y_change <- sin(rad_y)*migr_step_n
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*migr_step_n
            agent.df$x_change[k] <- x_change
            # new positions
            if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
              agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
            } else {
              agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
            }
            if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
              agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
            } else {
              agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
            }
            ## keep position within arena
            if (agent.df$latitude[k] > lat_max_n) {
              agent.df$latitude[k] <- lat_max_n
            } 
            if (agent.df$longitude[k] > lon_max) {
              agent.df$longitude[k] <- lon_max
            } 
            else if (agent.df$longitude[k] < lon_min) {
              agent.df$longitude[k] <- lon_min
            } 
            ## distances from hydrophones
            agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
            agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
          }
          
          
          ## CASE 6: migrating south
          if (state.df$state[k] == "migrate_s") {
            ## this is a migration case, so no need to make searching vs. foraging decision
            ## position and heading
            agent.df$heading[k] <- rnorm(1,180,5)
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
            y_change <- sin(rad_y)*migr_step_s
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*migr_step_s
            agent.df$x_change[k] <- x_change
            # new positions
            if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
              agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
            } else {
              agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
            }
            if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
              agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
            } else {
              agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
            }
            ## keep position within arena
            if (agent.df$latitude[k] < lat_min_s) {
              agent.df$latitude[k] <- lat_min_s
            } 
            if (agent.df$longitude[k] > lon_max) {
              agent.df$longitude[k] <- lon_max
            } 
            else if (agent.df$longitude[k] < lon_min) {
              agent.df$longitude[k] <- lon_min
            } 
            ## distances from hydrophones
            agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
            agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
          }
        }
      }
    }
    
    ## RESIDENTS
    else {
      strategy <- 'resident'
      print(paste(i,'- resident'))
      ##### LOOP THROUGH DAYS FOR EACH INDIVIDUAL #####
      for (d in 1:n.days) {
        ## progress tracker
        print(paste('year:',y,'|| individual:',i,'|| day:',d))
        ## index placeholder 
        k <- (i-1)*n.days + d
        ##### if day 1 of year 1:
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
          agent.df$latitude[k] <- runif(1,lat_min_s,lat_max_s)    
          agent.df$x_change[k] <- 0
          ## heading
          agent.df$heading[k] <- runif(1,0.0001,360)
          ## distances from hydrophones
          agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
          agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
        } 
        
        ##### if day 1 but year > 1:
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
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
            step <- runif(1,0,300)
            #pick random direction for re-initiating search
            if (state.df$state[k] == 'Search' & prev.state == 'Forage') {
              newhead <- runif(1,0.0001,360)
            }
            #if continuing search, use more directed movement until forage is found
            else if (state.df$state[k] == 'Search' & prev.state == 'Search') {
              newhead <- rnorm(1,old.agent$heading[1],60)
            }
            #keep heading between 0-360
            if (newhead >= 0 & newhead <= 360) {
              agent.df$heading[k] <- newhead
            } else if (newhead > 360) {
              agent.df$heading[k] <- newhead - 360
            } else if (newhead < 0) {
              agent.df$heading[k] <- 360 + newhead
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
            # new positions
            if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
              agent.df$longitude[k] <- old.agent$longitude[1] + x_change
            } else {
              agent.df$longitude[k] <- old.agent$longitude[1] - x_change
            }
            if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
              agent.df$latitude[k] <- old.agent$latitude[1] + y_change
            } else {
              agent.df$latitude[k] <- old.agent$latitude[1] - y_change
            }
          }
          ## keep position within arena
          if (agent.df$longitude[k] > lon_max) {
            agent.df$longitude[k] <- lon_max
          } 
          else if (agent.df$longitude[k] < lon_min) {
            agent.df$longitude[k] <- lon_min
          } else {
            agent.df$longitude[k] <- agent.df$longitude[k]
          }
          if (agent.df$latitude[k] > lat_max_s) {  
            agent.df$latitude[k] <- lat_max_s
          } 
          else if (agent.df$latitude[k] < lat_min_s) {
            agent.df$latitude[k] <- lat_min_s
          } 
          ## distances from hydrophones
          agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
          agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
        }
        
        ##### for all other days:
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
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
            step <- runif(1,0,300)
            #pick random direction for re-initiating search
            if (state.df$state[k] == 'Search' & state.df$state[k-1] == 'Forage') {
              newhead <- runif(1,0.0001,360)
            }
            #if continuing search, use more directed movement until forage is found
            else if (state.df$state[k] == 'Search' & state.df$state[k-1] == 'Search') {
              newhead <- rnorm(1,agent.df$heading[k-1],60)
            }
            #keep heading between 0-360
            if (newhead >= 0 & newhead <= 360) {
              agent.df$heading[k] <- newhead
            } else if (newhead > 360) {
              agent.df$heading[k] <- newhead - 360
            } else if (newhead < 0) {
              agent.df$heading[k] <- 360 + newhead
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
            y_change <- sin(rad_y)*step*xy_adjust
            # Use cosine to determine the movement in x (longitude)
            rad_x <- angle*0.0174532925
            x_change <- cos(rad_x)*step
            agent.df$x_change[k] <- x_change
            # new positions
            if (agent.df$heading[k] > 0 & agent.df$heading[k] <= 180) {
              agent.df$longitude[k] <- agent.df$longitude[k-1] + x_change
            } else {
              agent.df$longitude[k] <- agent.df$longitude[k-1] - x_change
            }
            if (agent.df$heading[k] > 270 && agent.df$heading[k] <= 360 || agent.df$heading[k] >= 0 && agent.df$heading[k] <= 90) {
              agent.df$latitude[k] <- agent.df$latitude[k-1] + y_change
            } else {
              agent.df$latitude[k] <- agent.df$latitude[k-1] - y_change
            }
          }
          ## keep position within arena
          if (agent.df$longitude[k] > lon_max) {
            agent.df$longitude[k] <- lon_max
          } 
          else if (agent.df$longitude[k] < lon_min) {
            agent.df$longitude[k] <- lon_min
          } 
          if (agent.df$latitude[k] > lat_max_s) {
            agent.df$latitude[k] <- lat_max_s
          } 
          else if (agent.df$latitude[k] < lat_min_s) {
            agent.df$latitude[k] <- lat_min_s
          } 
          ## distances from hydrophones
          agent.df$dist_h1[k] <- sqrt((h1_lat - agent.df$latitude[k])^2 + (h1_lon - agent.df$longitude[k])^2)
          agent.df$dist_h2[k] <- sqrt((h2_lat - agent.df$latitude[k])^2 + (h2_lon - agent.df$longitude[k])^2)
        }
      }
    }
  }
  # If final day of year and final individual:
  # (1) calculate % of days w/ clicks present at each hydrophone for each month of year
  # (2) store each agent's information from day 365 for calculating day 1 info next year
  # (3) store latitude of each agent at each timestep
  # (4) clear agent.df and state.df dataframes (but keep for vis if final year of model run)
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
    
    ## store latitudes of all agents at each timestep
    lats.df$latitude[jj1:jj2] <- agent.df$latitude
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

h1 <- h1_perc %>% filter(yr > 1)
h2 <- h2_perc %>% filter(yr > 1)
pa <- ggplot(h1, aes(x = as.factor(month), y = perc)) +
  geom_boxplot(color = "red") +
  ylim(c(0,100))
pb <- ggplot(h2, aes(x = as.factor(month), y = perc)) +
  geom_boxplot(color = "blue") +
  ylim(c(0,100))
pa/pb

ggplot(agent.df, aes(x=step, y = latitude)) + geom_point(color = "black") +
  geom_hline(yintercept = h1_lat, color = "red") +
  geom_hline(yintercept = h2_lat, color = "blue") 

ggplot(agent.df, aes(x=step, y = longitude)) + geom_point()

##### save files for later formal visualization
write.csv(agent.df, file = "outputs/files/simulation_outputs/partial_agentdf.csv",row.names = FALSE)
write.csv(h1_perc, file = "outputs/files/simulation_outputs/partial_h1perc.csv",row.names = FALSE)
write.csv(h2_perc, file = "outputs/files/simulation_outputs/partial_h2perc.csv",row.names = FALSE)
write.csv(lats.df, file = "outputs/files/simulation_outputs/partial_lats.csv",row.names = FALSE)

