---
title: 'Agent based Model: West coast Monarch Migration'
---

#Load Libraries----
#Required
library(circular)
library(CircStats)
library(raster)
library(BBmisc)
library(sf)
library(terra)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

#Optional
library(tictoc)
library(beepr)



#Directories----
#Mac
setwd("")

#Set up environment----
landscape <- raster("Covariates/inverted_normalized_altitude.tif")
StartingRange <- read_sf("Shape/StartingRangeFull.shp")

#Import Specific overwintering locations
EndingPoints <- read_sf("Shape/Overwintering_points.shp")
#Create a buffer around the points. Agents within 5km are considered to have reached the location
EndRange <-  st_union(st_buffer(EndingPoints$geometry, dist = 5000))



#Parameter set up---------

#Navigation strategy (1 = True; 0 equals Vector)
true_or_vector <- 0

#Standard deviation and Variacne of Pied flycatcher orientations 
#assuming normal distribution
sd_rad <- 0.9033
var_rad <- sd_rad^2

#Converting variance to Concentration Parameter for circular dist. used in sim
kappa_rad <- 1/var_rad

#Historic mean orientation of monarch butterflies
orientation_angle <- 225

#Setting kappa for simulation
kappa <- kappa_rad #For bird kappa
kappa <- kappa_rad/2 # half kappa

var_rad <- 1/kappa

max_steps <- 1400/(exp(-0.5*var_rad))

#mean number of days
mean_days <- 60


#Mean number of steps in a day
mean_steps <- max_steps/mean_days

#Set range it takes to consider goal reached
goal.reached <- 1000

#Set how many agents you want to record
num.agents <- 1000

#Set Random seed
seed_list <- round(runif(num.agents, min = 0, max = 1), 7)


#Number of observations in our distributions
n <- 1000

#Number of Steps per day distribution (S)
#(if the number of steps per day is low, be careful for negative distances)
dist_walk_distance <- rnorm(n, mean = mean_steps, sd = 3)
#Round to whole number steps. 
dist_walk_distance <- floor(dist_walk_distance)

#Total time distribution (T)
num_day_stop_migration <- rnorm(n, mean = mean_days, sd = 3)
num_day_stop_migration <- floor(num_day_stop_migration)

#Circular distribution for all possible orientations
circle.df <- seq(0, 360, 0.1) 
circle.df2 <- circular(circle.df, units = c("degrees"), 
                       modulo = c("2pi"),
                       template = ("geographics"))


#Creates Empty Matrix for which direction agents move in
vector_walk <- matrix(0, ncol = 1, nrow = 1)

#Creates Empty Matrix used to sample dist_walk_distance to get the actual distance an agent will move.
walk_distance<- matrix(0, ncol = 1, nrow = 1)

#Creates Empty Matrix used to sample num_day_stop_migration to get the temporal length of migration
stop_migration <- matrix(0, ncol = 1, nrow = 1)



#All vector Agent Matrix and Data frames. Used based on whihch format is needed.
all.vector.agents <-  matrix(ncol = 7, nrow = 0)
all.vector.matrix <- matrix(ncol = 7, nrow = 0)
grid_walk_matrix <- matrix(ncol=3, nrow = 0)


#Sampling Distribution function
dis.fun <- function(n) sample(circle.df,
                              n, # n draws            
                              TRUE,  # with replaceme
                              circ.dis[,2] # using t
)



#Vector used to record which Agent is run on which core during loop
current.para <- c(1:num.agents)

#NA Values
na.value <- 0.0016

#Random seed for random start locations
set.seed(seed_list[1])
random_start_list <- st_coordinates(st_sample(StartingRange, size=num.agents))


####SImulation########
tic("1000 Agents, Kappa = #, landscape = X")
print("Model Start")


#Detect and assign cores! Subtract from total cores so as to not crash computer
parallel::detectCores()
n.cores <- parallel::detectCores() - 1


#create the cluster for Mac type = "FORK, for Windows type = "PSOCK"
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)


#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()

#Loop below#
#Set Current Agent to 0 to start the loop
current.agent <- 0

output <- foreach(j = 1:num.agents, .combine=rbind,
                  .packages = c('circular', 'CircStats', 'raster', 'BBmisc', 'sf', 'terra')) %dopar% {
                    set.seed(seed_list[j])
                    ###Reseting Values for New Agent###
                    stop_migration[1,1] <- sample(num_day_stop_migration,1)
                    nd <- 0 #Sets day to 0
                    agent.matrix <- matrix(ncol=6, nrow=0) #Clears agent.matrix data from previous agent
                    
                    #Setting Random start location
                    random_start <- matrix(random_start_list[j,], ncol=2)
                    location_cell <- cellFromXY(landscape, random_start)
                    location.cord <- xyFromCell(landscape, location_cell)
                    start <- rowColFromCell(landscape, location_cell)
                    location <- start
                    # Resets and sets the first step to starting location
                    all.step.matrix <- matrix(c(0, location[1], location[2], location.cord[1], location.cord[2]), nrow = 1, ncol = 5)
                    
                    
                    if(true_or_vector == 1){
                    
                      repeat{#### Loop to generate a single True agent####
                      
                      walk_distance[1,1] <- sample(dist_walk_distance, 1)#Sample "distance walk distribution for how many steps a day#
                      
                      ns <- 0 #Sets number of steps to zero to start day
                      
                      grid_walk <- location
                      
                      #Determine distance from nearest overwintering location
                      location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                      dist.away.from.end <- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                      
                      #Determine if Agents are in the home in range
                      is_within <- st_within(st_point(location.cord), EndRange)
                      
                      all.step.matrix <- matrix(c(0, location[1], location[2], location.cord[1], location.cord[2]), nrow = 1, ncol = 5)
                      
                      
                      #Normal True navigation day
                      if(length(unlist(is_within[[1]])) == 0){
                        repeat{
                          
                          # Converts the slope into degrees using the arch tangential equation             
                          location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                          
                          chosen.overwinter <- st_coordinates(EndingPoints[17,2])
                          
                          
                          location_cell <- cellFromXY(landscape, location.cord)
                          
                          location<- rowColFromCell(landscape, location_cell)
                          
                          x <- chosen.overwinter[1,1]- location.cord[1,1]
                          y <- chosen.overwinter[1,2]- location.cord[1,2]
                          
                          degs <- ((180/pi)*atan2(x,y))+360
                          
                          #Converts "degs" into a circular variable with north at zero         
                          mu <-  circular(degs, units = c("degrees"), 
                                          modulo = c("2pi"),
                                          template = ("geographics"))
                          
                          #Creates the orientation distribution and samples from it
                          circ.dis <- data.frame(Angle= circle.df,
                                                 Density = dvonmises(circle.df2,
                                                                     mu,
                                                                     kappa,
                                                                     log = F))
                          
                          
                          #find landscape conditions based on location           
                          land.loc <- floor(location)  
                          
                          row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,
                                   land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                          
                          col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,
                                   land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                          
                          
                          #Modify Distribution by taking values from landscape
                          matrix.mod <- landscape[cbind(row,col)]   
                          
                          #Allows navigators to traverse landscape with no data      
                          matrix.mod[is.na(matrix.mod)]= na.value
                          
                          
                          #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                          #Square 1               
                          circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                          circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                          #Square 2            
                          circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                          #Square 3            
                          circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                          #Square 4                
                          circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                          #Square 5         
                          circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                          #square 6
                          circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                          #Square 7         
                          circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                          #Square 8           
                          circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                          
                          
                          #Sample from modified distribution by converting distrubtion into a function
                          dis.fun <- function(n) sample(circle.df,                
                                                        n, # n draws                 
                                                        TRUE,  # with replacement
                                                        circ.dis[,2] # using these probabilities
                          )    
                          #Sample function        
                          vector_walk <- dis.fun(1)
                          
                          ###based on orientation selected, moves the agent to the correct square                 
                          if(vector_walk>=337.5){
                            grid_walk <- rbind(grid_walk, c(-1,0))
                          }else if(vector_walk<=22.5){
                            grid_walk <- rbind(grid_walk, c(-1,0))
                          }else if(vector_walk>22.5&vector_walk<67.5){
                            grid_walk <- rbind(grid_walk, c(-1,1))
                          } else if(vector_walk>=67.5&vector_walk<=112.5){
                            grid_walk <- rbind(grid_walk, c(0,1))
                          } else if(vector_walk>112.5&vector_walk<157.5){
                            grid_walk <- rbind(grid_walk, c(1,1))
                          } else if(vector_walk>=157.5&vector_walk<=202.5){
                            grid_walk <- rbind(grid_walk, c(1,0))
                          } else if(vector_walk>202.5&vector_walk<247.5){
                            grid_walk <- rbind(grid_walk, c(1,-1))
                          } else if(vector_walk>=247.5&vector_walk<=292.5){
                            grid_walk <- rbind(grid_walk, c(0,-1))
                          } else{
                            grid_walk <- rbind(grid_walk, c(-1,-1))}
                          
                          #record 1 single step based on the orientation chosen       
                          i <- length(grid_walk[,1])                
                          df.sum<- colSums(tail(grid_walk,n=2))                 
                          grid_walk[i, 1:2] <- df.sum[1:2]               
                          
                          #Setting "ocation" to the current position of the agent           
                          location <- tail(grid_walk, n=1)
                          
                          location_cell <- cellFromRowCol(landscape, location[1,1], location[1,2])
                          location.cord <- xyFromCell(landscape, location_cell)
                          is_within <- st_within(st_point(location.cord), EndRange)
                          
                          #End the Day loop based on number of walks that day##             
                          ns <- ns + 1
                          step.matrix<- cbind(ns, df.sum[1], df.sum[2],location.cord[1], location.cord[2])
                          all.step.matrix <- rbind(all.step.matrix, step.matrix)
                          final.step.location <- tail(all.step.matrix[,2:3],1)
                          
                          if(ns == walk_distance | length(is_within[[1]]) == 1){break}
                        }}else{
                          repeat{
                            
                            location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                            dist.points<- as.matrix(st_distance(location_sf, EndingPoints))
                            min.point<- min(dist.points)
                            home.overwinter <- which(dist.points==min.point)
                            chosen.overwinter <- st_coordinates(EndingPoints[home.overwinter,2])
                            
                            
                            location_cell <- cellFromXY(landscape, location.cord)
                            
                            location<- rowColFromCell(landscape, location_cell)
                            
                            x <- chosen.overwinter[1,1]- location.cord[1,1]
                            y <- chosen.overwinter[1,2]- location.cord[1,2]
                            
                            # Converts the slope into degrees using the arch tangential equation                  
                            degs <- ((180/pi)*atan2(x,y))+360 #Set at zero for Compass navigation
                            
                            #Converts "degs" into a circular variable with north at zero                 
                            mu <-  circular(degs, units = c("degrees"),                 
                                            modulo = c("2pi"),               
                                            template = ("geographics"))
                            #Creates the orientation distribution and samples from it
                            circ.dis <- data.frame(Angle= circle.df,
                                                   Density = dvonmises(circle.df2,                  
                                                                       mu,                 
                                                                       kappa,                 
                                                                       log = F))
                            #find landscape conditions based on location              
                            land.loc <- floor(location) 
                            row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,                  
                                     land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                            
                            col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,                  
                                     land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                            
                            
                            #Modify Distribution by taking values from landscape                 
                            matrix.mod <- landscape[cbind(row,col)]   
                            
                            #Allows navigators to traverse landscape with no data      
                            matrix.mod[is.na(matrix.mod)]= na.value
                            
                            #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                            #Square 1               
                            circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                            circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                            #Square 2            
                            circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                            #Square 3            
                            circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                            #Square 4                
                            circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                            #Square 5         
                            circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                            #square 6
                            circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                            #Square 7         
                            circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                            #Square 8           
                            circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                            
                            #Sample from modified distribution by converting distrubtion into a function
                            
                            dis.fun <- function(n) sample(circle.df,
                                                          n, # n draws             
                                                          TRUE,  # with replacement     
                                                          circ.dis[,2] # using these probabilities
                            )
                            
                            
                            #Sample function         
                            vector_walk <- dis.fun(1)
                            
                            
                            ###based on orientation selected, moves the agent to the correct square               
                            if(vector_walk>=337.5){
                              grid_walk <- rbind(grid_walk, c(-1,0))
                            }else if(vector_walk<=22.5){
                              grid_walk <- rbind(grid_walk, c(-1,0))
                            }else if(vector_walk>22.5&vector_walk<67.5){
                              grid_walk <- rbind(grid_walk, c(-1,1))
                            } else if(vector_walk>=67.5&vector_walk<=112.5){
                              grid_walk <- rbind(grid_walk, c(0,1))
                            } else if(vector_walk>112.5&vector_walk<157.5){
                              grid_walk <- rbind(grid_walk, c(1,1))
                            } else if(vector_walk>=157.5&vector_walk<=202.5){
                              grid_walk <- rbind(grid_walk, c(1,0))
                            } else if(vector_walk>202.5&vector_walk<247.5){
                              grid_walk <- rbind(grid_walk, c(1,-1))
                            } else if(vector_walk>=247.5&vector_walk<=292.5){
                              grid_walk <- rbind(grid_walk, c(0,-1))
                            } else{
                              grid_walk <- rbind(grid_walk, c(-1,-1))}
                            
                            
                            #record 1 single step based on the orientation chosen               
                            i <- length(grid_walk[,1])        
                            df.sum<- colSums(tail(grid_walk,n=2))            
                            grid_walk[i, 1:2] <- df.sum[1:2]
                            
                            #Setting "start" to the current position of the agent
                            location <- tail(grid_walk, n=1)
                            
                            location_cell <- cellFromRowCol(landscape, location[1,1], location[1,2])
                            location.cord <- xyFromCell(landscape, location_cell)
                            
                            
                            is_within <- st_within(st_point(location.cord), EndRange)
                            
                            #End the Day loop based on number of walks that day##  
                            ns <- ns + 1   
                            step.matrix<- cbind(ns, df.sum[1], df.sum[2], location.cord[1], location.cord[2])      
                            all.step.matrix <- rbind(all.step.matrix, step.matrix)
                            final.step.location <- tail(all.step.matrix[,2:3],1)
                            
                            location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                            dist.away.from.end<- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                            
                            
                            if(ns == walk_distance | dist.away.from.end < goal.reached){break}}
                        }
                      
                      #Create Matrix to store steps for each day of the agents movement
                      
                      nd <- nd+1                
                      day.matrix <- matrix(data = c(rep(nd,length(all.step.matrix[,1])), all.step.matrix), ncol = 6, nrow = length(all.step.matrix[,1]))             
                      agent.matrix <- rbind(agent.matrix, day.matrix)
                      
                      
                      #End the Agent Loop based on number of days moved###               
                      
                      if(nd == stop_migration | dist.away.from.end < goal.reached){break}
                      
                      }}else{
                        #Loop to generate a single Vector agent
                        repeat{
                          
                          walk_distance[1,1] <- sample(dist_walk_distance, 1)#Sample "distance walk distribution for how many steps a day#
                          
                          ns <- 0 #Sets number of steps to zero to start day
                          
                          grid_walk <- location
                          
                          #Determine distance from nearest overwintering location
                          location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                          dist.away.from.end<- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                          
                          #Determine if Agents are in the home in range
                          is_within <- st_within(st_point(location.cord), EndRange)
                          
                          all.step.matrix <- matrix(c(0, location[1], location[2], location.cord[1], location.cord[2]), nrow = 1, ncol = 5)
                          
                          
                          #Normal Vector Day
                          if(length(unlist(is_within[[1]])) == 0){
                            repeat{
                              
                              # Converts the slope into degrees using the arch tangential equation             
                              degs <-  orientation_angle
                              
                              #Converts "degs" into a circular variable with north at zero         
                              mu <-  circular(degs, units = c("degrees"), 
                                              modulo = c("2pi"),
                                              template = ("geographics"))
                              
                              #Creates the orientation distribution and samples from it
                              circ.dis <- data.frame(Angle= circle.df,
                                                     Density = dvonmises(circle.df2,
                                                                         mu,
                                                                         kappa,
                                                                         log = F))
                              
                              
                              #find landscape conditions based on location           
                              land.loc <- floor(location)  
                              
                              row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,
                                       land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                              
                              col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,
                                       land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                              
                              
                              #Modify Distribution by taking values from landscape
                              matrix.mod <- landscape[cbind(row,col)]   
                              
                              #Allows navigators to traverse landscape with no data      
                              matrix.mod[is.na(matrix.mod)]= na.value
                              
                              
                              #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                              #Square 1               
                              circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                              circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                              #Square 2            
                              circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                              #Square 3            
                              circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                              #Square 4                
                              circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                              #Square 5         
                              circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                              #square 6
                              circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                              #Square 7         
                              circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                              #Square 8           
                              circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                              
                              
                              #Sample from modified distribution by converting distrubtion into a function
                              dis.fun <- function(n) sample(circle.df,                
                                                            n, # n draws                 
                                                            TRUE,  # with replacement
                                                            circ.dis[,2] # using these probabilities
                              )    
                              #Sample function        
                              vector_walk <- dis.fun(1)
                              
                              ###based on orientation selected, moves the agent to the correct square                 
                              if(vector_walk>=337.5){
                                grid_walk <- rbind(grid_walk, c(-1,0))
                              }else if(vector_walk<=22.5){
                                grid_walk <- rbind(grid_walk, c(-1,0))
                              }else if(vector_walk>22.5&vector_walk<67.5){
                                grid_walk <- rbind(grid_walk, c(-1,1))
                              } else if(vector_walk>=67.5&vector_walk<=112.5){
                                grid_walk <- rbind(grid_walk, c(0,1))
                              } else if(vector_walk>112.5&vector_walk<157.5){
                                grid_walk <- rbind(grid_walk, c(1,1))
                              } else if(vector_walk>=157.5&vector_walk<=202.5){
                                grid_walk <- rbind(grid_walk, c(1,0))
                              } else if(vector_walk>202.5&vector_walk<247.5){
                                grid_walk <- rbind(grid_walk, c(1,-1))
                              } else if(vector_walk>=247.5&vector_walk<=292.5){
                                grid_walk <- rbind(grid_walk, c(0,-1))
                              } else{
                                grid_walk <- rbind(grid_walk, c(-1,-1))}
                              
                              #record 1 single step based on the orientation chosen       
                              i <- length(grid_walk[,1])                
                              df.sum<- colSums(tail(grid_walk,n=2))                 
                              grid_walk[i, 1:2] <- df.sum[1:2]               
                              
                              #Setting "ocation" to the current position of the agent           
                              location <- tail(grid_walk, n=1)
                              location.cord.col <- (location[2]*0.008333333)-124.7376388887
                              location.cord.row <- 48.9956944443-(location[1]*0.008333333)
                              location.cord <- cbind(location.cord.col, location.cord.row)
                              is_within <- st_within(st_point(location.cord), EndRange)
                              
                              #End the Day loop based on number of walks that day##             
                              ns <- ns + 1
                              step.matrix<- cbind(ns, df.sum[1], df.sum[2],location.cord[1], location.cord[2])
                              all.step.matrix <- rbind(all.step.matrix, step.matrix)
                              final.step.location <- tail(all.step.matrix[,2:3],1)
                              
                              if(ns == walk_distance | length(is_within[[1]]) == 1){break}
                            }}else{
                              repeat{
                                
                                location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                                dist.points<- as.matrix(st_distance(location_sf, EndingPoints))
                                min.point<- min(dist.points)
                                home.overwinter <- which(dist.points==min.point)
                                chosen.overwinter <- st_coordinates(EndingPoints[home.overwinter,5])
                                
                                conv.col <- abs(location.cord[1,1])
                                conv.row <- abs(location.cord[1,2])
                                col <- floor(((124.7376388887 - conv.col)/0.0083333333))
                                row <- floor(((48.9956944443 - conv.row)/0.0083333333))
                                
                                location <- cbind(row,col)
                                
                                x <- floor(((124.7376388887 - abs(chosen.overwinter[1,1]))/0.0083333333)) - location[2]
                                y <- floor(((48.9956944443 - abs(chosen.overwinter[1,2]))/0.0083333333)) - location[1]
                                
                                # Converts the slope into degrees using the arch tangential equation                  
                                degs <- 180-((180/pi)*atan2(x,y)) #Set at zero for Compass navigation
                                
                                #Converts "degs" into a circular variable with north at zero                 
                                mu <-  circular(degs, units = c("degrees"),                 
                                                modulo = c("2pi"),               
                                                template = ("geographics"))
                                #Creates the orientation distribution and samples from it
                                circ.dis <- data.frame(Angle= circle.df,
                                                       Density = dvonmises(circle.df2,                  
                                                                           mu,                 
                                                                           kappa,                 
                                                                           log = F))
                                #find landscape conditions based on location              
                                land.loc <- floor(location) 
                                row <- c(land.loc[1]-1, land.loc[1]-1, land.loc[1], land.loc[1] + 1,                  
                                         land.loc[1] + 1, land.loc[1]+1, land.loc[1], land.loc[1]-1)
                                
                                col <- c(land.loc[2], land.loc[2]+1, land.loc[2]+1, land.loc[2]+1,                  
                                         land.loc[2], land.loc[2]-1, land.loc[2]-1, land.loc[2]-1)
                                
                                
                                #Modify Distribution by taking values from landscape                 
                                matrix.mod <- landscape[cbind(row,col)] 
                                
                                #Allows navigators to traverse landscape with no data            
                                matrix.mod[is.na(matrix.mod)]= na.value
                                
                                #Modify distribution based on squares (square numbering starts at top center and goes clockwise#              
                                #Square 1               
                                circ.dis[circ.dis$Angle<=22.5, 2] <- circ.dis[circ.dis$Angle<=22.5,2]*matrix.mod[1]
                                circ.dis[circ.dis$Angle>=337.5, 2] <- circ.dis[circ.dis$Angle>=337.5,2]*matrix.mod[1]               
                                #Square 2            
                                circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]<- circ.dis[circ.dis$Angle>22.5&circ.dis$Angle<67.5, 2]*matrix.mod[2]           
                                #Square 3            
                                circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2] <- circ.dis[circ.dis$Angle>=67.5&circ.dis$Angle<=112.5, 2]*matrix.mod[3]          
                                #Square 4                
                                circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2] <- circ.dis[circ.dis$Angle>112.5&circ.dis$Angle<157.5, 2]*matrix.mod[4]         
                                #Square 5         
                                circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2] <- circ.dis[circ.dis$Angle>=157.5&circ.dis$Angle<=202.5, 2]*matrix.mod[5]    
                                #square 6
                                circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2] <- circ.dis[circ.dis$Angle>202.5&circ.dis$Angle<247.5, 2]*matrix.mod[6]     
                                #Square 7         
                                circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2] <- circ.dis[circ.dis$Angle>=247.5&circ.dis$Angle<=292.5, 2]*matrix.mod[7]         
                                #Square 8           
                                circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2] <- circ.dis[circ.dis$Angle>292.5&circ.dis$Angle<337.5, 2]*matrix.mod[8]
                                
                                #Sample from modified distribution by converting distrubtion into a function
                                
                                dis.fun <- function(n) sample(circle.df,
                                                              n, # n draws             
                                                              TRUE,  # with replacement     
                                                              circ.dis[,2] # using these probabilities
                                )
                                
                                
                                #Sample function         
                                vector_walk <- dis.fun(1)
                                
                                
                                ###based on orientation selected, moves the agent to the correct square               
                                if(vector_walk>=337.5){
                                  grid_walk <- rbind(grid_walk, c(-1,0))
                                }else if(vector_walk<=22.5){
                                  grid_walk <- rbind(grid_walk, c(-1,0))
                                }else if(vector_walk>22.5&vector_walk<67.5){
                                  grid_walk <- rbind(grid_walk, c(-1,1))
                                } else if(vector_walk>=67.5&vector_walk<=112.5){
                                  grid_walk <- rbind(grid_walk, c(0,1))
                                } else if(vector_walk>112.5&vector_walk<157.5){
                                  grid_walk <- rbind(grid_walk, c(1,1))
                                } else if(vector_walk>=157.5&vector_walk<=202.5){
                                  grid_walk <- rbind(grid_walk, c(1,0))
                                } else if(vector_walk>202.5&vector_walk<247.5){
                                  grid_walk <- rbind(grid_walk, c(1,-1))
                                } else if(vector_walk>=247.5&vector_walk<=292.5){
                                  grid_walk <- rbind(grid_walk, c(0,-1))
                                } else{
                                  grid_walk <- rbind(grid_walk, c(-1,-1))}
                                
                                
                                #record 1 single step based on the orientation chosen               
                                i <- length(grid_walk[,1])        
                                df.sum<- colSums(tail(grid_walk,n=2))            
                                grid_walk[i, 1:2] <- df.sum[1:2]
                                
                                #Setting "start" to the current position of the agent
                                location <- tail(grid_walk, n=1)
                                location.cord <- cbind(((location[2]*0.008333333)-124.7376388887),(48.9956944443-(location[1]*0.008333333)))
                                is_within <- st_within(st_point(location.cord), EndRange)
                                
                                #End the Day loop based on number of walks that day##  
                                ns <- ns + 1   
                                step.matrix<- cbind(ns, df.sum[1], df.sum[2], location.cord[1], location.cord[2])      
                                all.step.matrix <- rbind(all.step.matrix, step.matrix)
                                final.step.location <- tail(all.step.matrix[,2:3],1)
                                
                                location_sf <- st_sfc(st_point(location.cord)) %>% st_set_crs(4326)
                                dist.away.from.end<- min(as.data.frame(st_distance(location_sf, EndingPoints)))
                                
                                
                                if(ns == walk_distance | dist.away.from.end < goal.reached){break}}
                              
                            }
                          
                          #Create Matrix to store steps for each day of the agents movement
                          
                          nd <- nd+1                
                          day.matrix <- matrix(data = c(rep(nd,length(all.step.matrix[,1])), all.step.matrix), ncol = 6, nrow = length(all.step.matrix[,1]))             
                          agent.matrix <- rbind(agent.matrix, day.matrix)
                          
                          
                          #End the Agent Loop based on number of days moved###               
                          
                          if(nd == stop_migration | dist.away.from.end < goal.reached){break}
                          
                        }
                        
                    }
                    
                    
                    current.agent <- current.agent + 1
                    
                    all.vector.matrix <- cbind(agent.matrix, c(current.para[j]))
                    
                    
                    all.vector.matrix
                    
                  }




parallel::stopCluster(cl = my.cluster)
colnames(output) <- c("Day", "Step", "Row", "Column", "X", "Y", "Agent")
toc()
print("DONE!")

#Prepare Output for Export
df_output <- as.data.frame(output)
beep(2)


#####Save Data########
#Full Data set
write.csv(df_output, ".csv", row.names = F)


#Final locations data set
#
grouped_data <- df_output %>% group_by(Agent)

# Select the final row for each agent
final_rows <- grouped_data %>% slice_tail(n = 1)

# Create a new data frame with the selected final rows
Final_location <- data.frame(final_rows)

write.csv(Final_location, ".csv")
