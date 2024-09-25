####Library-----------
library(inlabru)
library(INLA)
library(rnaturalearth)
library(sf)
library(sp)
library(ggplot2)
library(terra)
library(dplyr)



#### Set Working Directory for Data input ####

setwd("")



#Shapes---------------------------
all_overwinter <- read_sf("Shape/Western Monarch Overwintering Sites (Public).shp")
start_range <- read_sf("Shape/StartingRangeFull.shp")
all_overwinter_buffer <- st_buffer(all_overwinter$geometry, dist= 10000)
overwinter_buff <- st_union(all_overwinter_buffer)
overwinter_sf <- st_sf(geometry = overwinter_buff)
EndingPoints <- read_sf("Shape/Overwintering_points.shp")
pismo <- EndingPoints[17,]
pismo_buff <- st_buffer(pismo$geometry, dist = 10000)
pismo_sf <- st_sf(geometry = pismo_buff)

#Study Region and CRS formating

#Create shape for the study region
usa_states <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter((name %in%  c("Washington", "Oregon", "Idaho", "California", "Arizona", "Nevada", "Utah"))) %>%
  sf::as_Spatial()


map <- as(usa_states, "SpatialPolygons")
boundary <- sf::st_union(st_as_sf(map)) %>% sf::as_Spatial()
polygon_sf <- st_as_sf(boundary)

overwinter_sf <- st_transform(overwinter_sf, crs = st_crs(polygon_sf))


#Simulation Data-------------------------------------------------------------------
#import simulated observation locations
sim_obs_vector_k1.22 <- read.csv("Final_location_1000Agents_FullStartRange.csv")
sim_obs_vector_k0.61 <- read.csv("Final_location_vector_k0.6.csv")
sim_obs_true_k1.22 <- read.csv("Final_location_1000Agents_truenav_k1.22.csv")
sim_obs_true_k0.61 <- read.csv("Final_location_1000Agents_truenav_k.6.csv")
sim_obs_true_k0.15 <- read.csv("Final_location_1000Agents_truenav_k.15.csv")

#Vector 1.22 K
sim_obs_vector_k1.22<- sim_obs_vector_k1.22[, c(7,6)]
colnames(sim_obs_vector_k1.22) <- c("Latitude", "Longitude")

#Vector 0.61 K
sim_obs_vector_k0.61<- sim_obs_vector_k0.61[, c(7,6)]
colnames(sim_obs_vector_k0.61) <- c("Latitude", "Longitude")

#True 1.22 K
sim_obs_true_k1.22<- sim_obs_true_k1.22[, c(7,6)]
colnames(sim_obs_true_k1.22) <- c("Latitude", "Longitude")

#True 0.61 K
sim_obs_true_k0.61<- sim_obs_true_k0.61[, c(7,6)]
colnames(sim_obs_true_k0.61) <- c("Latitude", "Longitude")

# True 0.15 K
sim_obs_true_k0.15<- sim_obs_true_k0.15[, c(7,6)]
colnames(sim_obs_true_k0.15) <- c("Latitude", "Longitude")

#Convert to SF object
sim_sf_v_1.22 <- st_as_sf(sim_obs_vector_k1.22, coords = c('Longitude', 'Latitude'))
sim_sf_v_0.61 <- st_as_sf(sim_obs_vector_k0.61, coords = c('Longitude', 'Latitude'))
sim_sf_t_1.22 <- st_as_sf(sim_obs_true_k1.22, coords = c('Longitude', 'Latitude'))
sim_sf_t_0.61 <- st_as_sf(sim_obs_true_k0.61, coords = c('Longitude', 'Latitude'))
sim_sf_t_0.15 <- st_as_sf(sim_obs_true_k0.15, coords = c('Longitude', 'Latitude'))

st_crs(sim_sf_v_1.22) <- st_crs(polygon_sf)
st_crs(sim_sf_v_0.61) <- st_crs(polygon_sf)
st_crs(sim_sf_t_1.22) <- st_crs(polygon_sf)
st_crs(sim_sf_t_0.61) <- st_crs(polygon_sf)
st_crs(sim_sf_t_0.15) <- st_crs(polygon_sf)
#--------------------------Data Exploration

# Points remaining in the location
sim_sf_v_1.22 <- st_intersection(sim_sf_v_1.22, polygon_sf$geometry)
sim_sf_v_0.61 <- st_intersection(sim_sf_v_0.61, polygon_sf$geometry)
sim_sf_t_1.22 <- st_intersection(sim_sf_t_1.22, polygon_sf$geometry)
sim_sf_t_0.61 <- st_intersection(sim_sf_t_0.61, polygon_sf$geometry)
sim_sf_t_0.15 <- st_intersection(sim_sf_t_0.15, polygon_sf$geometry)

#subtract from total available to see how many monarchs "died" or flew off into the ocean
1000 - nrow(sim_sf_v_1.22)
1000 - nrow(sim_sf_v_0.61)
1000 - nrow(sim_sf_t_1.22)
1000 - nrow(sim_sf_t_0.61)
1000 - nrow(sim_sf_t_0.15)


#See how many in all overwintering locations
nrow(st_intersection(sim_sf_v_1.22, overwinter_sf$geometry))
nrow(st_intersection(sim_sf_v_0.61, overwinter_sf$geometry))
nrow(st_intersection(sim_sf_t_1.22, overwinter_sf$geometry))
nrow(st_intersection(sim_sf_t_0.61, overwinter_sf$geometry))
nrow(st_intersection(sim_sf_t_0.15, overwinter_sf$geometry))

#See how many in pismo
pismo_sf <- st_transform(pismo_sf, crs = st_crs(polygon_sf))


nrow(st_intersection(sim_sf_v_1.22, pismo_sf$geometry))
nrow(st_intersection(sim_sf_v_0.61, pismo_sf$geometry))
nrow(st_intersection(sim_sf_t_1.22, pismo_sf$geometry))
nrow(st_intersection(sim_sf_t_0.61, pismo_sf$geometry))

#Convert to sp object
sim_sp_v_1.22 <- as(sim_sf_v_1.22, "Spatial")
sim_sp_v_0.61 <- as(sim_sf_v_0.61, "Spatial")
sim_sp_t_1.22 <- as(sim_sf_t_1.22, "Spatial")
sim_sp_t_0.61 <- as(sim_sf_t_0.61, "Spatial")
sim_sp_t_0.15 <- as(sim_sf_t_0.15, "Spatial")

#Compare results visually to all overwintering locations
# Save each object as an RDS file
saveRDS(sim_sp_v_1.22, "sim_sp_v_1.22.rds")
saveRDS(sim_sp_v_0.61, "sim_sp_v_0.61.rds")
saveRDS(sim_sp_t_1.22, "sim_sp_t_1.22.rds")
saveRDS(sim_sp_t_0.61, "sim_sp_t_0.61.rds")
saveRDS(sim_sp_t_0.15, "sim_sp_t_0.15.rds")


#Area around observed monarch
ggplot() +
  gg(polygon_sf) +
  gg(overwinter_sf, lwd = 2)+
  gg(StartingRange)
  #gg(points_home, color = "red", cex = .5)+
  #gg(sp_winter_obs, color = "red")


#---------------------------Simulation Spde set up----------------------------

### Making Mesh and Boundary windows
#Make bounding window for study region
#vector sim with k1.22 is chosen bacause it has the largest bounding wihndow
xmin= min(sim_obs_vector_k1.22$Longitude)
xmax = max(sim_obs_vector_k1.22$Longitude)
ymin = min(sim_obs_vector_k1.22$Latitude)
ymax = max(sim_obs_vector_k1.22$Latitude)

coords <- matrix(c(xmin, ymin, 
                   xmax, ymin, 
                   xmax, ymax, 
                   xmin, ymax, 
                   xmin, ymin), 
                 ncol = 2, byrow = TRUE)

# Create a polygon from the coordinates
polygon <- st_polygon(list(coords))

# Convert the polygon to an sf object
polygon_sf <- st_sfc(polygon, crs = 4326)

#Crop us states with bounding square to get the coastline and study region

west_state <- ne_states(country = "united states of america", returnclass = "sf") %>%
  dplyr::filter((name %in%  c("Washington", "Oregon", "Idaho", "California", "Arizona", "Nevada", "Utah"))) %>%
  sf::as_Spatial()

map      <- as(west_state, "SpatialPolygons")
boundary <- sf::st_union(st_as_sf(map)) %>% sf::as_Spatial()
usa_crop     <- boundary

StatesBoundary.shp <- terra::intersect(usa_crop, st_bbox(polygon_sf))

#Create meshes for the simulations.

max.edge.v_1.22 <- diff(range(st_coordinates(sim_sf_v_1.22)[,1]))/(3*5)
max.edge.v_0.61 <- diff(range(st_coordinates(sim_sf_v_0.61)[,1]))/(3*5)
max.edge.t_1.22 <- diff(range(st_coordinates(sim_sf_t_1.22)[,1]))/(3*5)
max.edge.t_0.61 <- diff(range(st_coordinates(sim_sf_t_0.61)[,1]))/(3*5)
max.edge.t_0.15 <- diff(range(st_coordinates(sim_sf_t_0.15)[,1]))/(3*5)


mesh.sim.v_1.22 <- inla.mesh.2d(loc = sim_sp_v_1.22, boundary = StatesBoundary.shp, 
                         max.edge=max.edge.v_1.22, offset = c(0.5, 1), 
                         cutoff = 0.3)


mesh.sim.v_0.61 <- inla.mesh.2d(loc = sim_sp_v_0.61, boundary = StatesBoundary.shp, 
                                max.edge=max.edge.v_0.61, offset = c(0.5, 1), 
                                cutoff = 0.3)

mesh.sim.t_1.22 <- inla.mesh.2d(loc = sim_sp_t_1.22, boundary = StatesBoundary.shp, 
                                max.edge=max.edge.t_1.22, offset = c(0.5, 1), 
                                cutoff = 0.3)

mesh.sim.t_0.61 <- inla.mesh.2d(loc = sim_sp_t_0.61, boundary = StatesBoundary.shp, 
                                max.edge=max.edge.t_0.61, offset = c(0.5, 1), 
                                cutoff = 0.3)

mesh.sim.t_0.15 <- inla.mesh.2d(loc = sim_sp_t_0.15, boundary = StatesBoundary.shp, 
                                max.edge=max.edge.t_0.15, offset = c(0.5, 1), 
                                cutoff = 0.3)

#Convert CRS of shapes (sim version doesn't matter as they all have same crs)
fm_crs(StatesBoundary.shp) <- fm_crs(sim_sp_v_0.61)


mesh.sim.v_1.22$crs <- fm_crs(sim_sp_v_0.61)

mesh.sim.v_0.61$crs <- fm_crs(sim_sp_v_0.61)

mesh.sim.t_1.22$crs <- fm_crs(sim_sp_v_0.61)

mesh.sim.t_0.61$crs <- fm_crs(sim_sp_v_0.61)

mesh.sim.t_0.15$crs <- fm_crs(sim_sp_v_0.61)



# Monarch data import and formatting-----------------------------
#Observations as a shape file

dat <- data.table::fread("Observations [formated].csv")



# Elminate Hawaii and keep years 2018 onward. 
nov <- dat[Longitude >-140 & Longitude < -109.05 & year >= 2018 & month>=11]
feb <- dat[Longitude >-140 & Longitude < -109.05 & year >= 2018 & month<=2]
winter_dat <- rbind(nov, feb)
west_winter_dat <- winter_dat[,c(7,6)]

#Convert into SP and SF objects
sp_winter_obs <- SpatialPoints(coords = west_winter_dat[, 1:2])
sp_winter_df <- SpatialPointsDataFrame(coords = sp_winter_obs, data = west_winter_dat)
sf_winter_obs <- as(sp_winter_obs, "sf")

#Create Crop object
fm_crs(sf_winter_obs) <- fm_crs(StatesBoundary.shp)
crop <- st_as_sf(StatesBoundary.shp)

#Crop observations to study region
monarch_points_within_us <- st_intersection(sf_winter_obs, crop)

#Convert Observations into Spatial Points
sp_winter_obs <- as(monarch_points_within_us$geometry, "Spatial")




# Monarch Mesh and Boundary Creation--------------------------------------------------------
#Mesh
max.edge.monarch <- diff(range(sp_winter_obs@coords))/(3*5)



mesh.monarch <- inla.mesh.2d(loc = sp_winter_obs, 
                             boundary = StatesBoundary.shp, 
                             max.edge=max.edge.monarch, 
                             offset = c(0.5, 1), cutoff = 0.3)



ggplot() +
  gg(StatesBoundary.shp) +
  gg(sp_winter_obs, color = "blue", cex = .5)+
  gg(mesh.monarch)


########### load finished versions of Covariates and data----------------------------------------------

#All winter obs
sp_winter_obs <- readRDS("sp_winter_obs.rds")


#Covariates
human_density.sp <- readRDS("Covariates/humandensity_croped.rds")
altitude.sp <- readRDS("Covariates/altitude_croped.rds")
habitat.sp <- readRDS("Covariates/habitat_cropped_lowres.rds")

####mesh
#full
mesh.monarch <- readRDS("Shape/mesh.monarch.rds")

#Boundary
#full
StatesBoundary.shp <- readRDS("Shape/StatesBoundary.shp.rds")


#Convert CRS of OBS and sampler-------------------------------------------------
fm_crs(StatesBoundary.shp) <- fm_crs(altitude.sp)
fm_crs(sp_winter_obs) <- fm_crs(altitude.sp)
mesh.monarch$crs <- fm_crs(altitude.sp)

ggplot()+
  #gg(habitat.sp)+
  gg(StatesBoundary.shp)+
  gg(mesh.monarch)+
  gg(sp_winter_obs)
  

### Model 1: No covariates--------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

cmp.formula <- coordinates ~ mySmooth(coordinates, model = spde.monarch) + Intercept(1)

#Fit model
fit.monarch <- lgcp(cmp.formula, sp_winter_obs, samplers = StatesBoundary.shp, 
                    domain = list(coordinates = mesh.monarch),
                    options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(fit.monarch)

pred.monarch <- predict(
  fit.monarch,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + Intercept),
    loglambda = mySmooth + Intercept
  )
)

pl1 <- ggplot() +
  gg(data = pred.monarch$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(pred.monarch$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Full crop
saveRDS(fit.monarch, file = "Fit Results/Full_fit_spde_only(sigma=10, prior=1).rds")

###  Model 2: Habitat with SPDE----------------
pred.df <- fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp")


spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))



hcomp <- coordinates ~ 
  hab(habitat.sp, model = "factor_contrast") +
  mySmooth(coordinates, model = spde.monarch) +
  Intercept(1)


Hfit <- lgcp(hcomp, sp_winter_obs, samplers = StatesBoundary.shp, 
             domain = list(coordinates = mesh.monarch),
             options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


summary(Hfit)


h.int_log <- predict(
  Hfit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab),
    loglambda = mySmooth + hab
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = h.int_log$lambda, aes(color = NULL))+
  gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(h.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)



#Full save
saveRDS(Hfit, file = "Fit Results/Full_fit_habitat_lowres_cropped(sigma=10, prior=1).rds")

###  Model 2: Elevation with SPDE----------------------
fm_crs(StatesBoundary.shp) <- fm_crs(altitude.sp)
fm_crs(sp_winter_obs) <- fm_crs(altitude.sp)
mesh.monarch$crs <- fm_crs(altitude.sp)
mesh.sim$crs <- fm_crs(altitude.sp)


spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))


ecomp <- coordinates ~ elev(altitude.sp , model = "linear") +
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

efit <- lgcp(ecomp, sp_winter_obs, samplers = StatesBoundary.shp, domain = list(coordinates = mesh.monarch),
             options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


efit <- readRDS(file = "Fit Results/Full_fit_altitude_cropped(sigma=10, prior=1).rds")



e.int_log <- predict(
  efit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)

sim_v_1.22_fit <- predict(
  efit,
  fm_pixels(mesh.sim.v_1.22, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)

sim_t_1.22_fit <- predict(
  efit,
  fm_pixels(mesh.sim.t_1.22, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)

sim_v_0.61_fit <- predict(
  efit,
  fm_pixels(mesh.sim.v_0.61, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)


sim_t_0.61_fit <- predict(
  efit,
  fm_pixels(mesh.sim.t_0.61, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)

sim_t_0.15_fit <- predict(
  efit,
  fm_pixels(mesh.sim.t_0.15, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + Intercept),
    loglambda = mySmooth + elev + Intercept
  )
)

saveRDS(sim_v_1.22_fit, file = "Fit Results/sim_v_1.22_fit.rds")
saveRDS(sim_t_1.22_fit, file = "Fit Results/sim_t_1.22_fit.rds")
saveRDS(sim_v_0.61_fit, file = "Fit Results/sim_v_0.61_fit.rds")
saveRDS(sim_t_0.61_fit , file = "Fit Results/sim_t_0.61_fit.rds")
saveRDS(sim_t_0.15_fit, file = "Fit Results/sim_t_0.15_fit.rds")
saveRDS(efit, file = "Fit Results/efit_comp_used.rds")

#Multi
pl1 <- ggplot() +
  gg(e.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

pl2 <- ggplot() +
  gg(sim_v_1.22_fit$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")


pl3 <- ggplot() +
  gg(sim_t_1.22_fit$loglambda, aes(color = NULL)) +
  geom_tile()+
  #gg(sim_sp_t_1.22)+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")


pl4 <- ggplot() +
  gg(sim_v_0.61_fit$loglambda, aes(color = NULL)) +
  geom_tile()+
  #gg(sim_sp_t_1.22)+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")


pl5 <- ggplot() +
  gg(sim_t_0.61_fit$loglambda, aes(color = NULL)) +
  geom_tile()+
  #gg(sim_sp_t_0.61)+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

pl6 <- ggplot() +
  gg(sim_t_0.15_fit$loglambda, aes(color = NULL)) +
  geom_tile()+
  #gg(sim_sp_t_1.22)+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, pl3, pl4, pl5, pl6, cols = 6)

#Save full results
saveRDS(efit, file = "Fit Results/Monarch_altitude_fit(sigma=10, prior=1).rds")
saveRDS(e.int_log, file ="Fit Results/Monarch_altitude_pred(sigma=10, prior=1).rds")


#Revision Save
saveRDS(efit, file = "Fit Results/observation_fit_elevation.rds")
saveRDS(e.int_log, file ="Fit Results/observation_prediction_elevation.rds")
saveRDS(sim_com, file ="Fit Results/vector_sim_prediction_elevation.rds")

###  Model 2: Human Density with SPDE--------------------------------------
spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

#Human density
f.hum <- function(where) {
  # Extract the values
  v <- eval_spatial(human_density.sp, where, layer = "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(human_density.sp, where, v)
  }
  return(v)
}

hum.comp <- coordinates ~ hum(f.hum(.data.) , model = "linear") +
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

hum.fit <- lgcp(hum.comp, sp_winter_obs, samplers = StatesBoundary.shp, 
                domain = list(coordinates = mesh.monarch),
                options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(hum.fit)


hum.int_log <- predict(
  hum.fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hum + Intercept),
    loglambda = mySmooth + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = hum.int_log$lambda, aes(color = NULL))+
  gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hum.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(hum.fit, file = "Fit Results/Full_fit_humandensity_cropped(sigma=10, prior=1).rds.rds")




###  Model 3: Elevation and Habitat with SPDE----------------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

#function for missing values
f.elev <- function(where) {
  # Extract the values
  v <- eval_spatial(altitude.sp, where, layer = "AltitudeRaster")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(altitude.sp, where, v)
  }
  return(v)
}




hab_elev_comp <- coordinates ~ 
  hab(habitat.sp, model = "factor_contrast") + 
  elev(f.elev(.data.) , model = "linear") +
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

hab_elev_fit <- lgcp(hab_elev_comp, sp_winter_obs, samplers = StatesBoundary.shp, 
                     domain = list(coordinates = mesh.monarch),
                     options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(hab_elev_fit)


hab_elev_log <- predict(
  hab_elev_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab + elev + Intercept),
    loglambda = mySmooth + hab + elev + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = hab_elev_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hab_elev_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full resulkts
saveRDS(hab_elev_fit, file = "Fit Results/Full_fit_habitat_lowres_cropped_elevation_cropped(sigma=10, prior=1).rds")

### Model 3: Human Density and Habitat with SPDE----------------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

#Human density
f.hum <- function(where) {
  # Extract the values
  v <- eval_spatial(human_density.sp, where, layer = "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(human_density.sp, where, v)
  }
  return(v)
}




hab_hum_comp <- coordinates ~ 
  hab(habitat.sp, model = "factor_contrast") + 
  hum(f.hum(.data.) , model = "linear") +
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

hab_hum_fit <- lgcp(hab_hum_comp, sp_winter_obs, samplers = StatesBoundary.shp, 
                    domain = list(coordinates = mesh.monarch),
                    options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(hab_hum_fit)


hab_hum_log <- predict(
  hab_hum_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + hab + hum + Intercept),
    loglambda = mySmooth + hab + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = hab_hum_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(hab_hum_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(hab_hum_fit, file = "Fit Results/Full_fit_habitat_lowres_cropped_humdensity_cropped(sigma=10, prior=1).rds")


### Model 3: Elevation and Habitat with SPDE----------------------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

#Human density
f.hum <- function(where) {
  # Extract the values
  v <- eval_spatial(human_density.sp, where, layer = "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(human_density.sp, where, v)
  }
  return(v)
}

#function for missing values
f.elev <- function(where) {
  # Extract the values
  v <- eval_spatial(altitude.sp, where, layer = "AltitudeRaster")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(altitude.sp, where, v)
  }
  return(v)
}



elev_hum_comp <- coordinates ~ 
  elev(f.elev(.data.), model = "linear") +
  hum(f.hum(.data.), model = "linear") +
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

elev_hum_fit <- lgcp(elev_hum_comp, sp_winter_obs, samplers = StatesBoundary.shp, 
                     domain = list(coordinates = mesh.monarch),
                     options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))

summary(elev_hum_fit)


elev_hum_log <- predict(
  elev_hum_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + hum + Intercept),
    loglambda = mySmooth + elev + hum + Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = elev_hum_log$lambda, aes(color = NULL))+
  #gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(elev_hum_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#Save full results
saveRDS(elev_hum_fit, file = "Fit Results/Full_fit_altitude_cropped_humdensity_cropped(sigma=10, prior=1).rds")



###  Model 4: Elevation, habitat, and population density-----------------

spde.monarch <- inla.spde2.pcmatern(mesh.monarch,
                                    prior.sigma = c(10, 0.01),
                                    prior.range = c(1, 0.01))

#function for missing values
f.elev <- function(where) {
  # Extract the values
  v <- eval_spatial(altitude.sp, where, layer = "AltitudeRaster")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(altitude.sp, where, v)
  }
  return(v)
}

f.hum <- function(where) {
  # Extract the values
  v <- eval_spatial(human_density.sp, where, layer = "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec")
  # Fill in missing values
  if (any(is.na(v))) {
    v <- bru_fill_missing(human_density.sp, where, v)
  }
  return(v)
}


full_comp <- coordinates ~ 
  elev(f.elev(.data.), model = "linear") + 
  hum(f.hum(.data.), model = "linear") + 
  hab(habitat.sp,  model = "factor_contrast")+
  mySmooth(coordinates, model = spde.monarch) + Intercept(1)

full_fit <- lgcp(full_comp, sp_winter_obs, samplers = StatesBoundary.shp, 
                 domain = list(coordinates = mesh.monarch),
                 options = list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")))


summary(full_fit)

full.int_log <- predict(
  full_fit,
  fm_pixels(mesh.monarch, mask = StatesBoundary.shp, format = "sp"),
  ~ data.frame(
    lambda = exp(mySmooth + elev + hum + hab + Intercept),
    loglambda = mySmooth + elev + hum + hab+ Intercept
  )
)


#Multi
pl1 <- ggplot() +
  gg(data = full.int_log$lambda, aes(color = NULL))+
  gg(sp_winter_obs, color = "red")+
  geom_tile()+
  gg(StatesBoundary.shp) +
  scale_fill_viridis_c()+
  ggtitle("Observation Locations: Monarchs", subtitle = "(Response Scale)")

pl2 <- ggplot() +
  gg(full.int_log$loglambda, aes(color = NULL)) +
  geom_tile()+
  gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_viridis_c()+
  ggtitle("", subtitle = "(Log Transformed)")

multiplot(pl1, pl2, cols = 2)

#save full results
saveRDS(full_fit, "Fit Results/Full_fit_fullcropped(sigma=10, prior=1).rds")


### model DIC comparisons ---------------------------

smooth_only <- readRDS(file = "Fit Results/Full_fit_spde_only(sigma=10, prior=1).rds")
habitat <- readRDS(file = "Fit Results/Full_fit_habitat_lowres_cropped(sigma=10, prior=1).rds")
elevation <- readRDS(file = "Fit Results/Full_fit_altitude_cropped(sigma=10, prior=1).rds")
human_density <- readRDS(file = "Fit Results/Full_fit_humandensity_cropped(sigma=10, prior=1).rds")
habitat_elevation <- readRDS(file = "Fit Results/Full_fit_habitat_lowres_cropped_elevation_cropped(sigma=10, prior=1).rds")
habitat_HumanDensity <- readRDS(file = "Fit Results/Full_fit_habitat_lowres_cropped_humdensity_cropped(sigma=10, prior=1).rds")
elevation_HumanDensity <- readRDS(file = "Fit Results/Full_fit_altitude_cropped_humdensity_cropped(sigma=10, prior=1).rds")
all_cov <- readRDS("Fit Results/Full_fit_fullcropped(sigma=10, prior=1).rds")

#Full DIC
knitr::kable(deltaIC(smooth_only, habitat, elevation, human_density, habitat_elevation, habitat_HumanDensity, elevation_HumanDensity, all_cov, criterion = c("DIC")))


saveRDS(DIC_TABLE, file = "DIC_TABLE_Cropped_Results.rds")




# Relative Mean Square error---------------------

#Load results if not in environment
sim_v_1.22_fit <- readRDS(file = "Fit Results/sim_v_1.22_fit.rds")
sim_t_1.22_fit <- readRDS(file = "Fit Results/sim_t_1.22_fit.rds")
sim_v_0.61_fit <- readRDS(file = "Fit Results/sim_v_0.61_fit.rds")
sim_t_0.61_fit <- readRDS(file = "Fit Results/sim_t_0.61_fit.rds")
sim_t_0.15_fit <- readRDS(file = "Fit Results/sim_t_0.15_fit.rds")
efit <- readRDS(file = "Fit Results/efit_comp_used.rds")
e.int_log <- readRDS(file = "Fit Results/Monarch_altitude_pred(sigma=10, prior=1).rds")



monarch.lambda <- as.data.frame(e.int_log$loglambda$mean)
v.1.22.lambda <- as.data.frame(sim_v_1.22_fit$loglambda$mean)
t.1.22.lambda <- as.data.frame(sim_t_1.22_fit$loglambda$mean)
v.0.61.lambda <- as.data.frame(sim_v_0.61_fit$loglambda$mean)
t.0.61.lambda <- as.data.frame(sim_t_0.61_fit$loglambda$mean)
t.0.15.lambda <- as.data.frame(sim_t_0.15_fit$loglambda$mean)


rmse.v.1.22  <- sqrt((sum((monarch.lambda - v.1.22.lambda)^2)/nrow(monarch.lambda)))
rmse.t.1.22  <- sqrt((sum((monarch.lambda - t.1.22.lambda)^2)/nrow(monarch.lambda)))
rmse.v.0.61  <- sqrt((sum((monarch.lambda - v.0.61.lambda)^2)/nrow(monarch.lambda)))
rmse.t.0.61  <- sqrt((sum((monarch.lambda - t.0.61.lambda)^2)/nrow(monarch.lambda)))
rmse.t.0.15  <- sqrt((sum((monarch.lambda - t.0.15.lambda)^2)/nrow(monarch.lambda)))

#Store the coords for each predicted surface (the same for all simulations)
dataframe_for_cords <- as.data.frame(sim_t_0.15_fit$lambda)

###################Relative Ratio SIm vs Monarchs#####

log_rd_v.1.22 <- v.1.22.lambda  - monarch.lambda
log_rd_t.1.22 <- t.1.22.lambda  - monarch.lambda
log_rd_v.0.61 <- v.0.61.lambda - monarch.lambda
log_rd_t.0.61 <- t.0.61.lambda  - monarch.lambda
log_rd_t.0.15 <- t.0.15.lambda  - monarch.lambda



#combine dataframes for spatial graphing
log_rd_v.1.22 <- cbind(log_rd_v.1.22, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(log_rd_v.1.22) <- c("value", "coords.x1", "coords.x2")

log_rd_t.1.22  <- cbind(log_rd_t.1.22 , dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(log_rd_t.1.22 ) <- c("log.value", "coords.x1", "coords.x2")

log_rd_v.0.61  <- cbind(log_rd_v.0.61, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(log_rd_v.0.61) <- c("log.value", "coords.x1", "coords.x2")

log_rd_t.0.61  <- cbind(log_rd_t.0.61, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(log_rd_t.0.61) <- c("log.value", "coords.x1", "coords.x2")

log_rd_t.0.15  <- cbind(log_rd_t.0.15, dataframe_for_cords$coords.x1, dataframe_for_cords$coords.x2)
names(log_rd_t.0.15) <- c("log.value", "coords.x1", "coords.x2")

#turn df to spatial points
sp_log_rd_v.1.22 <- SpatialPointsDataFrame(
  coords = log_rd_v.1.22[, 2:3],
  data = log_rd_v.1.22
)

sp_log_rd_t.1.22 <- SpatialPointsDataFrame(
  coords = log_rd_t.1.22[, 2:3],
  data = log_rd_t.1.22
)

sp_log_rd_v.0.61 <- SpatialPointsDataFrame(
  coords = log_rd_v.0.61[, 2:3],
  data = log_rd_v.0.61
)

sp_log_rd_t.0.61 <- SpatialPointsDataFrame(
  coords = log_rd_t.0.61[, 2:3],
  data = log_rd_t.0.61
)

sp_log_rd_t.0.15 <- SpatialPointsDataFrame(
  coords = log_rd_t.0.15[, 2:3],
  data = log_rd_t.0.15
)


#Turn Spatial points into spatial pixel for graphing

log_rd_v.1.22_spdf <- as(sp_log_rd_v.1.22, "SpatialPixelsDataFrame")
log_rd_t.1.22_spdf <- as(sp_log_rd_t.1.22, "SpatialPixelsDataFrame")
log_rd_v.0.61_spdf <- as(sp_log_rd_v.0.61, "SpatialPixelsDataFrame")
log_rd_t.0.61_spdf <- as(sp_log_rd_t.0.61, "SpatialPixelsDataFrame")
log_rd_t.0.15_spdf <- as(sp_log_rd_t.0.15, "SpatialPixelsDataFrame")

#Save
saveRDS(log_rd_v.1.22_spdf, file = "Fit Results/log_rd_v.1.22_spdf.rds")
saveRDS(log_rd_t.1.22_spdf, file = "Fit Results/log_rd_t.1.22_spdf.rds")
saveRDS(log_rd_v.0.61_spdf, file = "Fit Results/log_rd_v.0.61_spdf.rds")
saveRDS(log_rd_t.0.61_spdf, file = "Fit Results/log_rd_t.0.61_spdf.rds")
saveRDS(log_rd_t.0.15_spdf, file = "Fit Results/log_rd_t.0.15_spdf.rds")



























