

# Library ------------------------
library(mapdata)
library(inlabru)
library(patchwork)
library(sf)
library(ggplot2)

# Import Shapes --------------------
setwd("")

#Starting Range
StartingRange <- read_sf("Shape/StartingRangeFull.shp")
StartingRange.df <- as.data.frame(StartingRange)
StartingRange.sp <- as(StartingRange.df$geometry, "Spatial")

#Import Specific overwintering locations
EndingPoints <- read_sf("Shape/Overwintering_points.shp")
#Create a buffer around the points. Agents within 5km are considered to have reached the location
EndRange <-  st_union(st_buffer(EndingPoints$geometry, dist = 5000))
EndRange.df <- as.data.frame(EndRange)
EndRange.sp <- as(EndRange.df$geometry, "Spatial")

# Usa Shapes
usa <- map_data("state")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

StatesBoundary.shp <- readRDS("Shape/StatesBoundary.shp.rds")


# Import Data -------------------------
#Model Fit result objects
sim_v_1.22_fit <- readRDS(file = "Fit Results/sim_v_1.22_fit.rds")
sim_t_1.22_fit <- readRDS(file = "Fit Results/sim_t_1.22_fit.rds")
sim_v_0.61_fit <- readRDS(file = "Fit Results/sim_v_0.61_fit.rds")
sim_t_0.61_fit <- readRDS(file = "Fit Results/sim_t_0.61_fit.rds")
sim_t_0.15_fit <- readRDS(file = "Fit Results/sim_t_0.15_fit.rds")
e.int_log <- readRDS(file = "Fit Results/Monarch_altitude_pred(sigma=10, prior=1).rds")

#Relative Risk Ratio graphing Object
log_rd_v.1.22_spdf <- readRDS(file = "Fit Results/log_rd_v.1.22_spdf.rds")
log_rd_t.1.22_spdf <- readRDS(file = "Fit Results/log_rd_t.1.22_spdf.rds")
log_rd_v.0.61_spdf <- readRDS(file = "Fit Results/log_rd_v.0.61_spdf.rds")
log_rd_t.0.61_spdf <- readRDS(file = "Fit Results/log_rd_t.0.61_spdf.rds")
log_rd_t.0.15_spdf <- readRDS(file = "Fit Results/log_rd_t.0.15_spdf.rds")

#Raw Data
# Load each object from an RDS file
sim_sp_v_1.22 <- readRDS("sim_sp_v_1.22.rds")
sim_sp_v_0.61 <- readRDS("sim_sp_v_0.61.rds")
sim_sp_t_1.22 <- readRDS("sim_sp_t_1.22.rds")
sim_sp_t_0.61 <- readRDS("sim_sp_t_0.61.rds")
sim_sp_t_0.15 <- readRDS("sim_sp_t_0.15.rds")



#---------Monarch Western Migration: Row 1 -----------------------

 ggplot() +
   geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", 
               color="black")+
   geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
   geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
                fill = "white", color="black")+
   gg(EndingPoints, color = "red", size = 2)+
   gg(StartingRange.sp, fill = "cyan", alpha = 0.1)+
   theme(legend.position="none")+
   coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
   theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-120, -110, by = 10))+
   xlab(NULL) + ylab(NULL)
  
  


 ggplot() +
   geom_polygon(data = usa, 
                aes(x=long, y = lat, group = group), 
                fill = "white", 
                color="black")+
   geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
                fill = "white", color="black")+
   geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
                fill = "white", color="black")+
   gg(sp_winter_obs)+
   gg(EndingPoints, color = "red", size = 2)+
   theme(legend.position="none")+
   coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-120, -110, by = 10))+
   xlab(NULL) + ylab(NULL)
   
 
 ggplot() +
   gg(StatesBoundary.shp, alpha = 0)+
   gg(e.int_log$loglambda, aes(color = NULL)) +
   geom_tile()+
   gg(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c()+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)
 
#----------- Model Results: Row 2 ------------------- 
 common_limits <-  c(-2, 2)
 
 pl2 <- ggplot() +
   gg(sim_v_1.22_fit$loglambda, aes(color = NULL)) +
   geom_tile()+
   #gg(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c(limits = common_limits,
                        oob = scales::squish,
                        name = "mean")+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)+
   annotate("text", x = -120, y = 32,
            label = paste("Value Range:", 
                          round(min(sim_v_1.22_fit$loglambda$mean)), 
                          "to", 
                          round(max(sim_v_1.22_fit$loglambda$mean))), # Dynamic label based on data range
            color = "black",                  # Text color
            size = 6)  
 
 
 pl3 <- ggplot() +
   gg(sim_t_1.22_fit$loglambda, aes(color = NULL)) +
   geom_tile()+
   #gg(sim_sp_t_1.22)+
   #gg(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c(limits = common_limits,
                        oob = scales::squish,
                        name = "mean")+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)+
   annotate("text", x = -120, y = 32,
            label = paste("Value Range:", 
                          round(min(sim_t_1.22_fit$loglambda$mean)), 
                          "to", 
                          round(max(sim_t_1.22_fit$loglambda$mean))), # Dynamic label based on data range
            color = "black",                  # Text color
            size = 6)  
 
 
 pl4 <- ggplot() +
   gg(sim_v_0.61_fit$loglambda, aes(color = NULL)) +
   geom_tile()+
   #gg(sim_sp_t_1.22)+
   #gg(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c(limits = common_limits,
                        oob = scales::squish,
                        name = "mean")+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)+
   annotate("text", x = -120, y = 32,
            label = paste("Value Range:", 
                          round(min(sim_v_0.61_fit$loglambda$mean)), 
                          "to", 
                          round(max(sim_v_0.61_fit$loglambda$mean))), # Dynamic label based on data range
            color = "black",                  # Text color
            size = 6)  
 
 
 pl5 <- ggplot() +
   gg(sim_t_0.61_fit$loglambda, aes(color = NULL)) +
   geom_tile()+
   #gg(sim_sp_t_0.61)+
   #gg(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c(limits = common_limits,
                        oob = scales::squish,
                        name = "mean")+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)+
   annotate("text", x = -120, y = 32,
            label = paste("Value Range:", 
                          round(min(sim_t_0.61_fit$loglambda$mean)), 
                          "to", 
                          round(max(sim_t_0.61_fit$loglambda$mean))), # Dynamic label based on data range
            color = "black",                  # Text color
            size = 6)
 
 #Supplementary figure s2
 pl6 <- ggplot() +
   gg(sim_t_0.15_fit$loglambda, aes(color = NULL)) +
   geom_tile()+
   #gg(sim_sp_t_1.22)+
   #g(StatesBoundary.shp, alpha = 0) +
   scale_fill_viridis_c(limits = common_limits,
                        oob = scales::squish,
                        name = "mean")+
   theme(axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         plot.title = element_text(size = 20),
         legend.key.size = unit(1, "cm"),
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20))+
   scale_x_continuous(breaks = seq(-125, -115, by = 10))+
   xlab(NULL) + ylab(NULL)+
   annotate("text", x = -120, y = 32,
            label = paste("Value Range:", 
                          round(min(sim_t_0.15_fit$loglambda$mean)), 
                          "to", 
                          round(max(sim_t_0.15_fit$loglambda$mean))), # Dynamic label based on data range
            color = "black",                  # Text color
            size = 6)
 
pl2 | pl3 | pl4 | pl5

 
 
#-----------Relative Risk Ratio (RRR)-----------------
common_limits <-  c(-4, 4)

pl2 <- ggplot() +
  gg(log_rd_v.1.22_spdf, aes(color = NULL)) +
  geom_tile()+
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "RRR")+
  #gg(StatesBoundary.shp, alpha = 0) +
  theme(axis.text.x = element_text(size = 20),
       axis.text.y = element_text(size = 20),
       plot.title = element_text(size = 20),
       legend.key.size = unit(1, "cm"),
       legend.text = element_text(size = 20),
       legend.title = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-125, -115, by = 10))+
  xlab(NULL) + ylab(NULL)+
  annotate("text", x = -120, y = 32,
           label = paste("Value Range:", 
                         round(min(log_rd_v.1.22_spdf$value)), 
                         "to", 
                         round(max(log_rd_v.1.22_spdf$value))), # Dynamic label based on data range
           color = "black",                  # Text color
           size = 6)     


pl3 <- ggplot() +
  gg(log_rd_t.1.22_spdf, aes(color = NULL)) +
  geom_tile()+
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "RRR")+
  #gg(StatesBoundary.shp, alpha = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-125, -115, by = 10))+
  xlab(NULL) + ylab(NULL)+
  annotate("text", x = -120, y = 32,
           label = paste("Value Range:", 
                         round(min(log_rd_t.1.22_spdf$log.value)), 
                         "to", 
                         round(max(log_rd_t.1.22_spdf$log.value))), # Dynamic label based on data range
           color = "black",                  # Text color
           size = 6) 

pl4 <- ggplot() +
  gg(log_rd_v.0.61_spdf, aes(color = NULL)) +
  geom_tile()+
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "RRR")+
  #gg(StatesBoundary.shp, alpha = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-125, -115, by = 10))+
  xlab(NULL) + ylab(NULL)+
  annotate("text", x = -120, y = 32,
           label = paste("Value Range:", 
                         round(min(log_rd_v.0.61_spdf$log.value)), 
                         "to", 
                         round(max(log_rd_v.0.61_spdf$log.value))), # Dynamic label based on data range
           color = "black",                  # Text color
           size = 6) 

pl5 <- ggplot() +
  gg(log_rd_t.0.61_spdf, aes(color = NULL)) +
  geom_tile()+
  #gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "RRR")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-125, -115, by = 10))+
  xlab(NULL) + ylab(NULL)+
  annotate("text", x = -120, y = 32,
           label = paste("Value Range:", 
                         round(min(log_rd_t.0.61_spdf$log.value)), 
                         "to", 
                         round(max(log_rd_t.0.61_spdf$log.value))), # Dynamic label based on data range
           color = "black",                  # Text color
           size = 6) 

#Supplementary figure S2
pl6 <- ggplot() +
  gg(log_rd_t.0.15_spdf, aes(color = NULL)) +
  geom_tile()+
  # gg(StatesBoundary.shp, alpha = 0) +
  scale_fill_gradientn(limits = common_limits,
                       colours = c("#1f78b4", "#b2b2b2", "#e31a1c"),
                       oob = scales::squish,
                       name = "RRR")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-125, -115, by = 10))+
  xlab(NULL) + ylab(NULL)+
  annotate("text", x = -120, y = 32,
           label = paste("Value Range:", 
                         round(min(log_rd_t.0.15_spdf$log.value)), 
                         "to", 
                         round(max(log_rd_t.0.15_spdf$log.value))), # Dynamic label based on data range
           color = "black",                  # Text color
           size = 6) 


pl2 | pl3 | pl4 | pl5

#------------Supplemental Figure 1--------------------------


a <- ggplot() +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", 
               color="black")+
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  gg(sim_sp_v_1.22)+
  gg(EndingPoints, color = "red", size = 2)+
  theme(legend.position="none")+
  coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-120, -110, by = 10))+
  xlab(NULL) + ylab(NULL)

b <- ggplot() +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", 
               color="black")+
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  gg(sim_sp_t_1.22)+
  gg(EndingPoints, color = "red", size = 2)+
  theme(legend.position="none")+
  coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-120, -110, by = 10))+
  xlab(NULL) + ylab(NULL)

c <- ggplot() +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", 
               color="black")+
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  gg(sim_sp_v_0.61)+
  gg(EndingPoints, color = "red", size = 2)+
  theme(legend.position="none")+
  coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-120, -110, by = 10))+
  xlab(NULL) + ylab(NULL)

d <- ggplot() +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", 
               color="black")+
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black")+
  gg(sim_sp_t_0.61)+
  gg(EndingPoints, color = "red", size = 2)+
  theme(legend.position="none")+
  coord_sf(xlim = c(-125, -110),  ylim = c(30, 49), expand = F)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(-120, -110, by = 10))+
  xlab(NULL) + ylab(NULL)




a | b| c| d
