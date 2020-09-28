#pathdir <-"C:/Users/holahh/Documents/GitHub/wk_WKEUVME"
#EcoReg <- "Celtic Seas"

#### code to produce fishing footprint polygons and tables per ecoregion

setwd(paste(pathdir,"3-Data analysis",EcoReg,sep="/")) 
outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

shapeEEZ <- readOGR(dsn = paste(pathdir,"1-Input data/EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410") 
shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeReg  <- subset(shapeEcReg, Ecoregion== EcoReg)

# packages
library(sp)
library(sf)
library(tidyverse)
library(rgdal)

#### fig set up

fig1 <- readRDS(file = "fig1.rds")

# Get the world map
worldMap <- map_data("world")

# get boundaries of ecoregion used for all plots
minlong <- round(min(fig1$long)-0.1,digits = 0)
maxlong <- round(max(fig1$long)+ 0.1,digits = 0)
minlat  <- round(min(fig1$lat)- 0.1,digits = 0)
maxlat  <- round(max(fig1$lat)+ 0.1,digits = 0)
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# plotting specifics
pointsize <- 0.5
fig_width  <- (maxlong-minlong)/2.5
fig_length <- (maxlat-minlat)/2

#### Fig 0.1 scenario 0. "base case" presence of fishing footprint 2009-2011

# load in vms data
fig6 <- readRDS(file = "fig6.rds") 
fig6$ref <- as.factor(fig6$ref)

# subset for fishing c-squares
fig6_f <- fig6 %>% 
  filter(ref==1) %>% 
  dplyr::select(long,lat)

# create c-square polygons from centroid values
library(eSDM)
fig6_sf <- pts2poly_centroids(fig6_f, 0.026, crs = 4326)

# to unify polygons
fig6_sf <- st_union(fig6_sf) 
fig6_sf <- st_cast(fig6_sf, "POLYGON")

# to remove holes from polygons
fig6_sf <- nngeo::st_remove_holes(fig6_sf)

# write polygon shapefile to output folder
write_sf(fig6_sf, paste(outdir,"fish_foot_scen_0.shp",sep="/"))

# plot scenario 0 fishing footprint
jpeg(file = paste(outdir,"Fig_scen_0.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_sf(data = fig6_sf,col="black",fill="white")
figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
figmap <- figmap +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11),
                          legend.position ="bottom") +
  ggtitle("Scenario 0. 2009-2011 footprint") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
print(figmap2)
dev.off() 


#### Fig. scenario 2 & 3. a cell must touch one/two other cell on an edge or vertex

# create fishing/unfished subsets
dat <- fig6 %>% 
  filter(ref==1)

#dat_u <- fig6 %>% 
#  filter(ref==0) %>% 
#  dplyr::select(long,lat)

# calculate the number of adjacent cells for fished subset
dat$long <- round(dat$long,3)
dat$lat <- round(dat$lat,3)
lonlats <- paste(dat$long, dat$lat, sep=":")

for(i in (1:dim(dat)[1])){
  
  nw  <- paste(dat$long[i]-0.05, dat$lat[i]+0.05, sep=":")
  nn  <- paste(dat$long[i]+0.00, dat$lat[i]+0.05, sep=":")
  ne  <- paste(dat$long[i]+0.05, dat$lat[i]+0.05, sep=":")
  ee  <- paste(dat$long[i]+0.05, dat$lat[i]+0.00, sep=":")
  se  <- paste(dat$long[i]+0.05, dat$lat[i]-0.05, sep=":")
  ss  <- paste(dat$long[i]+0.00, dat$lat[i]-0.05, sep=":")
  sw  <- paste(dat$long[i]-0.05, dat$lat[i]-0.05, sep=":")
  ww  <- paste(dat$long[i]-0.05, dat$lat[i]+0.00, sep=":")
  
  
  dat$adjacent.cells[i] <- sum(nw %in% lonlats[-i], nn %in% lonlats[-i], ne %in% lonlats[-i],
                               ee %in% lonlats[-i], se %in% lonlats[-i], ss %in% lonlats[-i],
                               sw %in% lonlats[-i], ww %in% lonlats[-i])
}

table(dat$adjacent.cells)
dat$adjacent.cells <- as.factor(dat$adjacent.cells)

jpeg(file = paste(outdir,"Fig_Adjacent_cells.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_point(data=dat, aes(x=long, y=lat , col=adjacent.cells),
                                shape=15,size=0.5,na.rm=T) 
figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
figmap <- figmap +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11),
                          legend.position ="bottom") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
print(figmap2)
dev.off() 

dat$adjacent.cells <- as.numeric(dat$adjacent.cells)

# add total fishing footprint in scenario 2 and 3 to .rds file
names(dat)
dat$scenario_2 <- 0
dat$scenario_2[dat$adjacent.cells > 1] <- 1
dat$scenario_3 <- 0
dat$scenario_3[dat$adjacent.cells > 2] <- 1

fig6b <- dat %>% 
  dplyr::select(-adjacent.cells)

saveRDS(fig6b,  paste(pathdir,"3-Data analysis",EcoReg,"fig6b.rds",sep="/"))

## scenario 2 a cell must touch one other cell (on an edge or vertex)

scen_2 <- dat %>% 
  filter(adjacent.cells > 1) %>% 
  dplyr::select(long,lat)

## scenario 2 a cell must touch two other cell (on an edge or vertex)
scen_3 <- dat %>% 
  filter(adjacent.cells > 2) %>% 
  dplyr::select(long,lat)

dat2 <- dat %>% 
  dplyr::select(long,lat)

# create c-square polygons from centroid values
library(eSDM)
pols_2 <- pts2poly_centroids(scen_2, 0.0256, crs = 4326)
pols_3 <- pts2poly_centroids(scen_3, 0.0256, crs = 4326)


# to unify polygons
pols_2_sf <- st_union(pols_2)
pols_2_sf <- st_cast(pols_2_sf, "POLYGON")
pols_2_sf <- nngeo::st_remove_holes(pols_2_sf)

write_sf(pols_2_sf, paste(outdir,"fish_foot_scen_2.shp",sep="/"))


# plot scenario 2 fishing footprint
jpeg(file = paste(outdir,"Fig_scen_2.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_sf(data = pols_2_sf,col="black",fill="white")
figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
figmap <- figmap +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11),
                          legend.position ="bottom") +
  ggtitle("Scenario 2. A cell must touch 1 other") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
print(figmap2)
dev.off() 


# to unify polygons
pols_3_sf <- st_union(pols_3) 
pols_3_sf <- st_cast(pols_3_sf, "POLYGON")
pols_3_sf <- nngeo::st_remove_holes(pols_3_sf)

write_sf(pols_3_sf, paste(outdir,"fish_foot_scen_3.shp",sep="/"))


# plot scenario 3 fishing footprint
jpeg(file = paste(outdir,"Fig_scen_3.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_sf(data = pols_3_sf,col="black",fill="white")
figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
figmap <- figmap +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11),
                          legend.position ="bottom") +
  ggtitle("Scenario 3. A cell must touch 2 others") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
print(figmap2)
dev.off() 

#plot(st_geometry(pols_3_sf), col = sf.colors(12, categorical = TRUE), border = 'black', 
#     axes = TRUE)
