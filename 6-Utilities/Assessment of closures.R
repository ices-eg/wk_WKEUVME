
library(maptools)
library(raster)

# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS(paste(EcoReg,"_depth.rds",sep=""))
  
  depth <- subset(depthreg,depthreg@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- gUnaryUnion(reg)
  
# create histogram of closure areas within 400-800 meter depth range
  # scenario 1
  scenario1 <- st_read(paste(pathdir_nogit,"Buffer shapefile/VME_buffer_v1_2020May19_Scenario1.shp",sep="/"))
  sce1 <- as_Spatial(scenario1)
  sce1_df <- as.data.frame(sce1)
  sce1 <-SpatialPolygonsDataFrame(sce1,sce1_df)
  sce1<- disaggregate(sce1)
  sce1 <- spTransform(sce1,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce1)){
    together <- gIntersects(sce1[i,],reg)
    inout <- c(inout,together)
  }
  
  sce1 <- sce1[c(which(inout == TRUE)),]
  sce1 <- sce1[-59,] ## some tiny polygon in middle will go away
  
  # scenario 2
  scenario2 <- st_read(paste(pathdir_nogit,"Buffer shapefile/VME_buffer_v4_2020May21_Scenario2_dissolved.shp",sep="/"))
  sce2 <- as_Spatial(scenario2)
  sce2_df <- as.data.frame(sce2)
  sce2 <-SpatialPolygonsDataFrame(sce2,sce2_df)
  sce2<- disaggregate(sce2)
  sce2 <- spTransform(sce2,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce2)){
    together <- gIntersects(sce2[i,],reg)
    inout <- c(inout,together)
  }
  
  sce2 <- sce2[c(which(inout == TRUE)),]
  sce2 <- sce2[-63,] ## some tiny polygon in middle will go away
  
  # scenario 3
  scenario3 <- st_read(paste(pathdir_nogit,"Buffer shapefile/VME_buffer_v4_2020May21_Scenario2_dissolved.shp",sep="/"))
  sce3 <- as_Spatial(scenario3)
  sce3_df <- as.data.frame(sce3)
  sce3 <-SpatialPolygonsDataFrame(sce3,sce3_df)
  sce3<- disaggregate(sce3)
  sce3 <- spTransform(sce3,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce3)){
    together <- gIntersects(sce3[i,],reg)
    inout <- c(inout,together)
  }
  
  sce3 <- sce3[c(which(inout == TRUE)),]
  sce3 <- sce3[-63,] ## some tiny polygon in middle will go away
  

  par(mfrow=c(1,3))
  area_sce1 <- area(sce1)/1000000
  hist(log10(area_sce1),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 1")
  axis(1,c(2,2.48,3,3.477), c(TeX("$10^2$"),TeX("$10^{2.5}$"), TeX("$10^3$"), TeX("$10^{3.5}$")))

  area_sce2 <- area(sce2)/1000000
  hist(log10(area_sce2),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 2")
  axis(1,c(2,2.48,3,3.477), c(TeX("$10^2$"),TeX("$10^{2.5}$"), TeX("$10^3$"), TeX("$10^{3.5}$")))
  
  area_sce3 <- area(sce3)/1000000
  hist(log10(area_sce3),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 3")
  axis(1,c(2,2.48,3,3.477), c(TeX("$10^2$"),TeX("$10^{2.5}$"), TeX("$10^3$"), TeX("$10^{3.5}$")))
  
# calculate fraction of area in fishing footprint
  clos1 <- over(depthreg,s_po)
  depthreg@data$clos1 <- clos1[1:nrow(clos1),]
  depthreg@data$clos1[depthreg@data$clos1 == 0 ]  <- 1 
  depthreg@data$clos1[is.na(depthreg@data$clos1)] <- 0 


