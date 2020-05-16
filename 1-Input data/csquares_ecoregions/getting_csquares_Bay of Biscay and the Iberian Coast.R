
# script for spatial data grid Bay of Biscay/Iberian Coast

  # install libraries
  library(rgdal)
  library(sp)
  library(raster)
  
# set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/WKEUVME/1-Input data/"
  
# assign area of interest
  gt<-(GridTopology(c(-14.975, 35.025), c(0.05, 0.05), c(320, 300))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
  grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
  spix <- as(grt, "SpatialPixels")
  spol <- as(spix, "SpatialPolygons")
  rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
  LOCUNI<-as.data.frame(seq(1,length(spix)))
  rownames(LOCUNI)<-rnames
  bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
  bargrid@bbox # make sure "min" is a whole number
  
# assign c-squares
  # source coords_to_csquare_VMStools.R
  coord <- coordinates(bargrid)
  squares<-CSquare(coord[,1],coord[,2],0.05)
  bargrid@data$csquares <- squares
  
# assign EEZ
  shapeEEZ <- readOGR(dsn = paste(pathdir,"EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410")
  #plot(shapeEEZ)
  shapeEEZ@proj4string # check coordinates reference system
  shapeEEZ <- spTransform(shapeEEZ,CRS(proj4string(bargrid))) # make it similar to bargrid
  shapeEEZ@proj4string # check coordinates reference system again
  tr <- over(bargrid,shapeEEZ)
  bargrid@data$EEZ <- tr$Country 
  
  # assign ICES ecoregions
  shapeEcReg <- readOGR(dsn = paste(pathdir,"ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  #plot(shapeEcReg)
  shapeEcReg@proj4string # check coordinates reference system
  shapeEcReg <- spTransform(shapeEcReg,CRS(proj4string(bargrid))) # make it similar to bargrid
  shapeEcReg@proj4string # check coordinates reference system again
  tr <- over(bargrid,shapeEcReg)
  bargrid@data$EcReg <- tr$Ecoregion 
  bargrid <- subset(bargrid,bargrid@data$EcReg =="Bay of Biscay and the Iberian Coast")
  
  # get surface area of each grid cell 
  bargrid@data$area_sqkm <- area(bargrid) / 1000000
  
  #head(bargrid@data)
  bargrid@data <- bargrid@data[,c(2,4,3,5)] ## get c-square, ecoreg, eez, area_sqkm
  colnames(bargrid@data)[2] <- "Ecoregion"
  dd <- coordinates(bargrid)
  colnames(dd) <- c("long","lat")
  bargrid@data$long <- dd[,1]
  bargrid@data$lat  <- dd[,2]
  
  Bbic <- bargrid
  
  setwd(paste(pathdir,"csquares_ecoregions",sep="/"))
  save(Bbic, file="Bay of Biscay and the Iberian Coast_region.RData")
  