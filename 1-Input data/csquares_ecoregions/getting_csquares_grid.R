
# get a c-square 0.05 x 0.05 grid, this is used to map all VMEs (also outside ecoregions) 

# install libraries
library(rgdal)
library(sp)
library(raster)

# assign area of interest
gt<-(GridTopology(c(-18.975, 35.025), c(0.05, 0.05), c(600, 800))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
spix <- as(grt, "SpatialPixels")
spol <- as(spix, "SpatialPolygons")
rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
LOCUNI<-as.data.frame(seq(1,length(spix)))
rownames(LOCUNI)<-rnames
bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
bargrid@bbox # make sure "min" is a whole number

coord <- coordinates(bargrid)
bargrid@data$long <- coord[,1]
bargrid@data$long <- round(bargrid@data$long, digits = 3)
bargrid@data$lat <- coord[,2]
bargrid@data$lat <- round(bargrid@data$lat, digits = 3)
bargrid@data$uni <- paste(bargrid@data$long,bargrid@data$lat) 

squares<-CSquare(bargrid@data$long,bargrid@data$lat,0.05)
bargrid@data$csquares <- squares

setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
saveRDS(bargrid, file = "Region_csquare_grid.rds")
