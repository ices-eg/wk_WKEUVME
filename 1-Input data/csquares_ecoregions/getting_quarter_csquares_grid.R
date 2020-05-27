
# get one quarter of a c-square 0.05 x 0.05 grid, this is used to estimate buffers 

# assign area of interest
gt<-(GridTopology(c(-17.0625, 35.0625), c(0.025, 0.025), c(760, 1160))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
spix <- as(grt, "SpatialPixels")
spol <- as(spix, "SpatialPolygons")
rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
LOCUNI<-as.data.frame(seq(1,length(spix)))
rownames(LOCUNI)<-rnames
bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
bargrid@bbox 

coord <- coordinates(bargrid)
bargrid@data$long <- coord[,1]
bargrid@data$long <- round(bargrid@data$long, digits = 4)
bargrid@data$lat <- coord[,2]
bargrid@data$lat <- round(bargrid@data$lat, digits = 4)
bargrid@data$uni <- paste(bargrid@data$long,bargrid@data$lat) 

setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
saveRDS(bargrid, file = "Region_0.25_csquare_grid.rds")
