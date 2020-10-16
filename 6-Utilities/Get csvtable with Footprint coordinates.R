
# code to derive fishing footprint shapefiles, maps and coordinates

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS("Celtic Seas_depth.rds")
depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
depthall <- rbind(depthreg,depthreg2)

# get fishing footprints
EcoReg <- "Celtic Seas"
source(paste(pathdir,"6-Utilities/Get fishing footprint mbcg_static.R", sep="/"))
FootprintCS <- Footprint
EcoReg <- "Bay of Biscay and the Iberian Coast"
source(paste(pathdir,"6-Utilities/Get fishing footprint mbcg_static.R", sep="/"))
Footprint <- rbind(Footprint, FootprintCS)

Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("Both_footprint")])
colnames(Footprint2@data)[ncol(Footprint2)] <- "Both_footprint"
Footprint_all <- subset(Footprint2,Footprint2@data$Both_footprint == 1)
Freg <- unionSpatialPolygons(Footprint_all,Footprint_all$Both_footprint)
Footprint_all <- gUnaryUnion(Freg)

Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("Static_footprint")])
colnames(Footprint2@data)[ncol(Footprint2)] <- "Static_footprint"
Footprint_static <- subset(Footprint2,Footprint2@data$Static_footprint == 1)
Freg <- unionSpatialPolygons(Footprint_static,Footprint_static$Static_footprint)
Footprint_static <- gUnaryUnion(Freg)

Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("MBCG_footprint")])
colnames(Footprint2@data)[ncol(Footprint2)] <- "MBCG_footprint"
Footprint_mobile <- subset(Footprint2,Footprint2@data$MBCG_footprint == 1)
Freg <- unionSpatialPolygons(Footprint_mobile,Footprint_mobile$MBCG_footprint)
Footprint_mobile <- gUnaryUnion(Freg)

scedat <- c("Footprint_all","Footprint_mobile","Footprint_static")

for(p in 1:3){
  reg <- get(scedat[p])
  proj4string(reg) <- CRS(proj4string(depthreg)) 
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS(proj4string(depthreg)) )
  reg   <- st_as_sf(reg) # combine polygons that intersect
  scenar <- st_cast(reg,"POLYGON") # create the polygon file
  reg <- as(scenar, 'Spatial')     # convert back (results in fewer, but larger shapefiles)
  
  # get coords + holes
  coords  <- ggplot2::fortify(reg)
  coords[,1] <- round(coords[,1],digits =3)
  coords[,2] <- round(coords[,2],digits =3)
  
  # check now which rows match with the row above
  datmatch <- c(1)
  for (n in 1:(nrow(coords)-1)){
    nbmatch <-  sum(match(coords[n,c(1,2,7)],coords[n+1,c(1,2,7)]),na.rm=T)
    datmatch <- c(datmatch,nbmatch)
  }
  
  # bind with coords and remove rows that match
  coords <- cbind(coords,datmatch)
  coords <- subset(coords,!(coords$datmatch == 6))
  coords <- coords[,-8]
  
  # now re-order and combine to data frame
  Poly_No  <- coords$id
  ord <- as.data.frame(table(Poly_No))
  ord <- ord[order(as.numeric(as.character(ord[,1]))),]
  Coord_order <- c()
  for (q in 1:nrow(ord)){
  No <- paste(ord$Poly_No[q],1:ord$Freq[q],sep="_")
  Coord_order <- c(Coord_order,No)
  }  
  
  runpol <- data.frame(Poly_No,Coord_order,Longitude = coords[,1], Latitude = coords[,2], Hole = coords[,4],Group = coords[,7])

  write.csv(runpol,paste(pathdir,"5-Output/Footprint (region)",paste(scedat[p],"coords.csv",sep="_"),sep="/"), row.names=FALSE)
}
