
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

# plot all footprints
scedat <- c("Footprint_all","Footprint_mobile","Footprint_static")

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS("Celtic Seas_depth.rds")
depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
depthall <- rbind(depthreg,depthreg2)
depth <- subset(depthall,depthall@data$within ==1)
regdepth <- unionSpatialPolygons(depth,depth$within)
regdepth <- gUnaryUnion(regdepth)
regdepth   <- st_as_sf(regdepth)
regdepth <- as(regdepth, 'Spatial')

for (p in 1:3){
  reg <- get(scedat[p])
  proj4string(reg) <- CRS(proj4string(depthreg)) 
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS(proj4string(depthreg)) )
  reg   <- st_as_sf(reg) # combine polygons that intersect
  scenar <- st_cast(reg,"POLYGON") # create the polygon file
  reg <- as(scenar, 'Spatial')     # convert back (results in fewer, but larger shapefiles)

  # Get the world map
  worldMap <- map_data("world")
  
  # get polygons
  shapeEEZ <- readOGR(dsn = paste(pathdir,"1-Input data/EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410") 
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== "Bay of Biscay and the Iberian Coast" | Ecoregion== "Celtic Seas")
  
  # get boundaries of ecoregion used for all plots
  minlong <- round(min(depth$long)-0.1,digits = 0)
  maxlong <- round(max(depth$long)+ 0.1,digits = 0)
  minlat  <- round(min(depth$lat)- 0.1,digits = 0)
  maxlat  <- round(max(depth$lat)+ 0.1,digits = 0)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  # plotting specifics
  pointsize <- 0.5
  fig_width  <- (maxlong-minlong)/2.5
  fig_length <- (maxlat-minlat)/2
  
  coordcsv <- read.csv(paste(pathdir,"5-Output/Footprint (region)",paste(scedat[p],"coords.csv",sep="_"),sep="/"),header=T)
  coordcsv <- coordcsv[!duplicated(coordcsv[1]),]
  
  # plot closures
  pdf(file = paste(pathdir,"5-Output/Footprint (region)",paste(scedat[p],"pdf",sep="."),sep="/"), width=8.5, height=14)
  figmap <- ggplot() + geom_polygon(data=regdepth, aes(x = long, y = lat, group = group),color= NA,fill="lightblue")
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
  figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
  figmap  <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap  <- figmap +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  
  figmap <- figmap + geom_polypath(data= reg, aes(x = long, y = lat, group = group),color=NA,fill="orange")
  
  figmap_scea <- figmap + annotate(geom="text", x=coordcsv$Longitude, y=coordcsv$Latitude, label=coordcsv$Poly_No, color="red", size = 2)
  print(figmap_scea) 
  
  dev.off()
  
}