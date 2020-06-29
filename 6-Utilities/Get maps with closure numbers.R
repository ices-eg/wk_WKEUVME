

# plot all closures with information on VMEs that overlap with 400-800 meter range
scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS("Celtic Seas_depth.rds")
depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
depthall <- rbind(depthreg,depthreg2)

for (p in 1:4){
  depth <- subset(depthall,depthall@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- gUnaryUnion(reg)
  reg   <- st_as_sf(reg)
  
  scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[p],"shp",sep="."),sep="/"))
  scenar <- st_cast(scenar,"POLYGON")
  
  # find all polygons that intersect
  overpol      <- sf::st_intersection(scenar,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  
  scea <- as(scenar, 'Spatial')
  scea@data$rown <- rownames(scea@data)
  scea <- subset(scea, scea@data$rown %in% c(overpol))
  scea@data$FID <- 1:(nrow(scea@data))
  
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
  
  reg <- as(reg, 'Spatial')

  coordcsv <- read.csv(paste(pathdir,"5-Output/Closure options (region)",paste(scedat[p],"coords.csv",sep="_"),sep="/"),header=T)
  coordcsv <- coordcsv[!duplicated(coordcsv[1]),]
  
  # plot closures
  pdf(file = paste(pathdir,"5-Output/Closure options (region)",paste(scedat[p],"pdf",sep="."),sep="/"), width=8.5, height=14)
  figmap <- ggplot() + geom_polygon(data=reg, aes(x = long, y = lat, group = group),color="lightblue",fill="lightblue")
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
  
  figmap <- figmap + geom_polygon(data= scea, aes(x = long, y = lat, group = group),color="orange",fill="orange")
 
  figmap_scea <- figmap + annotate(geom="text", x=coordcsv$Longitude, y=coordcsv$Latitude, label=coordcsv$Poly_No, color="red", size = 2)
  print(figmap_scea) 
  
 dev.off()
  
}

  
  
  
