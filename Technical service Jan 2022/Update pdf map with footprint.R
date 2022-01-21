# 4.	Update the pdf maps representing the VMEs in EU waters in the 4 
# scenarii and 4 options, as provided in ICES technical service of 15 
# December 2021, by adding the "footprint_all" pattern as background

## set path to folder 
  pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/WKEUVME_noGIT" # stores the data from sharepoint

## get libraries
  source(paste(pathdir,"6-Utilities/Libraries_WKEUVME.R",sep="/"))

# Get depth data 400 - 800 m EUVME
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthreg3 <- readRDS("Tip of portugal_depth.rds")
  depthreg3 <- subset(depthreg3,is.na(depthreg3@data$Ecoregion) & !(is.na(depthreg3@data$mean_depth_emodnet)))
  depthall <- rbind(depthreg,depthreg2)
  depthreg3 <- spTransform(depthreg3,CRS(proj4string(depthall))) 
  depthall <- spTransform(depthall,CRS(proj4string(depthreg3))) 
  depthall <- rbind(depthall,depthreg3)
  
  depth <- subset(depthall,depthall@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- rgeos::gUnaryUnion(reg)
  reg   <- st_as_sf(reg)
  reg <-  st_transform(reg, "EPSG:4326")  
  
# load EEZs
  EEZ <- readOGR(dsn = paste(pathdir, "1-Input data/EEZ_land_union_v3_202003",sep="/"), layer = "EEZ_Land_v3_202030")
  EEZ_EUVME_sp <- subset(EEZ,EEZ@data$UNION %in% c("Ireland", "France","Portugal","Spain"))
  EEZ_EUVME <- raster::aggregate(EEZ_EUVME_sp)
  EEZ_EUVME   <- st_as_sf(EEZ_EUVME)
  EEZ_EUVME
  
  sf_use_s2(FALSE)
  reg <- st_intersection(reg,EEZ_EUVME)
  reg <- reg %>% st_collection_extract(type="POLYGON")
  reg_sp <- as_Spatial(reg)

# load footprint combined 
  Footdir <- paste(pathdir,"Technical service Jan 2022/Output/Fishing footprint update",sep="/")
  footall <- st_read(paste(Footdir,"Footprint_all.shp",sep="/"))
  footall <- as(footall, 'Spatial')  
  
# load data for pdf plot
  worldMap <- map_data("world")   # Get the world map
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== "Bay of Biscay and the Iberian Coast" | Ecoregion== "Celtic Seas")
  
  minlong <- round(min(depth$long)-0.1,digits = 0)
  maxlong <- round(max(depth$long)+ 0.1,digits = 0)
  minlat  <- round(min(depth$lat)- 0.1,digits = 0)
  maxlat  <- 57.1 #round(max(depth$lat)+ 0.1,digits = 0)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  leg1 <- data.frame(long= c(57,57,57),lat=c(-15,-14,-13),
                     group=c("Combined footprint","VME closures","400-800 m depth border")) # random numbers outside plot
  
  figmap <- ggplot() + geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
  figmap <- figmap +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=11),
                            legend.title  = element_text(size=5,color = "white"),                            
                            legend.position =c(0.8,.95),
                            legend.box.background = element_rect(colour = "black",size = 2)) +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap  <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
  figmap  <- figmap +  geom_polygon(data = EEZ_EUVME_sp, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap  <- figmap +  geom_polypath(data = footall, aes(x = long, y = lat, group = group),color=NA,fill="lightblue")
  figmap  <- figmap  + geom_polygon(data=reg_sp, aes(x = long, y = lat, group = group),color="black",fill=NA,size = 0.01)
  figmap <-  figmap + geom_point(data = leg1, aes(x = long, y = lat,color=as.factor(group),shape = as.factor(group))) + 
                                   scale_shape_manual(values = c(0,15, 15)) +
     scale_color_manual(values = c("black","lightblue","orange"))

# get all scenarios 
  scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  closid_Scenario1_option1 <- c(1:41,43:75,77:82)
  closid_Scenario1_option2 <- c(1:39,41:73,75:81)
  closid_Scenario2_option1 <- c(1:49,51:86,88:92,94)
  closid_Scenario2_option2 <- c(1:83,123:127)

for (sce in 1:4){
  scenar <- st_read(paste(pathdir,"Technical service Jan 2022/Output/VME update",paste(scedat[sce],"shp",sep="."),sep="/"))
  coord <- read.csv(paste(pathdir,"Technical service Jan 2022/Output/VME update",paste(scedat[sce],"coords.csv",sep="_"),sep="/"))
  
  # make pdf map
  coordcsv <- coord[!duplicated(coord[1]),]  # get one coordinate per closure 
  scea <- as(scenar, 'Spatial')  
  
  outdir <- paste(pathdir,"Technical service Jan 2022/Output/VME update",sep="/" )
  pdf(file = paste(outdir,paste(scedat[sce],"pdf",sep="."),sep="/"), width=8.5, height=12)
  figmap_scea <- figmap + geom_polypath(data= scea, aes(x = long, y = lat, group = group),color=NA,fill="orange")
  figmap_scea <- figmap_scea + annotate(geom="text", x=coordcsv$Longitude, y=coordcsv$Latitude, label=coordcsv$Poly_No, color="red", size = 2)
  print(figmap_scea) 
  dev.off()
}