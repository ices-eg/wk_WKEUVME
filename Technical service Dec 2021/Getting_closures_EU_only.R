
# code to produce update of EUVME advice - Technical Service 1 (December 2021)
# --> obtain closure coordinates for the EEZ's of Ireland, France, Portugal and Spain
# --> use EEZ boundaries as as mapped by the Flanders Marine Institute in the 2020 Union of the ESRI Country
#     shapefile and the Exclusive Economic Zones (version 3), available at https://doi.org/10.14284/403

# the area now includes all 400-800 m depth within the EEZ of Portugal but outside BoB-IC ICES ecoregion
# this area has no VME habitat/index based on VME data 25 May 2020 - it was therefore not necessarily to 
# re-run the closure scenarios. We only had to update the maps with the new boundaries

# added tip of Portugal depth.rds to 1-Input data/csquares_ecoregions_depth (data file obtained outside github)

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

# load data for pdf plot
  worldMap <- map_data("world")   # Get the world map
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== "Bay of Biscay and the Iberian Coast" | Ecoregion== "Celtic Seas")
  
  minlong <- round(min(depth$long)-0.1,digits = 0)
  maxlong <- round(max(depth$long)+ 0.1,digits = 0)
  minlat  <- round(min(depth$lat)- 0.1,digits = 0)
  maxlat  <- round(max(depth$lat)+ 0.1,digits = 0)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  figmap <- ggplot() + geom_polygon(data=reg_sp, aes(x = long, y = lat, group = group),color=NA,fill="lightblue")
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
  figmap  <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
  figmap  <- figmap +  geom_polygon(data = EEZ_EUVME_sp, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  #figmap  <- figmap +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  
# get all scenarios 
  scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  closid_Scenario1_option1 <- c(1:41,43:75,77:82)
  closid_Scenario1_option2 <- c(1:39,41:73,75:81)
  closid_Scenario2_option1 <- c(1:49,51:86,88:92,94)
  closid_Scenario2_option2 <- c(1:83,123:127)
  
  for (sce in 1:4){
    scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[sce],"shp",sep="."),sep="/"))
    scenar <- st_cast(scenar,"POLYGON")
    scenar <- st_make_valid(scenar)
    
    # find all polygons that intersect with 400-800 m depth 
    overpol <- sf::st_intersects(scenar,reg)
    overp   <- as.data.frame(overpol)
    sce_int <- scenar[overp$row.id,] 
    sce_int <- sce_int[!duplicated(sce_int$geometry), ]
    
    # get the final closures
    new_clos <- st_intersection(sce_int,EEZ_EUVME)
    new_clos$FID <- eval(parse(text = paste("closid",scedat[sce],sep="_")))
    outdir <- paste(pathdir,"Technical service Dec 2021/Output",sep="/")
    write_sf(new_clos, paste(outdir,paste(scedat[sce],"shp", sep="."),sep="/"))
    
  # write csv
    coord <- matrix(data=NA,nrow=1,ncol=6)
    colnames(coord) <- c("Poly_no","Coord_order","Longitude","Latitude","Hole","Group")
    
    for (iclos in 1:nrow(new_clos)){
      cd_output <- st_coordinates(new_clos$geometry[iclos])  
      cd_output[,1] <- round(cd_output[,1], digits=4)
      cd_output[,2] <- round(cd_output[,2], digits=4)
      
      # check now which rows match with the row above
      datmatch <- c(1)
      for (n in 1:(nrow(cd_output)-1)){
        nbmatch <-  sum(match(cd_output[n,c(1,2,4)],cd_output[n+1,c(1,2,4)]),na.rm=T)
        datmatch <- c(datmatch,nbmatch)
      }
      
      # bind with coords and remove rows that match the previous row
      cd_output <- cbind(cd_output,datmatch)
      cd_output <- subset(cd_output,!(cd_output[,5] == 6))
      cd_output <- cd_output[,-5]
      nb  <- rep(new_clos$FID[iclos],nrow(cd_output))
      ord <- paste(nb,1:nrow(cd_output),sep="_") 
      lon <- cd_output[,1]  
      lat <- cd_output[,2]
      hole <- rep(ifelse(length(unique(cd_output[,4]))>1,TRUE,FALSE),nrow(cd_output))
      gr <- paste(nb,cd_output[,4],sep=".")
      datclos <- data.frame(nb,ord,lon,lat,hole,gr)
      colnames(datclos) <- colnames(coord)
      coord <- rbind(coord,datclos)
    }
    coord <- coord[-1,]
    
    write.csv(coord,paste(outdir,paste(scedat[sce],"coords.csv",sep="_"),sep="/"), row.names=FALSE)
    
# make pdf map
  coordcsv <- coord[!duplicated(coord[1]),]  # get one coordinate per closure 
  scea <- as(new_clos, 'Spatial')  
  
  pdf(file = paste(outdir,paste(scedat[sce],"pdf",sep="."),sep="/"), width=8.5, height=14)
  figmap_scea <- figmap + geom_polypath(data= scea, aes(x = long, y = lat, group = group),color=NA,fill="orange")
  figmap_scea <- figmap_scea + annotate(geom="text", x=coordcsv$Longitude, y=coordcsv$Latitude, label=coordcsv$Poly_no, color="red", size = 2)
  print(figmap_scea) 
  dev.off()
  }
  
  
    