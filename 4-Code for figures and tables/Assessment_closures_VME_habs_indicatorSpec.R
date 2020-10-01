
# check how many VME habitat and indicators are within the closure
  outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

# get ICES shapefile ecoregion
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== EcoReg)
  
# load VME habitat and indicator species locations, overlay with ecoregion
  VMEobs <- read.csv(paste(pathdir_nogit,
                           "VME data repository/VME observations and csquares/VME_Extraction_of_precenses_25052020.csv",sep="/"),
                     header=T,sep=",",row.names = NULL)
  VMEobs <- subset(VMEobs,VMEobs$StartLongitude > -20)
  
  # select all VME observations
  VMEobs_subp <- VMEobs
  
  # create points based on the middle lat and long
  data <- data.frame(VMEid =VMEobs_subp$ï..ICES_ID,
                     VME_longitude = VMEobs_subp$MiddleLongitude,
                     VME_latitude  = VMEobs_subp$MiddleLatitude,
                     stringsAsFactors = F)
  
  coordinates(data) <- c("VME_longitude","VME_latitude")
  data@data$VME_longitude <- VMEobs_subp$MiddleLongitude
  data@data$VME_latitude <- VMEobs_subp$MiddleLatitude
  VMEobs_points <- data
  proj4string(VMEobs_points) <- CRS(proj4string(shapeReg)) 
  
  # over lay and get indicator/habitat per region
  tt <- over(VMEobs_points,shapeReg)
  VMEobs$Ecoregion <- tt$Ecoregion
  
  VMEobs_sub   <- subset(VMEobs,VMEobs$Ecoregion == EcoReg)
  dathabitat   <- aggregate(ï..ICES_ID ~ HabitatType, data = VMEobs_sub, FUN = length)
  datindicator <- aggregate(ï..ICES_ID ~ VME_Indicator, data = VMEobs_sub, FUN = length)
  
  # select all VME observations within the ecoregion
  VMEobs_subp <- VMEobs_sub
  
  # create points based on the middle lat and long
  data <- data.frame(VMEid =VMEobs_subp$ï..ICES_ID,
                     VME_longitude = VMEobs_subp$MiddleLongitude,
                     VME_latitude  = VMEobs_subp$MiddleLatitude,
                     stringsAsFactors = F)
  
  coordinates(data) <- c("VME_longitude","VME_latitude")
  data@data$VME_longitude <- VMEobs_subp$MiddleLongitude
  data@data$VME_latitude <- VMEobs_subp$MiddleLatitude
  VMEobs_points <- data
  proj4string(VMEobs_points) <- CRS(proj4string(shapeReg)) 
  
# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS(paste(EcoReg,"_depth.rds",sep=""))
  
  depth <- subset(depthreg,depthreg@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- gUnaryUnion(reg)
  
  # overlay with 400-800 meter depth band
  reg <- spTransform(reg,CRS(proj4string(VMEobs_points))) # make it similar to VMEobs_points
  tt <- over(VMEobs_points,reg)
  VMEobs_sub$within <- tt
  
  VMEobs_sub2   <- subset(VMEobs_sub,VMEobs_sub$within == 1)
  dathabitat_depth   <- aggregate(ï..ICES_ID ~ HabitatType, data = VMEobs_sub2, FUN = length)
  datindicator_depth <- aggregate(ï..ICES_ID ~ VME_Indicator, data = VMEobs_sub2, FUN = length)
  
  # combine the table
  dathabitat <- cbind(dathabitat, dathabitat_depth[match(dathabitat[,1],dathabitat_depth[,1]), c(2)])
  datindicator <- cbind(datindicator, datindicator_depth[match(datindicator[,1],datindicator_depth[,1]), c(2)])
  
# select all VME observations within the ecoregion and 400-800 meter depth
  VMEobs_subp <- VMEobs_sub2
  
  # create points based on the middle lat and long
  data <- data.frame(VMEid =VMEobs_subp$ï..ICES_ID,
                     VME_longitude = VMEobs_subp$MiddleLongitude,
                     VME_latitude  = VMEobs_subp$MiddleLatitude,
                     stringsAsFactors = F)
  
  coordinates(data) <- c("VME_longitude","VME_latitude")
  data@data$VME_longitude <- VMEobs_subp$MiddleLongitude
  data@data$VME_latitude <- VMEobs_subp$MiddleLatitude
  VMEobs_points <- data
  proj4string(VMEobs_points) <- CRS(proj4string(shapeReg)) 
  
  # run the four scenarios
  sce_name <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  
# check overlap between closures and VME habitats/indicators
for(i in 1:4){
  scenario <- st_read(paste(pathdir,"2-Data processing",paste(sce_name[i],"shp",sep="."),sep="/"))
  scenario <- st_cast(scenario,"POLYGON")
  reg <- st_as_sf(reg)
  sce <- sf::st_intersection(scenario,reg)
  sce <- as(sce, 'Spatial')
  tt <- over(VMEobs_points,sce)
  VMEobs_sub2$sce <- tt
  
  VMEobs_loop   <- subset(VMEobs_sub2,VMEobs_sub2$sce == 0)
  dathabitat_sce   <- aggregate(ï..ICES_ID ~ HabitatType, data = VMEobs_loop, FUN = length)
  datindicator_sce <- aggregate(ï..ICES_ID ~ VME_Indicator, data = VMEobs_loop, FUN = length)
  
  # combine the table
  dathabitat <- cbind(dathabitat, dathabitat_sce[match(dathabitat[,1],dathabitat_sce[,1]), c(2)])
  datindicator <- cbind(datindicator, datindicator_sce[match(datindicator[,1],datindicator_sce[,1]), c(2)])
  
}
  
# combine scenario 1 option 2, scenario 2 option 1
  scen1b <- st_read(paste(pathdir,"2-Data processing",paste(sce_name[2],"shp",sep="."),sep="/"))
  scen2a <- st_read(paste(pathdir,"2-Data processing",paste(sce_name[3],"shp",sep="."),sep="/"))
  
  scen1b2a <- rbind(scen1b, scen2a)
  scen1b2a <- st_union(scen1b2a)
  
  scenario <- st_cast(scen1b2a,"POLYGON")
  reg <- st_as_sf(reg)
  sce <- sf::st_intersection(scenario,reg)
  sce <- as(sce, 'Spatial')
  tt <- over(VMEobs_points,sce)
  VMEobs_sub2$sce <- tt
  
  VMEobs_loop   <- subset(VMEobs_sub2,!(is.na(VMEobs_sub2$sce)))
  dathabitat_sce   <- aggregate(ï..ICES_ID ~ HabitatType, data = VMEobs_loop, FUN = length)
  datindicator_sce <- aggregate(ï..ICES_ID ~ VME_Indicator, data = VMEobs_loop, FUN = length)
  
  # combine the table
  dathabitat <- cbind(dathabitat, dathabitat_sce[match(dathabitat[,1],dathabitat_sce[,1]), c(2)])
  datindicator <- cbind(datindicator, datindicator_sce[match(datindicator[,1],datindicator_sce[,1]), c(2)])
  
# give colnames  
  colnames(dathabitat) <- c("VME habitat","Ecoregion","400-800 m depth","Sce1_O1","Sce1_O2","Sce2_O1","Sce2_O2","Sce1_O2-Sce2_O1")
  dathabitat <- subset(dathabitat,!(dathabitat$`VME habitat` == "NULL" ))
  colnames(datindicator) <- c("VME indicator","Ecoregion","400-800 m depth","Sce1_O1","Sce1_O2","Sce2_O1","Sce2_O2","Sce1_O2-Sce2_O1")
  datindicator <- subset(datindicator,!(datindicator$`VME indicator` == "NULL" ))

# save table
write.csv(dathabitat, paste(outdir,"Table_closure_VMEhabitat.csv",sep="/"), 
          row.names = FALSE, quote=FALSE)

write.csv(datindicator, paste(outdir,"Table_closure_VMEindicator.csv",sep="/"), 
          row.names = FALSE, quote=FALSE)
