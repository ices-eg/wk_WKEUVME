
### scenario 1 -- option 2
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
  grid_csquare <- readRDS("Region_csquare_grid.rds")

# load VMEs
  VMEobs <- read.csv(paste(pathdir_nogit,
                         "VME data repository/VME observations and csquares/VME_Extraction_of_precenses_25052020_v2.csv",sep="/"),
                   header=T,sep=",",row.names = NULL)
  VMEobs <- subset(VMEobs,VMEobs$StartLongitude > -20)

# overlap between VME elements and VME observations are now based on the midpoint of the line
# the below code allows to change to using the entire polyline (yet some lines cover long distances, hence check with WGDEC)
  # create polylines
    # VMEobs_subl <- subset(VMEobs,VMEobs$GeometryType =="line" | VMEobs$GeometryType =="Line")
    # data <- data.frame(VMEid =VMEobs_subl$ï..ICES_ID,
    #                   start_VME_longitude = VMEobs_subl$StartLongitude,
    #                   start_VME_latitude  = VMEobs_subl$StartLatitude,
    #                   end_VME_longitude    = as.numeric(as.character(VMEobs_subl$EndLongitude)),
    #                   end_VME_latitude     = as.numeric(as.character(VMEobs_subl$EndLatitude)),
    #                   stringsAsFactors = F)
    
    # funRes<-function(N){
    # coords1 <- cbind(c(data$start_VME_longitude[N],data$end_VME_longitude[N]),
    #                 c(data$start_VME_latitude[N],data$end_VME_latitude[N]))
    # sln1 <- Lines(Line(coords1),  ID = data$VMEid[N])
    # return(sln1)
    # }
    
    # row.names(data) <- data$VMEid
    # VMEobs_lines  <- SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(data),funRes)),data)
  # create polypoints
    # VMEobs_subp <- subset(VMEobs,VMEobs$GeometryType =="point" | VMEobs$GeometryType =="Point")

# select all VME observations
  VMEobs_subp <- VMEobs

# create polypoints based on the middle lat and long
  data <- data.frame(VMEid =VMEobs_subp$ï..ICES_ID,
                     VME_longitude = VMEobs_subp$MiddleLongitude,
                     VME_latitude  = VMEobs_subp$MiddleLatitude,
                     stringsAsFactors = F)

  coordinates(data) <- c("VME_longitude","VME_latitude")
  data@data$VME_longitude <- VMEobs_subp$MiddleLongitude
  data@data$VME_latitude <- VMEobs_subp$MiddleLatitude
  VMEobs_points <- data

# load VME elements
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))

# change VME observations to same projection
  Proj     <- as(Bank, 'Spatial')
  proj4string(VMEobs_points) <- CRS(proj4string(Proj)) 
  VMEobs_points <- st_as_sf(VMEobs_points)
  VMEobs_poly <- VMEobs_points
  
  # polylines if used....
    # proj4string(VMEobs_lines) <- CRS(proj4string(Proj)) 
    # VMEobs_lines <- st_as_sf(VMEobs_lines)
    # VMEobs_poly <- st_join(VMEobs_lines,VMEobs_points)

# get all VME elements that overlap with VME observations
  Bank_over       <- st_intersection(Bank,VMEobs_poly)
  Coralmound_over <- st_intersection(Coralmound,VMEobs_poly)
  Mudvolcano_over <- st_intersection(Mudvolcano,VMEobs_poly)
  Seamount_over   <- st_intersection(Seamount,VMEobs_poly)

# Bank -- select all element polygons that intersect with VME observations
  Bank_over <- as(Bank_over, 'Spatial')
  Bank     <- as(Bank, 'Spatial')
  Bank_over <- spTransform(Bank_over,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  Bank <- spTransform(Bank,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  
  Bank_fin <- over(Bank,Bank_over)
  Bank_fin <- subset(Bank_fin,!(is.na(Bank_fin$OBJECTID)))  
  polidx <- unique(Bank_fin$OBJECTID)
  Bank_fin <- Bank[Bank$OBJECTID %in% c(polidx),]

#  Coralmound -- select all element polygons that intersect with VME observations
  Coralmound_over <- as(Coralmound_over, 'Spatial')
  Coralmound     <- as(Coralmound, 'Spatial')
  Coralmound_over <- spTransform(Coralmound_over,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  Coralmound <- spTransform(Coralmound,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  
  Coralmound_fin <- over(Coralmound,Coralmound_over)
  Coralmound_fin <- subset(Coralmound_fin,!(is.na(Coralmound_fin$OBJECTID)))  
  polidx <- unique(Coralmound_fin$OBJECTID)
  Coralmound_fin <- Coralmound[Coralmound$OBJECTID %in% c(polidx),]

# Mudvolcano -- select all element polygons that intersect with VME observations
  Mudvolcano_over <- as(Mudvolcano_over, 'Spatial')
  Mudvolcano     <- as(Mudvolcano, 'Spatial')
  Mudvolcano_over <- spTransform(Mudvolcano_over,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  Mudvolcano <- spTransform(Mudvolcano,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  
  Mudvolcano_fin <- over(Mudvolcano,Mudvolcano_over)
  Mudvolcano_fin <- subset(Mudvolcano_fin,!(is.na(Mudvolcano_fin$OBJECTID)))  
  polidx <- unique(Mudvolcano_fin$OBJECTID)
  Mudvolcano_fin <- Mudvolcano[Mudvolcano$OBJECTID %in% c(polidx),]

# seamounts -- select all element polygons that intersect with VME observations
  Seamount_over <- as(Seamount_over, 'Spatial')
  Seamount     <- as(Seamount, 'Spatial')
  Seamount_over <- spTransform(Seamount_over,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  Seamount <- spTransform(Seamount,CRS(proj4string(grid_csquare))) # make it similar to grid_csquare
  
  Seamount_fin <- over(Seamount,Seamount_over)
  Seamount_fin <- subset(Seamount_fin,!(is.na(Seamount_fin$OBJECTID)))  
  polidx <- unique(Seamount_fin$OBJECTID)
  Seamount_fin <- Seamount[Seamount$OBJECTID %in% c(polidx),]

# overlay with csquare grid within 400-800 meters, hence get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthall <- rbind(depthreg,depthreg2)
  depth <- subset(depthall,depthall@data$within ==1)

  # and overlay
  tt <- over(depth,Bank_fin)
  depth@data$Bank <- tt$LandformOr
  tt <- over(depth,Coralmound_fin)
  depth@data$Coralmound <- tt$LandformOr
  tt <- over(depth,Mudvolcano_fin)
  depth@data$Mudvolcano <- tt$LandformOr
  tt <- over(depth,Seamount_fin)
  depth@data$Seamount <- tt$LandformOr
  
# select all vme element grid cells that are closed
  element_close <- subset(depth,!(is.na(depth@data$Bank)) | !(is.na(depth@data$Coralmound)) | 
                          !(is.na(depth@data$Mudvolcano)) | !(is.na(depth@data$Seamount)))
  element_close@data$elementclose <- 1

# now continue as scenario 1 option 1 but remove all VMEs inside element as these are already protected,
# unless the VMEs are isolated c-squares

# add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  VME <-  cbind(VME, depthall@data[match(VME$CSquare,depthall@data$csquares), c("long","lat")])
  VME$uni <- paste(VME$long,VME$lat)

# find all isolated c-squares within elements that are also not connected to another VME habitat/index
  element_close@data$uni   <- paste( element_close@data$long, element_close@data$lat)
  
  isocsquare <- c()
  for (j in 1:nrow(element_close@data)){
    long <- element_close@data$long[j]
    lat <- element_close@data$lat[j]
    
    sublong <- c(long,long-0.05,long+0.05)
    sublat  <- c(lat,lat-0.05,lat+0.05)
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    
    # nb of c-squares with elements that are surrounding
    nb <- length(which(!(is.na(match(element_close@data$uni,uni)))))
    if(nb == 1){
      
      # check if other VMEs are connected
      nb2 <- length(which(!(is.na(match(VME$uni,uni)))))
      if (nb2 == 1){
        element_close@data$elementclose[j] <- 100
      }
    }
  }

# get all long x lat at 0.25 c-square format
  tt <- grid_csquare@data
  tt$long <- round(tt$long, digits = 4)
  tt$lat <- round(tt$lat, digits = 4)
  
  tt1 <- tt
  tt2 <- tt
  tt3 <- tt
  tt4 <- tt
  
  tt1$long <- tt$long - 0.05/4
  tt1$lat  <- tt$lat - 0.05/4
  tt2$long <- tt$long +  0.05/4
  tt2$lat  <- tt$lat +  0.05/4
  tt3$long <- tt$long -  0.05/4
  tt3$lat  <- tt$lat +  0.05/4
  tt4$long <- tt$long +  0.05/4
  tt4$lat  <- tt$lat - 0.05/4
  tt1$coords <- paste(tt1$long,tt1$lat)
  tt2$coords <- paste(tt2$long,tt2$lat)
  tt3$coords <- paste(tt3$long,tt3$lat)
  tt4$coords <- paste(tt4$long,tt4$lat)

# remove all VMEs inside element as these are already protected (and not buffered)
  VME <- cbind(VME, element_close@data[match(VME$CSquare,element_close@data$csquares), c("elementclose")])
  colnames(VME)[ncol(VME)] <-"elementclose"
  VME$elementclose[is.na(VME$elementclose)] <- 100 # same number as the isolated c-squares as above
  VME <- subset(VME,VME$elementclose == 100)

# continue as in scenario 1 option 1
  VMEdepth <- cbind(grid_csquare, VME[match(grid_csquare@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"
  VMEdepth <- subset(VMEdepth@data,!(is.na(VMEdepth@data$VME_Class)))
  VME_high <- VMEdepth[VMEdepth$VME_Class %in% c(3,2,1),]
  VME_high$long <- round(VME_high$long, digits = 3)
  VME_high$lat <- round(VME_high$lat, digits = 3)

# create buffer around all closed VMEs
  tt1$buffer <- NA
  tt2$buffer <- NA
  tt3$buffer <- NA
  tt4$buffer <- NA
  
  for (i in 1:nrow(VME_high)){
    long <- VME_high$long[i]
    lat  <- VME_high$lat[i]
    
    sublong <- c(long-(0.05*0.75),long-(0.05*0.25),long+(0.05*0.25),long+(0.05*0.75))
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05*0.75),lat-(0.05*0.25),lat+(0.05*0.25),lat+(0.05*0.75))
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    
    tt1[tt1$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt2[tt2$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt3[tt3$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt4[tt4$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
  }

# select all VME low index that are adjacent/joining
  VME_low <- VMEdepth[VMEdepth$VME_Class %in% c(0),]
  VME_low$uni <- paste(VME_low$long,VME_low$lat)
  VME_low$joining <- NA
  VME_low$long <- round(VME_low$long, digits = 4)
  VME_low$lat <- round(VME_low$lat, digits = 4)
  
  for (i in 1:nrow(VME_high)){
    long <- VME_high$long[i]
    lat  <- VME_high$lat[i]
    
    sublong <- c(long-(0.05),long+(0.05),long)
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05),lat+(0.05),lat)
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    VME_low[VME_low$uni %in% c(as.character(uni[,1])),"joining"] <- 100
  }
  
# and add buffer around all adjacent/joining low index cells
  tt1$buffer_low <- NA
  tt2$buffer_low <- NA
  tt3$buffer_low <- NA
  tt4$buffer_low <- NA
  
  VME_low <- subset(VME_low,VME_low$joining == 100)
  for (i in 1:nrow(VME_low)){
    long <- VME_low$long[i]
    lat  <- VME_low$lat[i]
    
    sublong <- c(long-(0.05*0.75),long-(0.05*0.25),long+(0.05*0.25),long+(0.05*0.75))
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05*0.75),lat-(0.05*0.25),lat+(0.05*0.25),lat+(0.05*0.75))
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    
    tt1[tt1$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt2[tt2$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt3[tt3$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt4[tt4$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
  }
  
  ttall <- rbind(tt1,tt2,tt3,tt4)

## get 0.25 c-square grid to combine all data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
  bargrid <- readRDS("Region_0.25_csquare_grid.rds")
  
  bargrid2 <- cbind(bargrid, ttall[match(bargrid@data$uni,ttall$coords), c("csquares")])
  colnames(bargrid2@data)[5] <- "csquares"
  bargrid2 <- subset(bargrid2,!(is.na(bargrid2@data$csquares)))
  
  bargrid2 <- cbind(bargrid2, ttall[match(bargrid2@data$uni,ttall$coords), c("buffer")])
  colnames(bargrid2@data)[6] <- "buffer"
  
  bargrid2 <- cbind(bargrid2, VME_high[match(bargrid2@data$csquares,VME_high$csquares), c("VME_Class")])
  colnames(bargrid2@data)[7] <- "VME"
  
  bargrid2 <- cbind(bargrid2, VME_low[match(bargrid2@data$csquares,VME_low$csquares), c("VME_Class")])
  colnames(bargrid2@data)[8] <- "VME_low"
  
  bargrid2 <- cbind(bargrid2, ttall[match(bargrid2@data$uni,ttall$coords), c("buffer_low")])
  colnames(bargrid2@data)[9] <- "buffer_low"
  
  bargrid2 <- cbind(bargrid2, element_close@data[match(bargrid2@data$csquares,element_close@data$csquares), c("elementclose")])
  colnames(bargrid2@data)[10] <- "elementclose"

## fill holes
  bargrid2@data$summing <- rowSums(bargrid2@data[,c(6,7,8,9,10)],na.rm = T) 
  bargrid2@data$summing[bargrid2@data$summing > 0] <- 1
  newdat <- subset(bargrid2,bargrid2@data$summing == 1)
  tt <- unionSpatialPolygons(newdat,newdat$summing)
  reg <- gUnaryUnion(tt)
  
  # add gbuffer to make sure that filling holes work (see example scenario 1 option 1 filling holes)
  proj4string(reg) <- CRS(proj4string(bargrid)) 
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS(proj4string(bargrid)) )
  #
  
  reg   <- st_as_sf(reg)
  area_thresh <- units::set_units(50, km^2)
  reg_dropped <- fill_holes(reg, threshold = area_thresh)

## and make leaflet map
  clos12 <- as(reg_dropped, 'Spatial')

  # get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthall <- rbind(depthreg,depthreg2)
  depth <- subset(depthall,depthall@data$within ==1)
  
  # add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  VMEdepth <- cbind(depthall, VME[match(depthall@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"
  
  VME_habitat <- subset(VMEdepth,VMEdepth@data$VME_Class == 3)
  VME_high   <- subset(VMEdepth,VMEdepth@data$VME_Class == 2)
  VME_medium <- subset(VMEdepth,VMEdepth@data$VME_Class == 1)
  VME_low   <- subset(VMEdepth,VMEdepth@data$VME_Class == 0)
  
  # load VME elements
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
  
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white")%>%
    addPolygons(data = clos12, group = "Closures",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "yellow") %>%
    addPolygons(data = VME_habitat, group = "VME habitat",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = VME_high, group = "VME index - high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "blue") %>%
    addPolygons(data = VME_medium, group = "VME index - medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#df65b0") %>%
    addPolygons(data = VME_low, group = "VME index - low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#df65b0") %>%
    addPolygons(data = Bank, group = "Bank",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "brown") %>%
    addPolygons(data = Coralmound, group = "Coralmound",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "brown") %>%
    addPolygons(data = Mudvolcano, group = "Mudvolcano",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "brown") %>%
    addPolygons(data = Seamount, group = "Seamount",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "brown") %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Closures",
                        "VME habitat","VME index - high",
                        "VME index - medium","VME index - low","Bank","Coralmound","Mudvolcano","Seamount"),
      options = layersControlOptions(collapsed = FALSE)
    )

# save output
  outdir <- paste(pathdir,"5-Output/Closure options (region)",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Scenario1_option2.html")
  
  outdir <- paste(pathdir,"2-Data processing",sep="/") 
  write_sf(reg_dropped, paste(outdir,"Scenario1_option2.shp",sep="/"))
  
# and clean
  rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit")))