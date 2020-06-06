
### scenario 1 -- option 1

  setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
  grid_csquare <- readRDS("Region_csquare_grid.rds")
  
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

# add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
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

## fill holes
  bargrid2@data$summing <- rowSums(bargrid2@data[,c(6,7,8,9)],na.rm = T) 
  bargrid2@data$summing[bargrid2@data$summing > 0] <- 1
  newdat <- subset(bargrid2,bargrid2@data$summing == 1)
  tt <- unionSpatialPolygons(newdat,newdat$summing)
  reg <- gUnaryUnion(tt)
  
  # add gbuffer to make sure that filling holes works
  proj4string(reg) <- CRS(proj4string(bargrid)) 
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS(proj4string(bargrid)) )
  #
  
  reg   <- st_as_sf(reg)
  area_thresh <- units::set_units(50, km^2)
  reg_dropped <- fill_holes(reg, threshold = area_thresh)

## and make leaflet map
  clos11 <- as(reg_dropped, 'Spatial')

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
  
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white")%>%
    addPolygons(data = clos11, group = "Closures",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = VME_habitat, group = "VME habitat",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = VME_high, group = "VME index - high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "blue") %>%
    addPolygons(data = VME_medium, group = "VME index - medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#df65b0") %>%
    addPolygons(data = VME_low, group = "VME index - low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#df65b0") %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Closures",
                        "VME habitat","VME index - high",
                        "VME index - medium","VME index - low"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
# save output
  outdir <- paste(pathdir,"5-Output/Closure options (region)",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Scenario1_option1.html")
  
  outdir <- paste(pathdir,"2-Data processing",sep="/") 
  write_sf(reg_dropped, paste(outdir,"Scenario1_option1.shp",sep="/"))
  
# and clean
rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit")))