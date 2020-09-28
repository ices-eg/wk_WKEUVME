
### scenario 2 -- option 2

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
  
# now check with low index VME index cells are unfished
  refyear <- 2009:2018

  # get vms data Celtic Seas
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS("Celtic Seas_vms.rds")
  nam <- c(paste("SAR_total",refyear,sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$SAR <- rowMeans(vmsreg[indexcol],na.rm=T)

  # get vms data bay of Biscay and the Iberian Coast
  vmsreg2 <- readRDS("Bay of Biscay and the Iberian Coast_vms.rds")
  nam <- c(paste("SAR_total",refyear,sep="_"))
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$SAR <- rowMeans(vmsreg2[indexcol],na.rm=T)

  # combine
  vmsall <- rbind(vmsreg[,c("c_square","lat","lon","SAR")],
                  vmsreg2[,c("c_square","lat","lon","SAR")])
  
  VMEdepth <- cbind(VMEdepth, vmsall[match(VMEdepth$csquares,vmsall$c_square), c("SAR")])
  colnames(VMEdepth)[ncol(VMEdepth)] <- "SAR" 
  VMEdepth$SAR[is.na(VMEdepth$SAR)] <- 0
  VMEdepth$VME_Class[VMEdepth$SAR < 0.43] <- 5 # low SAR
  
  VME_high <- VMEdepth[VMEdepth$VME_Class %in% c(5),]
  VME_high$long <- round(VME_high$long, digits = 3)
  VME_high$lat <- round(VME_high$lat, digits = 3)
  
# select all VME low index that are adjacent/joining
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

## fill holes
  bargrid2@data$summing <- rowSums(bargrid2@data[,c(6,7)],na.rm = T) 
  bargrid2@data$summing[bargrid2@data$summing > 0] <- 1
  newdat <- subset(bargrid2,bargrid2@data$summing == 1)
  tt <- unionSpatialPolygons(newdat,newdat$summing)
  reg <- gUnaryUnion(tt)
  reg   <- st_as_sf(reg)
  area_thresh <- units::set_units(50, km^2)
  reg_dropped <- fill_holes(reg, threshold = area_thresh)
  
## and make leaflet map
  clos22 <- as(reg_dropped, 'Spatial')

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
  
  # get vms data Celtic Seas
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS("Celtic Seas_vms.rds")
  nam <- c(paste("SAR_total",refyear,sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$SAR <- rowMeans(vmsreg[indexcol],na.rm=T)
  
  # get vms data bay of Biscay and the Iberian Coast
  vmsreg2 <- readRDS("Bay of Biscay and the Iberian Coast_vms.rds")
  nam <- c(paste("SAR_total",refyear,sep="_"))
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$SAR <- rowMeans(vmsreg2[indexcol],na.rm=T)
  
  # combine
  vmsall <- rbind(vmsreg[,c("c_square","lat","lon","SAR")],
                  vmsreg2[,c("c_square","lat","lon","SAR")])
  
  fishingVME <- cbind(VMEdepth, vmsall[match(VMEdepth$csquares,vmsall$c_square), c("SAR")])
  colnames(fishingVME@data)[ncol(fishingVME@data)] <- "SAR"
  lowfish <- subset(fishingVME,fishingVME@data$SAR < 0.43 & !(is.na(fishingVME@data$VME_Class)))
  highfish <- subset(fishingVME,fishingVME@data$SAR >= 0.43 & !(is.na(fishingVME@data$VME_Class)))
        
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white")%>%
    addPolygons(data = clos22, group = "Closures",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = lowfish, group = "VME fishing <0.43",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =   "green") %>%
    addPolygons(data = highfish, group = "VME fishing >= 0.43",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =   "purple") %>%
   
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Closures",
                        "VME fishing <0.43", "VME fishing >= 0.43"),
      options = layersControlOptions(collapsed = FALSE)
    )
 
# save output 
  outdir <- paste(pathdir,"5-Output/Closure options (region)",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Scenario2_option2.html")
  
  outdir <- paste(pathdir,"2-Data processing",sep="/") 
  write_sf(reg_dropped, paste(outdir,"Scenario2_option2.shp",sep="/"))
  
# and clean
  rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit")))