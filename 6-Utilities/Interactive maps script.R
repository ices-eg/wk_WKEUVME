
# create interactive map

# depth # get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthall <- rbind(depthreg,depthreg2)
  depth <- subset(depthall,depthall@data$within ==1)
  
  depthsize <- unionSpatialPolygons(depth,depth$within)
  depthsize <- gUnaryUnion(depthsize)
  
# get shapefile ecoregion
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== "Celtic Seas" | Ecoregion== "Bay of Biscay and the Iberian Coast")

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
  
# get VME data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
  grid_csquare <- readRDS("Region_csquare_grid.rds")
  
# add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  VMEdepth <- cbind(VME, depthall@data[match(VME$CSquare,depthall@data$csquares), c("mean_depth_emodnet")])
  colnames(VMEdepth)[ncol(VMEdepth)] <- "depth"
  VMEdepth <- subset(VMEdepth, !(is.na(VMEdepth$depth)))
  VMEdepth <- cbind(depthall, VMEdepth[match(depthall@data$csquares,VMEdepth$CSquare), c("VME_Class")])
  colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"
  
  VMEhabitat <- subset(VMEdepth,VMEdepth@data$VME_Class ==3)
  VMEindexH <- subset(VMEdepth,VMEdepth@data$VME_Class ==2)
  VMEindexM <- subset(VMEdepth,VMEdepth@data$VME_Class ==1)
  VMEindexL <- subset(VMEdepth,VMEdepth@data$VME_Class ==0)

# add VME physical elements within the ecoregions
  depth_elements <- depthall
  depth_elements$inside <- 1
  
  reg <- unionSpatialPolygons(depth_elements,depth_elements$inside)
  reg <- gUnaryUnion(reg)
  reg   <- st_as_sf(reg)
  
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  overpol <- sf::st_intersection(Bank,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  Bank       <- as(Bank, 'Spatial')
  Bank@data$rown <- rownames(Bank@data)
  Bank <- subset(Bank, Bank@data$rown %in% c(overpol))

  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  overpol <- sf::st_intersection(Coralmound,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  Coralmound       <- as(Coralmound, 'Spatial')
  Coralmound@data$rown <- rownames(Coralmound@data)
  Coralmound <- subset(Coralmound, Coralmound@data$rown %in% c(overpol))

  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  overpol <- sf::st_intersection(Mudvolcano,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  Mudvolcano       <- as(Mudvolcano, 'Spatial')
  Mudvolcano@data$rown <- rownames(Mudvolcano@data)
  Mudvolcano <- subset(Mudvolcano, Mudvolcano@data$rown %in% c(overpol))
  
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
  overpol <- sf::st_intersection(Seamount,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  Seamount       <- as(Seamount, 'Spatial')
  Seamount@data$rown <- rownames(Seamount@data)
  Seamount <- subset(Seamount, Seamount@data$rown %in% c(overpol))

  VMEelements <- rbind(Bank,Coralmound,Mudvolcano,Seamount, makeUniqueIDs = TRUE)

# closures that intersect with 400-800 meter boundary
  scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  scedat_within <- c("Scenario1_option1_within","Scenario1_option2_within","Scenario2_option1_within","Scenario2_option2_within")
  
  for (p in 1:4){
    reg <- unionSpatialPolygons(depth,depth$within)
    reg <- gUnaryUnion(reg)
    reg   <- st_as_sf(reg)
    
    scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[p],"shp",sep="."),sep="/"))
    scenar <- st_cast(scenar,"POLYGON")
    
    # find all polygons that intersect
    overpol <- sf::st_intersection(scenar,reg)
    overpol <- as(overpol, 'Spatial')
    assign(scedat_within[p],overpol)
    overpol <- rownames(overpol@data)

    scea <- as(scenar, 'Spatial')
    scea@data$rown <- rownames(scea@data)
    scea <- subset(scea, scea@data$rown %in% c(overpol))
    assign(scedat[p],scea)
  }
  
  
# get SAR and 10/90 percentile
  EcoReg <- "Bay of Biscay and the Iberian Coast"
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))
  
  # define few params
  #refyear <- 2009:2011

  #nam <- paste("SAR_Otter",refyear,sep="_")
  #indexcol <- which(names(vmsreg) %in% nam) 
  #vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
  #sardat <- cbind(depth, vmsreg[match(depth$csquares,vmsreg$c_square), c("otrefyear")])
  #colnames(sardat@data)[ncol(sardat)] <- "Otter_intensity"
  #sardat@data$Otter_intensity[is.na(sardat@data$Otter_intensity)] <- 0
  #sardat <- subset(sardat,sardat@data$Otter_intensity > 0) # & fig8$adjacent.cells > 0)
  #sardat <- sardat[order(-sardat@data$Otter_intensity),]
  #sardat@data$perc <- cumsum(sardat@data$Otter_intensity) / sum(sardat@data$Otter_intensity)*100
  
  #cats <- c(0, 90,  100)
  #sardat@data$cats <- cut(sardat@data$perc,c(cats))
  #refsar10 <- subset(sardat,sardat@data$cats == "(0,90]")
  #refsar90 <- subset(sardat,sardat@data$cats == "(90,100]")
  
  # define few params
  #refyear <- 2012:2015
  
  #nam <- paste("SAR_Otter",refyear,sep="_")
  #indexcol <- which(names(vmsreg) %in% nam) 
  #vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
  #sardat <- cbind(depth, vmsreg[match(depth$csquares,vmsreg$c_square), c("otrefyear")])
  #colnames(sardat@data)[ncol(sardat)] <- "Otter_intensity"
  #sardat@data$Otter_intensity[is.na(sardat@data$Otter_intensity)] <- 0
  #sardat <- subset(sardat,sardat@data$Otter_intensity > 0) # & fig8$adjacent.cells > 0)
  #sardat <- sardat[order(-sardat@data$Otter_intensity),]
  #sardat@data$perc <- cumsum(sardat@data$Otter_intensity) / sum(sardat@data$Otter_intensity)*100
  
  #cats <- c(0, 90,  100)
  #sardat@data$cats <- cut(sardat@data$perc,c(cats))
  #aftersar10 <- subset(sardat,sardat@data$cats == "(0,90]")
  #aftersar90 <- subset(sardat,sardat@data$cats == "(90,100]")
  
  # define few params
  refyear <- 2015:2018
  
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
  sardat <- cbind(depth, vmsreg[match(depth$csquares,vmsreg$c_square), c("otrefyear")])
  colnames(sardat@data)[ncol(sardat)] <- "SAR_intensity"
  sardat@data$SAR_intensity[is.na(sardat@data$SAR_intensity)] <- 0
  sardat <- subset(sardat,sardat@data$SAR_intensity > 0) # & fig8$adjacent.cells > 0)
  sardat <- sardat[order(-sardat@data$SAR_intensity),]
  sardat@data$perc <- cumsum(sardat@data$SAR_intensity) / sum(sardat@data$SAR_intensity)*100
  
  cats <- c(0, 90,  100)
  sardat@data$cats <- cut(sardat@data$perc,c(cats))
  after2sar10 <- subset(sardat,sardat@data$cats == "(0,90]")
  after2sar90 <- subset(sardat,sardat@data$cats == "(90,100]")
  
  mean2sar_low <- subset(sardat,sardat@data$SAR_intensity < 0.43)
  mean2sar_int1 <- subset(sardat,sardat@data$SAR_intensity >= 0.43 & sardat@data$SAR_intensity < 1)
  mean2sar_int2 <- subset(sardat,sardat@data$SAR_intensity >= 1 & sardat@data$SAR_intensity < 3)
  mean2sar_high <- subset(sardat,sardat@data$SAR_intensity >= 3)
  
  EcoReg <- "Celtic Seas"
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))
  # define few params
  refyear <- 2015:2018
  
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
  sardat <- cbind(depth, vmsreg[match(depth$csquares,vmsreg$c_square), c("otrefyear")])
  colnames(sardat@data)[ncol(sardat)] <- "SAR_intensity"
  sardat@data$SAR_intensity[is.na(sardat@data$SAR_intensity)] <- 0
  sardat <- subset(sardat,sardat@data$SAR_intensity > 0) # & fig8$adjacent.cells > 0)
  sardat <- sardat[order(-sardat@data$SAR_intensity),]
  sardat@data$perc <- cumsum(sardat@data$SAR_intensity) / sum(sardat@data$SAR_intensity)*100
  
  cats <- c(0, 90,  100)
  sardat@data$cats <- cut(sardat@data$perc,c(cats))
  after2sar102 <- subset(sardat,sardat@data$cats == "(0,90]")
  after2sar902 <- subset(sardat,sardat@data$cats == "(90,100]")
  
  mean2sar_low2 <- subset(sardat,sardat@data$SAR_intensity < 0.43)
  mean2sar_int12 <- subset(sardat,sardat@data$SAR_intensity >= 0.43 & sardat@data$SAR_intensity < 1)
  mean2sar_int22 <- subset(sardat,sardat@data$SAR_intensity >= 1 & sardat@data$SAR_intensity < 3)
  mean2sar_high2 <- subset(sardat,sardat@data$SAR_intensity >= 3)
  
  aftersar10 <- rbind(after2sar10,after2sar102)
  aftersar90 <- rbind(after2sar90,after2sar902)
  mean2sar_low2 <- rbind(mean2sar_low,mean2sar_low2)
  mean2sar_int12 <- rbind(mean2sar_int1,mean2sar_int12)
  mean2sar_int22 <- rbind(mean2sar_int2,mean2sar_int22)
  mean2sar_high2 <- rbind(mean2sar_high,mean2sar_high2)
  
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    
    # depth
    addPolygons(data = depthsize, group="Depth 400-800 m",
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    
    # ecoregion boundaries
    addPolygons(data = shapeReg, group="Ecoregion boundaries",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    
    # fishing footprint
    addPolygons(data = Footprint_all, group = "Footprint combined 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#c7e9c0") %>%
    addPolygons(data = Footprint_static, group = "Footprint static 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "purple") %>%
    addPolygons(data = Footprint_mobile, group = "Footprint MBCG 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "orange") %>%
    
    # VME data
    addPolygons(data = VMEhabitat, group = "VME habitat",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#cc00ff") %>%
    addPolygons(data = VMEindexH, group = "VME index high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = VMEindexM, group = "VME index medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "orange") %>%
    addPolygons(data = VMEindexL, group = "VME index low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "yellow") %>%
    addPolygons(data = VMEelements, group = "VME physical elements",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "brown") %>%
    
    # closures
    addPolygons(data = Scenario1_option1, group = "Closure 1_1",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#fd8d3c") %>%
    addPolygons(data = Scenario1_option2, group = "Closure 1_2",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#feb24c") %>%
    addPolygons(data = Scenario2_option1, group = "Closure 2_1",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#fed976") %>%
    addPolygons(data = Scenario2_option2, group = "Closure 2_2",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#ffeda0") %>%
    
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Ecoregion boundaries","Footprint combined 2009-2011", "Footprint static 2009-2011",
                        "Footprint MBCG 2009-2011", "VME habitat","VME index high", "VME index medium","VME index low",
                        "VME physical elements","Closure 1_1","Closure 1_2","Closure 2_1","Closure 2_2"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # save output
  outdir <- paste(pathdir,"5-Output",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Interactive maps.html")
  

  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    
    # depth
    addPolygons(data = depthsize, group="Depth 400-800 m",
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    
    # ecoregion boundaries
    addPolygons(data = shapeReg, group="Ecoregion boundaries",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    
    # fishing footprint
    addPolygons(data = Footprint_mobile, group = "Footprint MBCG 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#74c476") %>%
    
    # closures
    addPolygons(data = Scenario1_option1_within, group = "Closure 1_1 within 400-800m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#fd8d3c") %>%
    addPolygons(data = Scenario1_option2_within, group = "Closure 1_2 within 400-800m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#feb24c") %>%
    addPolygons(data = Scenario2_option1_within, group = "Closure 2_1 within 400-800m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#fed976") %>%
    addPolygons(data = Scenario2_option2_within, group = "Closure 2_2 within 400-800m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#ffeda0") %>%
    
    # 10-90 percentile
    addPolygons(data = aftersar10, group = "MBCG 10-100% core fishing area 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = aftersar90, group = "MBCG 0-10% 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "orange") %>%

    # mean SAR 
    addPolygons(data = mean2sar_low2, group = "Average SAR 0-0.43 in 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#feebe2") %>%
    addPolygons(data = mean2sar_int12, group = "Average SAR 0.43-1 in 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#fbb4b9") %>%
    addPolygons(data = mean2sar_int22, group = "Average SAR 1-3 in 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#f768a1") %>%
    addPolygons(data = mean2sar_high2, group = "Average SAR >3 in 2015-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#ae017e") %>% 
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Ecoregion boundaries","Footprint MBCG 2009-2011",
                        "Closure 1_1 within 400-800m","Closure 1_2 within 400-800m",
                        "Closure 2_1 within 400-800m","Closure 2_2 within 400-800m",
                        "MBCG 10-100% core fishing area 2015-2018","MBCG 0-10% 2015-2018",
                        "Average SAR 0-0.43 in 2015-2018","Average SAR 0.43-1 in 2015-2018",
                        "Average SAR 1-3 in 2015-2018","Average SAR >3 in 2015-2018"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # save output
  outdir <- paste(pathdir,"5-Output",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Interactive map MBCG.html")
  