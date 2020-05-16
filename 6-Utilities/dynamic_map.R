
outdir <- paste(pathdir,"7-mapdynamic",sep="/") 

# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthall <- rbind(depthreg,depthreg2)
  depth <- subset(depthall,depthall@data$within ==1)

# get vms data Celtic Seas
  setwd(paste(pathdir,"2-Data processing",sep="/"))
  vmsreg <- readRDS("Celtic Seas_vms.rds")

# SAR_total in reference period
  refyear <- 2009:2011
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$ref <- rowMeans(vmsreg[indexcol],na.rm=T)

  # SAR_total in period 2012-2018
  afteryear <- 2012:2018
  nam <- paste("SAR_total",afteryear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$after <- rowMeans(vmsreg[indexcol],na.rm=T)  
  vmsreg <- vmsreg[,c("c_square","ref","after")]
  
# get vms data bay of Biscay and the Iberian Coast
  vmsreg2 <- readRDS("Bay of Biscay and the Iberian Coast_vms.rds")
  
  # SAR_total in reference period
  refyear <- 2009:2011
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$ref <- rowMeans(vmsreg2[indexcol],na.rm=T)
  
  # SAR_total in period 2012-2018
  afteryear <- 2012:2018
  nam <- paste("SAR_total",afteryear,sep="_")
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$after <- rowMeans(vmsreg2[indexcol],na.rm=T)  
  vmsreg2 <- vmsreg2[,c("c_square","ref","after")]
  
  vmsall <- rbind(vmsreg,vmsreg2)

  fishing_ref <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("ref")])
  colnames(fishing_ref@data)[11] <- c("fishing 2009-2011")
  fishing_ref <- subset(fishing_ref,fishing_ref@data$`fishing 2009-2011` > 0)
  
  fishing_after <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("after")])
  colnames(fishing_after@data)[11] <- c("fishing 2012-2018")
  fishing_after <- subset(fishing_after,fishing_after@data$`fishing 2012-2018` > 0)

# add VMEs
  VME <- st_read(paste(pathdir,"1-Input data/VMEWeighting/web_GIS_VMEWeightingAlgorithmPolygon.shp",sep="/"))
  VME <- as.data.frame(VME)
  VMEdepth <- cbind(depth, VME[match(depth@data$csquares,VME$C_Square), c("VME_Class")])
  colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"
  
  VME_low    <- subset(VMEdepth,VMEdepth@data$VME_Class == 3)
  VME_medium <- subset(VMEdepth,VMEdepth@data$VME_Class == 2 | VMEdepth@data$VME_Class == 1)
  VME_high   <- subset(VMEdepth,VMEdepth@data$VME_Class == 0)
  
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = depth,group="Depth 400-800 m",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5)%>%
  addPolygons(data = fishing_ref, group = "Fishing 2009-2011",
            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
  addPolygons(data = fishing_after, group = "Fishing 2012-2018",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "grey") %>%
  addPolygons(data = VME_low, group = "VME - low",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "yellow") %>%
  addPolygons(data = VME_medium, group = "VME - medium",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "orange") %>%
  addPolygons(data = VME_high, group = "VME - high",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "black") %>%
  
  # Layers control
  addLayersControl(
    overlayGroups = c("Depth 400-800 m", "Fishing 2009-2011", "Fishing 2012-2018","VME - low",
                      "VME - medium", "VME - high"),
    options = layersControlOptions(collapsed = FALSE)
  )

setwd(paste(pathdir,"7-mapdynamic",sep="/"))
saveWidget(m, file="m.html")
