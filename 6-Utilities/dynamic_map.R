
outdir <- paste(pathdir_nogit,"mapdynamic",sep="/") 

# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthall <- rbind(depthreg,depthreg2)
  depth <- subset(depthall,depthall@data$within ==1)

  refyear <- 2009:2011
  afteryear <- 2012:2019
  
# get vms data Celtic Seas
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS("Celtic Seas_vms.rds")

# footprint in reference period
  nam <- c(paste("SAR_total",refyear,sep="_"),paste("Static",refyear, sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$ref <- rowSums(vmsreg[indexcol])
  vmsreg$ref[vmsreg$ref > 0] <- 1
  
  # footprint in period 2012-2019
  nam <- c(paste("SAR_total",afteryear,sep="_"),paste("Static",afteryear, sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$after <- rowSums(vmsreg[indexcol])
  vmsreg$after[vmsreg$after > 0] <- 1
  
  # SAR trawling in ref period
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$refSAR <- rowSums(vmsreg[indexcol])
  vmsreg$refSAR[vmsreg$refSAR > 0] <- 1
  
  # Static in ref period
  nam <- paste("Static",afteryear, sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$refStatic <- rowSums(vmsreg[indexcol])
  vmsreg$refStatic[vmsreg$refStatic > 0] <- 1
  
# get vms data bay of Biscay and the Iberian Coast
  vmsreg2 <- readRDS("Bay of Biscay and the Iberian Coast_vms.rds")
  
  # footprint in reference period
  nam <- c(paste("SAR_total",refyear,sep="_"),paste("Static",refyear, sep="_"))
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$ref <- rowSums(vmsreg2[indexcol])
  vmsreg2$ref[vmsreg2$ref > 0] <- 1
  
  # footprint in period 2012-2019
  nam <- c(paste("SAR_total",afteryear,sep="_"),paste("Static",afteryear, sep="_"))
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$after <- rowSums(vmsreg2[indexcol])
  vmsreg2$after[vmsreg2$after > 0] <- 1
  
  # SAR trawling in ref period
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$refSAR <- rowSums(vmsreg2[indexcol])
  vmsreg2$refSAR[vmsreg2$refSAR > 0] <- 1
  
  # Static in ref period
  nam <- paste("Static",afteryear, sep="_")
  indexcol <- which(names(vmsreg2) %in% nam) 
  vmsreg2$refStatic <- rowSums(vmsreg2[indexcol])
  vmsreg2$refStatic[vmsreg2$refStatic > 0] <- 1
  
  vmsall <- rbind(vmsreg[,c("c_square","lat","lon","ref","after","refSAR","refStatic")],
                  vmsreg2[,c("c_square","lat","lon","ref","after","refSAR","refStatic")])

  fishing_ref <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("ref")])
  colnames(fishing_ref@data)[11] <- c("fishing 2009-2011")
  fishing_ref <- subset(fishing_ref,fishing_ref@data$`fishing 2009-2011` > 0)
  
  fishing_after <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("after")])
  colnames(fishing_after@data)[11] <- c("fishing 2012-2018")
  fishing_after <- subset(fishing_after,fishing_after@data$`fishing 2012-2018` > 0)

  fishing_refSAR <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("refSAR")])
  colnames(fishing_refSAR@data)[11] <- c("MBCG 2009-2011")
  fishing_refSAR <- subset(fishing_refSAR,fishing_refSAR@data$`MBCG 2009-2011` > 0)
  
  fishing_refStatic <- cbind(depth, vmsall[match(depth@data$csquares,vmsall$c_square), c("refStatic")])
  colnames(fishing_refStatic@data)[11] <- c("Static 2009-2011")
  fishing_refStatic <- subset(fishing_refStatic,fishing_refStatic@data$`Static 2009-2011` > 0)
  
# add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME weighted csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  VMEdepth <- cbind(depth, VME[match(depth@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"
  
  VME_low    <- subset(VMEdepth,VMEdepth@data$VME_Class == 3)
  VME_medium <- subset(VMEdepth,VMEdepth@data$VME_Class == 2 | VMEdepth@data$VME_Class == 1)
  VME_high   <- subset(VMEdepth,VMEdepth@data$VME_Class == 0)
  
# add VME elements
 source(paste(pathdir,"6-Utilities/Code_vme_elements.R",sep="/"))

# plot VME elements and VME data

  mele <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white")%>%
    addPolygons(data = Canyons, group = "Canyons",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#8c510a") %>%
    addPolygons(data = ContinentalSlope, group = "ContinentalSlope",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#bf812d") %>%
    addPolygons(data = Escarpments, group = "Escarpments",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#dfc27d") %>%
    addPolygons(data = Flanks, group = "Flanks",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#f6e8c3") %>%
    addPolygons(data = GlacialTroughs, group = "GlacialTroughs",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#c7eae5") %>%
    addPolygons(data = Guyots, group = "Guyots",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#80cdc1") %>%
    addPolygons(data = Ridges, group = "Ridges",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#35978f") %>%
    addPolygons(data = Seamounts, group = "Seamounts",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#35978f") %>%
    addPolygons(data = SteepSlopes_on_Ridges, group = "SteepSlopes_on_Ridges",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "purple") %>%
    addPolygons(data = VME_low, group = "VME - low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = VME_medium, group = "VME - medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "blue") %>%
    addPolygons(data = VME_high, group = "VME - high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#df65b0") %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Canyons", "ContinentalSlope",
                        "Escarpments","Flanks","GlacialTroughs",
                        "Guyots", "Ridges","Seamounts","SteepSlopes_on_Ridges","VME - low precaution",
                        "VME - medium precaution", "VME - high precaution"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white")%>%
    addPolygons(data = fishing_ref, group = "Fishing 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = fishing_after, group = "Fishing 2012-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "blue") %>%
    addPolygons(data = fishing_refSAR, group = "MBCG 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "green") %>%
    addPolygons(data = fishing_refStatic, group = "Static 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "purple") %>%
    addPolygons(data = VME_low, group = "VME - low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "yellow") %>%
    addPolygons(data = VME_medium, group = "VME - medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "orange") %>%
    addPolygons(data = VME_high, group = "VME - high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "black") %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Fishing 2009-2011", "Fishing 2012-2018",
                        "MBCG 2009-2011","Static 2009-2011","VME - low precaution",
                        "VME - medium precaution", "VME - high precaution"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addPolygons(data = depth,group="Depth 400-800 m",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "grey")%>%
    addPolygons(data = fishing_ref, group = "Fishing 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
    addPolygons(data = fishing_after, group = "Fishing 2012-2018",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "blue") %>%
    addPolygons(data = fishing_refSAR, group = "MBCG 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "green") %>%
    addPolygons(data = fishing_refStatic, group = "Static 2009-2011",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "purple") %>%
    addPolygons(data = VME_low, group = "VME - low",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "yellow") %>%
    addPolygons(data = VME_medium, group = "VME - medium",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "orange") %>%
    addPolygons(data = VME_high, group = "VME - high",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "black") %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Depth 400-800 m", "Fishing 2009-2011", "Fishing 2012-2018",
                        "MBCG 2009-2011","Static 2009-2011","VME - low precaution",
                        "VME - medium precaution", "VME - high precaution"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
setwd(outdir)
#saveWidget(mfs, file="map_satelite.html")
saveWidget(m, file="map_map.html")
saveWidget(mele, file="map_vmeElements.html")
