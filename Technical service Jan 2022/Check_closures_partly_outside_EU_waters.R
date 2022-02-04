
# check in which part of the closure polygon a VME habitat and index is present
# since it are only a few closures, this was done manually

# check new closures
library(leaflet)
library(htmlwidgets)

# Set path
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
reg_sp <- rgeos::gUnaryUnion(reg)
reg   <- st_as_sf(reg_sp)  
reg <-  st_transform(reg, "EPSG:4326")  

# load EEZs
EEZ <- readOGR(dsn = paste(pathdir, "1-Input data/EEZ_land_union_v3_202003",sep="/"), layer = "EEZ_Land_v3_202030")
EEZ_EUVME_sp <- subset(EEZ,EEZ@data$UNION %in% c("Ireland", "France","Portugal","Spain"))
EEZ_EUVME <- raster::aggregate(EEZ_EUVME_sp)
EEZ_EUVME   <- st_as_sf(EEZ_EUVME)
EEZ_Ireland <- subset(EEZ,EEZ@data$UNION %in% c("Ireland"))
EEZ_France <- subset(EEZ,EEZ@data$UNION %in% c("France"))
EEZ_Portugal <- subset(EEZ,EEZ@data$UNION %in% c("Portugal"))
EEZ_Spain <- subset(EEZ,EEZ@data$UNION %in% c("Spain"))
EEZ_UK <-  subset(EEZ,EEZ@data$UNION %in% c("United Kingdom"))
EEZ_UK <- raster::aggregate(EEZ_UK)
EEZ_UK   <- st_as_sf(EEZ_UK)

# create new depth intersection
sf_use_s2(FALSE)
new_reg <- st_intersection(reg,EEZ_EUVME)
new_reg <- new_reg %>% st_collection_extract(type="POLYGON")

# load Ecoregion
shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeReg  <- subset(shapeEcReg, Ecoregion== "Western Mediterranean Sea")
shapeReg   <- st_as_sf(shapeReg)
EEZ_France   <- st_as_sf(EEZ_France)
EEZ_France2 <- st_difference(EEZ_France,shapeReg)
EEZ_France <- as_Spatial(EEZ_France2)
EEZ_Spain   <- st_as_sf(EEZ_Spain)
EEZ_Spain2 <- st_difference(EEZ_Spain,shapeReg) 
EEZ_Spain <- as_Spatial(EEZ_Spain2)

# get closures
closedir <- paste(pathdir,"Technical service Jan 2022/Output/VME update",sep="/")
scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
sce <- 1
scen11 <- st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
scen11 <- as_Spatial(scen11)

sce <- 2
scen12 <- st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
scen12 <- as_Spatial(scen12)

sce <- 3
scen21 <- st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
scen21 <- as_Spatial(scen21)

sce <- 4
scen22 <- st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
scen22 <- as_Spatial(scen22)

# get VME data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
grid_csquare <- readRDS("Region_csquare_grid.rds")

# add VMEs
VME <- read.csv(paste(pathdir_nogit,
                      "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
VME <- VME[,-1]
VMEdepth <- cbind(grid_csquare, VME[match(grid_csquare@data$csquares,VME$CSquare), c("VME_Class")])
colnames(VMEdepth@data)[ncol(VMEdepth)] <- "VME_Class"

VMEhabitat <- subset(VMEdepth,VMEdepth@data$VME_Class ==3)
VMEindexH <- subset(VMEdepth,VMEdepth@data$VME_Class ==2)
VMEindexM <- subset(VMEdepth,VMEdepth@data$VME_Class ==1)
VMEindexL <- subset(VMEdepth,VMEdepth@data$VME_Class ==0)

## load 
scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
closid_Scenario1_option1 <- c(1:41,43:75,77:82)
closid_Scenario1_option2 <- c(1:39,41:73,75:81)
closid_Scenario2_option1 <- c(1:49,51:86,88:92,94)
closid_Scenario2_option2 <- c(1:83,123:127)

scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[1],"shp",sep="."),sep="/"))
scenar <- st_cast(scenar,"POLYGON")
scenar <- st_make_valid(scenar)
overpol <- sf::st_intersects(scenar,reg)
overp   <- as.data.frame(overpol)
sce_int <- scenar[overp$row.id,] 
sce_int <- sce_int[!duplicated(sce_int$geometry), ]
new_clos <- st_intersects(sce_int,EEZ_EUVME)
new_clos   <- as.data.frame(new_clos)
sce_int <- sce_int[new_clos$row.id,] 
n11 <- sce_int[which(closid_Scenario1_option1 %in% c(41, 43, 77, 80, 82)),]

scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[2],"shp",sep="."),sep="/"))
scenar <- st_cast(scenar,"POLYGON")
scenar <- st_make_valid(scenar)
overpol <- sf::st_intersects(scenar,reg)
overp   <- as.data.frame(overpol)
sce_int <- scenar[overp$row.id,] 
sce_int <- sce_int[!duplicated(sce_int$geometry), ]
new_clos <- st_intersects(sce_int,EEZ_EUVME)
new_clos   <- as.data.frame(new_clos)
sce_int <- sce_int[new_clos$row.id,] 
n12 <- sce_int[which(closid_Scenario1_option2 %in% c(39,41,76,79,81)),]

scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[3],"shp",sep="."),sep="/"))
scenar <- st_cast(scenar,"POLYGON")
scenar <- st_make_valid(scenar)
overpol <- sf::st_intersects(scenar,reg)
overp   <- as.data.frame(overpol)
sce_int <- scenar[overp$row.id,] 
sce_int <- sce_int[!duplicated(sce_int$geometry), ]
new_clos <- st_intersects(sce_int,EEZ_EUVME)
new_clos   <- as.data.frame(new_clos)
sce_int <- sce_int[new_clos$row.id,] 
n21 <- sce_int[which(closid_Scenario2_option1 %in% c(49,51,88,91,94)),]

scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[4],"shp",sep="."),sep="/"))
scenar <- st_cast(scenar,"POLYGON")
scenar <- st_make_valid(scenar)
overpol <- sf::st_intersects(scenar,reg)
overp   <- as.data.frame(overpol)
sce_int <- scenar[overp$row.id,] 
sce_int <- sce_int[!duplicated(sce_int$geometry), ]
new_clos <- st_intersects(sce_int,EEZ_EUVME)
new_clos   <- as.data.frame(new_clos)
sce_int <- sce_int[new_clos$row.id,] 
n22 <- sce_int[which(closid_Scenario2_option2 %in% c(48,79,83,123)),]

mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # depth
  addPolygons(data = new_reg, group="400-800 depth",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
  
  # ecoregion boundaries
  #addPolygons(data = shapeReg, group="Ecoregion boundaries",
  #            stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "grey") %>%
  
  # EEZ boundaries
  addPolygons(data = EEZ_Ireland, group="EEZ Ireland",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = EEZ_France, group="EEZ France",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = EEZ_Spain, group="EEZ Spain",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = EEZ_Portugal, group="EEZ Portugal",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = EEZ_UK, group="EEZ UK",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  
  #addPolygons(data = scen11, group="scenario 1 option 1",
  #            stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
  #addPolygons(data = scen12, group="scenario 1 option 2",
  #            stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "orange") %>%
  #addPolygons(data = scen21, group="scenario 2 option 1",
  #            stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "#bcbddc") %>%
  #addPolygons(data = scen22, group="scenario 2 option 2",
  #            stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "#c7e9c0") %>%
  
  addPolygons(data = VMEhabitat, group="VME Habitat",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#cc00ff") %>%
  addPolygons(data = VMEindexH, group="VME Index High",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "red")  %>%
  addPolygons(data = VMEindexM, group="VME Index Medium",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "orange")  %>%
  addPolygons(data = VMEindexL, group="VME Index Low",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "yellow")  %>%
  
  addPolygons(data = n11, group="Sce1 - Opt1",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "green") %>%
  addPolygons(data = n12, group="Sce1 - Opt2",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "orange") %>%
  addPolygons(data = n21, group="Sce2 - Opt1",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "red") %>%
  addPolygons(data = n22, group="Sce2 - Opt2",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "purple") %>%
  
  addLayersControl(
    overlayGroups = c("400-800 depth","EEZ Ireland","EEZ France","EEZ Spain","EEZ Portugal","EEZ UK"
                      ,"VME Habitat","VME Index High","VME Index Medium","VME Index Low","Sce1 - Opt1",
                      "Sce1 - Opt2","Sce2 - Opt1","Sce2 - Opt2"),
    options = layersControlOptions(collapsed = FALSE)
  )

#"scenario 1 option 1","scenario 1 option 2","scenario 2 option 1",
#"scenario 2 option 2"
