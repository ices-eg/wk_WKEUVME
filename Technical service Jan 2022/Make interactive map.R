
# check new closures
library(leaflet)
library(htmlwidgets)

# Set path
pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"

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

# create new depth intersection
sf_use_s2(FALSE)
new_reg <- st_intersection(reg,EEZ_EUVME)
new_reg <- new_reg %>% st_collection_extract(type="POLYGON")

# load Ecoregion
#shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
#shapeReg  <- subset(shapeEcReg, Ecoregion== "Bay of Biscay and the Iberian Coast" | Ecoregion== "Celtic Seas")

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

# get footprint
Footdir <- paste(pathdir,"Technical service Jan 2022/Output/Fishing footprint update",sep="/")
scedat <- c("Footprint_all","Footprint_mobile","Footprint_static")
sce <- 1
footall <- st_read(paste(Footdir,paste(scedat[sce],"shp",sep="."),sep="/"))
footall <- as_Spatial(footall)

sce <- 2
footmob <- st_read(paste(Footdir,paste(scedat[sce],"shp",sep="."),sep="/"))
footmob <- as_Spatial(footmob)

sce <- 3
footstat <- st_read(paste(Footdir,paste(scedat[sce],"shp",sep="."),sep="/"))
footstat <- as_Spatial(footstat)

# set folder to save leaflet
outdir <-paste(pathdir,"Technical service Jan 2022",sep="/")

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
  
  addPolygons(data = scen11, group="scenario 1 option 1",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
  addPolygons(data = scen12, group="scenario 1 option 2",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "orange") %>%
  addPolygons(data = scen21, group="scenario 2 option 1",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "#bcbddc") %>%
  addPolygons(data = scen22, group="scenario 2 option 2",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "#c7e9c0") %>%

  addPolygons(data = footall, group="Footprint combined",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "#c7e9c0") %>%
  addPolygons(data = footmob, group="Footprint MBCG",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "orange")  %>%
  addPolygons(data = footstat, group="Footprint static",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "purple")  %>%
    
  addLayersControl(
    overlayGroups = c("400-800 depth","EEZ Ireland","EEZ France","EEZ Spain","EEZ Portugal",
                      "scenario 1 option 1","scenario 1 option 2","scenario 2 option 1",
                      "scenario 2 option 2","Footprint combined","Footprint MBCG","Footprint static"),
    options = layersControlOptions(collapsed = FALSE)
  )

# save output
setwd(outdir)
saveWidget(mfs, file="Leaflet EUVME updated.html")

