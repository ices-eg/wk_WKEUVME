
# goal is to verify that apart from selecting the EEZ's of Ireland, France, Portugal and Spain
# we have identical closures
# 

# check new closures
library(leaflet)
library(htmlwidgets)

# Set path
pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"
outdir <- paste(pathdir,"Technical service Dec 2021/Output",sep="/")

scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
sce <- 4
scenar <- st_read(paste(outdir,paste(scedat[sce],"shp",sep="."),sep="/"))
scenar <- as_Spatial(scenar)

oldcoords <- paste(pathdir,"5-Output/Closure options (region)",sep="/")
old <- read.csv(paste(oldcoords,paste(scedat[sce],"coords.csv",sep="_"),sep="/"),sep=",")

firstPoints <- SpatialPoints(coords = cbind(old$Longitude,old$Latitude))
#points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)


mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # depth
  addPolygons(data = scenar, group="scenar",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
  addCircles(data=firstPoints,group="old") %>%
  
  addLayersControl(
    overlayGroups = c("scenar","old"),
    options = layersControlOptions(collapsed = FALSE)
  )

mfs