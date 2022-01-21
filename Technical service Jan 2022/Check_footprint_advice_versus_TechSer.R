
# goal is to verify that apart from selecting the EEZ's of Ireland, France, Portugal and Spain
# we have identical footprints
# 

# check new closures
library(leaflet)
library(htmlwidgets)

# Set path
pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"
outdir <- paste(pathdir,"Technical service Jan 2022/Output/Fishing footprint update",sep="/")

scedat <- c("Footprint_all","Footprint_static","Footprint_mobile")
sce <- 1
new <- read.csv(paste(outdir,paste(scedat[sce],"coords.csv",sep="_"),sep="/"),sep=",")

oldcoords <- paste(pathdir,"5-Output/Footprint (region)",sep="/")
old <- read.csv(paste(oldcoords,paste(scedat[sce],"coords.csv",sep="_"),sep="/"),sep=",")

points <-  SpatialPoints(coords = cbind(new$Longitude,new$Latitude))

firstPoints <- SpatialPoints(coords = cbind(old$Longitude,old$Latitude))
#points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)


mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # depth
  addCircles(data=points,group="new") %>%
  addCircles(data=firstPoints,group="old",col="red") %>%
  
  addLayersControl(
    overlayGroups = c("new","old"),
    options = layersControlOptions(collapsed = FALSE)
  )

mfs
