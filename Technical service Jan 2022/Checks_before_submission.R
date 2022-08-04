
# final check  before submission

set_dir <- "C:/Users/danie/Dropbox/Werk/Benthic assessments BENTHIS and ICES/2020 Deep sea/Technical service 2/Technical Service 2 - data correction"

scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
scedat <- c("Footprint_all","Footprint_static","Footprint_mobile")
# closures
outdir <- paste(set_dir,"Fishing footprint/Coordinates shp",sep="/")
scenar1 <- st_read(paste(outdir,paste(scedat[1],"shp", sep="."),sep="/"))
scenar2 <- st_read(paste(outdir,paste(scedat[2],"shp", sep="."),sep="/"))
scenar3 <- st_read(paste(outdir,paste(scedat[3],"shp", sep="."),sep="/"))
scenar4 <- st_read(paste(outdir,paste(scedat[4],"shp", sep="."),sep="/"))

library(leaflet)
library(htmlwidgets)

mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # depth
  addPolygons(data = scenar1, group="scenar1",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
  addPolygons(data = scenar2, group="scenar2",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "orange") %>%
  addPolygons(data = scenar3, group="scenar3",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "purple") %>%
  addPolygons(data = scenar4, group="scenar4",
              stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
  
 # addCircles(data=firstPoints,group="old") %>%
  
  addLayersControl(
    overlayGroups = c("scenar1","scenar2","scenar3","scenar4"),
    options = layersControlOptions(collapsed = FALSE)
  )

mfs