
## script to estimate depth per c-square 

# data obtained from EMODnet Bathymetry Consortium (2018). 
# EMODnet Digital Bathymetry (DTM 2018). EMODnet Bathymetry Consortium. 
# https://doi.org/10.12770/18ff0d48-b203-4a65-94a9-5fd8b0ec35f6. 
# Data downloaded April 2020. 


  library(raster)
  
# set path
  pathdir <- "C:/Users/pdvd/Online for git/WKEUVME"
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
  
# Tile names from EMODNET 
  boxname <- c("C5","C4","C3","D2","D3","D4","D5","E3","E2","F2","F3","F4","G3","E4")

#  get all c-squares for the three ecoregions 
  load("Bay of Biscay and the Iberian Coast_Region.RData")
  BB <- Bbic
  load("Greater North Sea_region.RData")
  NS <- Northsea
  load("Celtic Seas_region.RData")
  CS <- Celtic

# get the coordinates of the c-squares
  NScoord <- coordinates(NS)
  NScoord <- data.frame(NScoord)
  NScoord$csquare <- NS@data$csquares
  CScoord <- coordinates(CS)
  CScoord <- data.frame(CScoord)
  CScoord$csquare <- CS@data$csquares
  BBcoord <- coordinates(BB)
  BBcoord <- data.frame(BBcoord)
  BBcoord$csquare <- BB@data$csquares
  
# and combine all 
  allreg <- rbind(NScoord,CScoord,BBcoord)
  colnames(allreg) <- c("long","lat","csquares")

# now open each tile 
# and select the data that falls within the range of the three ecoregions 
  
  for (box in 1: length(boxname)){
    tt <- raster(unzip(paste("C:/Users/pdvd/Dropbox/Emodnet Data/",boxname[box],"_2018.msl.zip",sep="")))
    mat <- raster::as.matrix(tt)
    coords <- coordinates(tt)
    long_depth <- unique(coords[,1])
    lat_depth <-  unique(coords[,2])
    newreg <- subset(allreg,allreg$long > (tt@extent[1]-0.1) & allreg$long < (tt@extent[2]+0.1))
    newreg <- subset(newreg,newreg$lat > (tt@extent[3]-0.1) & newreg$lat < (tt@extent[4]+0.1))
  
    # get for each c-square the long x lat that falls within the c-square (this seems quicker than sp::overlay, sf::st_join) 
    depth_data <- as.data.frame(matrix(data=NA,nrow=nrow(newreg),ncol=2500))
    depth_data[,1] <- newreg$csquares
  
    for (idx in 1:nrow(newreg)){
      pointLat <- newreg$lat[idx]
      Lat_min <- pointLat - 0.025
      Lat_min <- round(Lat_min,digits=5)
      Lat_max <- pointLat + 0.025
      Lat_max <- round(Lat_max,digits=5)
      
      get_lat <- which(lat_depth >= Lat_min & lat_depth <= Lat_max)
      
      pointLong <- newreg$long[idx]
      Long_min <- pointLong - 0.025
      Long_min <- round(Long_min,digits=5)
      Long_max <- pointLong + 0.025
      Long_max <- round(Long_max,digits=5)
      
      get_long <- which(long_depth >= Long_min & long_depth <= Long_max)
      
      # merge all long and lat that fall within the range
      dat <- merge(get_lat,get_long)
      depths <-  apply(dat, 1, function(x, output) {mat[x[1],x[2]]} ) 
      
      if (length(depths) > 0){
        depth_data[idx,2:(length(depths)+1)] <- depths
      }
    }
    saveRDS(depth_data,  paste(boxname[box],"depth.rds",sep="_"))
  
  }





