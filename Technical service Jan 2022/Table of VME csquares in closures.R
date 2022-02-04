
# plot closures and analyse effect of closures on fisheries

# get depth data
# Get depth data 400 - 800 m EUVME
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS("Celtic Seas_depth.rds")
depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
depthreg3 <- readRDS("Tip of portugal_depth.rds")
depthreg3 <- subset(depthreg3,is.na(depthreg3@data$Ecoregion) & !(is.na(depthreg3@data$mean_depth_emodnet)))
depthall <- rbind(depthreg,depthreg2)
depthreg3 <- spTransform(depthreg3,CRS(proj4string(depthall))) 
depthall <- spTransform(depthall,CRS(proj4string(depthreg3))) 
depthreg <- rbind(depthall,depthreg3)

depth <- subset(depthall,depthall@data$within ==1)
reg <- unionSpatialPolygons(depth,depth$within)
reg_sp <- rgeos::gUnaryUnion(reg)
reg   <- st_as_sf(reg_sp)  
reg <-  st_transform(reg, "EPSG:4326")  

# create histogram of closure areas within 400-800 meter depth range
# scenario 1 - option 1
scenario1a <- st_read(paste(pathdir,"Technical service Jan 2022/Output/VME update/Scenario1_option1.shp",sep="/"))
scenario1a <- st_cast(scenario1a,"POLYGON")
sce1a      <- sf::st_intersection(scenario1a,reg)

# scenario 1 - option 2
scenario1b <- st_read(paste(pathdir,"Technical service Jan 2022/Output/VME update/Scenario1_option2.shp",sep="/"))
scenario1b <- st_cast(scenario1b,"POLYGON")
sce1b      <- sf::st_intersection(scenario1b,reg)

# scenario 2 - option 1
scenario2a <- st_read(paste(pathdir,"Technical service Jan 2022/Output/VME update/Scenario2_option1.shp",sep="/"))
scenario2a <- st_cast(scenario2a,"POLYGON")
sce2a      <- sf::st_intersection(scenario2a,reg)

# scenario 2 - option 2
scenario2b <- st_read(paste(pathdir,"Technical service Jan 2022/Output/VME update/Scenario2_option2.shp",sep="/"))
scenario2b <- st_cast(scenario2b,"POLYGON")
sce2b      <- sf::st_intersection(scenario2b,reg)

# now plot closures that fall in the 400-800 meter depth range
types <- vapply(sf::st_geometry(sce1a), function(x) {
  class(x)[2]
}, "")
sce1a <- sce1a[ grepl("*POLYGON", types), ]
sce1a <- as(sce1a, 'Spatial')

types <- vapply(sf::st_geometry(sce1b), function(x) {
  class(x)[2]
}, "")
sce1b <- sce1b[ grepl("*POLYGON", types), ]
sce1b <- as(sce1b, 'Spatial')

types <- vapply(sf::st_geometry(sce2a), function(x) {
  class(x)[2]
}, "")
sce2a <- sce2a[ grepl("*POLYGON", types), ]
sce2a <- as(sce2a, 'Spatial')

types <- vapply(sf::st_geometry(sce2b), function(x) {
  class(x)[2]
}, "")
sce2b <- sce2b[ grepl("*POLYGON", types), ]
sce2b <- as(sce2b, 'Spatial')

# get VME
VME <- read.csv(paste(pathdir_nogit,
                      "VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
VME <- VME[,-1]
depthreg <- cbind(depthreg, VME[match(depthreg@data$csquares,VME$CSquare), c("VME_Class")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "VME_Class"

## now get 0.25 c-square grid
setwd(paste(pathdir,"1-Input data/csquares_ecoregions",sep="/"))
gridall <- readRDS("Region_0.25_csquare_grid.rds")

# and merge depthreg file at 0.25 c-square grid format
tt <- depthreg@data
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
tt1$uni <- paste(tt1$long,tt1$lat)
tt2$uni <- paste(tt2$long,tt2$lat)
tt3$uni <- paste(tt3$long,tt3$lat)
tt4$uni <- paste(tt4$long,tt4$lat)

ttall <- rbind(tt1,tt2,tt3,tt4)
nam <- colnames(ttall)

gridall2 <- merge(x = gridall, y = ttall[ , c(nam)], by = "uni", all.x=TRUE)
depthwithin <- subset(gridall2, gridall2@data$within == 1)

# now overlay the different closure scenarios 
sce1a <- spTransform(sce1a,CRS(proj4string(depthwithin))) # make it similar to depthwithin
clos1a <- over(depthwithin,sce1a)
depthwithin@data$clos1a <- clos1a[1:nrow(clos1a),1]
depthwithin@data$clos1a[!(is.na(depthwithin@data$clos1a))]  <- 1 
depthwithin@data$clos1a[is.na(depthwithin@data$clos1a)] <- 0 

sce1b <- spTransform(sce1b,CRS(proj4string(depthwithin))) # make it similar to depthwithin
clos1b <- over(depthwithin,sce1b)
depthwithin@data$clos1b <- clos1b[1:nrow(clos1b),1]
depthwithin@data$clos1b[!(is.na(depthwithin@data$clos1b))]  <- 1 
depthwithin@data$clos1b[is.na(depthwithin@data$clos1b)] <- 0 

sce2a <- spTransform(sce2a,CRS(proj4string(depthwithin))) # make it similar to depthwithin
clos2a <- over(depthwithin,sce2a)
depthwithin@data$clos2a <- clos2a[1:nrow(clos2a),1]
depthwithin@data$clos2a[!(is.na(depthwithin@data$clos2a))]  <- 1 
depthwithin@data$clos2a[is.na(depthwithin@data$clos2a)] <- 0 

sce2b <- spTransform(sce2b,CRS(proj4string(depthwithin))) # make it similar to depthwithin
clos2b <- over(depthwithin,sce2b)
depthwithin@data$clos2b <- clos2b[1:nrow(clos2b),1]
depthwithin@data$clos2b[!(is.na(depthwithin@data$clos2b))]  <- 1 
depthwithin@data$clos2b[is.na(depthwithin@data$clos2b)] <- 0 

# make table per row
depthwithin <- depthwithin@data

# create table
tablenew <- data.frame(matrix(data=NA,nrow = 38, ncol= 9))

# vme habitat / index closed 
# scenario 1a
tablenew [4,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$VME_Class == 3))/4 
tablenew [5,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$VME_Class == 2))/4
tablenew [6,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$VME_Class == 1))/4
tablenew [7,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$VME_Class == 0))/4

tablenew [4,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$VME_Class == 3))/4 
tablenew [5,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$VME_Class == 2))/4
tablenew [6,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$VME_Class == 1))/4
tablenew [7,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$VME_Class == 0))/4

# scenario 1b
tablenew [4,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$VME_Class == 3))/4
tablenew [5,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$VME_Class == 2))/4
tablenew [6,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$VME_Class == 1))/4
tablenew [7,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$VME_Class == 0))/4

tablenew [4,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$VME_Class == 3))/4
tablenew [5,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$VME_Class == 2))/4
tablenew [6,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$VME_Class == 1))/4
tablenew [7,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$VME_Class == 0))/4

# scenario 2a
tablenew [4,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 3))/4 
tablenew [5,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 2))/4
tablenew [6,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 1))/4
tablenew [7,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 0))/4

tablenew [4,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 3))/4
tablenew [5,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 2))/4
tablenew [6,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 1))/4
tablenew [7,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 0))/4

# scenario 2b
tablenew [4,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 3))/4 
tablenew [5,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 2))/4
tablenew [6,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 1))/4
tablenew [7,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 0))/4

tablenew [4,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 3))/4 
tablenew [5,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 2))/4
tablenew [6,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 1))/4
tablenew [7,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 0))/4
