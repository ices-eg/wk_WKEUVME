
# get the csv coordinates of all closures with information on VMEs that overlap with 400-800 meter range
scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS("Celtic Seas_depth.rds")
depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
depthall <- rbind(depthreg,depthreg2)

depth <- subset(depthall,depthall@data$within ==1)
reg <- unionSpatialPolygons(depth,depth$within)
reg <- gUnaryUnion(reg)
reg   <- st_as_sf(reg)

for (p in 1:4){
  scenar <- st_read(paste(pathdir,"2-Data processing",paste(scedat[p],"shp",sep="."),sep="/"))
  scenar <- st_cast(scenar,"POLYGON")
  
# find all polygons that intersect
  overpol      <- sf::st_intersection(scenar,reg)
  overpol <- as(overpol, 'Spatial')
  overpol <- rownames(overpol@data)
  
  scea <- as(scenar, 'Spatial')
  scea@data$rown <- rownames(scea@data)
  scea <- subset(scea, scea@data$rown %in% c(overpol))
  scea@data$FID <- 1:(nrow(scea@data))
  
# get VME c-square data
  VME <- read.csv(paste(pathdir_nogit,"VME data repository/VME observations and csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]

  source(paste(pathdir,"6-Utilities/csquare_to_coords_VMStools.R",sep="/"))
  lonlat <- CSquare2LonLat(as.character(VME$CSquare),0.05)
  VME$long <- round(lonlat$SI_LONG,digits=3)
  VME$lat  <- round(lonlat$SI_LATI,digits=3)
  
  coord <-data.frame(VME$long, VME$lat)
  colnames(coord)<- c("Longitude", "Latitude")
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  coord <- spTransform(coord,CRS(proj4string(scea)))
  overVME <- over(coord,scea)
  VME$pol <- overVME$FID

  # get VME actual observations
  VMobs <- read.csv(paste(pathdir_nogit,
                          "VME data repository/VME observations and csquares/VME_Extraction_of_precenses_25052020.csv",sep="/"),
                    header=T,sep=",",row.names = NULL)
  VMobs <- as.data.frame(VMobs)
  
  coord <-data.frame(VMobs$MiddleLongitude, VMobs$MiddleLatitude)
  colnames(coord)<- c("Longitude", "Latitude")
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  coord <- spTransform(coord,CRS(proj4string(scea)))
  overVME <- over(coord,scea)
  VMobs$pol <- overVME$FID
  
  # now link all together
  runpol <- as.data.frame(matrix(data=NA,nrow=1,ncol=7))
  colnames(runpol) <- c("Poly_No","Coord_No","Longitude","Latitude","VME_Csquare","VME_Habitat","VME_Indicator")

for(j in 1:nrow(scea@data)){
  coords  <- scea@polygons[[j]]@Polygons[[1]]@coords
  coords[,1] <- round(coords[,1],digits =3)
  coords[,2] <- round(coords[,2],digits =3)
  coords <- unique(coords[ , 1:2 ] )
  Poly_No <- rep(j,nrow(coords))
  Coord_No <- paste(Poly_No,1:nrow(coords),sep="_")
 
  # get vme c-square data
  tt <- subset(VME,VME$pol == j)
  if(nrow(tt) > 0){
    csq  <- paste(unique(tt$VME_Class_Name), collapse = '_ ')
  } else{
    csq <- c()
  }
  
  # get vme habitat/indicator information
  tt <- subset(VMobs,VMobs$pol == j)
  if(nrow(tt) > 0){
    obq  <- paste(unique(tt$VME_Indicator), collapse = '_ ')
    obq_hab <- paste(unique(tt$HabitatType), collapse = '_ ')
  } else{
    obq <- c()
    obq_hab <- c()
  }
  
  infor <- data.frame(Poly_No,Coord_No,Longitude = coords[,1], Latitude = coords[,2], VME_Csquare = rep(csq,length(Poly_No)),
                      VME_Habitat = rep(obq_hab,length(Poly_No)), VME_Indicator = rep(obq,length(Poly_No)))
  runpol <- rbind(runpol,infor)  
  
  }
  
runpol <- runpol[-1,]

# add which polygons intersect with VME elements
  runpol$VME_Elements <- NA

  # get VME elements
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
  
  # get polygons with bank
  Proj     <- as(Bank, 'Spatial')
  proj4string(scea) <- CRS(proj4string(Proj)) 
  scea <- st_as_sf(scea)
  Bank_over      <- st_intersection(scea,Bank)
  Bank_over      <- as(Bank_over, 'Spatial')
  pol_with_bank  <- unique(Bank_over@data$FID)

  # get polygons with coralmound
  Coralmound_over      <- st_intersection(scea,Coralmound)
  Coralmound_over      <- as(Coralmound_over, 'Spatial')
  pol_with_Coralmound  <- unique(Coralmound_over@data$FID)
  
  # get polygons with Mudvolcano
  Mudvolcano_over      <- st_intersection(scea,Mudvolcano)
  Mudvolcano_over      <- as(Mudvolcano_over, 'Spatial')
  pol_with_Mudvolcano  <- unique(Mudvolcano_over@data$FID)
  
  # get polygons with Seamount
  Seamount_over      <- st_intersection(scea,Seamount)
  Seamount_over      <- as(Seamount_over, 'Spatial')
  pol_with_Seamount  <- unique(Seamount_over@data$FID)

  # some closure polygons have multiple elements
  totele <- data.frame(pol = c(pol_with_bank,pol_with_Coralmound,pol_with_Mudvolcano,pol_with_Seamount),
                       element = c(rep("Bank",length(pol_with_bank)),rep("Coralmound",length(pol_with_Coralmound))
                         ,rep("Mudvolcano",length(pol_with_Mudvolcano)),rep("Seamount",length(pol_with_Seamount))))
  
  uni_pol <- unique(totele$pol)
  
  for (i in 1:length(uni_pol)){
    tt <- subset(totele,totele$pol == uni_pol[i])
    runpol$VME_Elements[runpol$Poly_No == tt$pol[1]] <- paste(unique(tt$element), collapse = '_ ')
  } 
  
  write.csv(runpol,paste(pathdir,"5-Output/Closure options (region)",paste(scedat[p],"coords.csv",sep="_"),sep="/"), row.names=FALSE)
}
  