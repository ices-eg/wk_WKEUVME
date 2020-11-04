
## get closure consequences table for ADG advice

  outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

  # get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS(paste(EcoReg,"_depth.rds",sep=""))
  
  depth <- subset(depthreg,depthreg@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- gUnaryUnion(reg)
  reg   <- st_as_sf(reg)

## load closure scenarios
  # scenario 1 - option 1
  scenario1a <- st_read(paste(pathdir,"2-Data processing/Scenario1_option1.shp",sep="/"))
  scenario1a <- st_cast(scenario1a,"POLYGON")
  sce1a      <- sf::st_intersection(scenario1a,reg)
  sce1a <- as(sce1a, 'Spatial')
  
  # scenario 1 - option 2
  scenario1b <- st_read(paste(pathdir,"2-Data processing/Scenario1_option2.shp",sep="/"))
  scenario1b <- st_cast(scenario1b,"POLYGON")
  sce1b      <- sf::st_intersection(scenario1b,reg)
  sce1b <- as(sce1b, 'Spatial')
  
  # scenario 2 - option 1
  scenario2a <- st_read(paste(pathdir,"2-Data processing/Scenario2_option1.shp",sep="/"))
  scenario2a <- st_cast(scenario2a,"POLYGON")
  sce2a      <- sf::st_intersection(scenario2a,reg)
  sce2a <- as(sce2a, 'Spatial')
  
  # scenario 2 - option 2
  scenario2b <- st_read(paste(pathdir,"2-Data processing/Scenario2_option2.shp",sep="/"))
  scenario2b <- st_cast(scenario2b,"POLYGON")
  sce2b      <- sf::st_intersection(scenario2b,reg)
  sce2b <- as(sce2b, 'Spatial')
  
## get footprint
  source(paste(pathdir,"6-Utilities/Get fishing footprint mbcg_static.R",sep="/"))
  depthreg <- cbind(depthreg, Footprint[match(depthreg@data$csquares,Footprint$csquares), c("Both_footprint")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "Both_footprint"
  
  depthreg <- cbind(depthreg, Footprint[match(depthreg@data$csquares,Footprint$csquares), c("MBCG_footprint")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "MBCG_footprint"
  
  depthreg <- cbind(depthreg, Footprint[match(depthreg@data$csquares,Footprint$csquares), c("Static_footprint")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "Static_footprint"
  
## get footprint area
  sum(depthreg@data$area_sqkm[depthreg@data$Both_footprint == 1],na.rm=T)
  sum(depthreg@data$area_sqkm[depthreg@data$Static_footprint == 1],na.rm=T)
  sum(depthreg@data$area_sqkm[depthreg@data$MBCG_footprint == 1],na.rm=T)
  
 ## get VMS and check fisheries consequences 2015:2018
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))
  
  # define year params
  afteryear2 <- 2015:2018
  
  # SAR intensity in 2015-2018 period
  nam <- paste("SAR_total",afteryear2,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$afterSAR2 <- rowMeans(vmsreg[indexcol])
  depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("afterSAR2")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "afterSAR2"
  
  # get core fishing ground within 400-800 meter
  IREG <- subset(depthreg@data,depthreg@data$within == 1)
  # if NA change to zero == no fishing
  IREG$afterSAR2[is.na(IREG$afterSAR2)] <- 0
  
  # subset to the MBCG footprint only NOTE: not needed, we define the core area for the entire region irrespective of the footprint
  # IREG <- subset(IREG, IREG$MBCG_footprint > 0)
  IREG <- IREG[order(IREG$afterSAR2),]
  IREG$perc2 <- cumsum(IREG$afterSAR2) / sum(IREG$afterSAR2)*100
  quat <- c(-1, 0, 10,  101)
  IREG$cat2 <- cut(IREG$perc2,c(quat))
  depthreg <- cbind(depthreg, IREG[match(depthreg@data$csquares,IREG$csquares), c("cat2")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "core_area_after2"
  
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
  gridall2 <- subset(gridall2, gridall2@data$Ecoregion == EcoReg)
  
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
  
  # create dataframe
  depthwithin <- depthwithin@data
  
  # add last option from stakeholder meeting
  depthwithin$clos1b2a <- depthwithin$clos1b + depthwithin$clos2a
  depthwithin$clos1b2a[depthwithin$clos1b2a >1] <- 1

## create table closure consequences
  tablenew <- data.frame(matrix(data=NA,nrow = 6, ncol= 10))
  
  # within combined footprint
  tablenew [1,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 
  tablenew [1,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Both_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [1,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 
  tablenew [1,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Both_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [1,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 
  tablenew [1,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Both_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [1,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4
  tablenew [1,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Both_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [1,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4
  tablenew [1,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$Both_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Both_footprint == 1 ],na.rm=T)/4)*100
  
  # within static footprint
  tablenew [2,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4
  tablenew [2,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Static_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [2,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4  
  tablenew [2,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4  /
    (sum(depthwithin$area_sqkm[depthwithin$Static_footprint == 1 ],na.rm=T)/4)*100
    
  tablenew [2,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4 
  tablenew [2,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Static_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [2,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4  
  tablenew [2,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Static_footprint == 1 ],na.rm=T)/4)*100 
  
  tablenew [2,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4  
  tablenew [2,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$Static_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$Static_footprint == 1 ],na.rm=T)/4)*100 
  
  # within MBCG footprint
  tablenew [3,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4  
  tablenew [3,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$MBCG_footprint == 1 ],na.rm=T)/4)*100 
  
  tablenew [3,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4 
  tablenew [3,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4  /
    (sum(depthwithin$area_sqkm[depthwithin$MBCG_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [3,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4 
  tablenew [3,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4  /
    (sum(depthwithin$area_sqkm[depthwithin$MBCG_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [3,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4
  tablenew [3,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$MBCG_footprint == 1 ],na.rm=T)/4)*100
  
  tablenew [3,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4
  tablenew [3,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$MBCG_footprint == 1 ],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$MBCG_footprint == 1 ],na.rm=T)/4)*100
  
  # core footprint based on SAR
  tablenew [4,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(10,101]" 
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [4,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(10,101]" 
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [4,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(10,101]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [4,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(10,101]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [4,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(10,101]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [4,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(10,101]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [4,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(10,101]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [4,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(10,101]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [4,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(10,101]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [4,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(10,101]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  # less important area based on SAR
  tablenew [5,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(0,10]" 
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [5,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(0,10]" 
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [5,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(0,10]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [5,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(0,10]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [5,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(0,10]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [5,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(0,10]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [5,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(0,10]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [5,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(0,10]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [5,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(0,10]"
                                 & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [5,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(0,10]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  # unfished area based on SAR
  tablenew [6,1] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(-1,0]" 
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [6,2] <- sum(depthwithin$area_sqkm[depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(-1,0]" 
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [6,3] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [6,4] <- sum(depthwithin$area_sqkm[depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [6,5] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [6,6] <- sum(depthwithin$area_sqkm[depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [6,7] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [6,8] <- sum(depthwithin$area_sqkm[depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew [6,9] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4
  tablenew [6,10] <- sum(depthwithin$area_sqkm[depthwithin$clos1b2a == 1 & depthwithin$core_area_after2 == "(-1,0]"
                                              & depthwithin$MBCG_footprint == 1],na.rm=T)/4 / 
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" & depthwithin$MBCG_footprint == 1],na.rm=T)/4) * 100
  
  tablenew[,c(1,3,5,7,9)] <- tablenew[,c(1,3,5,7,9)]/1000
  tablenew <- round(tablenew,digits = 2)
  
  colnames(tablenew) <- c("S1O1_area", "S1O1_perc","S1O2_area", "S1O2_perc","S2O1_area","S2O1_perc",
                          "S2O2_area", "S2O2_perc","S1O2_S2O1_area", "S1O2_S2O1_perc")
  rownames(tablenew) <- c("Combined footprint","Static footprint", 
                          "MBCG footprint", "Core MBCG area 2015-18 within the MBCG footprint",
                          "Less important MBCG area 2015-18 within the MBCG footprint",
                          "Unfished MBCG area 2015-18 within the MBCG footprint")
  
  write.csv(tablenew, paste(outdir,"Table_ADG_report_closure_consequences.csv",sep="/")) 
  
  
# table 2
  
  ## create table for footprint consequences
  tablenew <- data.frame(matrix(data=NA,nrow = 3, ncol= 2))
  
  # core based on SAR outside MBCG footprint
  tablenew [1,1] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4
  tablenew [1,2] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4 /
              (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]"],na.rm=T)/4) * 100
  
  # less important based on SAR outside MBCG footprint
  tablenew [2,1] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4
  tablenew [2,2] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(0,10]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4 /
    (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(10,101]"],na.rm=T)/4) * 100
  
  tablenew [3,1] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4
  tablenew [3,2] <- sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]" 
                                              & depthwithin$MBCG_footprint == 0],na.rm=T)/4 /
                   (sum(depthwithin$area_sqkm[depthwithin$core_area_after2 == "(-1,0]"],na.rm=T)/4) * 100
  
  
  colnames(tablenew) <- c("area", "perc")
  rownames(tablenew) <- c("Core MBCG area 2015-18 outside the MBCG footprint",
                          "Less important MBCG area 2015-18 outside the MBCG footprint",
                          "Unfished MBCG area 2015-18 outside the MBCG footprint")
  
  write.csv(tablenew, paste(outdir,"Table_ADG_report_footprint_consequences.csv",sep="/")) 
