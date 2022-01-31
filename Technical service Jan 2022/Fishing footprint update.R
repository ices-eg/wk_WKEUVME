
# Technical service 2 - fishing footprint part
# Deliver the coordinates of the footprint "Footprint_all", "Footprint_mobile",
# "Footprint_static" in EU waters (EEZs of France, Ireland, Spain and Portugal) in csv, .xlsx 
# and .shp files, also produced as maps in pdf. 

## set path to folder 
  pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/WKEUVME_noGIT" # stores the data from sharepoint

## get libraries
  source(paste(pathdir,"6-Utilities/Libraries_WKEUVME.R",sep="/"))

  # get fishing footprints
  EcoReg <- "Celtic Seas"
  source(paste(pathdir,"6-Utilities/Get fishing footprint mbcg_static.R", sep="/"))
  FootprintCS <- Footprint
  EcoReg <- "Bay of Biscay and the Iberian Coast"
  source(paste(pathdir,"6-Utilities/Get fishing footprint mbcg_static.R", sep="/"))
  Footprint <- rbind(Footprint, FootprintCS)
  
  # get fishing footprint update
  load(paste(pathdir_nogit,"VMS data repository/csquares_update.RData",sep="/"))
  dat_up$ref_sar <- NA
  dat_up$ref_static <- NA
  dat_up$ref <- NA
  colnames(dat_up)[4]<- "MBCG_footprint"
  colnames(dat_up)[5]<- "Static_footprint"
  dat_up$Both_footprint <- rowSums(dat_up[,c("MBCG_footprint","Static_footprint")])
  dat_up$Both_footprint <- ifelse(dat_up$Both_footprint > 0, 1, 0)
  
  # 17 missed C-squares between CS and BoBIC (now checked with VMS data)
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg  <- readRDS("Celtic Seas_depth.rds")
  depthreg2 <- readRDS("Bay of Biscay and the Iberian Coast_depth.rds")
  depthreg3 <- readRDS("Tip of portugal_depth.rds")
  depthreg3 <- subset(depthreg3,is.na(depthreg3@data$Ecoregion) & !(is.na(depthreg3@data$mean_depth_emodnet)))
  depthall  <- rbind(depthreg,depthreg2)
  depthreg3 <- spTransform(depthreg3,CRS(proj4string(depthall))) 
  depthall  <- spTransform(depthall,CRS(proj4string(depthreg3))) 
  depthall  <- rbind(depthall,depthreg3)
  depth     <- subset(depthall,depthall@data$within ==1)
  depth     <- cbind(depth,Footprint[match(depth@data$csquares,Footprint$csquares),c("MBCG_footprint","Static_footprint","Both_footprint")]) 
  depth     <- subset(depth, depth$long > -10 & depth$long <7.5)
  depth     <- subset(depth, depth$lat > 47 & depth$lat < 50)
  depth     <- subset(depth,depth$Both_footprint ==0)
  miscsq    <- subset(depthall@data,depthall@data$csquares %in% depth$csquares)
  miscsq     <- cbind(miscsq,dat_up[match(miscsq$csquares,dat_up$csquares),
                                    c("ref_sar","MBCG_footprint","ref_static","Static_footprint", "ref", "Both_footprint")]) 

  # remove the c-squares with missing information
  Footprint <- subset(Footprint,!(Footprint$csquares %in% miscsq$csquares))
  
  # Get depth data 400 - 800 m tip of Portugal
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  Port <- readRDS("Tip of portugal_depth.rds")
  Port <- subset(Port,is.na(Port@data$Ecoregion) & !(is.na(Port@data$mean_depth_emodnet)))
  Port <- subset(Port@data,Port@data$within ==1)
  Port <- cbind(Port,dat_up[match(Port$csquares,dat_up$csquares),
                            c("ref_sar","MBCG_footprint","ref_static","Static_footprint", "ref", "Both_footprint")]) 
  
  # remove one isolated c-square as part of the footprint rule
  Port$Static_footprint <- ifelse(Port$csquares =="7301:141:499:2",0,Port$Static_footprint)
  Port$Both_footprint <- ifelse(Port$csquares =="7301:141:499:2",0,Port$Both_footprint)
  
  # combine all c-squares
  Footprint <- rbind(Footprint,miscsq,Port)  
  
### now get footprint for static, mobile and both in EU waters
  sf_use_s2(FALSE)
  # load EEZs
  EEZ <- readOGR(dsn = paste(pathdir, "1-Input data/EEZ_land_union_v3_202003",sep="/"), layer = "EEZ_Land_v3_202030")
  EEZ_EUVME_sp <- subset(EEZ,EEZ@data$UNION %in% c("Ireland", "France","Portugal","Spain"))
  EEZ_EUVME <- raster::aggregate(EEZ_EUVME_sp)
  EEZ_EUVME   <- st_as_sf(EEZ_EUVME)
  EEZ_EUVME
  
  Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("Both_footprint")])
  colnames(Footprint2@data)[ncol(Footprint2)] <- "Both_footprint"
  Footprint_all <- subset(Footprint2,Footprint2@data$Both_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_all,Footprint_all$Both_footprint)
  Footprint_all <- gUnaryUnion(Freg)
  Footprint_all   <- st_as_sf(Footprint_all)
  Footprint_all <-  st_transform(Footprint_all, "EPSG:4326")
  Footprint_all <- st_intersection(Footprint_all,EEZ_EUVME)
  Footprint_all <- Footprint_all %>% st_collection_extract(type="POLYGON")
  Footprint_all <- as_Spatial(Footprint_all)
  
  Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("Static_footprint")])
  colnames(Footprint2@data)[ncol(Footprint2)] <- "Static_footprint"
  Footprint_static <- subset(Footprint2,Footprint2@data$Static_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_static,Footprint_static$Static_footprint)
  Footprint_static <- gUnaryUnion(Freg)
  Footprint_static   <- st_as_sf(Footprint_static)
  Footprint_static <-  st_transform(Footprint_static, "EPSG:4326")
  Footprint_static <- st_intersection(Footprint_static,EEZ_EUVME)
  Footprint_static <- Footprint_static %>% st_collection_extract(type="POLYGON")
  Footprint_static <- as_Spatial(Footprint_static)
  
  Footprint2 <- cbind(depthall, Footprint[match(depthall@data$csquares,Footprint$csquares), c("MBCG_footprint")])
  colnames(Footprint2@data)[ncol(Footprint2)] <- "MBCG_footprint"
  Footprint_mobile <- subset(Footprint2,Footprint2@data$MBCG_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_mobile,Footprint_mobile$MBCG_footprint)
  Footprint_mobile <- gUnaryUnion(Freg)
  Footprint_mobile   <- st_as_sf(Footprint_mobile)
  Footprint_mobile <-  st_transform(Footprint_mobile, "EPSG:4326")
  Footprint_mobile <- st_intersection(Footprint_mobile,EEZ_EUVME)
  Footprint_mobile <- Footprint_mobile %>% st_collection_extract(type="POLYGON")
  Footprint_mobile <- as_Spatial(Footprint_mobile)

  # load data for pdf plot
  depth <- subset(depthall,depthall@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- rgeos::gUnaryUnion(reg)
  reg   <- st_as_sf(reg)
  reg <-  st_transform(reg, "EPSG:4326")  
  sf_use_s2(FALSE)
  reg <- st_intersection(reg,EEZ_EUVME)
  reg <- reg %>% st_collection_extract(type="POLYGON")
  reg_sp <- as_Spatial(reg)
  
  worldMap <- map_data("world")   # Get the world map
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== "Bay of Biscay and the Iberian Coast" | Ecoregion== "Celtic Seas")
  
  minlong   <- round(min(depth$long)-0.1,digits = 0)
  maxlong   <- round(max(depth$long)+ 0.1,digits = 0)
  minlat    <- round(min(depth$lat)- 0.1,digits = 0)
  maxlat    <- 57.1 #round(max(depth$lat)+ 0.1,digits = 0)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  leg1 <- data.frame(long= c(57,57),lat=c(-15,-14)) # random numbers outside plot
  
  figmap <- ggplot() + geom_polypath(data=reg_sp, aes(x = long, y = lat, group = group),color=NA,fill="lightblue")
  figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="grey",fill="grey")
  figmap <- figmap +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=11),
                            legend.title  = element_text(size=11),
                            legend.position =c(0.8,.95),
                            legend.box.background = element_rect(colour = "black",size = 2)) +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap  <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
  figmap  <- figmap +  geom_polygon(data = EEZ_EUVME_sp, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap <-  figmap + geom_point(data = leg1, aes(x = long, y = lat,color=c("tr1","tr2")),shape=15)   + 
    scale_color_manual(values = c("lightblue","orange"),name ="Legend",labels=c("400-800 m depth","Footprint"))
  
  
  #figmap  <- figmap +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)

  scedat <- c("Footprint_all","Footprint_mobile","Footprint_static")
  id_Footprint_all <- c(1:12)
  id_Footprint_mobile <- c(1:28)
  id_Footprint_static <- c(1:29)

  outdir <- paste(pathdir,"Technical service Jan 2022/Output/Fishing footprint update",sep="/")
  
  for(p in 1:3){
    reg <- get(scedat[p])
    reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
    reg <- gBuffer(reg,width=0.0001)
    reg <- spTransform( reg, CRS(proj4string(depthreg)) )
    reg   <- st_as_sf(reg) # combine polygons
    scenar <- st_cast(reg,"POLYGON") # create the polygon file
    reg <- as(scenar, 'Spatial')     # convert back (results in fewer, but larger shapefiles)
    
    # save as .shp file
    scenar$Poly_no <- 1:nrow(scenar) # eval(parse(text = paste("id",scedat[p],sep="_")))
    write_sf(scenar, paste(outdir,paste(scedat[p],"shp", sep="."),sep="/"))
    
    # get coords + holes
    coords  <- ggplot2::fortify(reg)
    coords[,1] <- round(coords[,1],digits =3)
    coords[,2] <- round(coords[,2],digits =3)
    
    # check now which rows match with the row above
    datmatch <- c(1)
    for (n in 1:(nrow(coords)-1)){
      nbmatch <-  sum(match(coords[n,c(1,2,7)],coords[n+1,c(1,2,7)]),na.rm=T)
      datmatch <- c(datmatch,nbmatch)
    }
    
    # bind with coords and remove rows that match
    coords <- cbind(coords,datmatch)
    coords <- subset(coords,!(coords$datmatch == 6))
    coords <- coords[,-8]
    
    # now re-order and combine to data frame
    Poly_No  <- coords$id 
    ord <- as.data.frame(table(Poly_No))
    ord <- ord[order(as.numeric(as.character(ord[,1]))),]
    Coord_order <- c()
    for (q in 1:nrow(ord)){
      No <- paste(ord$Poly_No[q],1:ord$Freq[q],sep="_")
      Coord_order <- c(Coord_order,No)
    }  
    
    runpol <- data.frame(Poly_No,Coord_order,Longitude = coords[,1], Latitude = coords[,2], Hole = coords[,4],Group = coords[,7])
    write.csv(runpol,paste(outdir,paste(scedat[p],"coords.csv",sep="_"),sep="/"), row.names=FALSE)
  
    # make pdf map
    coordcsv <- runpol[!duplicated(runpol[1]),]  # get one coordinate per closure 
    scea <- reg 
    
    pdf(file = paste(outdir,paste(scedat[p],"pdf",sep="."),sep="/"), width=8.5, height=12)
    figmap_scea <- figmap + geom_polypath(data= scea, aes(x = long, y = lat, group = group),color=NA,fill="orange")
    figmap_scea <- figmap_scea + annotate(geom="text", x=coordcsv$Longitude, y=coordcsv$Latitude, label=coordcsv$Poly_No, color="red", size = 2)
    print(figmap_scea) 
    dev.off()
    
    }
  
  
