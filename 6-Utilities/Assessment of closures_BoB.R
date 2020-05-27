
# plot closures and analyse effect of closures on fisheries
outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS(paste(EcoReg,"_depth.rds",sep=""))

depth <- subset(depthreg,depthreg@data$within ==1)
reg <- unionSpatialPolygons(depth,depth$within)
reg <- gUnaryUnion(reg)
reg   <- st_as_sf(reg)

# create histogram of closure areas within 400-800 meter depth range
# scenario 1 - option 1
scenario1a <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario1_2020May22_v1.shp",sep="/"))
scenario1a <- st_as_sf(scenario1a)
sce1a      <- sf::st_intersection(scenario1a,reg)
areasce1a  <- sf::st_area(sce1a)/1000000

# scenario 1 - option 2
scenario1b <- st_read(paste(pathdir_nogit,"Closure options/VME_Closures_Scenario1_option2_2020May27_v3.shp",sep="/"))
scenario1b <- st_as_sf(scenario1b)
sce1b      <- sf::st_intersection(scenario1b,reg)
areasce1b  <- sf::st_area(sce1b)/1000000

# scenario 2 - option 1
scenario2a <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario2_Opt1_2020May22_v1.shp",sep="/"))
scenario2a <- st_as_sf(scenario2a)
sce2a      <- sf::st_intersection(scenario2a,reg)
areasce2a  <- sf::st_area(sce2a)/1000000

# scenario 2 - option 2
scenario2b <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario2_Opt2_2020May25_v2.shp",sep="/"))
scenario2b <- st_as_sf(scenario2b)
sce2b      <- sf::st_intersection(scenario2b,reg)
areasce2b  <- sf::st_area(sce2b)/1000000

jpeg(file = paste(outdir,"Figure_histogram_closures.jpeg",sep="/"), width=7, height=6,units ='in', res = 300)
par(mfrow=c(2,2))

hist(log10(unclass(areasce1a)),xlab="closure area (km2)",ylab="Frequency", xaxt="n", yaxt="n",main="scenario 1 option 1"
     ,ylim=c(0,24),xlim = c(1,3.6), breaks = 10)
axis(1,c(1,2,3,3.54), c("10","100","1000","3500"))
axis(2,c(0,12,24),las = 1)
text(3,20,paste("n =",length(areasce1a),sep=" "))

hist(log10(unclass(areasce1b)),xlab="closure area (km2)",ylab="Frequency", xaxt="n", yaxt="n",las=1,main="scenario 1 option 2"
     ,ylim=c(0,24),xlim = c(1,3.6), breaks = 11)
axis(1,c(1,2,3,3.54), c("10","100","1000","3500"))
axis(2,c(0,12,24),las = 1)
text(3,20,paste("n =", length(areasce1b),sep=" "))

hist(log10(unclass(areasce2a)),xlab="closure area (km2)",ylab="Frequency", xaxt="n", yaxt="n",las=1,main="scenario 2 option 1"
     ,ylim=c(0,24),xlim = c(1,3.6), breaks = 11)
axis(1,c(1,2,3,3.54), c("10","100","1000","3500"))
axis(2,c(0,12,24),las = 1)
text(3,20,paste("n =", length(areasce2a),sep=" "))

hist(log10(unclass(areasce2b)),xlab="closure area (km2)",ylab="Frequency", xaxt="n", yaxt="n",las=1,main="scenario 2 option 2"
     ,ylim=c(0,24),xlim = c(1,3.6), breaks = 11)
axis(1,c(1,2,3,3.54), c("10","100","1000","3500"))
axis(2,c(0,12,24),las = 1)
text(3,20,paste("n =", length(areasce2b),sep=" "))

dev.off()

# now plot closures that fall in the 400-800 meter depth range
sce1a <- as(sce1a, 'Spatial')
sce1b <- as(sce1b, 'Spatial')
sce2a <- as(sce2a, 'Spatial')
sce2b <- as(sce2b, 'Spatial')

# run producing figures and tables up to fig 1
jpeg(file = paste(outdir,"Figure_map_closures.jpeg",sep="/"), width=fig_width*2, height=fig_length*2,units ='in', res = 300)
figmap <- ggplot() + geom_point(data=fig1, aes(x=long, y=lat , col=as.factor(within)),
                                shape=15,size=0.5,na.rm=T) 
figmap <- figmap +  scale_color_manual(values = c("white","lightblue"),name ="",labels=c("","depth 400-800 m"))      
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
                          legend.position ="bottom") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
figmap  <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap  <- figmap +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)

figmap_sce1a <- figmap + geom_polygon(data= sce1a, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
  ggtitle("scenario 1 - option 1")
figmap_sce1b <- figmap + geom_polygon(data= sce1b, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
  ggtitle("scenario 1 - option 2")
figmap_sce2a <- figmap + geom_polygon(data= sce2a, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
  ggtitle("scenario 2 - option 1")
figmap_sce2b <- figmap + geom_polygon(data= sce2b, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
  ggtitle("scenario 2 - option 2")

print(grid.arrange(figmap_sce1a,figmap_sce1b, figmap_sce2a,figmap_sce2b, nrow = 2))

dev.off() 


### now make table to calculate overlap

# get footprint
source(paste(pathdir,"6-Utilities/Get_fishing_footprint_refperiod.R",sep="/"))
depthreg <- cbind(depthreg, Footprint[match(depthreg@data$csquares,Footprint$csquares), c("adjacent.cells")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "adjacent.cells"
depthreg@data$adjacent.cells[depthreg@data$adjacent.cells > 0 ] <- 1

# get VME
VME <- read.csv(paste(pathdir_nogit,
                      "VME data repository/VME weighted csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
VME <- VME[,-1]
depthreg <- cbind(depthreg, VME[match(depthreg@data$csquares,VME$CSquare), c("VME_Class")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "VME_Class"

# get VMS
setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))

# define few params
refyear <- 2009:2011
afteryear1 <- 2012:2015
afteryear2 <- 2016:2019
allyears <- 2009:2019
metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                  "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                  "OT_MIX_DMF_BEN","OT_SPF")
metier_static <- c("Static","Static_FPO","Static_GNS","Static_LLS") 

# fix for static gear column not coming through
for(yy in 1:length(allyears)){
  vmssub <- vmsreg[grep(allyears[yy],names(vmsreg))]
  nam <- names(vmssub[grep("Static",names(vmssub))])
  dat <- rowSums(vmssub[nam]) #HH
  dat[dat > 0] <- 1 
  vmsreg[nam[1]] <- dat
}

# SAR trawling in ref period
nam <- paste("SAR_total",refyear,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$refSAR <- rowMeans(vmsreg[indexcol])
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("refSAR")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "refSAR"
depthreg@data$refSAR[depthreg@data$adjacent.cells == 0] <- 0

# SAR trawling in after period 1
nam <- paste("SAR_total",afteryear1,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$afterSAR1 <- rowMeans(vmsreg[indexcol])
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("afterSAR1")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "afterSAR1"
depthreg@data$afterSAR1[depthreg@data$adjacent.cells == 0] <- 0

# SAR trawling in ref period
nam <- paste("SAR_total",afteryear2,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$afterSAR2 <- rowMeans(vmsreg[indexcol])
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("afterSAR2")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "afterSAR2"
depthreg@data$afterSAR2[depthreg@data$adjacent.cells == 0] <- 0

# Static in ref period
nam <- paste("Static",refyear, sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$refStatic <- rowSums(vmsreg[indexcol])
vmsreg$refStatic[vmsreg$refStatic > 0] <- 1
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("refStatic")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "refStatic"
depthreg@data$refStatic[depthreg@data$adjacent.cells == 0] <- 0

nam <- paste("Static",afteryear1, sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$afterStatic1 <- rowSums(vmsreg[indexcol])
vmsreg$afterStatic1[vmsreg$afterStatic1 > 0] <- 1
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("afterStatic1")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "afterStatic1"
depthreg@data$afterStatic1[depthreg@data$adjacent.cells == 0] <- 0

nam <- paste("Static",afteryear2, sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$afterStatic2 <- rowSums(vmsreg[indexcol])
vmsreg$afterStatic2[vmsreg$afterStatic2 > 0] <- 1
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("afterStatic2")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "afterStatic2"
depthreg@data$afterStatic2[depthreg@data$adjacent.cells == 0] <- 0

# SAR trawling in ref period
nam <- paste("SAR_total",c(refyear, allyears),sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$threshold <- rowMeans(vmsreg[indexcol])
vmsreg$threshold <- ifelse(vmsreg$threshold > 0.43, 1, 0)
depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("threshold")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "threshold"
depthreg@data$threshold[is.na(depthreg@data$threshold)] <- 0
depthreg@data$threshold[depthreg@data$adjacent.cells == 0] <- 0

# get core fishing ground otter trawling
# get region within 400-800 meter
IREG <- subset(depthreg@data,depthreg@data$within == 1)
IREG$EEZ <- factor(IREG$EEZ)
fig8 <- IREG # use code from figure 8
nam <- paste("SAR_Otter",refyear,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
fig8 <- cbind(fig8, vmsreg[match(fig8$csquares,vmsreg$c_square), c("otrefyear")])
colnames(fig8)[ncol(fig8)] <- "Otter_intensity"
fig8$Otter_intensity[is.na(fig8$Otter_intensity)] <- 0
fig8 <- subset(fig8,fig8$Otter_intensity > 0 & fig8$adjacent.cells > 0)
fig8 <- fig8[order(fig8$Otter_intensity),]
fig8$perc <- cumsum(fig8$Otter_intensity) / sum(fig8$Otter_intensity)*100
quat <- c(0, 10,  100)
fig8$cat <- cut(fig8$perc,c(quat))
depthreg <- cbind(depthreg, fig8[match(depthreg@data$csquares,fig8$csquares), c("cat")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "core_area"

nam <- paste("SAR_Otter",afteryear1,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$otafteryear1 <- rowMeans(vmsreg[indexcol])  
fig8 <- cbind(fig8, vmsreg[match(fig8$csquares,vmsreg$c_square), c("otafteryear1")])
colnames(fig8)[ncol(fig8)] <- "otafteryear1"
fig8$otafteryear1[is.na(fig8$otafteryear1)] <- 0
fig8 <- subset(fig8,fig8$otafteryear1 > 0 & fig8$adjacent.cells > 0)
fig8 <- fig8[order(fig8$otafteryear1),]
fig8$perc1 <- cumsum(fig8$otafteryear1) / sum(fig8$otafteryear1)*100
quat <- c(0, 10,  100)
fig8$cat1 <- cut(fig8$perc1,c(quat))
depthreg <- cbind(depthreg, fig8[match(depthreg@data$csquares,fig8$csquares), c("cat1")])
colnames(depthreg@data)[ncol(depthreg@data)] <- "core_area_after1"

nam <- paste("SAR_Otter",afteryear2,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$otafteryear2 <- rowMeans(vmsreg[indexcol])  
fig8 <- cbind(fig8, vmsreg[match(fig8$csquares,vmsreg$c_square), c("otafteryear2")])
colnames(fig8)[ncol(fig8)] <- "otafteryear2"
fig8$otafteryear2[is.na(fig8$otafteryear2)] <- 0
fig8 <- subset(fig8,fig8$otafteryear2 > 0 & fig8$adjacent.cells > 0)
fig8 <- fig8[order(fig8$otafteryear2),]
fig8$perc2 <- cumsum(fig8$otafteryear2) / sum(fig8$otafteryear2)*100
quat <- c(0, 10,  100)
fig8$cat2 <- cut(fig8$perc2,c(quat))
depthreg <- cbind(depthreg, fig8[match(depthreg@data$csquares,fig8$csquares), c("cat2")])
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

# make table per row
depthwithin <- depthwithin@data

tablenew <- data.frame(matrix(data=NA,nrow = 44, ncol= 9))

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

# vme habitat /index closed and below threshold
tablenew [10,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4
tablenew [10,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))/4

# vme habitat /index closed and above threshold
tablenew [11,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4
tablenew [11,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))/4

# c-squares part of fishing footprint
tablenew [14,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$adjacent.cells == 1 ))/4
tablenew [14,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$adjacent.cells == 1 ))/4

# c-squares part of static gears present
tablenew [17,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$refStatic == 1))/4
tablenew [17,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$refStatic == 1))/4
tablenew [17,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$refStatic == 1))/4
tablenew [17,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$refStatic == 1))/4
tablenew [17,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$refStatic == 1))/4
tablenew [17,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$refStatic == 1))/4
tablenew [17,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$refStatic == 1))/4
tablenew [17,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$refStatic == 1))/4

# c-squares part of SAR gears present
tablenew [18,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$refSAR > 0))/4
tablenew [18,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$refSAR > 0))/4
tablenew [18,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$refSAR > 0))/4
tablenew [18,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$refSAR > 0))/4
tablenew [18,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$refSAR > 0))/4
tablenew [18,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$refSAR > 0))/4
tablenew [18,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$refSAR > 0))/4
tablenew [18,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$refSAR > 0))/4

# core footprint based on SAR
tablenew [21,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$core_area == "(10,100]"))/4
tablenew [21,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$core_area == "(10,100]"))/4

# fraction of SAR in closed area
depthwithin$refSAR <- depthwithin$refSAR/4
tablenew [23,2] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos1a == 1],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,3] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos1a == 0],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,4] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos1b == 1],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,5] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos1b == 0],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,6] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos2a == 1],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,7] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos2a == 0],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,8] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos2b == 1],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))
tablenew [23,9] <- as.character(round(sum(depthwithin$refSAR[depthwithin$clos2b == 0],na.rm=T) / sum(depthwithin$refSAR,na.rm=T),digits = 2))

## now for period 2012-2015
n <- 27
# c-squares part of static gears present
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$afterStatic1 == 1))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$afterStatic1 == 1))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$afterStatic1 == 1))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$afterStatic1 == 1))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$afterStatic1 == 1))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$afterStatic1 == 1))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$afterStatic1 == 1))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$afterStatic1 == 1))/4

n <- 28
# c-squares part of SAR gears present
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$afterSAR1 > 0))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$afterSAR1 > 0))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$afterSAR1 > 0))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$afterSAR1 > 0))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$afterSAR1 > 0))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$afterSAR1 > 0))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$afterSAR1 > 0))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$afterSAR1 > 0))/4

n <- 31
# core footprint based on SAR
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$core_area_after1 == "(10,100]"))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$core_area_after1 == "(10,100]"))/4

n <- 33
# fraction of SAR in closed area
depthwithin$afterSAR1 <- depthwithin$afterSAR1/4
tablenew [n,2] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos1a == 1],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,3] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos1a == 0],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,4] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos1b == 1],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,5] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos1b == 0],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,6] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos2a == 1],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,7] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos2a == 0],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,8] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos2b == 1],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))
tablenew [n,9] <- as.character(round(sum(depthwithin$afterSAR1[depthwithin$clos2b == 0],na.rm=T) / sum(depthwithin$afterSAR1,na.rm=T),digits = 2))

## now for period 2016-2019
n <- 37
# c-squares part of static gears present
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$afterStatic2 == 1))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$afterStatic2 == 1))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$afterStatic2 == 1))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$afterStatic2 == 1))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$afterStatic2 == 1))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$afterStatic2 == 1))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$afterStatic2 == 1))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$afterStatic2 == 1))/4

n <- 38
# c-squares part of SAR gears present
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$afterSAR2 > 0))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$afterSAR2 > 0))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$afterSAR2 > 0))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$afterSAR2 > 0))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$afterSAR2 > 0))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$afterSAR2 > 0))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$afterSAR2 > 0))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$afterSAR2 > 0))/4

n <- 41
# core footprint based on SAR
tablenew [n,2] <- length(which(depthwithin$clos1a == 1 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,3] <- length(which(depthwithin$clos1a == 0 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,4] <- length(which(depthwithin$clos1b == 1 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,5] <- length(which(depthwithin$clos1b == 0 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,6] <- length(which(depthwithin$clos2a == 1 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,7] <- length(which(depthwithin$clos2a == 0 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,8] <- length(which(depthwithin$clos2b == 1 & depthwithin$core_area_after2 == "(10,100]"))/4
tablenew [n,9] <- length(which(depthwithin$clos2b == 0 & depthwithin$core_area_after2 == "(10,100]"))/4

n <- 43
# fraction of SAR in closed area
depthwithin$afterSAR2 <- depthwithin$afterSAR2/4
tablenew [n,2] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos1a == 1],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,3] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos1a == 0],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,4] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos1b == 1],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,5] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos1b == 0],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,6] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos2a == 1],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,7] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos2a == 0],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,8] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos2b == 1],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))
tablenew [n,9] <- as.character(round(sum(depthwithin$afterSAR2[depthwithin$clos2b == 0],na.rm=T) / sum(depthwithin$afterSAR2,na.rm=T),digits = 2))


tablenew [,1] <- c("","","VME protection","nb of c-squares with VME habitat","nb of c-squares with VME index high",
                   "nb of c-squares with VME index medium","nb of c-squares with VME index low",
                   "","VME protection and fishing impact threshold","nb of c-squares with VME habitat/index below SAR 0.43 threshold (2009-2019)",
                   "nb of c-squares with closed VME habitat/index above SAR 0.43 threshold (2009-2019)",
                   "","Fisheries footprint","nb of c-squares part of fishing footprint",
                   "", "Fisheries overlap (presence/absence) (2009-2011)",
                   "nb of c-squares with static bottom fishing (present)",
                   "nb of c-squares with mobile bottom fishing (SAR > 0)",
                   "","Fisheries overlap (core fishing ground) (2009-2011)",
                   "nb of c-squares that form core fishing area based on SAR  (method 1)",
                   "nb of c-squares that form core fishing area based on kw/h fished (method 2)",
                   "fraction of total SAR",
                   "fraction of total kw/h",
                   "", "Fisheries consequences (presence/absence) (2012-2015)",
                   "nb of c-squares with static bottom fishing (present)",
                   "nb of c-squares with mobile bottom fishing (SAR > 0)",
                   "","Fisheries consequences (core fishing ground) (2012-2015)",
                   "nb of c-squares that form core fishing area based on SAR (method 1)",
                   "nb of c-squares that form core fishing area based on kw/h fished (method 2)",
                   "fraction of total SAR",
                   "fraction of total kw/h",
                   "", "Fisheries consequences (presence/absence) (2016-2019)",
                   "nb of c-squares with static bottom fishing (present)",
                   "nb of c-squares with mobile bottom fishing (SAR > 0)",
                   "","Fisheries consequences (core fishing ground) (2016-2019)",
                   "nb of c-squares that form core fishing area based on SAR (method 1)",
                   "nb of c-squares that form core fishing area based on kw/h fished (method 2)",
                   "fraction of total SAR",
                   "fraction of total kw/h")
tablenew[1,] <- c("","Scenario 1 option 1","","Scenario 1 option 2","","Scenario 2 option 1","","Scenario 2 option 2","")
tablenew[2,] <- c("","within closure","outside closures","within closure","outside closures","within closure","outside closures","within closure","outside closures")
tablenew[3,2:9] <- c("","","","","","","","")
tablenew[8,] <- c("","","","","","","","","")
tablenew[9,2:9] <- c("","","","","","","","")
tablenew[12,] <- c("","","","","","","","","")
tablenew[13,2:9] <- c("","","","","","","","")
tablenew[15,] <- c("","","","","","","","","")
tablenew[16,2:9] <- c("","","","","","","","")
tablenew[19,] <- c("","","","","","","","","")
tablenew[20,2:9] <- c("","","","","","","","")
tablenew[25,] <- c("","","","","","","","","")
tablenew[26,2:9] <- c("","","","","","","","")
tablenew[29,] <- c("","","","","","","","","")
tablenew[30,2:9] <- c("","","","","","","","")
tablenew[35,] <- c("","","","","","","","","")
tablenew[36,2:9] <- c("","","","","","","","")
tablenew[39,] <- c("","","","","","","","","")
tablenew[40,2:9] <- c("","","","","","","","")

tablenew <- data.frame(tablenew)
write.csv(tablenew, paste(outdir,"Table_closure_options.csv",sep="/"), 
          row.names = FALSE, quote=FALSE)
