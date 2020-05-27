
source(paste(pathdir,"6-Utilities/Get_fishing_footprint_refperiod.R",sep="/"))

# plot closures and analyse effect of closures on fisheries









outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS(paste(EcoReg,"_depth.rds",sep=""))
  
  depth <- subset(depthreg,depthreg@data$within ==1)
  reg <- unionSpatialPolygons(depth,depth$within)
  reg <- gUnaryUnion(reg)
  
# create histogram of closure areas within 400-800 meter depth range
  # scenario 1
  scenario1 <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario1_2020May22_v1.shp",sep="/"))
  sce1 <- as_Spatial(scenario1)
  sce1_df <- as.data.frame(sce1)
  sce1 <-SpatialPolygonsDataFrame(sce1,sce1_df)
  sce1<- disaggregate(sce1)
  sce1 <- spTransform(sce1,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce1)){
    together <- gIntersects(sce1[i,],reg)
    inout <- c(inout,together)
  }
  
  sce1 <- sce1[c(which(inout == TRUE)),]

  # scenario 2 - option 1
  scenario2a <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario2_Opt1_2020May22_v1.shp",sep="/"))
  sce2a <- as_Spatial(scenario2a)
  sce2a_df <- as.data.frame(sce2a)
  sce2a <-SpatialPolygonsDataFrame(sce2a,sce2a_df)
  sce2a <- disaggregate(sce2a)
  sce2a <- spTransform(sce2a,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce2a)){
    together <- gIntersects(sce2a[i,],reg)
    inout <- c(inout,together)
  }
  
  sce2a <- sce2a[c(which(inout == TRUE)),]

  # scenario 2 - option 2
  scenario2b <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario2_Opt2_2020May25_v2.shp",sep="/"))
  sce2b <- as_Spatial(scenario2b)
  sce2b_df <- as.data.frame(sce2b)
  sce2b <-SpatialPolygonsDataFrame(sce2b,sce2b_df)
  sce2b<- disaggregate(sce2b)
  sce2b <- spTransform(sce2b,CRS(proj4string(reg))) # make it similar to bargrid
  
  inout <- c()
  for(i in 1:nrow(sce2b)){
    together <- gIntersects(sce2b[i,],reg)
    inout <- c(inout,together)
  }
  
  sce2b <- sce2b[c(which(inout == TRUE)),]

  jpeg(file = paste(outdir,"Figure_histogram_closures.jpeg",sep="/"), width=fig_width*2, height=fig_length/2,units ='in', res = 300)
  par(mfrow=c(1,3))
  area_sce1 <- area(sce1)/1000000
  hist(log10(area_sce1),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 1"
       ,ylim=c(0,35),xlim=c(1,4))
  axis(1,c(1,2,3,4), c(TeX("$10^1$"),TeX("$10^2$"), TeX("$10^3$"), TeX("$10^4$")))
  text(3.5,30,paste("n =", length(area_sce1),sep=" "))

  area_sce2a <- area(sce2a)/1000000
  hist(log10(area_sce2a),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 2 option 1"
       ,ylim=c(0,35),xlim=c(1,4))
  axis(1,c(1,2,3,4), c(TeX("$10^1$"),TeX("$10^2$"), TeX("$10^3$"), TeX("$10^4$")))
  text(3.5,30,paste("n =", length(area_sce2a),sep=" "))

  area_sce2b <- area(sce2b)/1000000
  hist(log10(area_sce2b),xlab="closure area (km2)",ylab="Frequency",las=1,xaxt="n",main="scenario 2 option 2"
       ,ylim=c(0,35),xlim=c(1,4))
  axis(1,c(1,2,3,4), c(TeX("$10^1$"),TeX("$10^2$"), TeX("$10^3$"), TeX("$10^4$")))
  text(3.5,30,paste("n =", length(area_sce2b),sep=" "))
  
  dev.off()
  
# now plot closures that fall in the 400-800 meter depth range
  # run producing figures and tables up to fig 1
  jpeg(file = paste(outdir,"Figure_map_closures.jpeg",sep="/"), width=fig_width*2, height=fig_length*0.8,units ='in', res = 300)
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
 
  figmap_sce1 <- figmap + geom_polygon(data= sce1, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
                  ggtitle("scenario 1")
  figmap_sce2a <- figmap + geom_polygon(data= sce2a, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
    ggtitle("scenario 2 - option 1")
  figmap_sce2b <- figmap + geom_polygon(data= sce2b, aes(x = long, y = lat, group = group),color="orange",fill="orange") +
    ggtitle("scenario 2 - option 2")

  print(grid.arrange(figmap_sce1, figmap_sce2a,figmap_sce2b, nrow = 1))
  
  dev.off() 
  

### now make table to calculate overlap
  
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
  afteryear <- 2012:2019
  metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                    "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                    "OT_MIX_DMF_BEN","OT_SPF")
  metier_static <- c("Static","Static_FPO","Static_GNS","Static_LLS") 
  
  # SAR trawling in ref period
  nam <- paste("SAR_total",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$refSAR <- rowMeans(vmsreg[indexcol])
  
  depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("refSAR")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "refSAR"
  
  # Static in ref period
  nam <- paste("Static",refyear, sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$refStatic <- rowSums(vmsreg[indexcol])
  vmsreg$refStatic[vmsreg$refStatic > 0] <- 1
  
  depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("refStatic")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "refStatic"
  
  # SAR trawling in ref period
  nam <- paste("SAR_total",c(refyear, afteryear),sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$threshold <- rowMeans(vmsreg[indexcol])
  vmsreg$threshold <- ifelse(vmsreg$threshold > 0.43, 1, 0)
  
  depthreg <- cbind(depthreg, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("threshold")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "threshold"
  depthreg@data$threshold[is.na(depthreg@data$threshold)] <- 0
  
  
  # get core fishing ground otter trawling
  # get region within 400-800 meter
  IREG <- subset(depthreg@data,depthreg@data$within == 1)
  IREG$EEZ <- factor(IREG$EEZ)
  
  # use code from figure 8
  fig8 <- IREG
  nam <- paste("SAR_Otter",refyear,sep="_")
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
  fig8 <- cbind(fig8, vmsreg[match(fig8$csquares,vmsreg$c_square), c("otrefyear")])
  colnames(fig8)[ncol(fig8)] <- "Otter_intensity"
  fig8$Otter_intensity[is.na(fig8$Otter_intensity)] <- 0
  fig8 <- subset(fig8,fig8$Otter_intensity > 0)
  fig8 <- fig8[order(fig8$Otter_intensity),]
  fig8$perc <- cumsum(fig8$Otter_intensity) / sum(fig8$Otter_intensity)*100
  quat <- c(0, 10,  100)
  fig8$cat <- cut(fig8$perc,c(quat))

  depthreg <- cbind(depthreg, fig8[match(depthreg@data$csquares,fig8$csquares), c("cat")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "core_area"
  
# calculate fraction of area in fishing footprint
  clos1 <- over(depthreg,sce1)
  depthreg@data$clos1 <- clos1[1:nrow(clos1),1]
  depthreg@data$clos1[!(is.na(depthreg@data$clos1))]  <- 1 
  depthreg@data$clos1[is.na(depthreg@data$clos1)] <- 0 

  clos2a <- over(depthreg,sce2a)
  depthreg@data$clos2a <- clos2a[1:nrow(clos2a),1]
  depthreg@data$clos2a[!(is.na(depthreg@data$clos2a))]  <- 1 
  depthreg@data$clos2a[is.na(depthreg@data$clos2a)] <- 0 
  
  clos2b <- over(depthreg,sce2b)
  depthreg@data$clos2b <- clos2b[1:nrow(clos2b),1]
  depthreg@data$clos2b[!(is.na(depthreg@data$clos2b))]  <- 1 
  depthreg@data$clos2b[is.na(depthreg@data$clos2b)] <- 0 
  
# make table per row
  depthwithin <- subset(depthreg@data, depthreg@data$within == 1)

  tablenew <- data.frame(matrix(data=NA,nrow = 22, ncol= 7))
  
  # vme habitat / index closed 
  # scenario 1
  tablenew [4,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$VME_Class == 3)) 
  tablenew [5,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$VME_Class == 2))
  tablenew [6,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$VME_Class == 1))
  tablenew [7,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$VME_Class == 0))
  
  tablenew [4,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$VME_Class == 3)) 
  tablenew [5,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$VME_Class == 2))
  tablenew [6,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$VME_Class == 1))
  tablenew [7,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$VME_Class == 0))
  
  # scenario 2a
  tablenew [4,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 3)) 
  tablenew [5,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 2))
  tablenew [6,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 1))
  tablenew [7,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$VME_Class == 0))
  
  tablenew [4,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 3)) 
  tablenew [5,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 2))
  tablenew [6,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 1))
  tablenew [7,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$VME_Class == 0))
  
  # scenario 2b
  tablenew [4,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 3)) 
  tablenew [5,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 2))
  tablenew [6,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 1))
  tablenew [7,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$VME_Class == 0))
  
  tablenew [4,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 3)) 
  tablenew [5,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 2))
  tablenew [6,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 1))
  tablenew [7,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$VME_Class == 0))
  
  # vme habitat /index closed and below threshold
  tablenew [10,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))
  tablenew [10,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))
  tablenew [10,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))
  tablenew [10,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))
  tablenew [10,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))
  tablenew [10,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$threshold == 0 & !(is.na(depthwithin$VME_Class))))

  # vme habitat /index closed and above threshold
  tablenew [11,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))
  tablenew [11,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))
  tablenew [11,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))
  tablenew [11,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))
  tablenew [11,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))
  tablenew [11,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$threshold == 1 & !(is.na(depthwithin$VME_Class))))

  # c-squares part of fishing footprint
  tablenew [14,2] <- length(which(depthwithin$clos1 == 1 & depthwithin$Footprint == 1 ))
  tablenew [14,3] <- length(which(depthwithin$clos1 == 0 & depthwithin$Footprint == 1 ))
  tablenew [14,4] <- length(which(depthwithin$clos2a == 1 & depthwithin$Footprint == 1 ))
  tablenew [14,5] <- length(which(depthwithin$clos2a == 0 & depthwithin$Footprint == 1 ))
  tablenew [14,6] <- length(which(depthwithin$clos2b == 1 & depthwithin$Footprint == 1 ))
  tablenew [14,7] <- length(which(depthwithin$clos2b == 0 & depthwithin$Footprint == 1 ))
  
  # c-squares part of static gears present
  footp_only <- subset(depthwithin, depthwithin$Footprint == 1)
  
  tablenew [15,2] <- length(which(footp_only$clos1 == 1 & footp_only$refStatic == 1))
  tablenew [15,3] <- length(which(footp_only$clos1 == 0 & footp_only$refStatic == 1))
  tablenew [15,4] <- length(which(footp_only$clos2a == 1 & footp_only$refStatic == 1))
  tablenew [15,5] <- length(which(footp_only$clos2a == 0 & footp_only$refStatic == 1))
  tablenew [15,6] <- length(which(footp_only$clos2b == 1 & footp_only$refStatic == 1))
  tablenew [15,7] <- length(which(footp_only$clos2b == 0 & footp_only$refStatic == 1))

  # c-squares part of SAR gears present
  tablenew [16,2] <- length(which(footp_only$clos1 == 1 & footp_only$refSAR > 0))
  tablenew [16,3] <- length(which(footp_only$clos1 == 0 & footp_only$refSAR > 0))
  tablenew [16,4] <- length(which(footp_only$clos2a == 1 & footp_only$refSAR > 0))
  tablenew [16,5] <- length(which(footp_only$clos2a == 0 & footp_only$refSAR > 0))
  tablenew [16,6] <- length(which(footp_only$clos2b == 1 & footp_only$refSAR > 0))
  tablenew [16,7] <- length(which(footp_only$clos2b == 0 & footp_only$refSAR > 0))
  
  # core footprint based on SAR
  tablenew [19,2] <- length(which(footp_only$clos1 == 1 & footp_only$core_area == "(10,100]"))
  tablenew [19,3] <- length(which(footp_only$clos1 == 0 & footp_only$core_area == "(10,100]"))
  tablenew [19,4] <- length(which(footp_only$clos2a == 1 & footp_only$core_area == "(10,100]"))
  tablenew [19,5] <- length(which(footp_only$clos2a == 0 & footp_only$core_area == "(10,100]"))
  tablenew [19,6] <- length(which(footp_only$clos2b == 1 & footp_only$core_area == "(10,100]"))
  tablenew [19,7] <- length(which(footp_only$clos2b == 0 & footp_only$core_area == "(10,100]"))
  
  # fraction of SAR in closed area
  tablenew [21,2] <- as.character(round(sum(footp_only$refSAR[footp_only$clos1 == 1]) / sum(footp_only$refSAR),digits = 2))
  tablenew [21,3] <- as.character(round(sum(footp_only$refSAR[footp_only$clos1 == 0]) / sum(footp_only$refSAR),digits = 2))
  tablenew [21,4] <- as.character(round(sum(footp_only$refSAR[footp_only$clos2a == 1]) / sum(footp_only$refSAR),digits = 2))
  tablenew [21,5] <- as.character(round(sum(footp_only$refSAR[footp_only$clos2a == 0]) / sum(footp_only$refSAR),digits = 2))
  tablenew [21,6] <- as.character(round(sum(footp_only$refSAR[footp_only$clos2b == 1]) / sum(footp_only$refSAR),digits = 2))
  tablenew [21,7] <- as.character(round(sum(footp_only$refSAR[footp_only$clos2b == 0]) / sum(footp_only$refSAR),digits = 2))
  
tablenew [,1] <- c("","","VME protection","nb of c-squares with VME habitat","nb of c-squares with VME index high",
                       "nb of c-squares with VME index medium","nb of c-squares with VME index low",
                       "","VME protection and fishing impact threshold","nb of c-squares with VME habitat/index below SAR 0.43 threshold (2009-2019)",
                       "nb of c-squares with closed VME habitat/index above SAR 0.43 threshold (2009-2019)",
                       "","Fisheries consequences (total footprint)",
                       "nb of c-squares part of fishing footprint (all gears) (2009-2011)",
                       "nb of c-squares with static bottom fishing (present) (2009-2011)",
                       "nb of c-squares with mobile bottom fishing (SAR > 0) (2009-2011)",
                       "","Fisheries consequences (core fishing ground)",
                       "nb of c-squares that form core fishing area based on SAR (2009-2011) (method 1)",
                       "nb of c-squares that form core fishing area based on kw/h fished (2009-2011) (method 2)",
                       "fraction of total SAR (2009-2011)",
                       "fraction of total kw/h (2009-2011)")
tablenew[1,] <- c("","Scenario 1","","Scenario 2 option 1","","Scenario 2 option 2","")
tablenew[2,] <- c("","within closure","outside closures","within closure","outside closures","within closure","outside closures")
tablenew[3,2:7] <- c("","","","","","")
tablenew[8,] <- c("","","","","","","")
tablenew[9,2:7] <- c("","","","","","")
tablenew[12,] <- c("","","","","","","")
tablenew[13,2:7] <- c("","","","","","")
tablenew[17,] <- c("","","","","","","")
tablenew[18,2:7] <- c("","","","","","")

tablenew <- data.frame(tablenew)
write.csv(tablenew, paste(outdir,"Table_closure_options.csv",sep="/"), 
          row.names = FALSE, quote=FALSE) 
