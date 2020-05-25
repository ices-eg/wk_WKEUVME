
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
  scenario2b <- st_read(paste(pathdir_nogit,"Closure options/VME_buffer_Scenario2_Opt2_2020May22_v1.shp",sep="/"))
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
  
  
### now estimate overlap, download fishing footprint
  footprint_path <- paste(pathdir,"3-Data analysis",EcoReg,sep = "/")
  footp <- readRDS(paste(footprint_path,"fig6b.rds",sep="/"))
  depthreg <- cbind(depthreg, footp[match(depthreg@data$csquares,footp$csquares), c("scenario_2")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "Footprint"
  
  # get VME
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME weighted csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  
  depthreg <- cbind(depthreg, VME[match(depthreg@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(depthreg@data)[ncol(depthreg@data)] <- "VME_Class"
  
  # get vms data
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))
  
  # define few params
  refyear <- 2009:2011
  afteryear <- 2012:2019
  metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                    "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                    "OT_MIX_DMF_BEN","OT_SPF")
  metier_static <- "Static"
  
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

  # vme habitat / index closed
  table(depthwithin$VME_Class[depthwithin$clos1 == 1])  
  table(depthwithin$VME_Class[depthwithin$clos1 == 0])
  table(depthwithin$VME_Class[depthwithin$clos2a == 1])  
  table(depthwithin$VME_Class[depthwithin$clos2a == 0])
  table(depthwithin$VME_Class[depthwithin$clos2b == 1])  
  table(depthwithin$VME_Class[depthwithin$clos2b == 0])
  
  # vme habitat /index closed and below threshold
  sum(table(depthwithin$VME_Class[depthwithin$clos1 == 1 & depthwithin$threshold == 0]))
  sum(table(depthwithin$VME_Class[depthwithin$clos1 == 0 & depthwithin$threshold == 0]))
  
  sum(table(depthwithin$VME_Class[depthwithin$clos2a == 1 & depthwithin$threshold == 0]))
  sum(table(depthwithin$VME_Class[depthwithin$clos2a == 0 & depthwithin$threshold == 0]))
  
  sum(table(depthwithin$VME_Class[depthwithin$clos2b == 1 & depthwithin$threshold == 0]))
  sum(table(depthwithin$VME_Class[depthwithin$clos2b == 0 & depthwithin$threshold == 0]))
  
  # vme habitat /index closed and above threshold
  sum(table(depthwithin$VME_Class[depthwithin$clos1 == 1 & depthwithin$threshold == 1]))
  sum(table(depthwithin$VME_Class[depthwithin$clos1 == 0 & depthwithin$threshold == 1]))
  
  sum(table(depthwithin$VME_Class[depthwithin$clos2a == 1 & depthwithin$threshold == 1]))
  sum(table(depthwithin$VME_Class[depthwithin$clos2a == 0 & depthwithin$threshold == 1]))
  
  sum(table(depthwithin$VME_Class[depthwithin$clos2b == 1 & depthwithin$threshold == 1]))
  sum(table(depthwithin$VME_Class[depthwithin$clos2b == 0 & depthwithin$threshold == 1]))
  
  # c-squares part of fishing footprint
  table(depthwithin$Footprint[depthwithin$clos1 == 1 ])
  table(depthwithin$Footprint[depthwithin$clos1 == 0 ])
  table(depthwithin$Footprint[depthwithin$clos2a == 1 ])
  table(depthwithin$Footprint[depthwithin$clos2a == 0 ])
  table(depthwithin$Footprint[depthwithin$clos2b == 1 ])
  table(depthwithin$Footprint[depthwithin$clos2b == 0 ])
    
  # c-squares part of static gears present
  footp_only <- subset(depthwithin, depthwithin$Footprint == 1)
  table(footp_only$clos1[footp_only$refStatic == 1])
  table(footp_only$clos2a[footp_only$refStatic == 1 ])
  table(footp_only$clos2b[footp_only$refStatic == 1 ])
  
  # c-squares part of SAR gears present
  table(footp_only$clos1[footp_only$refSAR > 0])
  table(footp_only$clos2a[footp_only$refSAR > 0])
  table(footp_only$clos2b[footp_only$refSAR > 0])

  # c-squares part of core fishing area
  table(footp_only$clos1[footp_only$core_area == "(10,100]"])
  table(footp_only$clos2a[footp_only$core_area == "(10,100]"])
  table(footp_only$clos2b[footp_only$core_area == "(10,100]"])
  
  # fraction of SAR in closed area
  aggregate(footp_only$refSAR,by=list(footp_only$clos1),sum,na.rm=T)
  aggregate(footp_only$refSAR,by=list(footp_only$clos2a),sum,na.rm=T)
  aggregate(footp_only$refSAR,by=list(footp_only$clos2b),sum,na.rm=T)
  