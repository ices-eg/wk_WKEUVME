
#### code to produce figures and tables per ecoregion

  setwd(paste(pathdir,"3-Data analysis",EcoReg,sep="/")) 
  outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

  shapeEEZ <- readOGR(dsn = paste(pathdir,"1-Input data/EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410") 
  shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  shapeReg  <- subset(shapeEcReg, Ecoregion== EcoReg)
  
#### fig 1
  fig1 <- readRDS(file = "fig1.rds")

  # Get the world map
  worldMap <- map_data("world")
  
  # get boundaries of ecoregion used for all plots
  minlong <- round(min(fig1$long)-0.1,digits = 0)
  maxlong <- round(max(fig1$long)+ 0.1,digits = 0)
  minlat  <- round(min(fig1$lat)- 0.1,digits = 0)
  maxlat  <- round(max(fig1$lat)+ 0.1,digits = 0)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  # plotting specifics
  pointsize <- 0.5
  fig_width  <- (maxlong-minlong)/2.5
  fig_length <- (maxlat-minlat)/2
  
  jpeg(file = paste(outdir,"Figure_1_depth.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)

  figmap <- ggplot() + geom_point(data=fig1, aes(x=long, y=lat , col=as.factor(within)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = c("white","darkblue"),name ="",labels=c("","depth 400-800 m",""))      
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
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 

#### table 2
  tab2 <- readRDS(file = "tab2.rds")
  write.csv(tab2, paste(outdir,"Table_2.csv",sep="/"))
  
#### table 3
  tab3 <- readRDS(file = "tab3.rds")
  tab3 <- cbind(tab3[[1]],tab3[[2]])
  tab3 <- tab3[c(1:5,12,6:11),]
  write.csv(tab3, paste(outdir,"Table_3.csv",sep="/"))
 
#### figure 6
  fig6 <- readRDS(file = "fig6.rds") 
  
  jpeg(file = paste(outdir,"Figure_6.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig6, aes(x=long, y=lat , col=as.factor(ref)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = c("#bcbddc","#54278f"),name ="",labels=c("unfished","fished"))      
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
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
  
#### figure 7
  fig7 <- readRDS(file = "fig7.rds") 
  
  jpeg(file = paste(outdir,"Figure_7a.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig7, aes(x=long, y=lat , col=as.factor(ref_cat)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = c("#bcbddc","#54278f"),name ="",labels=c("unfished","fished"))      
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
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
  
  jpeg(file = paste(outdir,"Figure_7b.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig7, aes(x=long, y=lat , col=as.factor(ref_vs_after)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = c("#bcbddc","#54278f","#b30000","#fcae91"),name ="",
                                         labels=c("unfished (both periods)","only fished in 2009-11",
                                                  "only fished in 2012-18","fished (both periods)"))      
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
                            legend.position = "bottom") +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap <- figmap + guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))

  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
 
#### figure 8
  fig8a <- readRDS(file = "fig8a.rds") 
  
  # SAR of otter trawl in range 
  jpeg(file = paste(outdir,"Figure_8a.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  nnew <- sort(fig8a)
  plot(log10(nnew+1), xlab="C-squares within 400-800m", ylab="SAR (avg. 2009-2011)",las=1, yaxt="n",xaxt="n")  
  axis(1,c(0,round(length(nnew)/2,digits = 0),length(nnew)))
  axis(2,c(0,log10(1+1),log10(10+1)),c(0,1,10),las=1)
  id1 <- which(abs(nnew-0)==min(abs(nnew-0)))
  id1 <- id1[length(id1)]
  id2 <- which(abs(nnew-0.5)==min(abs(nnew-0.5)))
  id3 <- which(abs(nnew-2)==min(abs(nnew-2)))
  lines(c(id1,id1),c(-1,log10(0+1)),lwd=2,lty=5)
  lines(c(id2,id2),c(-1,log10(0.5+1)),lwd=2,lty=5) 
  lines(c(id3,id3),c(-1,log10(2+1)),lwd=2,lty=5) 
  text(id1-300,log10(0.2+1),"unfished")
  text(id2-300,log10(0.5 +1),"low")
  text(id3-300,log10(2 +1),"medium")
  text(id3+100,log10(6 +1),"high")
  dev.off()
  
  fig8b <- readRDS(file = "fig8b.rds") 
  
  jpeg(file = paste(outdir,"Figure_8b.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig8b, aes(x=long, y=lat , col=as.factor(Otter_cat)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = c("#253494","#a1dab4","#41b6c4","#ffffcc"),name ="Otter trawl intensity:")      
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
                            legend.position = "bottom") +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap <- figmap + guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
  
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
  

#### table 4
  tab4 <- readRDS(file = "tab4.rds")
  colnames(tab4) <- c("Region","VME presence","Precaution low -- fished","Precaution low -- total",
                      "Precaution medium -- fished", "Precaution medium -- total",
                      "Precaution high -- fished","Precaution high -- total")
  write.csv(tab4, paste(outdir,"Table_4.csv",sep="/")) 
  
  
#### Figure 9
  source(paste(pathdir,"4-Code for figures and tables/Figure9_source_restricted.R",sep="/"))
  
  # reset the paths
  setwd(paste(pathdir,"3-Data analysis",EcoReg,sep="/")) 
  outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 
  
  fig9$category[fig9$category =="none_0"] <- NA
  fig9$category[fig9$category =="none_1"] <- NA
  fig9 <- subset(fig9,!(is.na(fig9$category)))
  
  colfig9 <- c("#238b45","#bae4b3", "#e31a1c","#fdbe85","#2171b5","#bdd7e7")
  
  jpeg(file = paste(outdir,"Figure_9.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig9, aes(x=long, y=lat , col=as.factor(category)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = colfig9,name ="Category:",
                                         labels=c("high - unfished","high - fished","low - unfished","low - fished",
                                                  "medium - unfished", "medium - fished"))      
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
                            legend.position = "bottom") +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap <- figmap + guides(colour = guide_legend(override.aes = list(size=5),nrow=3,byrow=TRUE))
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
  
#### Figure 10
  fig10 <- readRDS(file = "fig10.rds")

  colfig10 <- c("#bae4b3", "#238b45", "#fdbe85","#e31a1c","#bdd7e7","#2171b5")
  
  jpeg(file = paste(outdir,"Figure_10.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
  figmap <- ggplot() + geom_point(data=fig10, aes(x=long, y=lat , col=as.factor(category)),
                                  shape=15,size=0.5,na.rm=T) 
  figmap <- figmap +  scale_color_manual(values = colfig10,name ="Category:",
                                         labels=c("low intensity - no VME","low intensity - VME",
                                                  "med/high int. - no VME","med/high int. - VME",
                                                  "unfished - no VME", "unfished - VME"))      
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
                            legend.position = "bottom") +
    scale_x_continuous(breaks=coordxmap, name = "longitude") +
    scale_y_continuous(breaks=coordymap, name = "latitude")  +
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap <- figmap + guides(colour = guide_legend(override.aes = list(size=5),nrow=3,byrow=TRUE))
  
  figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
  figmap2 <- figmap2 +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
  print(figmap2)
  dev.off() 
  
  rm(list=setdiff(ls(), c("pathdir" , "EcoReg")))