
shapeEEZ <- readOGR(dsn = paste(pathdir,"1-Input data/EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410") 
shapeEcReg <- readOGR(dsn = paste(pathdir,"1-Input data/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeReg  <- subset(shapeEcReg, Ecoregion== EcoReg)

outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

#### fig 1
setwd(paste(pathdir,"3-Data analysis",EcoReg,sep="/")) 
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

Footprint$temp <- Footprint$MBCG_footprint

jpeg(file = paste(outdir,"Figure_MBCGfootprint.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_point(data=Footprint, aes(x=long, y=lat , col=as.factor(temp)),
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

Footprint$temp <- Footprint$Static_footprint

jpeg(file = paste(outdir,"Figure_Staticfootprint.jpeg",sep="/"), width=fig_width, height=fig_length,units ='in', res = 300)
figmap <- ggplot() + geom_point(data=Footprint, aes(x=long, y=lat , col=as.factor(temp)),
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
