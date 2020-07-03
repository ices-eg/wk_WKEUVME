
xmi <- -15
xma <- -4
ymi <- 40
yma <- 46

sub <- crop(Footprint, extent(xmi , xma, ymi, yma))

# get boundaries of ecoregion used for all plots
minlong <- xmi + 1
maxlong <- xma -1
minlat  <- ymi +1
maxlat  <- yma -1
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

jpeg(file = paste(outdir,"Figure_raster.jpeg",sep="/"), width=7.5, height=5,units ='in', res = 300)
figmap <- ggplot() + geom_polygon(data=sub, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="grey")
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
#figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))
#figmap2 <- figmap +  geom_polygon(data = shapeEEZ, aes(x = long, y = lat, group = group),color="grey",fill=NA)
figmap2 <- figmap +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)

print(figmap2)
dev.off() 
