
# plot closures and analyse effect of closures on fisheries based on table 4

outdir <- paste(pathdir,"5-Output",EcoReg,sep="/") 

# get table 4
tab4 <- read.csv(paste(pathdir,"5-Output",EcoReg,"Table_closure_options.csv",sep="/"),
                header=T,sep=",",row.names = NULL)

colnames(tab4) <- c("Metric","s1o1_in_closure", "s1o1_ou_closure" ,"s1o2_in_closure" ,"s1o2_ou_closure", 
                    "s2o1_in_closure", "s2o1_ou_closure", "s2o2_in_closure" ,"s2o2_ou_closure")
tab4 <- tab4[-c(1,2,3,8,9,12,13,15,16,19,20,23,24,27,28,31,32,35,36),]

tab4$Theme <- c("VME protection", "VME protection", "VME protection", "VME protection",
                "VME protection and fishing impact threshold", "VME protection and fishing impact threshold", 
                "Fisheries footprint", "Fisheries overlap (presence/absence) (2009-2011)" , 
                "Fisheries overlap (presence/absence) (2009-2011)", "Fisheries overlap (core fishing ground) (2009-2011)",
                "Fisheries overlap (core fishing ground) (2009-2011)", "Fisheries consequences (presence/absence) (2012-2015)",   
                "Fisheries consequences (presence/absence) (2012-2015)",  "Fisheries consequences (core fishing ground) (2012-2015)",
                "Fisheries consequences (core fishing ground) (2012-2015)", "Fisheries consequences (presence/absence) (2016-2018)",   
                "Fisheries consequences (presence/absence) (2016-2018)",  "Fisheries consequences (core fishing ground) (2016-2018)",
                "Fisheries consequences (core fishing ground) (2016-2018)")
tab4$Group <- c("A","A","A","A","B","B","B", "C", "C", "D" ,"D" ,"E" ,"E", "F", "F", "G", "G", "H", "H")

for (i in 2:9){
  tab4[,i] <- as.numeric(as.character(tab4[,c(i)]))
}

tab4$tot <- tab4$s1o1_in_closure + tab4$s1o1_ou_closure


tab4$Metric <- c("VME habitat", "VME index  high", "VME index  medium","VME index low",
                 "VME habitat/index below SAR 0.43","VME habitat/index above SAR 0.43",
                 "Part of fishing footprint","Static bottom fishing gears present","Mobile bottom gear fishing (SAR > 0)",
                 "Core fishing area C-squares","Fraction of total SAR","Static bottom fishing gears present",
                 "Mobile bottom gear fishing (SAR > 0)","Core fishing area C-squares","Fraction of total SAR",
                 "Static bottom fishing gears present","Mobile bottom gear fishing (SAR > 0)",
                 "Core fishing area C-squares","Fraction of total SAR")

# make figure Lenaick

x1 = tab4

### sum of c-square per metric
In = x1[,str_which(colnames(x1), "in")]
percent = In/x1$tot*100
x2 = cbind(x1[,c(11,10,1)], percent)
toplot = x2 %>%
  pivot_longer(-c(1:3), names_to = "scenario", values_to = "Percentage")

#Full plot
#ggplot(data=toplot, aes(x=Metric, y=Percentage, fill=scenario)) +
#  geom_bar(stat="identity", position=position_dodge())+
#  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))

# Plots per thematics
A = ggplot(data=toplot[which(toplot$Group == "A"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("a) VME Protection") + xlab("") + ylab("% C-squares")+
  scale_x_discrete(labels=c("VME habitat","VME index \n high","VME index \n medium","VME index \n low")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))

B = ggplot(data=toplot[which(toplot$Group == "B"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("b) VME protection and fishing impact threshold") + xlab("")+ ylab("% C-squares")+
  scale_x_discrete(labels=c("Part of fishing \n footprint","Closed VME \n habitat/index \n above SAR 0.43","Closed VME \n habitat/index \n below SAR 0.43")) +
  theme(legend.position = c(0.2, 0.7)) + theme(plot.title = element_text(size=9)) +
  theme(legend.text= element_text(size=8)) + theme(legend.title = element_text(size=8))

C = ggplot(data=toplot[which(toplot$Group == "C"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("c) Fisheries overlap (presence/absence) (2009-2011)") + xlab("")+ ylab("% C-squares")+
  scale_x_discrete(labels=c("Mobile bottom gear \n fishing (SAR >0)","Static bottom fishing \n gears present")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))
  
D = ggplot(data=toplot[which(toplot$Group == "D"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("d) Fisheries overlap (core fishing ground) (2009-2011)") + xlab("")+ ylab("%")+
  scale_x_discrete(labels=c("Fraction of total SAR","Core fishing area \n C-squares")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))

E = ggplot(data=toplot[which(toplot$Group == "E"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("e) Fisheries consequences (presence/absence) (2012-2015)") + xlab("") + ylab("% C-squares")+
  scale_x_discrete(labels=c("Mobile bottom gear \n fishing (SAR >0)","Static bottom fishing \n gears present")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))

F1 = ggplot(data=toplot[which(toplot$Group == "F"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("f) Fisheries consequences (core fishing ground) (2012-2015)") + xlab("") + ylab("%")+
  scale_x_discrete(labels=c("Fraction of total SAR","Core fishing area \n C-squares")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))

G = ggplot(data=toplot[which(toplot$Group == "G"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("g) Fisheries consequences (presence/absence) (2016-2018)") + xlab("") + ylab("% C-squares")+
  scale_x_discrete(labels=c("Mobile bottom gear \n fishing (SAR >0)","Static bottom fishing \n gears present")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))

H = ggplot(data=toplot[which(toplot$Group == "H"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("h) Fisheries consequences (core fishing ground) (2016-2018)") + xlab("") + ylab("%")+
  scale_x_discrete(labels=c("Fraction of total SAR","Core fishing area \n C-squares")) +
  theme(legend.position = "none") + theme(plot.title = element_text(size=9))


jpeg(file = paste(outdir,"Figure_barplot.jpeg",sep="/"), width=16, height=6,units ='in', res = 300)
print(grid.arrange(A,B,C,D,E,F1,G,H, nrow = 2))
dev.off() 

### make figures Jan

# get table 4
tab4 <- read.csv(paste(pathdir,"5-Output",EcoReg,"Table_closure_options.csv",sep="/"),
                 header=T,sep=",",row.names = NULL)

rown <- tab4[2,2:9]
tab4 <- tab4[-c(1,2,3,8,9,12,13,15,16,19,20,23,24,27,28,31,32,35,36),]
coln <- tab4[,1]
tab4 <- tab4[,2:9]

tab4[] <- lapply(tab4, function(x) {
            if(is.factor(x)) as.numeric(as.character(x)) else x
        })
new <- t(as.matrix(tab4))
colnames(new) <- coln
new <- data.frame(new)

head(new)
new$totalVME <- rowSums(new[,1:4])
new$VMEhighq <- rowSums(new[,1:3])

if (EcoReg == "Celtic Seas"){
  yl1min <- 150; yl1max <- 270
  xl1min <- 0; xl1max <- 0.15
  yl2min <- 100; yl2max <- 200
  xl2min <- 0; xl2max <- 1000
  xl3min <- 500; xl3max <- 1200
  move <- 5; move2 <- 4
  
} else{
  yl1min <- 45; yl1max <- 65
  xl1min <- 0; xl1max <- 0.15
  yl2min <- 35; yl2max <- 50
  xl2min <- 0; xl2max <- 500
  xl3min <- 200; xl3max <- 350
  move <- 1; move2 <- 1
  }

labels=c("1-1","1-2","2-1","2-2")

jpeg(file = paste(outdir,"Scatterplots_1.jpeg",sep="/"), width=10, height=5,units ='in', res = 300)
par(mfrow=c(1,2),mar=c(4.5,6,1,1),xaxs="i",yaxs="i")
plot((new[c(1,3,5,7),"totalVME"])~new$fraction.of.total.SAR[c(1,3,5,7)],col="darkgreen",pch=1:4,ylab=
      "Number of c-squares with VME in closures \n (habitats, high, medium, low) ",las=1,cex=2,xlim=c(xl1min,xl1max),
      ylim=c(yl1min,yl1max),xlab="Fraction of SAR in closures")
points((new[c(1,3,5,7),"totalVME"])~new$fraction.of.total.SAR.1[c(1,3,5,7)],col="blue",pch=1:4,cex=2)                                 
points(new[c(1,3,5,7),"totalVME"]~new$fraction.of.total.SAR.2[c(1,3,5,7)],col="red",pch=1:4,cex=2)   

loc <- (new[c(1,3,5,7),"totalVME"])-move
loc <- c(loc[1],loc[2]-move2,loc[3],loc[4])
text(y=loc,x=colMeans(rbind(
      new$fraction.of.total.SAR[c(1,3,5,7)],
      new$fraction.of.total.SAR.1[c(1,3,5,7)],
      new$fraction.of.total.SAR.2[c(1,3,5,7)])),labels=labels)
legend(title="Period",legend=c("2016-2018","2012-2015","2009-2011"),col=c("red","blue","darkgreen"),pch=19,cex=1,"topleft", bty = "n")
legend(title="Scenario",legend=c(labels),pch=1:4,cex=1,"topright", bty = "n")
mtext("A)",side=3,outer=T,adj=0,line=-1) 
mtext("B)", side = 3, line = -1, outer = T)
box()

plot((new[c(1,3,5,7),"VMEhighq"])~new$fraction.of.total.SAR[c(1,3,5,7)],col="darkgreen",pch=1:4,ylab=
       "Number of c-squares with VME in closures \n (habitats, high, medium) ",las=1,cex=2,xlim=c(xl1min,xl1max),ylim=c(yl2min,yl2max),xlab="Fraction of SAR in closures")
points((new[c(1,3,5,7),"VMEhighq"])~new$fraction.of.total.SAR.1[c(1,3,5,7)],col="blue",pch=1:4,cex=2)                                 
points(new[c(1,3,5,7),"VMEhighq"]~new$fraction.of.total.SAR.2[c(1,3,5,7)],col="red",pch=1:4,cex=2)   

loc <- (new[c(1,3,5,7),"VMEhighq"])-move
loc <- c(loc[1]+move+move2,loc[2]-move2,loc[3],loc[4])
text(y=loc,x=colMeans(rbind(
  new$fraction.of.total.SAR[c(1,3,5,7)],
  new$fraction.of.total.SAR.1[c(1,3,5,7)],
  new$fraction.of.total.SAR.2[c(1,3,5,7)]))
  ,labels=labels)
legend(title="Period",legend=c("2016-2018","2012-2015","2009-2011"),col=c("red","blue","darkgreen"),pch=19,cex=1,"topleft", bty = "n")
legend(title="Scenario",legend=c(labels),pch=1:4,cex=1,"topright", bty = "n")
box()

dev.off()

jpeg(file = paste(outdir,"Scatterplots_2.jpeg",sep="/"), width=10, height=5,units ='in', res = 300)
par(mfrow=c(1,2),mar=c(4.5,6,1,1),xaxs="i",yaxs="i")
plot((new[c(1,3,5,7),"totalVME"])~new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0.[c(1,3,5,7)],col="darkgreen",pch=1:4,ylab=
      "Number of c-squares with VME in closures \n (habitats, high, medium, low) ",las=1,cex=2,xlim=c(xl2min,xl2max),ylim=c(yl1min,yl1max),
      xlab="Closed C-squares with mobile fishing (SAR>0)")
points((new[c(1,3,5,7),"totalVME"])~new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0..1[c(1,3,5,7)],col="blue",pch=1:4,cex=2)                                 
points(new[c(1,3,5,7),"totalVME"]~new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0..2[c(1,3,5,7)],col="red",pch=1:4,cex=2)   

loc <- (new[c(1,3,5,7),"totalVME"])-move
loc <- c(loc[1],loc[2]-move2,loc[3],loc[4])
text(y=loc,x=colMeans(rbind(
  new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0[c(1,3,5,7)],
  new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0..1[c(1,3,5,7)],
  new$nb.of.c.squares.with.mobile.bottom.fishing..SAR...0..2[c(1,3,5,7)]))
  ,labels=labels)
legend(title="Period",legend=c("2016-2018","2012-2015","2009-2011"),col=c("red","blue","darkgreen"),pch=19,cex=1,"topleft", bty = "n")
legend(title="Scenario",legend=c(labels),pch=1:4,cex=1,"topright", bty = "n")
mtext("A)",side=3,outer=T,adj=0,line=-1) 
mtext("B)", side = 3, line = -1, outer = T)
box()

plot((new[c(1,3,5,7),"totalVME"])~new$nb.of.c.squares.with.static.bottom.fishing..present.[c(1,3,5,7)],col="darkgreen",pch=1:4,ylab=
     "Number of c-squares with VME in closures \n (habitats, high, medium, low) ",las=1,cex=2,xlim=c(xl2min,xl2max),ylim=c(yl1min,yl1max),
     xlab="Closed C-squares with static bottom fishing")
points((new[c(1,3,5,7),"totalVME"])~new$nb.of.c.squares.with.static.bottom.fishing..present..1[c(1,3,5,7)],col="blue",pch=1:4,cex=2)                                 
points(new[c(1,3,5,7),"totalVME"]~new$nb.of.c.squares.with.static.bottom.fishing..present..2[c(1,3,5,7)],col="red",pch=1:4,cex=2)   

loc <- (new[c(1,3,5,7),"totalVME"])-move
loc <- c(loc[1],loc[2]-move2,loc[3],loc[4])
text(y=loc,x=colMeans(rbind(
  new$nb.of.c.squares.with.static.bottom.fishing..present.[c(1,3,5,7)],
  new$nb.of.c.squares.with.static.bottom.fishing..present..1[c(1,3,5,7)],
  new$nb.of.c.squares.with.static.bottom.fishing..present..2[c(1,3,5,7)]))
  ,labels=labels)
legend(title="Period",legend=c("2016-2018","2012-2015","2009-2011"),col=c("red","blue","darkgreen"),pch=19,cex=1,"topleft", bty = "n")
legend(title="Scenario",legend=c(labels),pch=1:4,cex=1,"topright", bty = "n")
mtext("A)",side=3,outer=T,adj=0,line=-1) 
mtext("B)", side = 3, line = -1, outer = T)
box()

dev.off()


jpeg(file = paste(outdir,"Scatterplots_3.jpeg",sep="/"), width=5, height=5,units ='in', res = 300)
par(mfrow=c(1,1),mar=c(4.5,6,1,1),xaxs="i",yaxs="i")
plot((new[c(1,3,5,7),"totalVME"])~new$nb.of.c.squares.part.of.fishing.footprint[c(1,3,5,7)],col="black",pch=1:4,ylab=
       "Number of c-squares with VME in closures \n (habitats, high, medium, low) ",las=1,cex=2,xlim=c(xl3min,xl3max),ylim=c(yl1min,yl1max),
     xlab="C-squares in closure that are part of fishing footprint")

loc <- (new[c(1,3,5,7),"totalVME"])-move
text(y=loc,x=colMeans(rbind(new$nb.of.c.squares.part.of.fishing.footprint[c(1,3,5,7)])) ,labels=labels)
legend(title="Scenario",legend=c(labels),pch=1:4,cex=1,"topright", bty = "n")
box()

dev.off()