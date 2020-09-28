
# run to get fig 8
outdir <- paste(pathdir,"3-Data analysis",EcoReg,sep="/") 

# get depth data
setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
depthreg <- readRDS(paste(EcoReg,"depth.rds",sep="_"))
depthreg <- subset(depthreg, !(depthreg$EEZ == "Faroe Is." | depthreg$EEZ == "Norway" |
                               depthreg$EEZ == "<NA>" | depthreg$EEZ =="Guernsey"))

# get region within 400-800 meter
IREG <- subset(depthreg@data,depthreg@data$within == 1)
IREG$EEZ <- factor(IREG$EEZ)

# get vms data
setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))

# define few params
refyear <- 2009:2011
afteryear <- 2016:2018
metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                  "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                  "OT_MIX_DMF_BEN","OT_SPF")
metier_static <- c("Static","Static_FPO","Static_GNS","Static_LLS") 


nam <- c(paste("SAR_total",refyear,sep="_"),paste(metier_static[2],refyear, sep="_"), 
         paste(metier_static[3],refyear, sep="_")
         ,paste(metier_static[4],refyear, sep="_"))

indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$ref <- rowSums(vmsreg[indexcol])
vmsreg$ref[vmsreg$ref > 0] <- 1

# combine
IREG <- cbind(IREG, vmsreg[match(IREG$csquares,vmsreg$c_square), c("ref")])
colnames(IREG)[ncol(IREG)] <- "ref"
IREG$ref[is.na(IREG$ref)] <- 0

# calculate adjacent squares to apply footprint scenario
dat <- IREG[IREG$ref==1,]
dat$long <- round(dat$long,3)
dat$lat <- round(dat$lat,3)
lonlats <- paste(dat$long, dat$lat, sep=":")

for(i in (1:dim(dat)[1])){
  
  nw  <- paste(dat$long[i]-0.05, dat$lat[i]+0.05, sep=":")
  nn  <- paste(dat$long[i]+0.00, dat$lat[i]+0.05, sep=":")
  ne  <- paste(dat$long[i]+0.05, dat$lat[i]+0.05, sep=":")
  ee  <- paste(dat$long[i]+0.05, dat$lat[i]+0.00, sep=":")
  se  <- paste(dat$long[i]+0.05, dat$lat[i]-0.05, sep=":")
  ss  <- paste(dat$long[i]+0.00, dat$lat[i]-0.05, sep=":")
  sw  <- paste(dat$long[i]-0.05, dat$lat[i]-0.05, sep=":")
  ww  <- paste(dat$long[i]-0.05, dat$lat[i]+0.00, sep=":")
  
  dat$adjacent.cells[i] <- sum(nw %in% lonlats[-i], nn %in% lonlats[-i], ne %in% lonlats[-i],
                               ee %in% lonlats[-i], se %in% lonlats[-i], ss %in% lonlats[-i],
                               sw %in% lonlats[-i], ww %in% lonlats[-i])
}
table(dat$adjacent.cells)

IREG <- merge(x = IREG, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
IREG$adjacent.cells[is.na(IREG$adjacent.cells)] <- 0

# now make figure 8
fig8 <- IREG

nam <- paste("SAR_Otter",refyear,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$otrefyear <- rowMeans(vmsreg[indexcol])  
fig8 <- cbind(fig8, vmsreg[match(fig8$csquares,vmsreg$c_square), c("otrefyear")])
colnames(fig8)[ncol(fig8)] <- "Otter_intensity"
fig8$Otter_intensity[is.na(fig8$Otter_intensity)] <- 0
fig8 <- subset(fig8,fig8$Otter_intensity > 0 & fig8$adjacent.cells > 0)
fig8 <- fig8[order(-fig8$Otter_intensity),]
fig8$perc <- cumsum(fig8$Otter_intensity) / sum(fig8$Otter_intensity)*100

quat <- seq(0, 100, by=2)
fig8$cat <- cut(fig8$perc,c(quat))
area <- aggregate(fig8$area_sqkm,list(fig8$cat),sum)
area <- cumsum(area[,2]/1000)/(sum(area[,2]/1000)) *100
area <- c(0,area)

fig8b <- fig8[order(fig8$Otter_intensity),]
fig8b$perc2 <- cumsum(fig8b$Otter_intensity) / sum(fig8b$Otter_intensity)*100

quat2 <- c(0, 10,  100)
fig8b$cat2 <- cut(fig8b$perc2,c(quat2))

quat3 <- seq(0, 100, by=10)
fig8b$cat3 <- cut(rev(fig8b$perc2),c(quat3))

# do similar for 2016-2018 period
fig8d <- IREG
nam <- paste("SAR_Otter",afteryear,sep="_")
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$otafteryear <- rowMeans(vmsreg[indexcol],na.rm = T)  
fig8d <- cbind(fig8d, vmsreg[match(fig8d$csquares,vmsreg$c_square), c("otafteryear")])
colnames(fig8d)[ncol(fig8d)] <- "Otter_intensity_after"
fig8d$Otter_intensity_after[is.na(fig8d$Otter_intensity_after)] <- 0
fig8d <- subset(fig8d,fig8d$Otter_intensity_after > 0) #& fig8$adjacent.cells > 0)
fig8d <- fig8d[order(fig8d$Otter_intensity_after),]
fig8d$perc3 <- cumsum(fig8d$Otter_intensity_after) / sum(fig8d$Otter_intensity_after)*100
fig8d$cat4 <- cut(rev(fig8d$perc3),c(quat3))


# for continuity between  fig9/10 although potentially no longer required. 
# thresholds to be determined during the workshop

#fig8$Otter_cat <- NA
#fig8$Otter_cat[fig8$Otter_intensity == 0] <- "unfished"
#fig8$Otter_cat[fig8$Otter_intensity > 0 & fig8$Otter_intensity < 0.5] <- "low"
#fig8$Otter_cat[fig8$Otter_intensity >= 0.5 & fig8$Otter_intensity < 2] <- "medium"
#fig8$Otter_cat[fig8$Otter_intensity >= 2] <- "high"

# save without intensity information per c-square
#indexcol <- which(names(fig8) %in% c("Otter_intensity","perc","cat","cat2","perc2","cat2","cat3")) 
#fig8b <- fig8[,-(indexcol)]


