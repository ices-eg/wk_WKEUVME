
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
afteryear <- 2012:2019
metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                  "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                  "OT_MIX_DMF_BEN","OT_SPF")
metier_static <- "Static"

nam <- c(paste("SAR_total",refyear,sep="_"),paste(metier_static[1],refyear, sep="_"))
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$ref <- rowSums(vmsreg[indexcol])
vmsreg$ref[vmsreg$ref > 0] <- 1

# combine
IREG <- cbind(IREG, vmsreg[match(IREG$csquares,vmsreg$c_square), c("ref")])
colnames(IREG)[ncol(IREG)] <- "ref"
IREG$ref[is.na(IREG$ref)] <- 0

# now make figure 8
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

quat <- seq(0, 100, by=1)
fig8$cat <- cut(fig8$perc,c(quat))
area <- aggregate(fig8$area_sqkm,list(fig8$cat),sum)
area <- cumsum(area[,2]/1000)/(sum(area[,2]/1000)) *100
area <- c(0,area)

quat2 <- c(0, 10,  100)
fig8$cat2 <- cut(fig8$perc,c(quat2))
