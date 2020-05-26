
# run to get fig 9 
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
metier_static <- c("Static","Static_FPO","Static_GNS","Static_LLS") #HH

nam <- c(paste("SAR_total",refyear,sep="_"),paste(metier_static[2],refyear, sep="_"), #HH
         paste(metier_static[3],refyear, sep="_")
         ,paste(metier_static[4],refyear, sep="_"))

indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$ref <- rowSums(vmsreg[indexcol])
vmsreg$ref[vmsreg$ref > 0] <- 1

# combine
IREG <- cbind(IREG, vmsreg[match(IREG$csquares,vmsreg$c_square), c("ref")])
colnames(IREG)[ncol(IREG)] <- "ref"
IREG$ref[is.na(IREG$ref)] <- 0

# calculate adjacent squares #HH
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


# get vme data
VME <- read.csv(paste(pathdir_nogit,
                      "VME data repository/VME weighted csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
VME <- VME[,-1]

# combine VME, vms
VMEVMS <- cbind(IREG, VME[match(IREG$csquares,VME$CSquare), c("VME_Class")])
colnames(VMEVMS)[ncol(VMEVMS)] <- "VME_Class"
VMEVMS$precaution <- "none"
VMEVMS$precaution[VMEVMS$VME_Class == 3] <- "low"
VMEVMS$precaution[VMEVMS$VME_Class == 2 | VMEVMS$VME_Class == 1 ] <- "medium"
VMEVMS$precaution[VMEVMS$VME_Class == 0 ] <- "high"
VMEVMS$number <- 1

fig9 <- VMEVMS
fig9$category <- paste(fig9$precaution,fig9$ref,sep="_")

#rm(list=setdiff(ls(), c("pathdir","pathdir_nogit","fig9" , "EcoReg")))
