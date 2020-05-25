
### analyse data per ecoregion following the assessment template

  outdir <- paste(pathdir,"3-Data analysis",EcoReg,sep="/") 

# get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  depthreg <- readRDS(paste(EcoReg,"depth.rds",sep="_"))
  depthreg <- subset(depthreg, !(depthreg$EEZ == "Faroe Is." | depthreg$EEZ == "Norway" |
                                   depthreg$EEZ == "<NA>" | depthreg$EEZ =="Guernsey"))

# get vms data
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg <- readRDS(paste(EcoReg,"vms.rds",sep="_"))
  
# get region within 400-800 meter
  IREG <- subset(depthreg@data,depthreg@data$within == 1)
  IREG$EEZ <- factor(IREG$EEZ)
  
# define few params
  refyear <- 2009:2011
  afteryear <- 2012:2019
  allyears <- 2009:2019
  metier_mbcg  <- c("Otter","Beam","Dredge","Seine", 
                    "OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
                    "OT_MIX_DMF_BEN","OT_SPF")
  metier_static <- c("Static","Static_FPO","Static_GNS","Static_LLS")
  
# figure 1 - depth c-squares 400-800 per ecoregion
  fig1 <- as.data.frame(depthreg)
  saveRDS(fig1,  paste(outdir,"fig1.rds",sep="/"))

# figure 6 - fishing footprint whole area
  # fix for static gear column not coming through
  for(yy in 1:length(allyears)){
   vmssub <- vmsreg[grep(allyears[yy],names(vmsreg))]
    nam <- names(vmssub[grep("Static",names(vmssub))])
    dat <- rowSums(vmssub[nam]) #HH
    dat[dat > 0] <- 1 
    vmsreg[nam[1]] <- dat
  }

  # total fishing footprint in reference period
  nam <- c(paste("SAR_total",refyear,sep="_"),paste(metier_static[1],refyear, sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$ref <- rowSums(vmsreg[indexcol])
  vmsreg$ref[vmsreg$ref > 0] <- 1
  
  # total fishing footprint in period 2012-2018
  nam <- c(paste("SAR_total",afteryear,sep="_"),paste(metier_static[1],afteryear, sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$after <- rowSums(vmsreg[indexcol])
  vmsreg$after[vmsreg$after > 0] <- 1

  # total static fishing footprint (static, FPO, GNS, LLS) in period 2009-2011 
  for(id in 1:length(metier_static)){ 
    nam <- c(paste(metier_static[id],refyear,sep="_")) 
    indexcol <- which(names(vmsreg) %in% nam) 
    dat <- rowSums(vmsreg[indexcol])
    dat[dat > 0] <- 1 
    vmsreg$dat <- dat #HH
    colnames(vmsreg)[ncol(vmsreg)] <- paste("ref",metier_static[id],sep="_") #HH
  }
 
  # total number of sub-gears per c-square in period 2009-2011
  vmssub <- vmsreg 
  subg <- c("SAR_OT_CRU","SAR_OT_DMF","SAR_OT_MIX","SAR_OT_MIX_CRU_DMF","SAR_OT_MIX_DMF_BEN", 
            "SAR_OT_SPF","Static_FPO","Static_GNS","Static_LLS") 
  vmssub$ref_count <- 0 
  for (id in 1:length(subg)){ 
    nam <- c(paste(subg[id],refyear,sep="_")) 
    indexcol <- which(names(vmsreg) %in% nam) 
    vmssub[,indexcol] <- sapply(vmssub[,indexcol],function(x) ifelse(x>0,1,x)) 
    vmssub$ref_subSAR <- rowSums(vmssub[indexcol])
    vmssub$ref_subSAR <- sapply(vmssub$ref_subSAR,function(x) ifelse(x>0,1,x))
    vmssub$ref_count <- rowSums(vmssub[,c("ref_count", "ref_subSAR")]) 
  }
  vmsreg$ref_count <- vmssub$ref_count 

  IREG <- cbind(IREG, vmsreg[match(IREG$csquares,vmsreg$c_square), c("ref","after","ref_count","ref_Static","ref_Static_FPO","ref_Static_LLS","ref_Static_GNS")])
  IREG$ref[is.na(IREG$ref)] <- 0
  IREG$after[is.na(IREG$after)] <- 0
  IREG$ref_count[is.na(IREG$ref_count)] <- 0 
  IREG$ref_Static[is.na(IREG$Static)] <- 0 
  IREG$ref_Static_FPO[is.na(IREG$ref_Static_FPO)] <- 0 
  IREG$ref_Static_LLS[is.na(IREG$ref_Static_LLS)] <- 0   
  IREG$ref_Static_GNS[is.na(IREG$ref_Static_GNS)] <- 0 
  
  fig6 <- IREG
                                
  # calculate number of adjacent csquares for fishery footprint scenarios
  dat <- fig6[fig6$ref==1,]
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
  
  fig6 <- merge(x = fig6, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
  fig6$adjacent.cells[is.na(fig6$adjacent.cells)] <- 0

  saveRDS(fig6,  paste(outdir,"fig6.rds",sep="/"))
  
  IREG <- cbind(IREG, fig6[match(IREG$csquares,fig6$csquares), c("adjacent.cells")])
  names(IREG)[ncol(IREG)] <- "adjacent.cells"
  IREG$adjacent.cells[is.na(IREG$adjacent.cells)] <- 0

# Table 2 - total numbers of C-squares and numbers of C-squares fished per EEZ within 400-800 m depth range
  tab2 <- table(droplevels(IREG$EEZ))
  
  # link footprint to depth region and estimate number of c-squares fished
  tab2 <- cbind(tab2,table(droplevels(IREG$EEZ[IREG$adjacent.cells > 0]))) 
  tab2 <- data.frame(rownames(tab2),tab2)
  rownames(tab2) <- c()
  colnames(tab2) <- c("EEZ","csquares in depth range","fished c-squares in depth range")
  tab2$percentage_fished <- tab2[,3]/tab2[,2]* 100                
  
  # get the sum of all EEZs  
  total <- colSums(tab2[,2:3])
  total2 <- data.frame("total",total[1],total[2],total[2]/total[1]*100)
  colnames(total2) <- colnames(tab2)
  rownames(total2) <- c()
  tab2 <- rbind(tab2,total2)
  tab2[,"percentage_fished"] <- round(tab2[,"percentage_fished"],digits = 0)
  saveRDS(tab2,  paste(outdir,"tab2.rds",sep="/"))
          
# Table 3 - fishing per metier and otter trawl sub-level metier
  tab3 <- as.data.frame(matrix(data=NA, nrow =length(unique(IREG$EEZ)),ncol=1))
  tab3[,1] <- unique(IREG$EEZ)
  
  # estimate number of c-squares fished between 400-800 m
  tab3 <- cbind(tab3,tab2[match(tab3[,1],tab2[,1]), c(3)])
  
  # get estimates for all MBCG
  
  vmssub <- vmsreg
  for (id in 1:length(metier_mbcg)){
    nam <- paste("SAR",metier_mbcg[id],refyear,sep="_")
    indexcol <- which(names(vmssub) %in% nam) 
    vmssub$ref_subSAR <- rowMeans(vmssub[indexcol])
    IREG_met <- cbind(IREG, vmssub[match(IREG$csquares,vmssub$c_square), c("ref_subSAR")])
    colnames(IREG_met)[ncol(IREG_met)] <- "ref_subSAR"
    refsubsar <- subset(IREG_met,IREG_met$ref_subSAR > 0 & IREG_met$adjacent.cells > 0)
    if (nrow(refsubsar) > 0){
      tt <- aggregate(refsubsar$csquares,by=list(refsubsar$EEZ),length)
      tab3 <- cbind(tab3, tt[match(tab3[,1],tt[,1]), c(2)])
    } else{
      tt <- rep(0,length(unique(IREG_met$EEZ)))
      tab3 <- cbind(tab3,tt)
    }
  }

  # get estimate for static gears
  for (id in 1:length(metier_static)){
    nam <- paste(metier_static[id],refyear,sep="_") 
    indexcol <- which(names(vmssub) %in% nam) 
    vmssub$ref_substat <- rowMeans(vmssub[indexcol],na.rm=T)
    IREG_met <- cbind(IREG, vmssub[match(IREG$csquares,vmssub$c_square), c("ref_substat")])
    colnames(IREG_met)[ncol(IREG_met)] <- "ref_substat"
    ref_substat <- subset(IREG_met,IREG_met$ref_substat > 0 & IREG_met$adjacent.cells > 0)
    if (nrow(ref_substat) > 0){
      tt <- aggregate(ref_substat$csquares,by=list(ref_substat$EEZ),length)
      tab3 <- cbind(tab3, tt[match(tab3[,1],tt[,1]), c(2)])
    } else{
      tt <- rep(0,length(unique(IREG_met$EEZ)))
      tab3 <- cbind(tab3,tt)
    }
   }
  
  tab3[is.na(tab3)] <- 0
  coln <- as.character(tab3[,1])
  tab3 <- t(tab3[,2:ncol(tab3)])
  rownames(tab3) <- c("400-800 (footprint)",metier_mbcg,metier_static)
  colnames(tab3) <- coln
    
  # estimate c-squares have multiple fishing gears
  tab3a <- data.frame(unclass(table(IREG$ref_count,IREG$EEZ))) 
  tab3a <- cbind(rownames(tab3a), data.frame(tab3a, row.names=NULL)) 
  names(tab3a)[1] <- "Number of Sub-gears" 
  tab3a <- tab3a[-1,]  
  total <- colSums(tab3a[,2:4])
  total2 <- data.frame("total",total[1],total[2],total[3]) 
  colnames(total2) <- colnames(tab3a) 
  tab3a <- rbind(tab3a,total2) 
  saveRDS(tab3a,  paste(outdir,"tab3a.rds",sep="/")) 
 
  # estimate number of c-squares fished all depths
  tab3all <- as.data.frame(matrix(data=NA, nrow =length(unique(IREG$EEZ)),ncol=1))
  tab3all[,1] <- unique(IREG$EEZ)
  
  # estimate number of c-squares total footprint fished all depths
  Allreg <- cbind(depthreg@data, vmsreg[match(depthreg@data$csquares,vmsreg$c_square), c("ref")])
  colnames(Allreg)[ncol(Allreg)] <- "ref"
  Allreg$ref[is.na(Allreg$ref)] <- 0
  Regtab <- table(droplevels(Allreg$EEZ[Allreg$ref > 0]))
  Regtab <- data.frame(rownames(Regtab),Regtab)
  tab3all <- cbind(tab3all,Regtab[match(tab3all[,1],Regtab[,2]), c(3)]) 
  
  # get estimates for all MBCG all depths
  vmssub <- vmsreg
  for (id in 1:length(metier_mbcg)){
    nam <- paste("SAR",metier_mbcg[id],refyear,sep="_")
    indexcol <- which(names(vmssub) %in% nam) 
    vmssub$ref_subSAR <- rowMeans(vmssub[indexcol])
    Allreg_met <- cbind(depthreg@data, vmssub[match(depthreg@data$csquares,vmssub$c_square), c("ref_subSAR")])
    colnames(Allreg_met)[ncol(Allreg_met)] <- "ref_subSAR"
    refsubsar <- subset(Allreg_met,Allreg_met$ref_subSAR > 0)
    if (nrow(refsubsar) > 0){
      tt <- aggregate(refsubsar$csquares,by=list(refsubsar$EEZ),length)
      tab3all <- cbind(tab3all, tt[match(tab3all[,1],tt[,1]), c(2)])
    } else{
      tt <- rep(0,length(unique(Allreg_met$EEZ)))
      tab3all <- cbind(tab3all,tt)
    }
  }
  
  # get estimate for static gears all depths
  vmssub <- vmsreg 
  for (id in 1:length(metier_static)){ 
    nam <- paste(metier_static[id],refyear,sep="_") 
    indexcol <- which(names(vmssub) %in% nam) 
    vmssub$ref_substat <- rowMeans(vmssub[indexcol],na.rm=T)
    Allreg_met <- cbind(depthreg@data, vmssub[match(depthreg@data$csquares,vmssub$c_square), c("ref_substat")])
    colnames(Allreg_met)[ncol(Allreg_met)] <- "ref_substat"
    ref_substat <- subset(Allreg_met,Allreg_met$ref_substat > 0)
    if (nrow(ref_substat) > 0){
      tt <- aggregate(ref_substat$csquares,by=list(ref_substat$EEZ),length)
      tab3all <- cbind(tab3all, tt[match(tab3all[,1],tt[,1]), c(2)])
    } else{
      tt <- rep(0,length(unique(Allreg_met$EEZ)))
      tab3all <- cbind(tab3all,tt)
    }
   }
  
  tab3all[is.na(tab3all)] <- 0
  coln <- as.character(tab3all[,1])
  tab3all <- t(tab3all[,2:ncol(tab3all)])
  rownames(tab3all) <- c("all (footprint)",metier_mbcg,metier_static)
  colnames(tab3all) <- coln
  
  tab3 <- list(tab3,tab3all)
  saveRDS(tab3,  paste(outdir,"tab3.rds",sep="/"))
  
# figure 7 - depth c-squares fished/unfished 
  fig7 <- IREG
  fig7$ref_cat <- ifelse(fig7$ref > 0,1,0)
  fig7$after_cat <- ifelse(fig7$after > 0,1,0)
  fig7$ref_vs_after <- paste(fig7$after_cat,fig7$ref_cat,sep="_")
  saveRDS(fig7,  paste(outdir,"fig7.rds",sep="/"))

# figure 8 look at otter trawl fishing intensity
  # not saved as data is restricted
  
# Table 4 VME occurences per ecoregion
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME weighted csquares/vme_extraction_weightingAlgorithm_15052020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  
  VMEVMS <- cbind(IREG, VME[match(IREG$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEVMS)[ncol(VMEVMS)] <- "VME_Class"
  VMEVMS$precaution <- "none"
  VMEVMS$precaution[VMEVMS$VME_Class == 3] <- "low"
  VMEVMS$precaution[VMEVMS$VME_Class == 2 | VMEVMS$VME_Class == 1 ] <- "medium"
  VMEVMS$precaution[VMEVMS$VME_Class == 0 ] <- "high"
  VMEVMS$number <- 1
  
  VMEVMS$EEZ <- as.factor(VMEVMS$EEZ)
  VMEVMS$precaution <- as.factor(VMEVMS$precaution)
  withVME  <- subset(VMEVMS,!(VMEVMS$precaution =="none"))
  withVME$precaution <- factor(withVME$precaution)
  tab4     <- data.frame(Region= "Total region", VME_presence = sum(withVME$number))
  
  tab4cat <- withVME %>%
    group_by(precaution) %>%
    summarize(sum = mean(number), n = n()) %>%
    complete(precaution)
  tab4cat <- as.data.frame(tab4cat)
  tab4cat <- tab4cat[!duplicated(tab4cat),]
  
  withVMEfish <- subset(withVME,withVME$ref == 1)
  tab4catf <- withVMEfish %>%
    group_by(precaution) %>%
    summarize(sum = mean(number), n = n()) %>%
    complete(precaution)
  tab4catf <- as.data.frame(tab4catf)
  tab4catf <- tab4catf[!duplicated(tab4catf),]
  
  tab4 <- data.frame(tab4,low_fished = tab4catf$n[tab4catf$precaution =="low"],low_all = tab4cat$n[tab4cat$precaution =="low"],
                     medium_fished = tab4catf$n[tab4catf$precaution =="medium"],medium_all = tab4cat$n[tab4cat$precaution =="medium"],
                    high_fished = tab4catf$n[tab4catf$precaution =="high"],high_all = tab4cat$n[tab4cat$precaution =="high"])
             
  # no per EEZ
  tab4sub  <- aggregate(withVME$number,by=list(withVME$EEZ),sum)
  
  tab4cat <- withVME %>%
    group_by(precaution,EEZ) %>%
    summarize(sum = mean(number), n = n()) %>%
    complete(precaution,EEZ)
  tab4cat <- as.data.frame(tab4cat)
  tab4cat <- tab4cat[!duplicated(tab4cat),]
  
  withVMEfish <- subset(withVME,withVME$ref == 1)
  tab4catf <- withVMEfish %>%
    group_by(precaution,EEZ) %>%
    summarize(sum = mean(number), n = n()) %>%
    complete(precaution,EEZ)
  tab4catf <- as.data.frame(tab4catf)
  tab4catf <- tab4catf[!duplicated(tab4catf),]
 
  tab4EEZ  <- data.frame(Region = tab4sub$Group.1, VME_presence = tab4sub$x)
  tt_f <- subset(tab4catf,tab4catf$precaution =="low")
  tab4EEZ <- cbind(tab4EEZ, tt_f[match(tab4EEZ$Region,tt_f$EEZ), c("n")])
  tt <- subset(tab4cat,tab4cat$precaution =="low")
  tab4EEZ <- cbind(tab4EEZ, tt[match(tab4EEZ$Region,tt$EEZ), c("n")])
  
  tt_f <- subset(tab4catf,tab4catf$precaution =="medium")
  tab4EEZ <- cbind(tab4EEZ, tt_f[match(tab4EEZ$Region,tt_f$EEZ), c("n")])
  tt <- subset(tab4cat,tab4cat$precaution =="medium")
  tab4EEZ <- cbind(tab4EEZ, tt[match(tab4EEZ$Region,tt$EEZ), c("n")])
  
  tt_f <- subset(tab4catf,tab4catf$precaution =="high")
  tab4EEZ <- cbind(tab4EEZ, tt_f[match(tab4EEZ$Region,tt_f$EEZ), c("n")])
  tt <- subset(tab4cat,tab4cat$precaution =="high")
  tab4EEZ <- cbind(tab4EEZ, tt[match(tab4EEZ$Region,tt$EEZ), c("n")])
  
  colnames(tab4EEZ) <-  colnames(tab4)
  
  tab4     <- rbind(tab4,tab4EEZ)
  saveRDS(tab4,  paste(outdir,"tab4.rds",sep="/"))

# figure 9  map of overlap between fishing and VME 
# not saved as data is restricted

# figure 10 map of overlap between fishing intensity otter trawls and VME
  fig10 <- VMEVMS
  fig10 <- cbind(fig10, fig8[match(fig10$csquares,fig8b$csquares), c("Otter_cat")])  
  colnames(fig10)[ncol(fig10)] <- "Otter_cat"
  fig10$Otter_cat <- gsub("medium","step",fig10$Otter_cat)
  fig10$Otter_cat <- gsub("high","step",fig10$Otter_cat)
  fig10$Otter_cat <- gsub("step","medium/high",fig10$Otter_cat)
  fig10$VME_ClassII <- ifelse(fig10$precaution == "none", 0, 1 )
  fig10$category <- paste(fig10$Otter_cat,fig10$VME_ClassII,sep="_")   
  saveRDS(fig10,  paste(outdir,"fig10.rds",sep="/"))
  
  rm(list=setdiff(ls(), c("pathdir","pathdir_nogit" , "EcoReg")))
  
