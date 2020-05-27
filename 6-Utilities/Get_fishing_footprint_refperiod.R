
# run to get fishing footprint

  # get depth data
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))
  Footprint <- readRDS(paste(EcoReg,"depth.rds",sep="_"))
  Footprint <- subset(Footprint, !(Footprint$EEZ == "Faroe Is." | Footprint$EEZ == "Norway" |
                                     Footprint$EEZ == "<NA>" | Footprint$EEZ =="Guernsey"))

  # get region within 400-800 meter
  Footprint <- subset(Footprint@data,Footprint@data$within == 1)

  # get vms data
  setwd(paste(pathdir_nogit,"VMS data repository",sep="/"))
  vmsreg_footprint <- readRDS(paste(EcoReg,"vms.rds",sep="_"))

  # define few params
  refyear_footprint <- 2009:2011
  metier_static_footprint <- c("Static_FPO","Static_GNS","Static_LLS") 

  nam_footprint <- c(paste("SAR_total",refyear_footprint,sep="_"), 
                         paste(metier_static_footprint[1],refyear_footprint, sep="_"), 
                         paste(metier_static_footprint[2],refyear_footprint, sep="_"), 
                         paste(metier_static_footprint[3],refyear_footprint, sep="_"))

  indexcol_footprint <- which(names(vmsreg_footprint) %in% nam_footprint) 
  vmsreg_footprint$ref <- rowSums(vmsreg_footprint[indexcol_footprint])
  vmsreg_footprint$ref[vmsreg_footprint$ref > 0] <- 1

  # combine
  Footprint <- cbind(Footprint, vmsreg_footprint[match(Footprint$csquares,vmsreg_footprint$c_square), c("ref")])
  colnames(Footprint)[ncol(Footprint)] <- "ref"
  Footprint$ref[is.na(Footprint$ref)] <- 0
  
  # calculate adjacent squares to apply footprint scenario
  dat <- Footprint[Footprint$ref==1,]
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
  #table(dat$adjacent.cells)
  
  Footprint <- merge(x = Footprint, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
  Footprint$adjacent.cells[is.na(Footprint$adjacent.cells)] <- 0

  rm("dat","nam_footprint","indexcol_footprint","vmsreg_footprint","refyear_footprint")
