
## load each region, link to the EMODNET mean/max/min, if 400-800 m

# set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/WKEUVME"

# get directory with regions 
  reg_dir <- paste(pathdir,"1-Input data/csquares_ecoregions",sep="/")

# set directory with depth data and save files there 
  setwd(paste(pathdir,"1-Input data/csquares_ecoregions_depth",sep="/"))

# get region data Bay of Biscay and I coast
  load(paste(reg_dir,"Bay of Biscay and the Iberian Coast_Region.RData",sep="/"))
  reg1 <- Bbic
  depth <- readRDS("depth_emod_csquare.rds")
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(2)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(3)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(4)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(5)])
  colnames(reg1@data)[7:10] <- colnames(depth)[2:5]
  saveRDS(reg1, file = "Bay of Biscay and the Iberian Coast_depth.rds")

# get region data Celtic Seas
  load(paste(reg_dir,"Celtic Seas_Region.RData",sep="/"))
  reg1 <- Celtic
  depth <- readRDS("depth_emod_csquare.rds")
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(2)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(3)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(4)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(5)])
  colnames(reg1@data)[7:10] <- colnames(depth)[2:5]
  saveRDS(reg1, file = "Celtic Seas_depth.rds")

# get region data Celtic Seas
  load(paste(reg_dir,"Greater North Sea_Region.RData",sep="/"))
  reg1 <- Northsea
  depth <- readRDS("depth_emod_csquare.rds")
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(2)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(3)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(4)])
  reg1 <- cbind(reg1, depth[match(reg1@data$csquares,depth$csquare), c(5)])
  colnames(reg1@data)[7:10] <- colnames(depth)[2:5]
  saveRDS(reg1, file = "Greater North Sea_depth.rds")

  