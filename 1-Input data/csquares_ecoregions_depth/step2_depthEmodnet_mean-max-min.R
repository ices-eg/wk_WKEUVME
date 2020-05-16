
## script to estimate: mean, min and max value per c-square and check if 400-800 depth is TRUE
# depth data is not available on github as the files are too large

# install libraries
  library(rgdal)
  library(sp)
  library(raster)

# set folder directory
  pathdir_git   <- "C:/Users/pdvd/Online for git/WKEUVME"
  pathdir_nogit <- "C:/Users/pdvd/Online for git/WKEUVME_noGIT"
  setwd(paste(pathdir_nogit,"Depth",sep="/"))

# get tile names of EMODNET data
  boxname <- c("C5","C4","C3","D2","D3","D4","D5","E3","E2","F2","F3","F4","G3","E4")

# open all EMODNET data pre-selected in step 1
  box <- 1
  BB <- readRDS(paste(boxname[box],"depth.rds",sep="_"))

  for (box in 2: length(boxname)){
      BB2 <- readRDS(paste(boxname[box],"depth.rds",sep="_"))
      BB <- rbind(BB,BB2)
  }

# some c-squares are positioned in multiple tile, 
# first select all c-squares positioned in one EMODNET tile  
# and estimate mean, min, max 
  tt <- which(duplicated(BB[,1]))
  BB <- data.frame(BB)
  dup <- BB[tt,1]
  
  uni <-  BB[!(BB$V1 %in% c(dup)),]
  avgrid  <- rowMeans(uni[,2:2500],na.rm = T) # mean
  avgrid <- abs(avgrid)
  mingrid_neg <- apply(uni[,2:2500], 1, FUN=min,na.rm=T) # min
  maxgrid <- abs(mingrid_neg)
  maxgrid_neg <- apply(uni[,2:2500], 1, FUN=max,na.rm=T) # max
  mingrid <- abs(maxgrid_neg)
  depth <- data.frame(csquare = uni[,1], mean_depth_emodnet = avgrid, min_depth_emodnet = mingrid,
           max_depth_emodnet = maxgrid)

# check if 400-800 depth is TRUE
  IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
  IREG$within <- 1 # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[5] <- "within"
  depth$within[is.na(depth$within)] <- 0 # if not TRUE

# now select all c-squares that were split by multiple tiles
  doubles <- BB[(BB$V1 %in% c(dup)),]
  tab <- as.data.frame(matrix(data=NA,ncol=4,nrow= length(dup)))

# subset per c-square and estimate mean, max and min  
  for(i in 1:length(dup)){
    tr <- subset(doubles,doubles$V1 == dup[i])
    tab[i,1] <- dup[i]
    trdat <- unlist(tr[1:nrow(tr),2:2500])
    avgrid  <- mean(trdat,na.rm = T) # mean
    tab[i,2] <- abs(avgrid)
    maxgrid_neg <- max(trdat,na.rm=T) # max
    tab[i,3] <- abs(maxgrid_neg)
    mingrid_neg <- min(trdat,na.rm=T) # min
    tab[i,4] <- abs(mingrid_neg)
  }

# check if 400-800 depth is TRUE
  colnames(tab) <- colnames(depth)[1:4]
  IREG <- subset(tab,!(tab$min_depth_emodnet > 800))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
  IREG$within <- 1  # if TRUE
  tab <- cbind(tab,IREG[match(tab$csquare,IREG$csquare),c("within")])
  colnames(tab)[5] <- "within"
  tab$within[is.na(tab$within)] <- 0 # if not TRUE

# combine all depth data per c-square
  depth_emod <- rbind(depth,tab)
  
# save depth data
  setwd(paste(pathdir_git,"1-Input data/csquares_ecoregions_depth/",sep="/"))
  saveRDS(depth_emod, file = "depth_emod_csquare.rds")
