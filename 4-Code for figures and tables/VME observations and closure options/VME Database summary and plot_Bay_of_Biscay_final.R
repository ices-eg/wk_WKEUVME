#-------------------------------------------------------------------------------------
# WKEUVME 18th - 28th May 2020
# VME Database summary and plot
# David Stirling (David.Stirling@gov.scot)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Notes:
# Summarise the VME database looking at the number of VME indicators and habitats will be contained in the closure vs the number available in the region by itself.

# For both Celtic Sea and Bay of Biscay and Iberian Penninsula ecoregions. 
# And 4 scenarios of closures


#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Preamble
#-------------------------------------------------------------------------------------
# clean slate
rm(list=ls())
.rs.restartR()

# Flags
recompute = FALSE
crop2depthpoly = TRUE

# work directory
wd = "E:/ICES - WKEUVME/Seapen work/"

# convenience functions
#loadRdata function
loadRdata=function(filename){
  load(filename)
  get(ls()[ls() != "filename"])
}
# Script name: instant_pkgs.R
# Purpose: Package installation and loading
# Author: Kay Cichini
# Date: 2012-06-19
# Licence: cc by-nc-sa
instant_pkgs <- function(pkgs) { 
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {install.packages(pkgs_miss)}
  if (length(pkgs_miss) == 0) {message("\n ...Packages were already installed!\n")}
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {install.packages(pkgs_miss)}
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  if (length(need_to_attach) > 0) {  for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)}
  if (length(need_to_attach) == 0) {  message("\n ...Packages were already loaded!\n")  }}

# Load packages
instant_pkgs(c("plyr",
               "rgdal",
               "raster",
               "sp",
               "sf", # this is awful with point data
               "rgeos",
               "maptools",
               "gridExtra",
               "fasterize",
               "ggplot2",
               "tidyverse",
               "devtools",
               "ggmap",
               "ggplot2",
               "devtools",
               "ggspatial",
               "ggsn",
               "cowplot",
               "reshape2",
               "viridis",
               "wesanderson",
               "ggnewscale",
               "ggspatial",
               "ggpubr"
))
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# Polygons for hierarchical areas
#-------------------------------------------------------------------------------------
# Bay of Biscay
CS = readOGR("E:/ICES WGDEC 2020/GIS", layer = "BoB_Ices_ecoregion")
plot(CS)

# Get the depth contour 400 - 800 m (using EMODNET smooth depth)
#-------------------------------------------------------------------------------------
source("E:/ICES - WKEUVME/R scripts/Derive depth contour polygon.r")
depth_band = gIntersection(depth_band_poly, CS)
plot(depth_band,col = "blue", add = T)

#-------------------------------------------------------------------------------------
# VME database - points / lines
#-------------------------------------------------------------------------------------
# For VME DB extraction: vme_extraction_25052020v2 - extraction without absences
vmedb = read.csv("E:/ICES - WKEUVME/Data folder - restricted data/VME extraction no abs - carlos/VME_Extraction_of_precenses_25052020_v2.csv")
head(vmedb)
coordinates(vmedb) = ~MiddleLongitude+MiddleLatitude
vmedb@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# CS area 
#-------------------------------------------------------------------------------------
vme.cs = vmedb[complete.cases(sp::over(vmedb, CS)),]
plot(vme.cs, pch = '.', col = "red", cex = 2, add = T)

# fields of interest: 5 "VME_Indicatoricator"  6 "HabitatType" 
lapply(c(5,6), function(i) unique(vme.cs@data[,i]))

foi = lapply(c(5,6), function(x){
  a = lapply(unique(vme.cs@data[,x]), function(i) vme.cs[vme.cs@data[,x] == i,])
  names(a) = unique(vme.cs@data[,x])
  a
})
names(foi) = c("VME indicator type", "VME habitat type")
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Read in closure shapefiles
#-------------------------------------------------------------------------------------
# closure scenarios, all but 2.2 invalid geoms
clsdir = "E:/ICES - WKEUVME/wk_WKEUVME-master_git_clone_9.06.20/wk_WKEUVME-master/5-Output/Closure options (region)"

if(!file.exists( paste0(clsdir, "/Scenario1_option1_fixed_geom.shp")) || recompute) {
  
  close.sc1 = readOGR(clsdir, "Scenario1_option1")
  close.sc1.opt2 = readOGR(clsdir, "Scenario1_option2")
  close.sc2.opt1 = readOGR(clsdir, "Scenario2_option1")
  close.sc2.opt2 = readOGR(clsdir, "Scenario2_option2")
  # check validity
  lapply(c(close.sc1, close.sc1.opt2, close.sc2.opt1, close.sc2.opt2), function(i) gIsValid(i))
  # fix geometries in 1.1,1.2 & 2.1 & write out
  names(geom_fix) = c("Scenario1_option1","Scenario1_option2","Scenario2_option1")
  geom_fix = lapply(c(close.sc1, close.sc1.opt2, close.sc2.opt1), function(i) clgeo_Clean(i, verbose = T))
  lapply(names(geom_fix), function(i) writeOGR(geom_fix[[i]], dsn = clsdir, layer = paste0(i,"_fixed_geom"), driver = "ESRI Shapefile"))
  # check validity
  lapply(geom_fix, function(i) gIsValid(i))
}
# redefine closure scenarios - NB these are the original files provided by Javier, however, they are all invalid
close.sc1 = readOGR(clsdir, "Scenario1_option1_fixed_geom")
close.sc1.opt2 = readOGR(clsdir, "Scenario1_option2_fixed_geom")
close.sc2.opt1 = readOGR(clsdir, "Scenario2_option1_fixed_geom")
close.sc2.opt2 = readOGR(clsdir, "Scenario2_option2")

# check validity
lapply(c(close.sc1, close.sc1.opt2, close.sc2.opt1, close.sc2.opt2), function(i) gIsValid(i))

# crop these to the depth polygon
if(crop2depthpoly == T){
  # scenario 1.1
  close.sc1 = gIntersection(close.sc1, depth_band, byid = TRUE, drop_lower_td = TRUE)
  # scenario 1.2
  close.sc1.opt2 = gIntersection(close.sc1.opt2, depth_band, byid = TRUE, drop_lower_td = TRUE)
  # scenario 2.1
  close.sc2.opt1 = gIntersection(close.sc2.opt1, depth_band, byid = TRUE, drop_lower_td = TRUE)
  # scenario 2.2
  close.sc2.opt2 = gIntersection(close.sc2.opt2, depth_band, byid = TRUE, drop_lower_td = TRUE)
}

# Depth Band polygon being used by workshop
if(crop2depthpoly == F){
  cs_depth = readRDS("E:/ICES - WKEUVME/Data folder - restricted data/Data from Daniel/Bay of Biscay and the Iberian Coast_depth.rds", refhook = NULL) 
  head(cs_depth)
  cs_depth@proj4string = vmedb@proj4string
  db2 = cs_depth[cs_depth$within == 1,]
  vmedb$db2 = sp::over(vmedb, db2)[,10]
  vme.dbnd2 = vmedb[!(is.na(vmedb$db2)),]
  plot(vme.dbnd2)
}
# Using polygon as the depth range specific not c-square specific
if(crop2depthpoly == T){
  vmedb$db2 = sp::over(vmedb, depth_band)
  vme.dbnd2 = vmedb[!(is.na(vmedb$db2)),]
  plot(vme.dbnd2)
}

# Depth band poly data
# create a list
foi.sp = lapply(c(5,6), function(x){
  a = lapply(unique(vme.dbnd2@data[,x]), function(i) vme.dbnd2[vme.dbnd2@data[,x] == i,])
  names(a) = unique(vme.dbnd2@data[,x])
  a
})
names(foi.sp) = c("VME indicator type", "VME habitat type")

#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# closure areas
#-------------------------------------------------------------------------------------
# closure scenario 1
# subset to those touching the depth band (Daniel's)
if(crop2depthpoly == F){
  t1 = cs_depth[cs_depth$within == 1,]
  t1@proj4string = close.sc1@proj4string
  close.sc1$within = over(close.sc1, t1)[,10]
  close.sc1.within = close.sc1[!(is.na(close.sc1$within)),]
  plot(close.sc1, col = "red")
  plot(close.sc1.within, col = "red")
}
# Use depth derived polygon
if(crop2depthpoly == T){
  vmedb$close.sc1 = sp::over(vmedb, close.sc1, returnList = FALSE)#[,6]
  vme.close.sc1 = vmedb[!(is.na(vmedb$close.sc1)),]
}
plot(vme.close.sc1)

# Get field of interest lists & reorder
foi.close.sc1 = lapply(c(5,6), function(x){
  a = lapply(unique(vme.close.sc1@data[,x]), function(i) vme.close.sc1[vme.close.sc1@data[,x] == i,])
  names(a) = unique(vme.close.sc1@data[,x])
  a
})
names(foi.close.sc1) = c("VME indicator type", "VME habitat type") 

#-------------------------------------------------------------------------------------
# closure scenario 1 option 2
# subset to those touching the depth band (Daniel's)
if(crop2depthpoly == F){
  t1 = cs_depth[cs_depth$within == 1,]
  t1@proj4string = close.sc1.opt2@proj4string
  close.sc1.opt2$within = over(close.sc1.opt2, t1)[,10]
  close.sc1..opt2.within = close.sc1.opt2[!(is.na(close.sc1.opt2$within)),]
  plot(close.sc1.opt2, col = "red")
  plot(close.sc1.opt2.within, col = "red")
}
# Use depth derived polygon
if(crop2depthpoly == T){
  vmedb$close.sc1.opt2 = sp::over(vmedb, close.sc1.opt2, returnList = FALSE)#[,6]
  vme.close.sc1.opt2 = vmedb[!(is.na(vmedb$close.sc1.opt2)),]
}
plot(vme.close.sc1.opt2)

# Get field of interest lists & reorder
foi.close.sc1.opt2 = lapply(c(5,6), function(x){
  a = lapply(unique(vme.close.sc1.opt2@data[,x]), function(i) vme.close.sc1.opt2[vme.close.sc1.opt2@data[,x] == i,])
  names(a) = unique(vme.close.sc1.opt2@data[,x])
  a
})
names(foi.close.sc1.opt2) = c("VME indicator type", "VME habitat type") 

#-------------------------------------------------------------------------------------
# closure scenario 2 option 1
# subset to those touching the depth band (Daniel's)
if(crop2depthpoly == F){
  close.sc2.opt1$within = over(close.sc2.opt1, t1)[,10]
  close.sc2.opt1.within = close.sc2.opt1[!(is.na(close.sc2.opt1$within)),]
  plot(close.sc2.opt1.within)
  vmedb$close.sc2.opt1 = over(vmedb, close.sc2.opt1, returnList = FALSE)[,6]
  vme.close.sc2.opt1 = vmedb[!(is.na(vmedb$close.sc2.opt1)),]
}
# Use depth derived polygon
if(crop2depthpoly == T){
  vmedb$vme.close.sc2.opt1 = sp::over(vmedb, close.sc2.opt1, returnList = FALSE)#[,6]
  vme.close.sc2.opt1 = vmedb[!(is.na(vmedb$vme.close.sc2.opt1)),]
}
plot(vme.close.sc2.opt1)

# Get field of interest lists & reorder
foi.close.sc2.opt1 = lapply(c(5,6), function(x){
  a = lapply(unique(vme.close.sc2.opt1@data[,x]), function(i) vme.close.sc2.opt1[vme.close.sc2.opt1@data[,x] == i,])
  names(a) = unique(vme.close.sc2.opt1@data[,x])
  a
})
names(foi.close.sc2.opt1) = c("VME indicator type", "VME habitat type") 

#-------------------------------------------------------------------------------------
# closure scenario 2 option 2
# subset to those touching the depth band (Daniel's)
if(crop2depthpoly == F){
  close.sc2.opt2$within = over(close.sc2.opt2, t1)[,10]
  close.sc2.opt2.within = close.sc2.opt2[!(is.na(close.sc2.opt2$within)),]
  plot(close.sc2.opt2.within)
  vmedb$close.sc2.opt2 = over(vmedb, close.sc2.opt2, returnList = FALSE)[,6]
  vme.close.sc2.opt2 = vmedb[!(is.na(vmedb$close.sc2.opt2)),]
}
# Use depth derived polygon
if(crop2depthpoly == T){
  vmedb$vme.close.sc2.opt2 = sp::over(vmedb, close.sc2.opt2, returnList = FALSE)#[,6]
  vme.close.sc2.opt2 = vmedb[!(is.na(vmedb$vme.close.sc2.opt2)),]
}
plot(vme.close.sc2.opt2)

foi.close.sc2.opt2 = lapply(c(5,6), function(x){
  a = lapply(unique(vme.close.sc2.opt2@data[,x]), function(i) vme.close.sc2.opt2[vme.close.sc2.opt2@data[,x] == i,])
  names(a) = unique(vme.close.sc2.opt2@data[,x])
  a
})
names(foi.close.sc2.opt2) = c("VME indicator type", "VME habitat type") 

#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Standardise plotting order
#-------------------------------------------------------------------------------------
# Indicators
lapply(list(foi, foi.sp, foi.close.sc1, foi.close.sc1.opt2, foi.close.sc2.opt1, foi.close.sc2.opt2), function(i) names(i$`VME indicator type`))

foi$`VME indicator type` = foi$`VME indicator type`[sort(names(foi$`VME indicator type`))]
foi.sp$`VME indicator type` = foi.sp$`VME indicator type`[sort(names(foi$`VME indicator type`))][1:9]
foi.close.sc1$`VME indicator type` = foi.close.sc1$`VME indicator type`[sort(names(foi$`VME indicator type`))][1:9]
foi.close.sc1.opt2$`VME indicator type` = foi.close.sc1.opt2$`VME indicator type`[sort(names(foi$`VME indicator type`))][1:9]
foi.close.sc2.opt1$`VME indicator type` = foi.close.sc2.opt1$`VME indicator type`[sort(names(foi$`VME indicator type`))][1:9]
foi.close.sc2.opt2$`VME indicator type` = foi.close.sc2.opt2$`VME indicator type`[sort(names(foi$`VME indicator type`))][1:9]
# check
lapply(list(foi, foi.sp, foi.close.sc1, foi.close.sc1.opt2, foi.close.sc2.opt1, foi.close.sc2.opt2), function(i) names(i$`VME indicator type`))


# Habitats
lapply(list(foi, foi.sp, foi.close.sc1, foi.close.sc1.opt2, foi.close.sc2.opt1, foi.close.sc2.opt2), function(i) names(i$`VME habitat type`))

foi$`VME habitat type` = foi$`VME habitat type`[sort(names(foi$`VME habitat type`))]
foi.sp$`VME habitat type` = foi.sp$`VME habitat type`[sort(names(foi$`VME habitat type`))]
foi.close.sc1$`VME habitat type` = foi.close.sc1$`VME habitat type`[sort(names(foi$`VME habitat type`))]
foi.close.sc1.opt2$`VME habitat type` = foi.close.sc1.opt2$`VME habitat type`[sort(names(foi$`VME habitat type`))]
foi.close.sc2.opt1$`VME habitat type` = foi.close.sc2.opt1$`VME habitat type`[sort(names(foi$`VME habitat type`))]
foi.close.sc2.opt2$`VME habitat type` = foi.close.sc2.opt2$`VME habitat type`[sort(names(foi$`VME habitat type`))]
# check
lapply(list(foi, foi.sp, foi.close.sc1, foi.close.sc1.opt2, foi.close.sc2.opt1, foi.close.sc2.opt2), function(i) names(i$`VME habitat type`))

#-------------------------------------------------------------------------------------
#--------------------- Summary Stats -------------------------------------------------
#-------------------------------------------------------------------------------------
# directory to save to
SumStatsdir = paste0(wd, "/Scenarios and VMEs/FINAL/Bay of Biscay and Iberian Coast Ecoregion/")

# histograms
Ind.hist.df = data.frame(
  VME_Indicator = plyr::ddply(vme.cs@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,1],
  ER = plyr::ddply(vme.cs@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2],
  DB = c(plyr::ddply(vme.dbnd2@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2], NA),
  Sc1.1 = c(plyr::ddply(vme.close.sc1@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2], NA),
  Sc1.2 = c(plyr::ddply(vme.close.sc1.opt2@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2], NA),
  Sc2.1 = c(plyr::ddply(vme.close.sc2.opt1@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2], NA),
  Sc2.2 = c(plyr::ddply(vme.close.sc2.opt2@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))[,2], NA)
)
if(!file.exists(paste0(SumStatsdir, "VME_Indicator_summary_NoR_all_areas_closures_Bay_of_Biscay.csv")) || recompute) {
  write.csv(Ind.hist.df, file = paste0(SumStatsdir, "VME_Indicator_summary_NoR_all_areas_closures_Bay_of_Biscay.csv")) 
}
Ind.hist.df 
Ind.hist.df_long = melt(Ind.hist.df, id.vars = "VME_Indicator")
names(Ind.hist.df_long) = c("VME_Ind", "Area", "value")

# Use position=position_dodge()
vme.ind.hist = lapply(unique(Ind.hist.df_long$VME_Ind), function(i){
  ggplot(data=Ind.hist.df_long[Ind.hist.df_long$VME_Ind == i,] , aes(x=VME_Ind, y=value, fill = Area)) +
    geom_bar(stat="identity", position=position_dodge())+
    labs(title=paste0( i ), y = "NoR", x = "VME Indicator") +
    scale_fill_viridis(discrete = T)  +
    theme_classic()
})
# plot
png(filename = paste0(SumStatsdir, "Bay_of_Biscay_VME_Indicator_bar_plots.png"),
    width = 4500, height = 4500, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
plot_grid(vme.ind.hist[[1]], vme.ind.hist[[2]], vme.ind.hist[[3]], vme.ind.hist[[4]], vme.ind.hist[[6]], vme.ind.hist[[7]], vme.ind.hist[[8]], vme.ind.hist[[9]], ncol = 3, nrow = 3)
dev.off()

# Habitats
Hab.hist.df = data.frame(
  HabitatType = plyr::ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType))[,1],
  ER = plyr::ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2],
  DB = plyr::ddply(vme.dbnd2@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2],
  Sc1.1 = plyr::ddply(vme.close.sc1@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2],
  Sc1.2 = plyr::ddply(vme.close.sc1.opt2@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2],
  Sc2.1 = plyr::ddply(vme.close.sc2.opt1@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2],
  Sc2.2 = plyr::ddply(vme.close.sc2.opt2@data, .(HabitatType), summarise, NoR = length(HabitatType))[,2]
)
if(!file.exists(paste0(SumStatsdir, "VME_Habitat_summary_NoR_all_areas_closures_Bay_of_Biscay.csv")) || recompute) {
  write.csv(Hab.hist.df, file = paste0(SumStatsdir, "VME_Habitat_summary_NoR_all_areas_closures_Bay_of_Biscay.csv")) 
}
Hab.hist.df 
Hab.hist.df_long = melt(Hab.hist.df, id.vars = "HabitatType")
names(Hab.hist.df_long) = c("VME_Hab", "Area", "value")

# Use position=position_dodge()
vme.Hab.hist = lapply(unique(Hab.hist.df_long$VME_Hab), function(i){
  ggplot(data=Hab.hist.df_long[Hab.hist.df_long$VME_Hab == i,] , aes(x=VME_Hab, y=value, fill = Area)) +
    geom_bar(stat="identity", position=position_dodge())+
    labs(title=paste0(i ), y = "NoR", x = "VME Habitat") +
    scale_fill_viridis(discrete = T) +
    theme_classic()
})
# plot
png(filename = paste0(SumStatsdir, "Bay_of_Biscay_VME_Habitat_bar_plots.png"),
    width = 3500, height = 3500, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
plot_grid(vme.Hab.hist[[1]], vme.Hab.hist[[2]], vme.Hab.hist[[3]], vme.Hab.hist[[4]], vme.Hab.hist[[5]], ncol = 3, nrow = 2)
dev.off()

#---------------------------------------
#------------- Plots -------------
#---------------------------------------

# Reproject & prep data:
# Ecoregion area polygon to crop by for eco region
p2 = readOGR(dsn = "E:/ICES - WKEUVME/R work", layer = "Bay of Biscay area polygon")
# depth band
depth_band.proj = spTransform(depth_band, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
depth_band_poly.df =  broom::tidy(depth_band.proj, region = "DN")
# land polygons
land = readOGR("E:/ICES - WKEUVME/GIS layers", "GSSHS_subset")
land = spTransform(land, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
land = gIntersection(land, p2)# ignore warnings - it works!
land.df = broom::tidy(land, region = "id")
# Ecoregion
CS.proj = spTransform(CS, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
CS.df = broom::tidy(CS.proj, region = "Ecoregion")
# Celtic sea base raster (GEBCO)
CS.bm = raster("E:/ICES - WKEUVME/R work/Bay of Biscay area.GEBCO.base.map_utm29n.tif")
# hillshade
terr = terrain(CS.bm, c("slope", "aspect"))
hill = hillShade(terr[["slope"]], terr[["aspect"]])
# convert rasters to data.frame
CS.bm.df = as.data.frame(rasterToPoints(CS.bm))
names(CS.bm.df) = c('Longitude', 'Latitude', 'Value')
hill.df = as.data.frame(rasterToPoints(hill))
names(hill.df) = c('Longitude', 'Latitude', 'Value')
# get center of r
mid.Lon = mean(CS.bm.df$Longitude)
mid.Lat = mean(CS.bm.df$Latitude)
# closures subset:
if(crop2depthpoly == F){
  close.sc1.df =  broom::tidy(close.sc1.within, region = "within")
  close.sc1.opt2.df =  broom::tidy(close.sc1.opt2.within, region = "within")
  close.sc2.opt1.df =  broom::tidy(close.sc2.opt1.within, region = "within")
  close.sc2.opt2.df =  broom::tidy(close.sc2.opt2.within, region = "within")
}
if(crop2depthpoly == T){
  close.sc1 = spTransform(close.sc1, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc1.df =  broom::tidy(close.sc1, region = "indb")
  close.sc1.opt2 = spTransform(close.sc1.opt2, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc1.opt2.df =  broom::tidy(close.sc1.opt2, region = "indb")
  close.sc2.opt1 = spTransform(close.sc2.opt1, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc2.opt1.df =  broom::tidy(close.sc2.opt1, region = "indb")
  close.sc2.opt2 = spTransform(close.sc2.opt2, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc2.opt2.df =  broom::tidy(close.sc2.opt2, region = "indb")
  # closures in entire ecoregion:
  close.sc1_ER = readOGR(clsdir, "Scenario1_option1_fixed_geom")
  close.sc1.opt2_ER = readOGR(clsdir, "Scenario1_option2_fixed_geom")
  close.sc2.opt1_ER = readOGR(clsdir, "Scenario2_option1_fixed_geom")
  close.sc2.opt2_ER = readOGR(clsdir, "Scenario2_option2")
  close.sc1_ER = spTransform(close.sc1_ER, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc1_ER = gIntersection(close.sc1_ER, CS.proj)
  close.sc1.df_ER =  broom::tidy(close.sc1_ER, region = "Id")
  close.sc1.opt2_ER = spTransform(close.sc1.opt2_ER, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc1.opt2_ER = gIntersection(close.sc1.opt2_ER, CS.proj)
  close.sc1.opt2.df_ER =  broom::tidy(close.sc1.opt2_ER, region = "OBJECTID")
  close.sc2.opt1_ER = spTransform(close.sc2.opt1_ER, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc2.opt1_ER = gIntersection(close.sc2.opt1_ER, CS.proj)
  close.sc2.opt1.df_ER =  broom::tidy(close.sc2.opt1_ER, region = "OBJECTID")
  close.sc2.opt2_ER = spTransform(close.sc2.opt2_ER, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  close.sc2.opt2_ER = gIntersection(close.sc2.opt2_ER, CS.proj)
  close.sc2.opt2.df_ER =  broom::tidy(close.sc2.opt2_ER, region = "OBJECTID")
}


######### --------------------- START of 6 panel plot figure for report ------------------------- ###################
# e.g. Sponge indicator
# colours
indicators = lapply(names(foi$`VME indicator type`), function(i){
  d = ggplot2::fortify(foi$`VME indicator type`[[i]]@data)
  # reproject data to utm29n
  x = foi$`VME indicator type`[[i]]@coords[,1]
  y = foi$`VME indicator type`[[i]]@coords[,2]
  xy = data.frame(cbind(x,y))
  xyp = project(as.matrix(xy), "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  xyp = SpatialPointsDataFrame(coords = xyp, data = xy,proj4string = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  d$long = xyp@coords[,1]
  d$lat = xyp@coords[,2]
  # ggplot
  ggplot()+
    geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
    geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.5, show.legend = F) + 
    scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
    scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
    geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
    
    geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 0.35, show.legend = F) +
    geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
    # For bubble plots on N
    new_scale_fill()+
    geom_point(aes(x = long, y = lat), data = d[d$Number == "NULL" | d$Number == 0,], col = "black", fill = "black", pch = 21, cex = 1, alpha = 0.3) +
    geom_point(aes(x = long, y = lat), size = 3, fill = "red", data = d[d$Number >= 1,], col = "black", pch = 21, alpha = 0.3) +
    labs(x = "Longitude", y = "Latitude", caption = "Celtis Seas") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.02, model = 'WGS84', transform = FALSE, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()  + 
    guides(fill = F)
})
names(indicators) = names(foi$`VME indicator type`)

indicators.db = lapply(names(foi.sp$`VME indicator type`), function(i){
  d = ggplot2::fortify(foi.sp$`VME indicator type`[[i]]@data)
  # reproject data to utm29n
  x = foi.sp$`VME indicator type`[[i]]@coords[,1]
  y = foi.sp$`VME indicator type`[[i]]@coords[,2]
  xy = data.frame(cbind(x,y))
  xyp = project(as.matrix(xy), "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  xyp = SpatialPointsDataFrame(coords = xyp, data = xy,proj4string = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  d$long = xyp@coords[,1]
  d$lat = xyp@coords[,2]
  # ggplot
  ggplot()+
    geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
    geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.5, show.legend = F) + 
    scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
    scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
    geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
    
    geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 0.35, show.legend = F) +
    geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
    # For bubble plots on N
    new_scale_fill()+
    geom_point(aes(x = long, y = lat), data = d[d$Number == "NULL" | d$Number == 0,], col = "black", fill = "black", pch = 21, cex = 1, alpha = 0.5) +
    geom_point(aes(x = long, y = lat), size = 3, fill = "red", data = d[d$Number >= 1,], col = "black", pch = 21, alpha = 0.3) +
    labs(x = "Longitude", y = "Latitude", caption = "400 - 800 m depth band") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.02, model = 'WGS84', transform = FALSE, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()  + 
    guides(fill = F)
})
names(indicators.db) = names(foi.sp$`VME indicator type`)

# Closure Scenario 1.1, 1.2, 2.1 & 2.2
indicators.cl.sc1 = lapply(list(close.sc1.df, close.sc1.opt2.df, close.sc2.opt1.df,close.sc2.opt2.df), function(j){
  a = lapply(c(foi.close.sc1[[1]], foi.close.sc1.opt2[[1]], foi.close.sc2.opt1[[1]], foi.close.sc2.opt2[[1]]), function(i){
    d = ggplot2::fortify(i@data) 
    # reproject data to utm29n
    x = i@coords[,1]
    y = i@coords[,2]
    xy = data.frame(cbind(x,y))
    xyp = project(as.matrix(xy), "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    xyp = SpatialPointsDataFrame(coords = xyp, data = xy, proj4string = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    d$long = xyp@coords[,1]
    d$lat = xyp@coords[,2]
    # ggplot
    ggplot()+
      geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
      geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
      scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
      scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
      geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
      
      geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 0.35, show.legend = F) +
      geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
      # geom_point(aes(x = long, y = lat, size = as.numeric(Number)), data = d[d$Number > 0,], col = "black", fill = "red", pch = 21, alpha = 0.3) +
      # geom_point(aes(x = long, y = lat), data = d[d$Number == "NULL" | d$Number == 0,], col = "red", fill = "black", pch = 21, cex = 1) +
      # scale_size_continuous(range = c(3,10)) +
      geom_point(aes(x = long, y = lat), data = d, col = "black", fill = "red", pch = 21, cex = 4, alpha = 0.3) +
      new_scale_colour()+
      geom_polygon(data = j, aes(x = long, y = lat, group = group, col = "Closure"), fill = "#FFFF00", show.legend = T) +
      scale_color_manual(values = c("Closure" = "#FFFF00"))+
      new_scale_fill()+
      # labs(title=paste0("Closures (400 - 800 m) & VME indicator: ", unique(i$VME_Ind) ), x = "Longitude", y = "Latitude") +
      labs(x = "Longitude", y = "Latitude") +
      ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.02, model = 'WGS84', transform = FALSE, dist_unit = "km", location = "bottomright") +
      annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
      coord_sf(crs = 32629)+
      theme_classic()
  })
  names(a) = c(names(foi.close.sc1[[1]]),names(foi.close.sc1.opt2[[1]]),names(foi.close.sc2.opt1[[1]]),names(foi.close.sc2.opt2[[1]]))
  return(a)
})


ga = ggarrange(indicators$Sponge, indicators.db$Sponge, indicators.cl.sc1[[1]]$Sponge + guides(colour = guide_legend(override.aes = list(colour = "#FFFF00"), title = "Scenario 1.1"), size = F), indicators.cl.sc1[[2]]$Sponge + guides(colour = guide_legend(override.aes = list(colour = "#FFFF00"), title = "Scenario 1.2"), size = F), indicators.cl.sc1[[3]]$Sponge + guides(colour = guide_legend(override.aes = list(colour = "#FFFF00"), title = "Scenario 2.1"), size = F), indicators.cl.sc1[[4]]$Sponge + guides(colour = guide_legend(override.aes = list(colour = "#FFFF00"), title = "Scenario 2.2"), size = F), ncol = 3, nrow = 2, widths = c(1,1,1), heights = c(1,1,1), common.legend = FALSE, legend="bottom" )

png(filename = paste0(SumStatsdir, "Bay_of_Biscay_scenarios_eg_sponge_indicator_6_panel.png"),
    width = 5760/3, height = 3600/3, units = "px", pointsize = 12,
    bg = "white", res = 400/3, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga)
dev.off()

######### --------------------- END of 6 panel plot for report ------------------------- ###################
