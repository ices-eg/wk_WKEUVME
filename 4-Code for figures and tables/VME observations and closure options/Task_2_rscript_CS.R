#-------------------------------------------------------------------------------------
# WKEUVME 18th - 28th May 2020
# Task 2 - Celtic Seas region
# David Stirling (David.Stirling@gov.scot)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Notes:
# Produce maps and summary statistics on the vme database records within the following hierachy of areas:
# 1. Celtic Sea Ecoregion
# 2. Depth band (for whole region, or for subregions? Or, both?)
# 3. Sub regions of this - based on the 9 sub area polygon drawn up with Georgios

# Useful categories to produce summery statistics on, e.g.: Vme Indicator type, vme habitat class, no of records by different collection methods, by institution, vessel?, 
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#--------------------- Preamble ---------------------
#-------------------------------------------------------------------------------------
# clean slate
rm(list=ls())
.rs.restartR()

# Flags
recompute = FALSE

# work directory
wd = "E:/ICES - WKEUVME/R work/"
wd.t2 = "E:/ICES - WKEUVME/Task 2/R_work/"

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
               "dplyr",
               "ggmap",
               "RColorBrewer",
               "wesanderson",
               "ggnewscale",
               "ggspatial",
               "ggpubr"
))
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#--------------------- Polygons for hierarchical areas ---------------------
#-------------------------------------------------------------------------------------
# Celtic Sea
CS = readOGR("E:/ICES WGDEC 2020/GIS", layer = "CS_Ices_ecoregion")
plot(CS)

# Depth band for CS - NB I derived this, so for consistency within the workshop I will use the supplied depth band polygon
source("E:/ICES - WKEUVME/R scripts/Derive depth contour polygon.r")
depth_band = gIntersection(depth_band_poly, CS)
plot(depth_band,col = "blue", add = T)

#-------------------------------------------------------------------------------------
# --------------------- VME database - points / lines ---------------------
#-------------------------------------------------------------------------------------
# For VME DB extraction: vme_extraction_25052020v2 - extraction without absences
vmedb = read.csv("E:/ICES - WKEUVME/Data folder - restricted data/VME extraction no abs - carlos/VME_Extraction_of_precenses_25052020_v2.csv")
head(vmedb)
coordinates(vmedb) = ~MiddleLongitude+MiddleLatitude
vmedb@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Recode some columns for convenience
# fix geometry coding
vmedb$GeometryType = ifelse(vmedb$GeometryType == "point" | vmedb$GeometryType == "Point", "Point", "Line")
summary(as.factor(vmedb$GeometryType))
# recode Survey methods
surv.meth = read.csv(paste0(wd.t2, "Survey_method_codes_meaning.csv"))
vmedb$SurveyMethod = recode(vmedb$SurveyMethod, AGT = "Agassiz trawl", BC = "Box corer",BOT = "Bottom trawl",GOV = "GOV trawl", GRT = "Granton trawl",JDT = "Jackson trawl",`NULL` = "NULL",PHOTO = "Photo",ROT = "Rock hopper otter trawl",SIDC = "Seabed imagery - drop camera",SIROV = "Seabed imagery - ROV system",SITC = "Seabed imagery - towed camera", UT = "Unknown trawl (legacy data only)", `VID-ToW` = "Video - towed video")
# Recode VME Sub Habitat Column: NULL -> No subhabitat and shorten some of the other classes as much too long for plotting later)
summary(as.factor(vmedb$VME_Habitat_Subtype))
vmedb$VME_Habitat_Subtype = recode(vmedb$VME_Habitat_Subtype, `NULL` = "No subhabitat", "Hard-bottom coral garden: hard-bottom gorgonian and black coral gardens" = "HBCG: gorgonian and black coral gardens", "Soft-bottom coral field: cup-coral fields" = "SBCF: cup-coral fields", "Soft-bottom coral garden: soft-bottom gorgonian and black coral gardens" = "SBCG: gorgonian and black coral gardens", "Hard-bottm coral garden: colonial scleractinians on rocky outcrops" = "HBCG: colonial scleractinians on rocky outcrops", "Soft-bottom coral garden: cauliflower coral fields" = "SBCG: cauliflower coral fields", "Hard-bottom coral garden: non-reefal scleractinian aggregations" = "HBCG: non-reefal scleractinian aggregations", "Hard-bottom coral garden: Stylasterid corals on hard substrata" = "HBCG: Stylasterid corals on hard substrata")
summary(as.factor(vmedb$VME_Habitat_Subtype))
unique(vmedb$VME_Habitat_Subtype)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#--------------------- CS area ---------------------
#-------------------------------------------------------------------------------------
vme.cs = vmedb[complete.cases(sp::over(vmedb, CS)),]
# check this using different method: gIntersection
# test1 = gIntersection(vmedb, CS)
# test1 # 3771 spatial points
# length(unique(vme.cs@coords))/2 # 3771 points - same as test1 
plot(vme.cs, add = T, pch = ".", cex = 3, col = "red")

# fields of interest: 5 "VME_Ind"  6 "HbttTyp" 24 "Ship" 25 "SrvyMth" 32 "GmtryTy" 36 "DatOwnr" 48 "Country"
lapply(c(5,6,25,32,48), function(i) unique(vme.cs@data[,i]))

foi = lapply(c(5,6,25, 32,48), function(x){
  a = lapply(unique(vme.cs@data[,x]), function(i) vme.cs[vme.cs@data[,x] == i,])
  names(a) = unique(vme.cs@data[,x])
  a
})
names(foi) = c("VME indicator type", "VME habitat type", "Survey method", "Geometry type", "Country") 

# for Page 75 - 81 of the report (summary statistics)
# VME habitats records total - i.e. non-NULL entries
sum(ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType))[c(1:4,6:9),2])
# VME indicators - all entries in the VME database in the subarea?
length(vme.cs)
# survey methods
ddply(vme.cs@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod))
write.csv(ddply(vme.cs@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod)), file = paste0(wd.t2,"Celtic_Seas_SurveyMethod_summary.csv"), row.names = F)
# habitats by group
ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType))
write.csv(ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType)), file = paste0(wd.t2,"Celtic_Seas_VME_HabitatType_summary.csv"), row.names = F)
# indicators by group 
ddply(vme.cs@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator))
write.csv(ddply(vme.cs@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator)), file = paste0(wd.t2,"Celtic_Seas_VME_Indicator_summary.csv"), row.names = F)
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# --------------------- Depth Band area ---------------------
#-------------------------------------------------------------------------------------
vme.dbnd = vmedb[complete.cases(sp::over(vmedb, depth_band)),]
plot(CS)
plot(depth_band,col = "blue", add = T)
plot(vme.dbnd, col = "red", add = T)

foi.dbnd = lapply(c(5,6,25,32,48), function(x){
  a = lapply(unique(vme.dbnd@data[,x]), function(i) vme.dbnd[vme.dbnd@data[,x] == i,])
  names(a) = unique(vme.dbnd@data[,x])
  a
})
names(foi.dbnd) = c("VME indicator type", "VME habitat type", "Survey method", "Geometry type", "Country") 
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# --------------------- Summary / descriptive stats ---------------------
#-------------------------------------------------------------------------------------
ddply(vme.dbnd@data, .(VME_Indicator, HabitatType, SurveyMethod, GeometryType, Country), summarise, Number_of_records = length(VME_Indicator), Habitats = length(HabitatType))

(write.csv(ddply(vme.dbnd@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator), N = sum(na.omit(as.numeric(Number))), W_kg = sum(na.omit(as.numeric(Weight_kg)))), file = paste0(wd.t2, "Depth_band_VME_Ind_summary.csv")))
(write.csv(ddply(vme.dbnd@data, .(HabitatType), summarise, NoR = length(HabitatType)), file = paste0(wd.t2, "Depth_band_VME_hab_summary.csv")))
(write.csv(ddply(vme.dbnd@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod)), file = paste0(wd.t2, "Depth_band_Survey_method_summary.csv")))
(write.csv(ddply(vme.dbnd@data, .(GeometryType), summarise, NoR = length(GeometryType)), file = paste0(wd.t2, "Depth_band_Geometry_Type_summary.csv"))) 
#-------------------------------------------------------------------------------------
# NB the Number of records is probably the one to go with here



#-------------------------------------------------------------------------------------
# --------------------- Subareas ---------------------
#-------------------------------------------------------------------------------------
# subarea
subareas = readOGR("E:/ICES - WKEUVME/Task 2","Celtic_Seas_9_subareas")
plot(CS)
plot(depth_band, add = T, col = "blue")
plot(subareas, add = T, col = "grey", alpha = 0.3)


# 9 subareas, 5 fields  
subareas.lists =  lapply(unique(subareas$Area_Name), function(i) {
  a = subareas[subareas$Area_Name == i,]
  names(a) = i
  b = vmedb[complete.cases(sp::over(vmedb, a)),]
  foi = lapply(c(5,6,25,32,48), function(x){
    g = lapply(unique(b@data[,x]), function(z) b[b@data[,x] == z,])
    names(g) = unique(b@data[,x])
    g
  })
  names(foi) = c("VME indicator type", "VME habitat type", "Survey method", "Geometry type", "Country") 
  foi
})
names(subareas.lists) = unique(subareas$Area_Name)

# Summary for all areas and fields of interest
# Indicators
IndSum = lapply(names(subareas.lists), function(h){
  do.call("rbind", lapply(subareas.lists[[h]][[1]], function(i){
    ddply(i@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator), N = sum(na.omit(as.numeric(Number))), W_kg = sum(na.omit(as.numeric(Weight_kg))))
  }))
})
names(IndSum) = names(subareas.lists)
# write out
lapply(names(IndSum), function(j) write.csv(IndSum[[j]], file = paste0(wd.t2, j, "_VME_Indicator_summary.csv"), row.names = F))
# Habitat Types
HabSum = lapply(names(subareas.lists), function(h){
  do.call("rbind", lapply(subareas.lists[[h]][[2]], function(i){
    ddply(i@data, .(HabitatType), summarise, NoR = length(HabitatType))
  }))
})
names(HabSum) = names(subareas.lists)
HabSum
# write out
lapply(names(HabSum), function(j) write.csv(HabSum[[j]], file = paste0(wd.t2, j, "_VME_Habitat_summary.csv"), row.names = F))
# Survey Types
SurSum = lapply(names(subareas.lists), function(h){
  do.call("rbind", lapply(subareas.lists[[h]][[3]], function(i){
    ddply(i@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod))
  }))
})
names(SurSum) = names(subareas.lists)
# write out
lapply(names(SurSum), function(j) write.csv(SurSum[[j]], file = paste0(wd.t2, j, "_Survey_method__summary.csv"), row.names = F))




# Depth band within area
# 9 subareas.db, 5 fields  
subareas.db.lists =  lapply(unique(subareas$Area_Name), function(i) {
  a = subareas[subareas$Area_Name == i,]
  names(a) = i
  b = vme.dbnd[complete.cases(sp::over(vme.dbnd, a)),]
  foi = lapply(c(5,6,25,32,48), function(x){
    g = lapply(unique(b@data[,x]), function(z) b[b@data[,x] == z,])
    names(g) = unique(b@data[,x])
    g
  })
  names(foi) = c("VME indicator type", "VME habitat type", "Survey method", "Geometry type", "Country") 
  foi
})
names(subareas.db.lists) = unique(subareas$Area_Name)

# Summary for all areas and fields of interest
# Indicators
IndSum.db = lapply(names(subareas.db.lists), function(h){
  do.call("rbind", lapply(subareas.db.lists[[h]][[1]], function(i){
    ddply(i@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator), N = sum(na.omit(as.numeric(Number))), W_kg = sum(na.omit(as.numeric(Weight_kg))))
  }))
})
names(IndSum.db) = names(subareas.db.lists)
lapply(names(IndSum.db), function(j) write.csv(IndSum.db[[j]], file = paste0(wd.t2, j, "_DepthBand_VME_Indicator_summary.csv"), row.names = F))
# Habitat Types
HabSum.db = lapply(names(subareas.db.lists), function(h){
  do.call("rbind", lapply(subareas.db.lists[[h]][[2]], function(i){
    ddply(i@data, .(HabitatType), summarise, NoR = length(HabitatType))
  }))
})
names(HabSum.db) = names(subareas.db.lists)
lapply(names(HabSum.db), function(j) write.csv(HabSum.db[[j]], file = paste0(wd.t2, j, "_DepthBand_VME_Habitat_summary.csv"), row.names = F))
# Survey Types
SurSum.db = lapply(names(subareas.db.lists), function(h){
  do.call("rbind", lapply(subareas.db.lists[[h]][[3]], function(i){
    ddply(i@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod))
  }))
})
names(SurSum.db) = names(subareas.db.lists)
lapply(names(SurSum.db), function(j) write.csv(SurSum.db[[j]], file = paste0(wd.t2, j, "_DepthBand_Survey_method__summary.csv"), row.names = F))


#---------------------------------------
#--------------------- Plots ---------------------
#---------------------------------------

# @ Ecoregion level as I don't think there is much merit in including the depth band plots as the information is essentially contained within these.

# Code for general / data exploratory plots included as well as those for the figures in the report - these are labelled 

# Setting up:

# Reproject & prep data:
# Ecoregion area polygon to crop by for eco region
p2 = readOGR(dsn = "E:/ICES - WKEUVME/R work", layer = "Celtic Sea area polygon")
# depth band
depth_band.proj = spTransform(depth_band, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
depth_band_poly.df =  broom::tidy(depth_band.proj, region = "DN")
# land polygons
land = readOGR("E:/ICES - WKEUVME/GIS layers", "GSSHS_subset")
land = spTransform(land, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
land = gIntersection(land, p2) # ignore warnings - it works!
land.df = broom::tidy(land, region = "id")
# Ecoregion
CS.proj = spTransform(CS, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
CS.df = broom::tidy(CS.proj, region = "Ecoregion")
# Celtic sea base raster (GEBCO)
CS.bm = raster(paste0(wd, "Celtic_Sea.GEBCO.base.map_utm29n.tif"))
# hillshade
terr = terrain(CS.bm, c("slope", "aspect"))
hill = hillShade(terr[["slope"]], terr[["aspect"]])
# subareas
subareas.proj = spTransform(subareas, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
subareas.df = broom::tidy(subareas.proj, region = "Area_Name")

# convert rasters to data.frame
CS.bm.df = as.data.frame(rasterToPoints(CS.bm))
names(CS.bm.df) = c('Longitude', 'Latitude', 'Value')
hill.df = as.data.frame(rasterToPoints(hill))
names(hill.df) = c('Longitude', 'Latitude', 'Value')

# get center of r
mid.Lon = mean(CS.bm.df$Longitude)
mid.Lat = mean(CS.bm.df$Latitude)


# VME indicators
# Sort plotting order
# Indicators
names(foi$`VME indicator type`)
foi$`VME indicator type` = foi$`VME indicator type`[sort(names(foi$`VME indicator type`))]
# check
names(foi$`VME indicator type`)
# colours
my.col = colorRampPalette(brewer.pal(11, "Spectral"))(11)
names(my.col) = names(foi$`VME indicator type`)
# plot objects
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
    geom_point(aes(x = long, y = lat), data = d[d$Number == "NULL" | d$Number == 0,], col = "black", fill = "black", pch = 21, cex = 1, alpha = 0.5) +
    geom_point(aes(x = long, y = lat, size = round( as.numeric(Number) ), fill = VME_Ind), data = d[d$Number >= 1,], col = "black", pch = 21, alpha = 0.7) +
    scale_size_continuous(range = c(3,8), breaks = pretty(round(as.numeric(d$Number)), n = 4) ) +
    scale_fill_manual(values=my.col[[i]]) +
    labs(title=paste0(i), x = "Longitude", y = "Latitude", size = "N") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.02, model = 'WGS84', transform = FALSE, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.3, "cm"),  width = unit(.3, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()  + 
    guides(fill = F)
})

#----------------- Figure 7 - Panel plot of VME indicators ---------------------
ga7 = ggarrange(indicators[[1]], indicators[[2]],indicators[[3]], indicators[[4]],indicators[[6]], indicators[[7]], indicators[[8]],indicators[[9]], indicators[[10]], indicators[[11]], ncol = 5, nrow = 2, widths = c(1,1,1,1,1), heights = c(1,1), common.legend = FALSE, legend="right" )

png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 7v2.png"),
    width = 7200, height = 4000, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga7)
dev.off()

# reduced file size
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 7v2_reduced.png"),
    width = 7200/3, height = 4000/3, units = "px", pointsize = 12,
    bg = "white", res = 300/3, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga7)
dev.off()

# NB A4 measures 210 × 297 millimeters == w:h ~1:1.4
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 7_a.png"),
    width = 4100, height = 2880, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
ga7a = ggarrange(indicators[[1]], indicators[[2]],indicators[[3]], indicators[[4]],indicators[[6]], indicators[[7]], ncol = 3, nrow = 2, widths = c(1,1), heights = c(1,1), common.legend = FALSE, legend="right" )
print(ga7a)
dev.off()
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 7_b.png"),
    width = (4100/3)*2, height = 2880, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
ga7b = ggarrange(indicators[[8]],indicators[[9]], indicators[[10]], indicators[[11]], ncol = 2, nrow = 2, widths = c(1,1), heights = c(1,1), common.legend = FALSE, legend="right" )
print(ga7b)
dev.off()
#-------------------------------------------------------------------------------

# Sort plotting order
# Habitats
names(foi$`VME habitat type`)
foi$`VME habitat type` = foi$`VME habitat type`[sort(names(foi$`VME habitat type`))]
# check
names(foi$`VME habitat type`)
# colours
my.col.hab = colorRampPalette(brewer.pal(12, "Spectral"))(9)
names(my.col.hab) = names(foi$`VME habitat type`)
# VME habitats
habitat = lapply(names(foi$`VME habitat type`), function(i){
  d = ggplot2::fortify(foi$`VME habitat type`[[i]]@data)
  # reproject data to utm29n
  x = foi$`VME habitat type`[[i]]@coords[,1]
  y = foi$`VME habitat type`[[i]]@coords[,2]
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
    geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
    geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
    geom_point(aes(x = long, y = lat, fill = HbttTyp), data = d, col = "black", pch = 21, cex = 3, alpha = 0.75) +
    labs(title=paste0("\nCeltic Seas Ecoregion\n","VME habitat type: ", i ), x = "Longitude", y = "Latitude") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()
})
names(habitat) = names(foi$`VME habitat type`)





# VME habitat subtype
# colours
my.col.hbst = colorRampPalette(brewer.pal(11, "Spectral"))(9)
names(my.col.hbst) = names(foi$`VME habitat type`)

habitats.cs.by.hab.sutype = lapply(names(foi$`VME habitat type`), function(i){
  d = ggplot2::fortify(foi$`VME habitat type`[[i]]@data)
  d$long = foi$`VME habitat type`[[i]]@coords[,1]
  d$lat = foi$`VME habitat type`[[i]]@coords[,2]
  # reproject data to utm29n
  x = foi$`VME habitat type`[[i]]@coords[,1]
  y = foi$`VME habitat type`[[i]]@coords[,2]
  xy = data.frame(cbind(x,y))
  xyp = project(as.matrix(xy), "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  xyp = SpatialPointsDataFrame(coords = xyp, data = xy,proj4string = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  d$long = xyp@coords[,1]
  d$lat = xyp@coords[,2]
  # ggplot
  ggplot()+
    geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
    geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
    scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
    scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
    geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
    geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
    geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
    new_scale_fill()+
    geom_point(aes(x = long, y = lat, fill = VME_H_S), data = d[d$VME_H_S != "NULL",], col = "black", pch = 21, alpha = 0.75, size = 3) +
    scale_fill_manual(values=colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(d$VME_H_S)))) +
    labs(title=paste0( i ), x = "Longitude", y = "Latitude", fill = "VME habitat subtype") +
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic() + theme(legend.text=element_text(size=12))
})
names(habitats.cs.by.hab.sutype) = names(foi$`VME habitat type`)

#----------------- Figure 6 - Panel plot of VME habitats & sub types ---------------------
ga6 = ggpubr::ggarrange(habitats.cs.by.hab.sutype[[1]], habitats.cs.by.hab.sutype[[2]],habitats.cs.by.hab.sutype[[3]], habitats.cs.by.hab.sutype[[4]],habitats.cs.by.hab.sutype[[6]], habitats.cs.by.hab.sutype[[8]], ncol = 3, nrow = 2, widths = c(1,1,1), heights = c(1,1), common.legend = FALSE, legend="right" , align = "hv")
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 6.png"),
    width = 8000, height = 5000, units = "px", pointsize = 12,
    bg = "white", res = 320, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga6)
dev.off()

# reduced file size!!!
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_figure 6_red.png"),
    width = 8000/3, height = 5000/3, units = "px", pointsize = 12,
    bg = "white", res = 320/3, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga6)
dev.off()
#-------------------------------------------------------------------------------




#------------------- Figure 8 subareas plot ---------------------------------------------

# Subarea plot
subareas.t2 = do.call("rbind", foi$`VME indicator type`)
subareas.t2 = subareas.t2[subareas.t2$VME_Ind != "NULL",] # remove NULL entries for when they are all being plotted together
subareas.t2 = spTransform(subareas.t2, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
my.col2 = colorRampPalette(brewer.pal(8, "Dark2"))(9)
subareas.t2.p = ggplot()+
  geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
  geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.5, show.legend = F) + 
  scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
  scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
  geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
  geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
  geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
  new_scale_color() +
  new_scale_fill() +
  geom_point(aes(x = subareas.t2@coords[,1], y = subareas.t2@coords[,2]), data = subareas.t2@data, col = "grey", pch = 19, cex = 0.5, alpha = 0.3) +
  geom_polygon(data = subareas.df, aes(x = long, y = lat, group = group, fill = id), col = NA, alpha = .8) +
  scale_colour_brewer(palette="Set3") +
  scale_fill_manual(values=my.col2) +
  labs(x = "Longitude", y = "Latitude", fill = "Subareas") +
  ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
  annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
  coord_sf(crs = 32629)+
  theme_classic()

# pdf(paste0(wd.t2, "Subarea_map.pdf"), width = 8, height = 10)
# subareas.t2.p
# dev.off()
png(filename = paste0(wd.t2, "Subarea_map.png"),
    width = 3600, height = 3600, units = "px", pointsize = 12,
    bg = "white", res = 350, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(subareas.t2.p)
dev.off()

# reduced file size
png(filename = paste0(wd.t2, "Subarea_map_reduced.png"),
    width = 3600/3, height = 3600/3, units = "px", pointsize = 12,
    bg = "white", res = 350/3, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(subareas.t2.p)
dev.off()
#----------------------------------------------------------------------------------------------










# One map based on aes - colour
# Indicators
ind.all = do.call("rbind", foi$`VME indicator type`)
ind.all = ind.all[ind.all$VME_Ind != "NULL",] # remove NULL entries for when they are all being plotted together
ind.all = spTransform(ind.all, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
my.col = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(ind.all$VME_Ind)))

ind.all.p = ggplot()+
  geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
  geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
  scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
  scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
  geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
  geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
  geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
  new_scale_color() +
  
  geom_point(aes(x = ind.all@coords[,1], y = ind.all@coords[,2], colour = VME_Ind), data = ind.all@data, pch = 19, alpha = 0.75, cex = 3) +
  scale_colour_manual(values=my.col) +
  
  labs(title=paste0("\nCeltic Seas Ecoregion\n","VME Indicator" ), x = "Longitude", y = "Latitude", colour = "VME Indicator") +
  ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
  annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
  coord_sf(crs = 32629)+
  theme_classic()
# pdf(paste0(wd.t2, "VME_Indicator_type_One_map.pdf"), width = 8, height = 10)
# ind.all.p
# dev.off()
png(filename = paste0(wd.t2, "Celtic Seas VME_Indicator_type_One_map.png"),
    width = 2880, height = 3600, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ind.all.p)
dev.off()

# Habitats
hab.all = do.call("rbind", foi$`VME habitat type`)
hab.all = hab.all[hab.all$HbttTyp != "NULL",] # remove NULL entries for when they are all being plotted together
hab.all = spTransform(hab.all, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
my.col = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(hab.all$HbttTyp)))

hab.all.p = ggplot()+
  geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
  geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
  scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
  scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
  geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
  geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
  geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
  new_scale_color() +
  
  
  geom_point(aes(x = hab.all@coords[,1], y = hab.all@coords[,2], colour = HbttTyp), data = hab.all@data, pch = 19, alpha = 0.75, cex = 3) +
  scale_colour_manual(values=my.col) +
  
  labs(title=paste0("\nCeltic Seas Ecoregion\n","VME Habitat Type" ), x = "Longitude", y = "Latitude", colour = "VME Habitat Type") +
  ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
  annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
  coord_sf(crs = 32629)+
  theme_classic()

# pdf(paste0(wd.t2, "VME_habitat_type_One_map.pdf"), width = 8, height = 10)
# ind.all.p
# dev.off()
png(filename = paste0(wd.t2, "Celtic Seas VME_habitat_type_One_map.png"),
    width = 2880, height = 3600, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(hab.all.p)
dev.off()

# Survey methods
# Survey methods

surv.all = do.call("rbind", foi$`Survey method`)
surv.all = surv.all[surv.all$SrvyMth != "NULL",] # remove NULL entries for when they are all being plotted together
surv.all = spTransform(surv.all, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
my.col = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(surv.all$SrvyMth)))
surv.all.p = ggplot()+
  geom_tile(data=CS.bm.df, aes(x=Longitude, y=Latitude, colour=Value), alpha=0.6, show.legend = F) + 
  geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
  scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
  scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
  geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
  geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
  geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
  new_scale_color() +
  
  geom_point(aes(x = surv.all@coords[,1], y = surv.all@coords[,2], colour = SrvyMth), data = surv.all@data, pch = 19, alpha = 0.75, cex = 3) +
  scale_colour_manual(values=my.col) +
  
  labs(title=paste0("\nCeltic Seas Ecoregion\n","Survey method" ), x = "Longitude", y = "Latitude", colour = "VME Survey Method") +
  ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
  annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
  coord_sf(crs = 32629)+
  theme_classic()

# pdf(paste0(wd.t2, "VME_Survey_methods_type_One_map.pdf"), width = 8, height = 10)
# geom.all.p
# dev.off()

png(filename = paste0(wd.t2, "Celtic Seas Survey_Method_One_map.png"),
    width = 2880, height = 3600, units = "px", pointsize = 12,
    bg = "white", res = 300, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(surv.all.p)
dev.off()

