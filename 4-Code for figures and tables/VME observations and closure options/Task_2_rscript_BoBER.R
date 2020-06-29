#-------------------------------------------------------------------------------------
# WKEUVME 18th - 28th May 2020
# Task 2 - Bay of Biscay Ecoregion
# David Stirling (David.Stirling@gov.scot)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Notes:
# Produce maps and summary statistics on the vme database records within the following hierachy of areas:
# 1. Bay of Biscay and Iberian Coast Ecoregion
# 2. Depth band (for whole region, or for subregions? Or, both?)

# Fields to produce summary statistics on: Vme Indicator type, vme habitat class, survey method

# I also need to produce the references for the records that underly the VME Index c-squares
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
# Bay of Biscay
CS = readOGR("E:/ICES WGDEC 2020/GIS", layer = "BoB_Ices_ecoregion")
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
plot(vme.cs, add = T, pch = ".", cex = 3, col = "red")

# fields of interest: 5 "VME_Ind"  6 "HbttTyp" 24 "Ship" 25 "SrvyMth" 32 "GmtryTy" 36 "DatOwnr" 48 "Country"
lapply(c(5,6,25,32,48), function(i) unique(vme.cs@data[,i]))

foi = lapply(c(5,6,25,32,48), function(x){
  a = lapply(unique(vme.cs@data[,x]), function(i) vme.cs[vme.cs@data[,x] == i,])
  names(a) = unique(vme.cs@data[,x])
  a
})
names(foi) = c("VME indicator type", "VME habitat type", "Survey method", "Geometry type", "Country") 
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# --------------------- Summary / descriptive stats ---------------------
#-------------------------------------------------------------------------------------
ddply(vme.cs@data, .(VME_Indicator, HabitatType, SurveyMethod, GeometryType, Country), summarise, Number_of_records = length(VME_Indicator), Habitats = length(HabitatType))

(write.csv(ddply(vme.cs@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator), N = sum(na.omit(as.numeric(Number))), W_kg = sum(na.omit(as.numeric(Weight_kg)))), file = paste0(wd.t2, "Bay_of_Biscay_ER_VME_Ind_summary.csv"), row.names = F))
(write.csv(ddply(vme.cs@data, .(HabitatType), summarise, NoR = length(HabitatType)), file = paste0(wd.t2, "Bay_of_Biscay_ER_VME_hab_summary.csv"), row.names = F))
(write.csv(ddply(vme.cs@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod)), file = paste0(wd.t2, "Bay_of_Biscay_ER_Survey_method_summary.csv"), row.names = F))
(write.csv(ddply(vme.cs@data, .(GeometryType), summarise, NoR = length(GeometryType)), file = paste0(wd.t2, "Bay_of_Biscay_ER_Geometry_Type_summary.csv"), row.names = F))
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
ddply(vme.dbnd@data,  .(VME_Indicator, HabitatType, SurveyMethod, GeometryType, Country), summarise, Number_of_records = length(VME_Indicator), Habitats = length(HabitatType))

(write.csv(ddply(vme.dbnd@data, .(VME_Indicator), summarise, NoR = length(VME_Indicator), N = sum(na.omit(as.numeric(Number))), W_kg = sum(na.omit(as.numeric(Weight_kg)))), file = paste0(wd.t2, "Bay_of_BiscayDepth_band_VME_Ind_summary.csv")))
(write.csv(ddply(vme.dbnd@data, .(HabitatType), summarise, NoR = length(HabitatType)), file = paste0(wd.t2, "Bay_of_BiscayDepth_band_VME_hab_summary.csv")))
(write.csv(ddply(vme.dbnd@data, .(SurveyMethod), summarise, NoR = length(SurveyMethod)), file = paste0(wd.t2, "Bay_of_BiscayDepth_band_Survey_method_summary.csv"))) 
(write.csv(ddply(vme.dbnd@data, .(GeometryType), summarise, NoR = length(GeometryType)), file = paste0(wd.t2, "Bay_of_BiscayDepth_band_Geometry_Type_summary.csv"))) 
#-------------------------------------------------------------------------------------
# NB the Number of records is probably the one to go with here



#---------------------------------------
#--------------------- Plots ---------------------
#---------------------------------------

# @ Ecoregion level as I don't think there is much merit in including the depth band plots as the information is essentially contained within these.
# Setting up:

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
CS.bm = raster(paste0(wd, "Bay of Biscay area.GEBCO.base.map_utm29n.tif"))
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


# Sort plotting order
# Indicators
names(foi$`VME indicator type`)
foi$`VME indicator type` = foi$`VME indicator type`[sort(names(foi$`VME indicator type`))]
# check
names(foi$`VME indicator type`)
foi$`VME indicator type`$`NULL` = NULL
# colours
my.col = colorRampPalette(brewer.pal(11, "Spectral"))(9)
names(my.col) = names(foi$`VME indicator type`)
# VME indicators
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
    geom_tile(data=hill.df, aes(x=Longitude, y=Latitude, fill=Value), alpha=0.3, show.legend = F) + 
    scale_fill_gradientn(colors=grey.colors(100), guide = FALSE) +
    scale_colour_gradientn(colors=grey.colors(100), guide = FALSE) +
    geom_polygon(data = land.df, aes(x = long, y = lat, group = group),col = "grey50", fill = "grey50", alpha = .5) +
    new_scale_fill()+
    geom_polygon(data = CS.df, aes(x = long, y = lat, group = group), col = "black", fill = NA, lwd = 1, show.legend = F) +
    geom_polygon(data = depth_band_poly.df, aes(x = long, y = lat, group = group), fill = "blue", alpha = .5) +
    geom_point(aes(x = long, y = lat, fill = VME_Indicator), size = 4, data = d, col = "black", pch = 21, alpha = 0.5, show.legend = F) +
    scale_fill_manual(values=my.col[[i]]) +
    labs(title= i, x = "Longitude", y = "Latitude", size = "N") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.02, model = 'WGS84', transform = FALSE, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()
})
names(indicators) = names(foi$`VME indicator type`)


ga = ggarrange(plotlist = indicators, ncol = 3, nrow = 3, widths = c(1,1,1), heights = c(1,1), common.legend = FALSE, legend="bottom" )
png(filename = paste0(wd.t2, "Bay of Biscay_VME_Indicator_Figure_7.png"),
    width = 3600/3, height = 3600/3, units = "px", pointsize = 12,
    bg = "white", res = 300/3, family = "", restoreConsole = TRUE,
    type = c("cairo-png"),
    symbolfamily="default")
print(ga)
dev.off()




# Sort plotting order
# Habitats
names(foi$`VME habitat type`)
foi$`VME habitat type` = foi$`VME habitat type`[sort(names(foi$`VME habitat type`))]
# check
names(foi$`VME habitat type`)
foi$`VME habitat type`$`NULL` = NULL
# colours
my.col.hab = colorRampPalette(brewer.pal(12, "Set3"))(5)
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
    new_scale_fill()+
    geom_point(aes(x = long, y = lat, fill = HabitatType), data = d, col = "black", pch = 21, size = 4, alpha = 0.75, show.legend = F) +
    scale_fill_manual(values=my.col.hab[[i]]) +
    labs(title=paste0( i ), x = "Longitude", y = "Latitude") +
    
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic()
})
names(habitat) = names(foi$`VME habitat type`)





# VME habitat subtype
# colours
my.col.hbst = colorRampPalette(brewer.pal(12, "Set3"))(5)
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
    geom_point(aes(x = long, y = lat, fill = VME_Habitat_Subtype), data = d[d$VME_Habitat_Subtype != "NULL",], col = "black", pch = 21, alpha = 0.75, size = 4) +
    scale_fill_manual(values=colorRampPalette(brewer.pal(12, "Set3"))(length(unique(d$VME_Habitat_Subtype)))) +
    labs(title=paste0( i ), x = "Longitude", y = "Latitude", fill = "VME habitat subtype") +
    ggsn::scalebar(CS.df, dist = 100, st.size=2, height=0.01, model = 'WGS84', transform = F, dist_unit = "km", location = "bottomright") +
    annotation_north_arrow(which_north = "true", height = unit(.5, "cm"),  width = unit(.5, "cm"), location = "tr", data = CS.bm.df) +
    coord_sf(crs = 32629)+
    theme_classic() + theme(legend.text=element_text(size=12))
})
names(habitats.cs.by.hab.sutype) = names(foi$`VME habitat type`)


#----------------- Figure 6 - Panel plot of VME habitats & sub types ---------------------
ga6 = ggpubr::ggarrange(plotlist = habitats.cs.by.hab.sutype, ncol = 3, nrow = 2, widths = c(1,1,1), heights = c(1,1), common.legend = FALSE, legend="right" , align = "hv")

png(filename = paste0(wd.t2, "Bay of Biscay_VME_Hab_sub_types_figure 6.png"),
width = 5000/3, height = 2500/3, units = "px", pointsize = 12,
bg = "white", res = 200/3, family = "", restoreConsole = TRUE,
type = c("cairo-png"),
symbolfamily="default")
print(ga6)
dev.off()
#-------------------------------------------------------------------------------
