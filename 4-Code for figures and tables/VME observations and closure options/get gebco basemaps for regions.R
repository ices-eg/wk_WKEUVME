#-------------------------------------------------------------------------------------
# WKEUVME 18th - 28th May 2020
# VME Database summary and plot
# David Stirling (David.Stirling@gov.scot)
#-------------------------------------------------------------------------------------

# Derive gebco bathymetry base maps
# GEBCO: citation: GEBCO Compilation Group (2020) GEBCO 2020 Grid (doi:10.5285/a29c5465-b138-234de053-6c86abc040b9).


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

# functions
# functions
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
               "wesanderson"
               
))
#-------------------------------------------------------------------------------------



# Celtic Sea
CS = readOGR("E:/ICES WGDEC 2020/GIS", layer = "CS_Ices_ecoregion")
plot(CS)

# Depth band for CS - NB I derived this, so for consistency within the workshop I will use the supplied depth band polygon
source("E:/ICES - WKEUVME/R scripts/Derive depth contour polygon.r")
depth_band = gIntersection(depth_band_poly, CS)
plot(depth_band,col = "blue", add = T)

r1 = raster("E:/gebco_2020_geotiff/gebco_2020_n90.0_s0.0_w0.0_e90.0.tif")
r2 = raster("E:/gebco_2020_geotiff/gebco_2020_n90.0_s0.0_w-90.0_e0.0.tif")

r3 = c(r1, r2)
r4 = do.call(merge, c(r3, tolerance = 0))

# Celtic Seas area to crop to: c(-18,47,4,64)
coords = matrix(c(-25, 40,
                  8, 40,
                  8, 72,
                  -25, 72,
                  -25, 40), 
                ncol = 2, byrow = TRUE)
P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(CS)
plot(Ps1, add = T, col = "green", axes = TRUE)
pid = sapply(slot(Ps1, "polygons"), function(x) slot(x, "ID"))
p.df = data.frame( ID=1:length(Ps1), row.names = pid) 
p = SpatialPolygonsDataFrame(Ps1, p.df)
class(p)
p@proj4string
# crop
r5 = crop(r4, p)
r5
my.col = rev(colorRampPalette(brewer.pal(5, "Greys"))(200))
plot(r5, col = my.col, alpha = 0.5)
plot(CS, add = T)
plot(depth_band, col="blue", add = T)
writeRaster(r5, paste0(wd, "Celtic_Sea.GEBCO.base.map.tif"), overwrite = T)

r5 = aggregate(r5, 5)
r5 = projectRaster(r5,crs = crs("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))#,res = 1)
r5
plot(r5)


# shapefile to crop to with extent larger than projected ecoregion:
coords2 = matrix(c(13459.01-(13459.01/100)*6, 5316750-(5316750/100),
                   1077523+(1077523/100)*6, 5316750-(5316750/100),
                   1077523+(1077523/100)*6, 7112729+(7112729/100),
                  13459.01-(13459.01/100)*6, 7112729+(7112729/100),
                  13459.01-(13459.01/100)*6, 5316750-(5316750/100)), 
                ncol = 2, byrow = TRUE)
P2 = Polygon(coords2)
Ps2 = SpatialPolygons(list(Polygons(list(P2), ID = "a")), proj4string=CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
pid2 = sapply(slot(Ps2, "polygons"), function(x) slot(x, "ID"))
p.df2 = data.frame( ID=1:length(Ps2), row.names = pid2) 
p2 = SpatialPolygonsDataFrame(Ps2, p.df2)
class(p2)
p2@proj4string
writeOGR(p2, dsn = "E:/ICES - WKEUVME/R work", layer = "Celtic Sea area polygon", driver = "ESRI Shapefile")

r5.c = crop(r5, p2)
plot(r5.c)
writeRaster(r5.c, paste0(wd, "Celtic_Sea.GEBCO.base.map_utm29n.tif"))


# Bay of Biscay and Iberian Coast area to crop to: c(-15,35,1,49)
bob=readOGR("E:/ICES WGDEC 2020/GIS", layer = "BoB_Ices_ecoregion")
coords3 = matrix(c(-20, 28,
                  6, 28,
                  6, 54,
                  -20, 54,
                  -20, 28), 
                ncol = 2, byrow = TRUE)
P3 = Polygon(coords)
Ps3 = SpatialPolygons(list(Polygons(list(P3), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
pid3 = sapply(slot(Ps3, "polygons"), function(x) slot(x, "ID"))
p.df3 = data.frame( ID=1:length(Ps3), row.names = pid3) 
p3 = SpatialPolygonsDataFrame(Ps3, p.df)
class(p3)
p3@proj4string

# crop
r6 = crop(r4, p3)
r6
plot(r6, col = my.col, alpha = 0.5)
plot(bob, add = T)
writeRaster(r6, paste0(wd, "Bay_of_Biscay.GEBCO.base.map.tif"), overwrite = T)

r7 = aggregate(r6, 5)
r8 = projectRaster(r7,crs = crs("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))#,res = 1)
r8
plot(r8)


# shapefile to crop to with extent larger than projected ecoregion:
extent(spTransform(bob, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

coords4 = matrix(c(81636.8-(81636.8/100)*6, 3983992-(3983992/100),
                   1199991+(1199991/100)*6, 3983992-(3983992/100),
                   1199991+(1199991/100)*6, 5326501+(5326501/100),
                   81636.8-(81636.8/100)*6, 5326501+(5326501/100),
                   81636.8-(81636.8/100)*6, 3983992-(3983992/100)), 
                 ncol = 2, byrow = TRUE)
P4 = Polygon(coords4)
Ps4 = SpatialPolygons(list(Polygons(list(P4), ID = "a")), proj4string=CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
pid4 = sapply(slot(Ps4, "polygons"), function(x) slot(x, "ID"))
p.df4 = data.frame( ID=1:length(Ps4), row.names = pid4) 
p4 = SpatialPolygonsDataFrame(Ps4, p.df4)
class(p4)
p4@proj4string

r8.c = crop(r8, p4)
plot(r8.c)
plot(spTransform(bob, CRSobj = CRS("+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")), add = T)

writeOGR(p4, dsn = "E:/ICES - WKEUVME/R work", layer = "Bay of Biscay area polygon", driver = "ESRI Shapefile")
writeRaster(r8.c, paste0(wd, "Bay of Biscay area.GEBCO.base.map_utm29n.tif"))
