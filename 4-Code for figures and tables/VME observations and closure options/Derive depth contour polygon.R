#-------------------------------------------------------------------------------------
# WKEUVME 18th - 28th May 2020
# Derive depth band contour polygon 400 - 800 m.
# David Stirling, May 2020
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Preamble
recompute = FALSE

# work directory
wd.dc = "E:/ICES - WKEUVME/R work/"

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
# NB the below only works when I'm logged into sys.admin - need to investigate the privalges set on the paths - function from: https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- "C:/Program Files/GDAL/gdal_polygonize.py"
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd.dc <- getwd.dc()
  on.exit(setwd.dc(owd.dc))
  setwd.dc(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}


# Load packages
instant_pkgs(c("rgdal",
               "raster",
               "sf",
               "rgeos",
               "maptools",
               "gridExtra",
               "sp",
               "fasterize",
               "ggplot2",
               "tidyverse",
               "devtools"
               
))
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Get the depth contour 400 - 800 m (using EMODNET smooth depth)
#-------------------------------------------------------------------------------------
if(!file.exists( paste0(wd.dc, "Emodnet_400_to_800m_band_cropped.tif") ) || recompute) {
  emod = raster("E:/Emodnet bathymetry/EMODnet_smooth_depth.tif")
  emod
  
  rclmat = matrix(c(-Inf, -800, NA,
                    -800, -400, 1,
                    -400, +Inf, NA), 
                  ncol = 3, byrow = TRUE)
  
  emod4to8 = reclassify(emod, rclmat)
  emod4to8
  plot(emod4to8)
  
  # write it out
  if(!file.exists( paste0(wd.dc, "Emodnet_400_to_800m_band.tif") ) || recompute) {
    writeRaster(emod4to8, filename = paste0(wd.dc, "Emodnet_400_to_800m_band.tif"))
  }
  
  # Crop to ICES ecoregions Celtic Sea and Bay of Biscay
  
  ices_eco = readOGR(dsn = "E:/ICES WGDEC 2020/GIS", layer = "BoB_andCS_ICES_ECOregions")
  
  as.character(ices_eco@proj4string) == as.character(emod4to8@crs)
  
  depth_band = crop(emod4to8, ices_eco)
  depth_band
  plot(depth_band)
  
  depth_band_masked = mask(depth_band, ices_eco)
  plot(depth_band_masked)
  
  
  writeRaster(depth_band_masked, filename = paste0(wd.dc, "Emodnet_400_to_800m_band_cropped.tif"))
}else depth_band_masked = raster(paste0(wd.dc, "Emodnet_400_to_800m_band_cropped.tif"))
#-------------------------------------------------------------------------------------
# polygonise - using gdal
#-------------------------------------------------------------------------------------
if(!file.exists( paste0(wd.dc, "400_to_800m_polygon.shp") ) || recompute) {
  # library("devtools")
  # install_github("ktaylora/landscapeAnalysis")
  library("landscapeAnalysis")
  depth_band_poly = landscapeAnalysis::rasterToPolygons(r = depth_band_masked, method = "gdal")
  
  depth_band_poly = gdal_polygonizeR(depth_band_masked)
  depth_band_poly
  plot(depth_band_poly)
  writeOGR(depth_band_poly, dsn = substr(wd.dc, 1, 24), layer = "400_to_800m_polygon", driver = "ESRI Shapefile")
}else depth_band_poly = readOGR(substr(wd.dc, 1, 24), layer = "400_to_800m_polygon_top_fixed")

depth_band_poly
# plot(depth_band_poly)
