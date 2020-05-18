
##########################################################
###### script to run ICES WKEUVME workshop outputs #######
##########################################################

#### data inputs
# 1- c-squares and depths per ecoregion (Greater North Sea, Celtic Seas, Bay of Biscay and Iberian Coast)
# 2- ICES_ecoregions shapefiles
# 3- EEZ shapefiles
# 4- VME weighting -- ICES WGDEC (located on WKEUVME sharepoint, restricted access)
# 5- VMS data -- ICES WGSFD (located on WKEUVME sharepoint, restricted access)

#### set path to folder
  pathdir <- "C:/Users/pdvd/Online for git/WKEUVME"
  pathdir_nogit <- "C:/Users/pdvd/Online for git/WKEUVME_noGIT" # only works after downloading data from sharepoint
  
#### get output
  library(sf)
  library(tidyverse)
  
#### get output per ecoregion
  EcoReg <-  "Celtic Seas"  # "Celtic Seas" or "Bay of Biscay and the Iberian Coast"
  source(paste(pathdir,"3-Data analysis/Code_to_get_data_figures_tables_per_ecoregion.R",sep="/"))

#### make plots and tables
  library(rgdal)
  library(sp)
  library(ggplot2)
  library(rgeos)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)

  source(paste(pathdir,"4-Code for figures and tables/Producing figures and tables.R",sep="/"))

#### make interactive maps
  library(leaflet)
  library(htmlwidgets)
  
  source(paste(pathdir,"6-Utilities/dynamic_map.R",sep="/"))
  
