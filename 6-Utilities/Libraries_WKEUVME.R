# libraries needed to run WK EUVME output

# ICES libraries
  #remotes::install_github("ices-tools-prod/icesSharePoint")
  library(icesSharePoint)
  
  # devtools::install_github("ices-tools-prod/icesVMS")
  # library(icesVMS), only needed to download VMS data from ICES data centre (data product is on sharepoint) 

# R libraries
  library(keyring)
  library(sf)
  library(tidyverse)
  library(rgdal)
  library(sp)
  library(ggplot2)
  library(ggpolypath)
  library(rgeos)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(maptools)
  library(raster)
  library(gridExtra)
  library(leaflet)
  library(htmlwidgets)
  library(smoothr)
  library(stringr)
  library(tidyr)
