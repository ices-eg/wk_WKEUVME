
# downloads and crops the VME elements

  pathelements <- paste(pathdir_nogit,"VME data repository/VME elements",sep="/")
  Canyons <- readOGR(dsn = pathelements, layer="Canyons_ICES_Ecoregion") 
  Canyons <- cbind(Canyons,coordinates(Canyons))
  Canyons <- subset(Canyons,Canyons@data$X1 > -17 & Canyons@data$X1 < 2)
  Canyons <- subset(Canyons,Canyons@data$X2 >  36 & Canyons@data$X2 < 64)
  
  ContinentalSlope <- readOGR(dsn = pathelements, layer="ContinentalSlope_ICES_Ecoregion") 
  ContinentalSlope <- cbind(ContinentalSlope,coordinates(ContinentalSlope))
  ContinentalSlope <- subset(ContinentalSlope,ContinentalSlope@data$X1 > -17 & ContinentalSlope@data$X1 < 2)
  ContinentalSlope <- subset(ContinentalSlope,ContinentalSlope@data$X2 >  36 & ContinentalSlope@data$X2 < 64)
  
  Escarpments <- readOGR(dsn = pathelements, layer="Escarpments_ICES_Ecoregion") 
  Escarpments <- cbind(Escarpments,coordinates(Escarpments))
  Escarpments <- subset(Escarpments,Escarpments@data$X1 > -17 & Escarpments@data$X1 < 2)
  Escarpments <- subset(Escarpments,Escarpments@data$X2 >  36 & Escarpments@data$X2 < 64)
  
  Flanks <- readOGR(dsn = pathelements, layer="Flanks_ICES_Ecoregions") 
  Flanks <- cbind(Flanks,coordinates(Flanks))
  Flanks <- subset(Flanks,Flanks@data$X1 > -17 & Flanks@data$X1 < 2)
  Flanks <- subset(Flanks,Flanks@data$X2 >  36 & Flanks@data$X2 < 64)
  
  GlacialTroughs <- readOGR(dsn = pathelements, layer="GlacialTroughs_ICES_Ecoregion") 
  GlacialTroughs <- cbind(GlacialTroughs,coordinates(GlacialTroughs))
  GlacialTroughs <- subset(GlacialTroughs,GlacialTroughs@data$X1 > -17 & GlacialTroughs@data$X1 < 2)
  GlacialTroughs <- subset(GlacialTroughs,GlacialTroughs@data$X2 >  36 & GlacialTroughs@data$X2 < 64)
  
  Guyots <- readOGR(dsn = pathelements, layer="Guyots_ICES_Ecoregion") 
  Guyots <- cbind(Guyots,coordinates(Guyots))
  Guyots <- subset(Guyots,Guyots@data$X1 > -17 & Guyots@data$X1 < 2)
  Guyots <- subset(Guyots,Guyots@data$X2 >  36 & Guyots@data$X2 < 64)
  
  Ridges <- readOGR(dsn = pathelements, layer="Ridges_ICES_Ecoregion") 
  Ridges <- cbind(Ridges,coordinates(Ridges))
  Ridges <- subset(Ridges,Ridges@data$X1 > -17 & Ridges@data$X1 < 2)
  Ridges <- subset(Ridges,Ridges@data$X2 >  36 & Ridges@data$X2 < 64)
  
  Seamounts <- readOGR(dsn = pathelements, layer="Seamounts_ICES_Ecoregion") 
  Seamounts <- cbind(Seamounts,coordinates(Seamounts))
  Seamounts <- subset(Seamounts,Seamounts@data$X1 > -17 & Seamounts@data$X1 < 2)
  Seamounts <- subset(Seamounts,Seamounts@data$X2 >  36 & Seamounts@data$X2 < 64)
  
  SteepSlopes_on_Ridges <- readOGR(dsn = pathelements, layer="SteepSlopes_on_Ridges_ICES_Ecoregions") 
  SteepSlopes_on_Ridges <- cbind(SteepSlopes_on_Ridges,coordinates(SteepSlopes_on_Ridges))
  SteepSlopes_on_Ridges <- subset(SteepSlopes_on_Ridges,SteepSlopes_on_Ridges@data$X1 > -17 & SteepSlopes_on_Ridges@data$X1 < 2)
  SteepSlopes_on_Ridges <- subset(SteepSlopes_on_Ridges,SteepSlopes_on_Ridges@data$X2 >  36 & SteepSlopes_on_Ridges@data$X2 < 64)

