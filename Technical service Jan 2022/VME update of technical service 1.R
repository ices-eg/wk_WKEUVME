
# Technical service 2 - VME part
#	Update the technical advice of 15 Dec. by including the descriptions 
# "VME_Csquare", "VME_Habitat", "VME_Indicator" and "VME_Elements" in the csv, 
# .xlsx and .shp files for  "all scenario and all options -updated". 

# set path to folders 
  pathdir <- "C:/Users/danie/Documents/Online for git/wk_WKEUVME"
  outdir_TS <- paste(pathdir,"Technical service Dec 2021/Output",sep="/")  # get folder technical service 1
  outdir_Ad <- paste(pathdir,"5-Output/Closure options (region)",sep="/")  # get folder advice
  outdir_TS2 <- paste(pathdir,"Technical service Jan 2022/Output/VME update",sep="/") # new folder for output

# get libraries
  source(paste(pathdir,"6-Utilities/Libraries_WKEUVME.R",sep="/"))
 
# set options + scenarios
  scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")

  for (sce in 1:4){
    # combine for .csv
    dat_TS <-  read.csv(paste(paste(outdir_TS,scedat[sce],sep="/"),"coords.csv",sep="_"),header=T,sep=",")
    dat_Ad <-   read.csv(paste(paste(outdir_Ad,scedat[sce],sep="/"),"coords.csv",sep="_"),header=T,sep=",")
    dat_TS <- cbind(dat_TS,dat_Ad[match(dat_TS$Poly_no,dat_Ad$Poly_No),c("VME_Csquare","VME_Habitat","VME_Indicator","VME_Elements")]) 
    colnames(dat_TS)[1] <- "Poly_No"
    #dat_TS$Longitude <- round(dat_TS$Longitude,digits = 3)
    #dat_TS$Latitude  <- round(dat_TS$Latitude ,digits = 3)
    write.csv(dat_TS,paste(outdir_TS2,paste(scedat[sce],"coords.csv",sep="_"),sep="/"), row.names=FALSE) 
    
    # combine for shapefile  
    scen_sf <- st_read(paste(outdir_TS,paste(scedat[sce],"shp",sep="."),sep="/"))
    scen_sf <- cbind(scen_sf,dat_Ad[match(scen_sf$FID,dat_Ad$Poly_No),c("VME_Csquare","VME_Habitat","VME_Indicator","VME_Elements")]) 
    colnames(scen_sf) <- c("Poly_No","VME_Csq","VME_Hab","VME_Ind","VME_Ele","geometry")
    write_sf(scen_sf, paste(outdir_TS2,paste(scedat[sce],"shp", sep="."),sep="/"))
  }  

# step 2 - VME update
# explain, in the service summary document, the rationale behind the removal 
# and the addition of a number of coordinates compared to the January 2021 advice

  for (sce in 1:4){
    dat_TS2  <-  read.csv(paste(paste(outdir_TS2,scedat[sce],sep="/"),"coords.csv",sep="_"),header=T,sep=",")
    dat_Ad   <-  read.csv(paste(paste(outdir_Ad,scedat[sce],sep="/"),"coords.csv",sep="_"),header=T,sep=",")
    excluded <-  unique(dat_Ad$Poly_No[!(dat_Ad$Poly_No %in% dat_TS2$Poly_No)])
    excluded_report <- data.frame(Poly_No_excluded = paste(excluded,collapse=", "))
    write.csv(excluded_report,paste(outdir_TS2,paste(scedat[sce],"closures_excluded.csv",sep="_"),sep="/"), row.names=FALSE)
    
    dat_Ad <- subset(dat_Ad,!(dat_Ad$Poly_No %in% excluded))
    dat_TS2$uni <- paste(dat_TS2$Poly_No,dat_TS2$Longitude,dat_TS2$Latitude,sep="_")
    dat_Ad$uni <- paste(dat_Ad$Poly_No,dat_Ad$Longitude,dat_Ad$Latitude,sep="_")
    
    # get all closures with removed coords
    coords_out <-  dat_Ad$uni[!(dat_Ad$uni %in% dat_TS2$uni)]
    coords_out <- data.frame(str_split_fixed(coords_out,"_",3))
    coords_out$coords <- paste(coords_out[,2],coords_out[,3],sep=" ")
    report <- data.frame(Poly_No = unique(coords_out$X1),Coordinates_excluded = NA)
    for (clos in 1:nrow(report)){
         report[clos,2] <- paste(coords_out$coords[coords_out$X1 == report[clos,1]], collapse="; ")
    }

    # get all closures with new coords
    coords_in  <-  dat_TS2$uni[!(dat_TS2$uni %in% dat_Ad$uni)] 
    coords_in <- data.frame(str_split_fixed(coords_in,"_",3))
    coords_in$coords <- paste(coords_in[,2],coords_in[,3],sep=" ")
    report2 <- data.frame(Poly_No = unique(coords_in$X1),Coordinates_included = NA)
    for (clos in 1:nrow(report2)){
         report2[clos,2] <- paste(coords_in$coords[coords_in$X1 == report2[clos,1]], collapse="; ")
    }
    report[,1] == report2[,1]  
    report$Coordinates_included <- report2$Coordinates_included
    
    write.csv(report,paste(outdir_TS2,paste(scedat[sce],"coordinates_inout.csv",sep="_"),sep="/"), row.names=FALSE)
    
    }  
  
  
  