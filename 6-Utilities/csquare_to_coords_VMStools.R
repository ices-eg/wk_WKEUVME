
# taken from VMStools

CSquare2LonLat <- function(csqr,degrees){
  
  ra          <- 1e-6 #Artificial number to add for rounding of 5'ves (round(0.5) = 0, but in Excel (where conversion comes from, it is 1)
  chars       <- as.numeric(nchar(csqr))
  tensqd      <- as.numeric(substr(csqr,1,1))+ra;      gqlat     <- (round(abs(tensqd-4)*2/10)*10/5)-1
  tenslatd    <- as.numeric(substr(csqr,2,2))+ra;      gqlon     <- (2*round(tensqd/10)-1)*-1
  tenslond    <- as.numeric(substr(csqr,3,4))+ra
  unitsqd     <- as.numeric(substr(csqr,6,6))+ra;      iqulat    <- round(unitsqd*2/10)
  unitslatd   <- as.numeric(substr(csqr,7,7))+ra;      iqulon    <- (round((unitsqd-1)/2,1) - floor((unitsqd-1)/2))*2
  unitslond   <- as.numeric(substr(csqr,8,8))+ra
  tenthsqd    <- as.numeric(substr(csqr,10,10))+ra;    iqtlat    <- round(tenthsqd*2/10)
  tenthslatd  <- as.numeric(substr(csqr,11,11))+ra;    iqtlon    <- (round((tenthsqd-1)/2,1) - floor((tenthsqd-1)/2))*2
  tenthslond  <- as.numeric(substr(csqr,12,12))+ra
  hundqd      <- as.numeric(substr(csqr,14,14))+ra;    iqhlat    <- round(hundqd*2/10)
  hundlatd    <- as.numeric(substr(csqr,15,15))+ra;    iqhlon    <- (round((hundqd-1)/2,1) - floor((hundqd-1)/2))*2
  hundlond    <- as.numeric(substr(csqr,16,16))+ra
  reso        <- 10^(1-floor((chars-4)/4))-((round((chars-4)/4,1)-floor((chars-4)/4))*10^(1-floor((chars-4)/4)))
  
  if(degrees < reso[1]) stop("Returning degrees is smaller than format of C-square")
  if(degrees == 10){   
    lat <- ((tenslatd*10)+5)*gqlat-ra                              
    lon <- ((tenslond*10)+5)*gqlon-ra              
  }
  if(degrees == 5){    
    lat <- ((tenslatd*10)+(iqulat*5)+2.5)*gqlat-ra
    lon <- ((tenslond*10)+(iqulon*5)+2.5)*gqlon-ra 
  }
  if(degrees == 1){    
    lat <- ((tenslatd*10)+ unitslatd+0.5)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond+0.5)*gqlon-ra 
  }
  if(degrees == 0.5){  
    lat <- ((tenslatd*10)+ unitslatd + (iqtlat*0.5)+0.25)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (iqtlon*0.5)+0.25)*gqlon-ra
  }         
  if(degrees == 0.1){  
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+0.05)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+0.05)*gqlon-ra
  }
  if(degrees == 0.05){ 
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+(iqhlat*0.05)+0.025)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+(iqhlon*0.05)+0.025)*gqlon-ra
  }
  if(degrees == 0.01){ 
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+(hundlatd*0.01)+0.005)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+(hundlond*0.01)+0.005)*gqlon-ra
  }
  return(data.frame(SI_LATI=lat,SI_LONG=lon))}