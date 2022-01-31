# some c-squares were missing information - two reasons:
## 1) boundary issue between BoB-IC and CS
## 2) no data was originally included outside BoB-IC but within EEZ of Portugal/Spain

# Enable universe(s) by ices-tools-prod
options(repos = c(
  icestoolsprod = 'https://ices-tools-prod.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('icesVMS')

# get the missing csquares
pathdir_nogit <- "C:/Users/danie/Documents/Online for git/WKEUVME_noGIT" # stores the data from sharepoint
dat_up <- read.csv(file = paste(pathdir_nogit,"VMS data repository/csquares_update.csv",sep="/"))

# connect to ices database and get mobile data 
icesConnect::set_username("vandenderen")   # enter password 
tt <- get_wgfbit_data3(datacall = 2019,year = 2009)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2009"

tt <- get_wgfbit_data3(datacall = 2018,year = 2009)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2009_c"

tt <- get_wgfbit_data3(datacall = 2019,year = 2010)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2010"

tt <- get_wgfbit_data3(datacall = 2018,year = 2010)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2010_c"

tt <- get_wgfbit_data3(datacall = 2019,year = 2011)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2011"

tt <- get_wgfbit_data3(datacall = 2018,year = 2011)
tt2 <- aggregate(tt[,"surface_sar"], by=list(tt$c_square),FUN = sum,na.rm=TRUE)
dat_up     <- cbind(dat_up,tt2[match(dat_up$csquares,tt2$Group.1),c(2)])
colnames(dat_up)[ncol(dat_up)] <- "Mobile_2011_c"

save(dat_up,file=paste(pathdir_nogit,"VMS data repository/csquares_update.RData",sep="/"))

# connect to ices database and get static data
st   <- get_passive_footprint(year = 2009,ecoregion = "NULL",datacall = 2019)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2009"

st   <- get_passive_footprint(year = 2009,ecoregion = "NULL",datacall = 2018)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2009_c"

st   <- get_passive_footprint(year = 2010,ecoregion = "NULL",datacall = 2019)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2010"

st   <- get_passive_footprint(year = 2010,ecoregion = "NULL",datacall = 2018)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2010_c"

st   <- get_passive_footprint(year = 2011,ecoregion = "NULL",datacall = 2019)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2011"

st   <- get_passive_footprint(year = 2011,ecoregion = "NULL",datacall = 2018)
st$fish <- 1
dat_up     <- cbind(dat_up,st[match(dat_up$csquares,st$c_square),c("fish")])
colnames(dat_up)[ncol(dat_up)] <- "Static_2011_c"

save(dat_up,file=paste(pathdir_nogit,"VMS data repository/csquares_update.RData",sep="/"))

mobile <- c("Mobile_2009" ,  "Mobile_2010" ,  "Mobile_2011"  ,
            "Mobile_2009_c", "Mobile_2010_c", "Mobile_2011_c" )
static <- c("Static_2009",   "Static_2009_c", "Static_2010" ,
            "Static_2010_c", "Static_2011" ,  "Static_2011_c")
dat_up$mobile <- rowSums(dat_up[,mobile],na.rm=T)
dat_up$mobile <- ifelse(dat_up$mobile >0,1,0)
dat_up$static <- rowSums(dat_up[,static],na.rm=T)
dat_up$static <- ifelse(dat_up$static >0,1,0)

dat_up <- dat_up[,c(1:3,16,17)]

save(dat_up,file=paste(pathdir_nogit,"VMS data repository/csquares_update.RData",sep="/"))






















