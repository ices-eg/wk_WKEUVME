
##########################################################
###### script to run ICES WKEUVME workshop outputs #######
##########################################################

#### data inputs
# 1- c-squares and depths per ecoregion (Greater North Sea, Celtic Seas, Bay of Biscay and Iberian Coast)
# 2- ICES_ecoregions shapefiles
# 3- EEZ shapefiles
# 4- VME weighting -- ICES WGDEC (located on WKEUVME sharepoint, restricted access)
# 5- VME observations -- ICES WGDEC (located on WKEUVME sharepoint, restricted access)
# 6- VME elements -- ICES WGDEC + WKEUVME (located on WKEUVME sharepoint, restricted access)
# 7- VMS data -- ICES WGSFD (located on WKEUVME sharepoint, restricted access)

## set path to folder and specify username 
  pathdir <- "C:/Users/pdvd/Online for git/WKEUVME"
  pathdir_nogit <- "C:/Users/pdvd/Online for git/WKEUVME_noGIT" # stores the data from sharepoint
  ices_username <- "vandenderen"

## install libraries
  source(paste(pathdir,"6-Utilities/Libraries_WKEUVME.R",sep="/"))

## download data sharepoint 
  options(icesSharePoint.username = ices_username)   # set ices username
  options(icesSharePoint.site = "/ExpertGroups/WKEUVME")  # set the site 
  spdir() # put password
  
  # run the script
  source(paste(pathdir,"6-Utilities/get_data_WKEUVME_sharepoint.R",sep="/"))
  # and unzip (manually)  
  
  # double check that password is not stored on your computer, "Element not found" is okay 
  keyring::key_delete("icesSharePoint", ices_username)
  
## run the closure scenario options, takes 20 minutes... (output is stored, hence no need to run)
  source(paste(pathdir,"6-Utilities/Scenario_1_option_1.R",sep="/"))
  source(paste(pathdir,"6-Utilities/Scenario_1_option_2.R",sep="/"))
  source(paste(pathdir,"6-Utilities/Scenario_2_option_1.R",sep="/"))
  source(paste(pathdir,"6-Utilities/Scenario_2_option_2.R",sep="/"))

## get ADG data product 
  
  # closure coordinates csv and maps with closure numbers whole region
  source(paste(pathdir,"6-Utilities/Get csvtable with closure coordinates.R",sep="/"))
  source(paste(pathdir,"6-Utilities/Get maps with closure numbers.R",sep="/"))
  
  # footprint coordinates csv and maps with footprint numbers whole region
  source(paste(pathdir,"6-Utilities/Get csvtable with Footprint coordinates.R",sep="/"))
  source(paste(pathdir,"6-Utilities/Get maps with footprint numbers.R",sep="/"))
  
  # interactive maps
  source(paste(pathdir,"6-Utilities/Interactive maps script.R",sep="/"))
  
## get output per ecoregion (technical report WKEUVME)
  EcoReg <-  "Bay of Biscay and the Iberian Coast"  # "Celtic Seas" or "Bay of Biscay and the Iberian Coast"
  
  # run to get output for plots and tables up to closure consequences
  source(paste(pathdir,"3-Data analysis/Code_to_get_data_figures_tables_per_ecoregion.R",sep="/"))

  # run to make plots and tables with VMS and VME up to closure consequences
  source(paste(pathdir,"4-Code for figures and tables/Producing figures and tables.R",sep="/"))

  # run to assess consequences of closure options (warnings are okay!)
  source(paste(pathdir,"4-Code for figures and tables/Assessment of closures.R",sep="/"))

  # run to assess consequences of closure options (table 4 - barplots and scatterplots)
  source(paste(pathdir,"4-Code for figures and tables/Closure_consequences_barplot_scatterplot.R",sep="/"))

## get assessment of closures per ecoregion (ADG report)
  source(paste(pathdir,"4-Code for figures and tables/Assessment of closures ADG table.R",sep="/"))
  
  