
This folder is not connected to the main workflow.
Contact: David.Stirling@gov.scot


Files Included:

'Derive depth contour polygon.R' - Derivation of depth band polygon from Emodnet bathymetry (sourced in all other scripts) 
	- See files in directory: 'Derive depth contour polygon files' for files called / produced by this script
	
'get gebco basemaps for regions.R' - Derivation of basemaps from gebco grid 2020: 
	- Gets the GEBCO raster base maps for plots
	- see associated directory for underlying files called
	
- Task 2 scripts: both ecoregions (including files called in here - particularly for plotting)
	'Task_2_rscript_CS.R': Celtic sea: Summarises data for ecoregion, depth band in ecoregion and then for 9 subareas as defined by polygons (entire polygon and depth band in polygon). Produces plots of vme indicator and habitats and sub-areas overview (Figs 6, 7 & 8)
	'Task_2_rscript_BoBER.R': BOB IC: Summarises data for ecoregion, depth band in ecoregion. Produces plots of vme indicator and habitats and sub-areas overview (figs 6 & 7)

- Task 7&8 scripts: both ecoregions
  'VME Database summary and plot_Celtic_Seas_final.R': Celtic Sea: summarises the VME data base for the Ecoregion, depthband in ecoregion and then for each of the 4 closure scenarios that lie within the 400 - 800 m depth band. Generates the data for the table (Table 10) and figs (bar plots: Figs 14 & 15) as well as the 6 panel example closures plot (Fig 16)
  'VME Database summary and plot_Bay_of_Biscay_final.R': BOB & IC: Does the same as above for this ecoregion. No table or figure numbers yet as not included in report first time round - they will be added.
	

I am also working on plots of scenarios closures for each indicator / habitat to supplement the closure vertices.


Files called for plots etc. directory:
# Ecoregion polygons:
CS_Ices_ecoregion.shp (downloaded from ICES)
BoB_Ices_ecoregion.shp (downloaded from ICES)
# Coastlines file: 
GSSHS_subset.shp (a subset of Global Self-consistent Hierarchical High-resolution Geography, GSHHG shape file)
# Base maps and ecoregion area polygons derived in 'get gebco basemaps for regions.R':
Bay of Biscay area.GEBCO.base.map_utm29n.tif
Celtic_Sea.GEBCO.base.map_utm29n.tif
Celtic Sea area polygon.shp
Bay of Biscay & Iberian Coast.shp
# Celtic Sea 9 sub areas - used to summarise VME data, to provide numbers for the text
Celtic_Seas_9_subareas.shp
# VME database extraction:
