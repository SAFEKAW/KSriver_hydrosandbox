## paths+packages.R

require(tidyverse)
require(sf)
require(raster)

# path to onedrive directory
path_onedrive <- "C:/Users/samzipper/OneDrive - The University of Kansas"
path_plots <- file.path(path_onedrive, "Research", "KansasRiver", "plots")
path_gis <- file.path(path_onedrive, "Research", "KansasRiver", "GIS")

# path and CRS for CDL data
path_cdl <- file.path(path_onedrive, "GIS_GeneralFiles/USDA_NASS_CDL/Kansas")
crs_cdl <- new("CRS", projargs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())
