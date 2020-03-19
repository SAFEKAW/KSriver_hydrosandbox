## paths+packages.R

require(tidyverse)
require(sf)
require(raster)
require(patchwork)

# path to onedrive directory
path_onedrive <- "C:/Users/samzipper/OneDrive - The University of Kansas"
path_plots <- file.path(path_onedrive, "Research", "KansasRiver", "plots")
path_gis <- file.path(path_onedrive, "Research", "KansasRiver", "GIS")
path_gis_general <- file.path(path_onedrive, "Research", "Kansas", "GISdata")

# path and CRS for CDL data
path_cdl <- file.path(path_onedrive, "GIS_GeneralFiles/USDA_NASS_CDL/Kansas")
crs_cdl <- new("CRS", projargs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# color palettes
pal_cdl <- c("Alfalfa/Hay" = "#FFA8E3", 
             "Corn" = "#FFD400",
             "Developed" = "#9C9C9C", 
             "Forest" = "#95CE93", 
             "Grass/Pasture" = "#E9FFBE", 
             "Open Water/Barren" = "#4D70A3", 
             "Open Water" = "#4D70A3",  
             "Water/Wetland" = "#4D70A3", 
             "Water" = "#4D70A3", 
             "Other" = "purple", 
             "Sorghum" = "#FF9E0F", 
             "Soy" = "#1C7300", 
             "Wetlands" = "#80B3B3", 
             "Wetland" = "#80B3B3", 
             "Win Wheat/Soy" = "#737300", 
             "Winter Wheat" = "#A87000", 
             "Wheat" = "#A87000")

col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

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
