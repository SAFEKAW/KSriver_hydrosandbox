## CDL_01_TrimRasters.R
# This script loads CDL data and creates new TIFs trimmed to the watershed and alluvial aquifer extent.

source(file.path("src", "paths+packages.R"))

# load watershed boundary
sf_watershed <- 
  sf::st_read(file.path(path_gis, "watershed_boundary", "boundary_only.shp")) %>% 
  sf::st_transform(crs_cdl)

sf_aquifer <- 
  sf::st_read(file.path(path_gis, "KSriver_AlluvialAquiferExtent.gpkg")) %>% 
  sf::st_transform(crs_cdl) %>% 
  sf::st_transform(crs_cdl)

# load CDL data
for (yr in seq(2008, 2018, 1)){
  r_cdl <- raster::raster(file.path(path_cdl, paste0("CDL_KS_", yr, ".tif")))
  
  # crop and save
  r_watershed_crop <- raster::crop(r_cdl, sf_watershed)
  r_watershed_mask <- raster::mask(r_watershed_crop, sf_watershed,
                                   filename = file.path(path_gis, "CDL", paste0("CDL_KSriver_Watershed_", yr, ".tif")),
                                   overwrite = T)
  
  r_aquifer_crop <- raster::crop(r_cdl, sf_aquifer)
  r_aquifer_mask <- raster::mask(r_aquifer_crop, sf_aquifer,
                                 filename = file.path(path_gis, "CDL", paste0("CDL_KSriver_Aquifer_", yr, ".tif")),
                                 overwrite = T)
}