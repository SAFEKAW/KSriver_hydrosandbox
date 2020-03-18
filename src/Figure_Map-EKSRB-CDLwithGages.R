## Figure_Map-EKSRB-CDLwithGages.R

source(file.path("src", "paths+packages.R"))

## prep CDL data
# plot snapshot of land use in a single year
yr <- 2018
r_cdl <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Watershed_", yr, ".tif")))
df_cdl <- as.data.frame(raster::rasterToPoints(r_cdl))
names(df_cdl) <- c("lon", "lat", "CDL")

## load watershed boundary
sf_boundary <- 
  sf::st_read(file.path(path_gis, "watershed_boundary", "boundary_only.shp")) %>% 
  sf::st_transform(crs = crs(r_cdl))

## load streamlines
sf_rivers <-
  sf::st_read(file.path(path_gis, "KSriver_MajorStreams.gpkg")) %>% 
  sf::st_transform(crs = crs(r_cdl))

## load index wells
sf_wells <- 
  sf::st_read(file.path(path_gis, "KSriver_IndexWells.gpkg")) %>% 
  sf::st_transform(crs = crs(r_cdl))

###### add WIMAS
###### add nitrate

## prep USGS gaging stations
require(dataRetrieval)
# grab site
USGS_sites <- whatNWISsites(statecode="KS",
                            parameterCd="00060")  # sites with discharge in cms

# get site info
USGS_site_info <- readNWISsite(USGS_sites$site_no)

sf_USGS <- 
  sf::st_as_sf(USGS_sites, coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>% 
  sf::st_transform(crs = crs(r_cdl))

sf_USGS_EKSRB <- sf_USGS[which(sf::st_within(sf_USGS, sf_boundary, sparse = F)), ]

site_data <- c()
for (site in sf_USGS_EKSRB$site_no){
  
  discharge <- readNWISdv(site, parameterCd="00060", 
                          "2019-03-01", "2020-02-29")
  
  if (is.null(site_data)) {
    discharge_all <- discharge
  } else {
    discharge_all <- rbind(discharge_all, discharge)
  }
  
  site_data <- c(site_data, dim(discharge)[1])
}

# subset to only sites with at least 300 days
sf_USGS_EKSRB <- sf_USGS_EKSRB[site_data > 300, ]

## plot
ggplot() +
  geom_sf(data = sf_boundary) +
  geom_sf(data = sf_rivers, color = "blue") +
  geom_sf(data = sf_USGS_EKSRB, color = "red") +
  geom_sf(data = sf_wells, color = "green")
#  geom_raster(data = dplyr::sample_frac(df_cdl, 0.1), aes(x = lon, y = lat, fill = CDL))
