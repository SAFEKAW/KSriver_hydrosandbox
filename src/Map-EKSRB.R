## Map-EKSRB.R

source(file.path("src", "paths+packages.R"))

## prep CDL data
# plot snapshot of land use in a single year
yr <- 2018
r_cdl <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Watershed_", yr, ".tif")))
df_cdl <- as.data.frame(raster::rasterToPoints(r_cdl))
names(df_cdl) <- c("lon", "lat", "cdl_class")

cdl_categories <- 
  tibble::tibble(cdl_class = c(1, 4, 5, 24, 26, 
                               36, 37, 111, 
                               121, 122, 123, 124, 
                               131, 141, 176, 190),
                 cdl_group = c("Corn", "Sorghum", "Soy", "Wheat", "Wheat",
                               "Alfalfa/Hay", "Alfalfa/Hay", "Water",
                               "Developed", "Developed", "Developed", "Developed",
                               "Water", "Forest", "Grass/Pasture", "Water"))

df_cdl_grouped <- 
  dplyr::left_join(df_cdl, cdl_categories, by = "cdl_class")# %>% 
 # tidyr::replace_na(list(cdl_group = "Other"))

## watershed boundary
sf_boundary <- 
  sf::st_read(file.path(path_gis, "watershed_boundary", "boundary_only.shp")) %>% 
  sf::st_transform(crs = crs(r_cdl))

## load streamlines
sf_rivers <-
  sf::st_read(file.path(path_gis_general, "hydro", "streams", "major_streams", "major_streams.gpkg")) %>% 
  sf::st_transform(crs = crs(r_cdl))

sf_rivers_EKSRB <- sf_rivers[which(sf::st_intersects(sf_rivers, sf_boundary, sparse = F)), ]

## load lakes
sf_lakes <-
  sf::st_read(file.path(path_gis_general, "hydro", "ks_lakes", "ks_lakes.gpkg")) %>% 
  sf::st_transform(crs = crs(r_cdl))

sf_lakes_EKSRB <- sf_lakes[which(sf::st_intersects(sf_lakes, sf_boundary, sparse = F)), ]

## load index wells
sf_wells <- 
  sf::st_read(file.path(path_gis, "KSriver_IndexWells.gpkg")) %>% 
  sf::st_transform(crs = crs(r_cdl))

# WIMAS
sf_wimas <-
  file.path(path_gis, "ks_watershed_wuse.gdb") %>% 
  sf::st_read(stringsAsFactors = F)  %>% 
  sf::st_transform(crs = crs(r_cdl))%>% 
  subset(!(PDIV_ID %in% c(30488, 76439))) # remove bowersock

# sensor locations
sf_sensors <- 
  file.path("data", "USGS_wq monitoring stations.csv") %>% 
  readr::read_csv() %>% 
  sf::st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = "+init=epsg:4326")

# nitrate WQ monitoring locations
sf_nitrate <-
  file.path("data", "USGS+KDHE_calibration stations.csv") %>% 
  readr::read_csv() %>% 
  sf::st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = "+init=epsg:4326")

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
# state map
kansas <- ggplot2::map_data("state", region = "kansas")
sf_kansas <- sf::st_as_sf(kansas, coords = c("long", "lat"), crs = "+init=epsg:4326") %>% 
  dplyr::summarize(geometry = st_combine(geometry)) %>% 
  sf::st_cast("POLYGON")

p_state <- 
  ggplot() +
  geom_sf(data = subset(sf_rivers, STRAHLER >= 4), color = "gray65") +
  geom_sf(data = sf_kansas, color = "black", fill = NA) +
  geom_sf(data = sf_boundary) +
  geom_sf(data = subset(sf_rivers_EKSRB, STRAHLER >= 4), color = col.cat.blu) +
  theme(panel.border = element_blank()) +
  coord_sf(expand = T) +
  labs(title = "(a) Location of EKSRB within Kansas") +
  ggsave(file.path("plots", "Figure_EKSRB-Map_LocationWithinState.png"),
         width = 95, height = 80, units = "mm") +
  NULL

# land cover map
p_cdl <-
  ggplot() +
  geom_raster(data = subset(df_cdl_grouped, !is.na(cdl_group)), aes(x = lon, y = lat, fill = cdl_group)) +
  scale_fill_manual(name = NULL, values = pal_cdl) +
  #geom_sf(data = sf_wimas[1:10, ]) +
  geom_sf(data = sf_boundary, color = NA, fill = NA) +
  scale_x_continuous(breaks = seq(-97, -95, 1)) +
  scale_y_continuous(breaks = seq(38.6, 39.8, 0.4)) +
  coord_sf(expand = T) +
  labs(title = "(b) Land cover [2018 CDL]") +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 3)) +
  NULL

#table(df_cdl_grouped$cdl_group)

(p_state + p_cdl + plot_layout(nrow = 2, heights = c(1, 1.5))) %>% 
  ggsave(file.path("plots", "Map-EKSRB_State+CDL.png"),
         plot = .,
         width = 100, height = 140, units = "mm")

## focus area map
x_extent <- sf::st_bbox(sf_boundary)[c("xmin", "xmax")]
y_extent <- sf::st_bbox(sf_boundary)[c("ymin", "ymax")]
tribs_plot <- c("REPUBLICAN R", "SMOKEY HILL R", "BIG BLUE R", "LITTLE BLUE R", "COON CR")

p_monitoring <- 
  ggplot() +
  geom_sf(data = sf_boundary, color = "black", fill = NA) +
  geom_sf(data = subset(sf_rivers, RR1NAME %in% tribs_plot), color = "gray65") +
  geom_sf(data = sf_lakes_EKSRB, fill = col.cat.blu, color = NA) +
  geom_sf(data = subset(sf_rivers_EKSRB, STRAHLER >= 3), color = col.cat.blu) +
  geom_sf(data = sf_USGS_EKSRB, color = col.cat.red, shape = 1, size = 2) +
  geom_sf(data = sf_sensors, color = col.cat.red, size = 2) +
  geom_sf(data = sf_wells, color = "black", size = 2, shape = 18) +
#  geom_sf(data = sf_nitrate, color = "black", shape = 3) +
  coord_sf(xlim = x_extent, ylim = y_extent, expand = T,
           label_axes = list(bottom = "E", left = "N")) +
  scale_x_continuous(breaks = seq(-97, -95, 1)) +
  scale_y_continuous(breaks = seq(38.6, 39.8, 0.4)) +
  labs(title = "EKSRB monitoring network") +
  ggsave(file.path("plots", "Map-EKSRB_Monitoring.png"), width = 95, height = 75, units = "mm") +
  ggsave(file.path("plots", "Map-EKSRB_Monitoring.pdf"), width = 95, height = 75, units = "mm", device = cairo_pdf) +
#  geom_raster(data = dplyr::sample_frac(df_cdl, 0.1), aes(x = lon, y = lat, fill = CDL))
  NULL


# (p_cdl + p_monitoring) %>% 
#   ggsave(file.path("plots", "Figure_EKSRB-Map_Monitoring+CDL.png"),
#          plot = .,
#          width = 190, height = 120, units = "mm")
