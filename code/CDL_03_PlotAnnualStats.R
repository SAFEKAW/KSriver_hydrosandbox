## CDL_03_PlotAnnualStats.R

source(file.path("src", "paths+packages.R"))

# load annual stats
cdl_sums <- readr::read_csv(file.path("results", "CDL_02_AnnualStats.csv"))

# plot snapshot of land use in a single year
yr <- 2018
r_aquifer <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Aquifer_", yr, ".tif")))
r_watershed <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Watershed_", yr, ".tif")))

png(file.path(path_plots, paste0("CDL_03_PlotAnnualStats_CDL-Aquifer-", yr, ".png")),
    width = 6, height = 4, units = "in", res = 300)
plot(r_aquifer)
dev.off()

png(file.path(path_plots, paste0("CDL_03_PlotAnnualStats_CDL-Watershed-", yr, ".png")),
    width = 6, height = 4, units = "in", res = 300)
plot(r_watershed)
dev.off()

# plot urban area through time
cdl_sums %>% 
  subset(cdl_group == "Developed") %>% 
  ggplot(aes(x = year, y = sum_cells)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~ domain, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2008, 2018, 1)) +
  scale_y_continuous(name = "Urban Cells") +
  ggsave(file.path(path_plots, "CDL_03_PlotAnnualStats_UrbanByYear.png"),
         width = 8, height = 4, units = "in")

# plot ag area through time
cdl_sums %>% 
  subset(cdl_group %in% c("Alfalfa/Hay", "Corn", "Sorghum", "Soy", "Win Wheat/Soy", "Winter Wheat")) %>% 
  dplyr::group_by(domain, year) %>% 
  dplyr::summarize(ag_cells = sum(sum_cells)) %>% 
  ggplot(aes(x = year, y = ag_cells)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~ domain, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2008, 2018, 1)) +
  scale_y_continuous(name = "Ag Cells") +
  ggsave(file.path(path_plots, "CDL_03_PlotAnnualStats_AgByYear.png"),
         width = 8, height = 4, units = "in")

# stacked bar chart through time
cdl_sums %>% 
  ggplot(aes(x = year, y = sum_cells, fill = cdl_group)) +
  geom_col() +
  facet_wrap(~ domain, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2008, 2018, 1)) +
  scale_y_continuous(name = "Number of Cells") +
  scale_fill_manual(values = pal_cdl) +
  ggsave(file.path(path_plots, "CDL_03_PlotAnnualStats_AllClassesByYear.png"),
         width = 8, height = 8, units = "in")
