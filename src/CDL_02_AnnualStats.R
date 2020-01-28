## CDL_02_AnnualStats.R
# This script calculates annual land use distribution within the KS River Watershed and aquifer extent.

source(file.path("src", "paths+packages.R"))

# CDL numeric categories - these are the same Jude used in the SD-6 categorization
cdl_categories <- 
  tibble::tibble(cdl_class = c(1, 4, 5, 24, 26, 
                               36, 37, 111, 
                               121, 122, 123, 124, 
                               131, 141, 176, 190),
                 cdl_group = c("Corn", "Sorghum", "Soy", "Winter Wheat", "Win Wheat/Soy",
                               "Alfalfa/Hay", "Alfalfa/Hay", "Open Water/Barren",
                               "Developed", "Developed", "Developed", "Developed",
                               "Open Water/Barren", "Forest", "Grass/Pasture", "Wetlands"))

start_flag <- T
for (yr in seq(2008, 2018, 1)){
  # load rasters (already cropped/masked)
  r_aquifer <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Aquifer_", yr, ".tif")))
  r_watershed <- raster::raster(file.path(path_gis, "CDL", paste0("CDL_KSriver_Watershed_", yr, ".tif")))
  
  # collect data
  aquifer_table <- table(r_aquifer[])
  aquifer_tibble <- 
    tibble::tibble(cdl_class = as.numeric(names(aquifer_table)),
                   n_cells = aquifer_table,
                   year = yr,
                   domain = "Aquifer") %>% 
    dplyr::left_join(cdl_categories, by = "cdl_class") %>% 
    tidyr::replace_na(list(cdl_group = "Other"))
  
  
  watershed_table <- table(r_watershed[])
  watershed_tibble <- 
    tibble::tibble(cdl_class = as.numeric(names(watershed_table)),
                   n_cells = watershed_table,
                   year = yr,
                   domain = "Watershed") %>% 
    dplyr::left_join(cdl_categories, by = "cdl_class") %>% 
    tidyr::replace_na(list(cdl_group = "Other"))
  
  # sum by CDL group
  cdl_sums_yr <-
    dplyr::bind_rows(watershed_tibble, aquifer_tibble) %>% 
    dplyr::group_by(domain, year, cdl_group) %>% 
    dplyr::summarize(sum_cells = sum(n_cells))
  
  # combine all years
  if (start_flag){
    cdl_sums <- cdl_sums_yr
    start_flag <- F
  } else {
    cdl_sums <- dplyr::bind_rows(cdl_sums, cdl_sums_yr)
  }
  
  print(paste0(yr, " complete"))
}

## save data
cdl_sums %>% 
  readr::write_csv(file.path("results", "CDL_02_AnnualStats.csv"))