## WIMAS_AnnualWaterUse.R

source(file.path("src", "paths+packages.R"))

# load WIMAS
df_wimas <-
  file.path(path_gis, "ks_watershed_wuse.gdb") %>% 
  sf::st_read(stringsAsFactors = F) %>% 
  sf::st_drop_geometry()

df_wimas$source[is.na(df_wimas$source)] <- "Blank"

# figure out an "other" category
for (yr in seq(1990, 2017)){
  df_wimas[ , paste0("Af_used_other_", yr)] <- 
    df_wimas[ , paste0("Af_used_", yr)] - 
    df_wimas[ , paste0("Af_used_irr_", yr)] - 
    df_wimas[ , paste0("Af_used_mun_", yr)] - 
    df_wimas[ , paste0("Af_used_stk_", yr)]
}

# melt
df_wimas_long <-
  df_wimas %>% 
  reshape2::melt(id = c("long_nad83", "lat_nad83", "county", "source", "PDIV_ID")) %>% 
  dplyr::rename(WaterUse_af = value)

# extract year and use
df_wimas_long$Year <- as.numeric(stringr::str_sub(df_wimas_long$variable, start = -4))
df_wimas_long$Sector <- stringr::str_sub(df_wimas_long$variable, end = -5)
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_"] <- "Total"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_irr_"] <- "Irrigation"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_mun_"] <- "Municipal"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_stk_"] <- "Stock"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_other_"] <- "Other"
df_wimas_long <- df_wimas_long[df_wimas_long$Sector != "Acres_", ]

# group by width
df_wimas_sector <- 
  df_wimas_long %>% 
  dplyr::group_by(Year, Sector) %>% 
  dplyr::summarize(WaterUse_af_total = sum(WaterUse_af)) %>% 
  subset(Sector != "Total")

ggplot(df_wimas_sector, aes(x = Year, y = WaterUse_af_total, fill = Sector)) +
  geom_col() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = NULL) +
  labs(title = "Annual Water Use by Sector") +
  ggsave(file.path("results", "WIMAS_AnnualWaterUse_BySector.png"),
         width = 120, height = 95, units = "mm")

# from https://kansasriver.org/protect/sand-and-gravel-dredging/industrial-pollution/electrical-energy-production/
#  - Jeffery: 14,000 af
#  - Tecumseh: 3000 af
#  - Lawrence: 4855 af
#    - combined: ~22,000 af
# PDIV 30488

df_wimas_sector_source <- 
  df_wimas_long %>% 
  dplyr::group_by(Year, Sector, source) %>% 
  dplyr::summarize(WaterUse_af_total = sum(WaterUse_af)) %>% 
  subset(Sector != "Total")

ggplot(df_wimas_sector_source, aes(x = Year, y = WaterUse_af_total, fill = Sector)) +
  geom_col() +
  facet_wrap(~source, ncol = 1, scales = "free_y",
             labeller = as_labeller(c("G" = "Groundwater", "Blank" = "Blank (presumed surface)"))) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = NULL) +
  labs(title = "Annual Water Use by Sector and Source",
       subtitle = "Note different y-axes") +
  ggsave(file.path("results", "WIMAS_AnnualWaterUse_BySector+Source.png"),
         width = 120, height = 120, units = "mm")
