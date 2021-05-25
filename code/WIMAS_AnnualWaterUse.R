## WIMAS_AnnualWaterUse.R

source(file.path("code", "paths+packages.R"))

# load WIMAS
df_wimas <-
  file.path(path_gis, "ks_watershed_wuse.gdb") %>% 
  sf::st_read(stringsAsFactors = F) %>% 
  sf::st_drop_geometry() %>% 
  subset(!(PDIV_ID %in% c(30488, 76439))) # remove bowersock

df_wimas$source[is.na(df_wimas$source)] <- "Surface Water"
df_wimas$source[df_wimas$source == "G"] <- "Groundwater"

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
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_irr_"] <- "Irrigation"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_mun_"] <- "Municipal"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_stk_"] <- "Industrial/Other"
df_wimas_long$Sector[df_wimas_long$Sector == "Af_used_other_"] <- "Industrial/Other"
df_wimas_long <- df_wimas_long[df_wimas_long$Sector != "Acres_", ]
df_wimas_long <- df_wimas_long[df_wimas_long$Sector != "Af_used_", ]

df_wimas_long$Sector <- factor(df_wimas_long$Sector, levels = c("Irrigation", "Municipal", "Industrial/Other"))

# group by sector
af_to_km3 <- 1233.48185532/(1000*1000)

df_wimas_yr <- 
  df_wimas_long %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(WaterUse_km3_total = sum(WaterUse_af*af_to_km3))

df_wimas_sector <- 
  df_wimas_long %>% 
  dplyr::group_by(Year, Sector) %>% 
  dplyr::summarize(WaterUse_km3 = sum(WaterUse_af*af_to_km3)) %>% 
  dplyr::left_join(df_wimas_yr, by = "Year") %>% 
  dplyr::mutate(WaterUse_prc = WaterUse_km3/WaterUse_km3_total)

df_wimas_sector_source <- 
  df_wimas_long %>% 
  dplyr::group_by(Year, Sector, source) %>% 
  dplyr::summarize(WaterUse_km3 = sum(WaterUse_af*af_to_km3)) %>% 
  dplyr::left_join(df_wimas_yr, by = "Year") %>% 
  dplyr::mutate(WaterUse_prc = WaterUse_km3/WaterUse_km3_total)

df_wimas_source <- 
  df_wimas_long %>% 
  dplyr::group_by(Year, source) %>% 
  dplyr::summarize(WaterUse_km3 = sum(WaterUse_af*af_to_km3)) %>% 
  dplyr::left_join(df_wimas_yr, by = "Year") %>% 
  dplyr::mutate(WaterUse_prc = WaterUse_km3/WaterUse_km3_total)

## make plots: first annual water use by sector

ggplot(df_wimas_sector, aes(x = Year, y = WaterUse_km3, fill = Sector)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray65") +
  scale_y_continuous(name = "Annual Water Use [km\u00B3]") +
  scale_x_continuous(expand = c(0.01,0.05),
                     breaks = seq(1990, 2015, 5)) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = NULL) +
  labs(title = "(a) Annual Water Use by Sector")

ggplot(df_wimas_sector, aes(x = Year, y = WaterUse_prc, fill = Sector)) +
  geom_col() +
  scale_y_continuous(name = "Annual Water Use [% of total]", labels = scales::percent, expand = c(0,0)) +
  scale_x_continuous(expand = c(0.01,0.05),
                     breaks = seq(1990, 2015, 5)) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = NULL) +
  labs(title = "(a) Annual Water Use by Sector")

ggplot(subset(df_wimas_sector_source, Sector == "Irrigation"), 
       aes(x = Year, y = WaterUse_km3, color = source)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  theme(legend.position = "bottom")



## calculate some stats
# slope for each sector
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector, Sector == "Municipal")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector, Sector == "Irrigation")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector, Sector == "Industrial/Other")))

# slope for each source
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_source, source == "Surface Water")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_source, source == "Groundwater")))

# slope for each sector by source
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Municipal" & source == "Surface Water")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Municipal" & source == "Groundwater")))

summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Irrigation" & source == "Surface Water")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Irrigation" & source == "Groundwater")))

summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Industrial/Other" & source == "Surface Water")))
summary(lm(WaterUse_km3 ~ Year, data = subset(df_wimas_sector_source, Sector == "Industrial/Other" & source == "Groundwater")))

# mean, sd, CV by sector and source
df_wimas_sector_source %>% 
  dplyr::group_by(Sector, source) %>% 
  dplyr::summarize(WaterUse_mean = mean(WaterUse_km3),
                   WaterUse_sd = sd(WaterUse_km3)) %>% 
  dplyr::mutate(WaterUse_CV = WaterUse_sd/WaterUse_mean)

sum()

ggplot(df_wimas_sector_source, aes(x = Year, y = WaterUse_km3, fill = Sector)) +
  geom_col() +
  facet_wrap(~source, ncol = 1) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = NULL) +
  labs(title = "Annual Water Use by Sector and Source") +
  ggsave(file.path("results", "WIMAS_AnnualWaterUse_BySector+Source.png"),
         width = 110, height = 110, units = "mm")

# total irrigation
ggplot(subset(df_wimas_sector, Sector == "Irrigation"), 
       aes(x = Year, y = WaterUse_km3, fill = Sector)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(subset(df_wimas_sector_source, Sector == "Irrigation"), 
       aes(x = Year, y = WaterUse_km3, color = source)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  theme(legend.position = "bottom")

## include gridMET in analysis
# load gridmet
df_met_mo <- 
  file.path(path_onedrive, "Research", "KansasRiver", "KansasRiver_gridMET", "Mean_Monthly_Weather.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Year = lubridate::year(Date),
                Month = lubridate::month(Date)) %>% 
  dplyr::select(-X1, Date)

# summarize annual and growing season
df_met_gs <- 
  df_met_mo %>% 
  subset(Month %in% seq(5, 9)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(Precip_gs_mm = sum(Precip_mm),
                   ETo_gs_mm = sum(ETo_grass_mm))

df_met <- 
  file.path(path_onedrive, "Research", "KansasRiver", "KansasRiver_gridMET", "Mean_Annual_Weather.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Year = lubridate::year(Date)) %>% 
  dplyr::select(-X1, Date)

# change in precip from previous year
df_met$PrecipChange <- c(NA, df_met$Precip_mm[2:41] - df_met$Precip_mm[1:40])

ggplot(df_met, aes(x = Year, y = Precip_mm)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_met, aes(x = Year, y = PrecipChange)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_met, aes(x = Year, y = Tmean)) +
  geom_point() +
  stat_smooth(method = "lm")

df_met %>% 
  dplyr::select(Year, Precip_mm, ETo_grass_mm) %>% 
  reshape2::melt(id = "Year") %>% 
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_point() +
  stat_smooth(method = "lm")

lm(Precip_mm ~ Year, data = df_met) %>% summary()
lm(ETo_grass_mm ~ Year, data = df_met) %>% summary()

df_met_gs %>% 
  dplyr::select(Year, Precip_gs_mm, ETo_gs_mm) %>% 
  reshape2::melt(id = "Year") %>% 
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_point() +
  stat_smooth(method = "lm")

## groundwater use vs. precip
df_wimas_sector_source_met <-
  dplyr::left_join(df_wimas_sector_source, df_met, by = "Year")

ggplot(subset(df_wimas_sector_source_met, Sector == "Irrigation"),
       aes(x = Precip, y = WaterUse_km3, color = source)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(subset(df_wimas_sector_source_met, Sector == "Municipal"),
       aes(x = Precip, y = WaterUse_km3, color = source)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(subset(df_wimas_sector_source_met, Sector == "Industrial/Other"),
       aes(x = Precip, y = WaterUse_km3, color = source)) +
  geom_point() +
  stat_smooth(method = "lm")

## final plots to save
p_annualuse <-
  ggplot(df_wimas_sector, aes(x = Year, y = WaterUse_km3, fill = Sector)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray65") +
  scale_y_continuous(name = "Annual Water Use [km\u00B3]") +
  scale_x_continuous(expand = c(0.01,0.05),
                     breaks = seq(1990, 2015, 5)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = NULL, values = c("Irrigation" = col.cat.red,
                                                "Municipal" = col.cat.blu,
                                                "Industrial/Other" = col.cat.org)) +
  labs(title = "(a) Annual Water Use by Sector")

p_irrigation.climate <-
  ggplot(subset(df_wimas_sector_source_met, Sector == "Irrigation"),
       aes(x = Precip_mm, y = WaterUse_km3, shape = source, linetype = source)) +
  stat_smooth(method = "lm", color = "gray45") +
  geom_point(color = col.cat.red) +
  scale_y_continuous(name = "Annual Irrigation [km\u00b3]", expand = c(0,0)) +
  scale_x_continuous(name = "Annual Precipitation [mm]") +
  scale_shape_manual(name = "Source", values = c("Groundwater" = 19, "Surface Water" = 1)) +
  scale_linetype_manual(name = "Source", values = c("Groundwater" = 1, "Surface Water" = 2)) +
  labs(title = "(b) Irrigation Sensitivity to Climate") +
  coord_cartesian(ylim = c(0, 50)) +
  theme(legend.position = "bottom") +
#  theme(legend.position = c(1,1),
#        legend.justification = c(1.01, 1.01)) +
  NULL

((p_annualuse / p_irrigation.climate) + plot_layout(heights = c(1, 1.35))) %>% 
  ggsave(file.path("plots", "WIMAS_AnnualUse+ClimateSensitivity.png"),
         plot = .,
         width = 94, height = 120, units = "mm")

# remove panel (a) - just plot annual use vs. precip
ggplot(subset(df_wimas_sector_source_met, Sector == "Irrigation"),
       aes(x = Precip_mm, y = WaterUse_km3, shape = source, linetype = source)) +
  stat_smooth(method = "lm", color = "gray45") +
  geom_point(color = col.cat.red) +
  scale_y_continuous(name = "Annual Irrigation [km\u00b3]", expand = c(0,0)) +
  scale_x_continuous(name = "Annual Precipitation [mm]") +
  scale_shape_manual(name = NULL, values = c("Groundwater" = 19, "Surface Water" = 1)) +
  scale_linetype_manual(name = NULL, values = c("Groundwater" = 1, "Surface Water" = 2)) +
  labs(title = "Irrigation Sensitivity to Climate") +
  coord_cartesian(ylim = c(0, 50)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "WIMAS_ClimateSensitivity.png"),
         width = 80, height = 70, units = "mm") +
  #  theme(legend.position = c(1,1),
  #        legend.justification = c(1.01, 1.01)) +
  NULL
