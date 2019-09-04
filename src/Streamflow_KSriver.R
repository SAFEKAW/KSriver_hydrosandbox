## Streamflow_KSriver.R

library(tidyverse)
source(file.path("src", "paths+packages.R"))

# list of gaging stations, upstream to downstream
siteNums <- c("06879150", "06879820", "06887500", "06888350",
              "06888990", "06889000", "06891000", "06891080", 
              "06892350", "06892518", "06892950")
siteInfo <- dataRetrieval::readNWISsite(siteNums)
siteData <- dataRetrieval::whatNWISdata(siteNumber = siteNums, service = "dv", 
                                        parameterCd = "00060", statCd = "00003")

# download data for each site
for (s in 1:dim(siteData)[1]){
  
  df_site <- dataRetrieval::readNWISdv(siteData$site_no[s], "00060",
                                       "1919-01-01","2018-12-31") %>% 
    dplyr::select(site_no, Date, X_00060_00003) %>% 
    magrittr::set_colnames(c("site_no", "Date", "discharge_cfs"))
  
  if (s == 1){
    df_all <- df_site
  } else {
    df_all <- dplyr::bind_rows(df_all, df_site)
  }
  
  print(paste0(s, " complete"))
}

# add site name
df_all <- dplyr::left_join(df_all, siteInfo, by = "site_no")

# add log discharge columns
df_all$discharge_cfs_log <- log10(df_all$discharge_cfs)
df_all$station <- factor(df_all$site_no, levels = siteInfo$site_no, labels = siteInfo$station_nm)

df_all$year <- lubridate::year(df_all$Date)
df_all$month <- lubridate::month(df_all$Date)
df_all$DOY <- lubridate::yday(df_all$Date)
df_all_mo <- 
  df_all %>% 
  dplyr::group_by(station, year, month) %>% 
  dplyr::summarize(discharge_cfs_mean = mean(discharge_cfs),
                   date_mean = mean(Date))

df_all_DOY <- 
  df_all %>% 
  dplyr::group_by(site_no, station, DOY) %>% 
  dplyr::summarize(discharge_cfs_mean = mean(discharge_cfs))

# all gaging stations, mean monthly flow
ggplot(df_all_mo, aes(x = date_mean, y = discharge_cfs_mean)) +
  geom_line() +
  facet_wrap(~ station, ncol = 2) + 
  labs(title = "Kansas River gages, upstream to downstream") +
  scale_y_log10(name = "Mean Monthly Discharge [cfs]") +
  ggsave(file.path(path_plots, "Streamflow_KSriver_MeanMonthly.png"),
         width = 8, height = 8, units = "in")

# long-term gaging stations
stations_longterm <- c("06887500", "06889000", "06892350")
df_all %>% 
  subset(site_no %in% stations_longterm) %>% 
  ggplot() +
  geom_line(aes(x = DOY, y = discharge_cfs, group = year), alpha = 0.25) +
  geom_line(data = subset(df_all_DOY, site_no %in% stations_longterm), 
            aes(x = DOY, y = discharge_cfs_mean), color = "blue", size = 2) +
  facet_wrap(~ station, ncol = 1) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_log10(name = "Discharge [cfs]") +
  ggsave(file.path(path_plots, "Streamflow_KSriver_DailyFlow.png"),
         width = 8, height = 8, units = "in")
