## NOAA-GHCND_GetLawrenceMetData.R

library(rnoaa)
library(tidyverse)

## define limits of search
date_start <- NULL   # NULL to go back as early as possible
date_end <- "2019-12-31"

## stations that are currently active: ku, airport, clinton lake, lecompton
# currently have all stations within 10 miles of Lawrence city limits

## get data
staid_ku <- "USC00144559"
df_ku <- 
  rnoaa::meteo_tidy_ghcnd(staid_ku, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# river - mostly just temperature data
staid_river <- "USC00144562"
df_river <- 
  rnoaa::meteo_tidy_ghcnd(staid_river, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# airport
staid_airport <- "USW00003997"
df_airport <- 
  rnoaa::meteo_tidy_ghcnd(staid_airport, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# southwest part of town
staid_klwn <- "USC00144567"
df_klwn <- 
  rnoaa::meteo_tidy_ghcnd(staid_klwn, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# clinton lake
staid_clinton <- "USC00141612"
df_clinton <-
  rnoaa::meteo_tidy_ghcnd(staid_clinton, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# lecompton
staid_lecompton <- "USC00144613"
df_lecompton <-
  rnoaa::meteo_tidy_ghcnd(staid_lecompton, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# tonganoxie
staid_tong <- "USC00148156"
df_tong <-
  rnoaa::meteo_tidy_ghcnd(staid_tong, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# vinland
staid_vinland <- "USC00148427"
df_vinland <-
  rnoaa::meteo_tidy_ghcnd(staid_vinland, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# worden
staid_worden <- "USC00149040"
df_worden <-
  rnoaa::meteo_tidy_ghcnd(staid_worden, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

# perry
staid_perry <- "USC00146333"
df_perry <-
  rnoaa::meteo_tidy_ghcnd(staid_perry, var = c("PRCP", "TMAX", "TMIN"),
                          date_min = date_start, date_max = date_end)

## combine as long data frame
df <- 
  dplyr::bind_rows(df_ku, df_airport, df_clinton, df_lecompton, df_river, 
                   df_klwn, df_tong, df_vinland, df_worden, df_perry) %>% 
  dplyr::rename(Station = id) %>% 
  # set units
  dplyr::mutate(prcp_mm = prcp/10, 
                tmax_c = tmax/10,
                tmin_c = tmin/10) %>% 
  dplyr::select(Station, date, prcp_mm, tmax_c, tmin_c)

## set station priority (1 = top priority)
station_priority <-
  tibble::tibble(StationName = c("KU", "Airport", "River", "KLWN", "Clinton", 
                                 "Lecompton", "Tonganoxie", "Vinland", "Worden", "Perry"),
                 Station = c(staid_ku, staid_airport, staid_river, staid_klwn, staid_clinton, 
                             staid_lecompton, staid_tong, staid_vinland, staid_worden, staid_perry),
                 Priority = seq(1, length(StationName)))

## add priority to data frame and melt
df_long <- 
  dplyr::left_join(df, station_priority, by = "Station") %>% 
  tidyr::pivot_longer(cols = c("prcp_mm", "tmax_c", "tmin_c"),
                      names_to = "variable")

## retain the highest priority measurement for each date and variable
df_trimmed <-
  df_long %>% 
  subset(complete.cases(.)) %>% 
  dplyr::group_by(date, variable) %>% 
  dplyr::filter(Priority == min(Priority)) %>% 
  dplyr::ungroup()

## separate for each variable to get a single station and value for each
df_prcp <- 
  subset(df_trimmed, variable == "prcp_mm") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(prcp_mm = value, station_prcp = Station) %>% 
  # add in gaps for missing dates
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmax <- 
  subset(df_trimmed, variable == "tmax_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmax_c = value, station_tmax = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmin <- 
  subset(df_trimmed, variable == "tmin_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmin_c = value, station_tmin = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

## join them all back together again, by date
df_all <-
  dplyr::left_join(df_prcp, df_tmax, by = "date") %>% 
  dplyr::left_join(df_tmin, by = "date") %>% 
  dplyr::arrange(date)

## inspect data for completeness
rnoaa::vis_miss(df_all)
df_all$date[is.na(df_all$tmax_c) & lubridate::year(df_all$date) > 1950]

## save output
df_all %>% 
  readr::write_csv(file.path("results", "NOAA-GHCND_LawrenceMetData.csv"))