## Lawrence_MetData.R

library(rnoaa)
library(tidyverse)

## get data
staid_ku <- "USC00144559"
df_ku <- 
  rnoaa::meteo_tidy_ghcnd(staid_ku, var = c("PRCP", "TMAX", "TMIN")) %>% 
  dplyr::mutate(station_prcp = staid_ku,
                station_tmax = staid_ku,
                station_tmin = staid_ku)

# river - mostly just temperature data
staid_river <- "USC00144562"
df_river <- 
  rnoaa::meteo_tidy_ghcnd(staid_river, var = c("PRCP", "TMAX", "TMIN"))

# airport
staid_airport <- "USW00003997"
df_airport <- 
  rnoaa::meteo_tidy_ghcnd(staid_airport, var = c("PRCP", "TMAX", "TMIN"))

# southwest part of town
staid_sw <- "USC00144567"
df_sw <- 
  rnoaa::meteo_tidy_ghcnd(staid_sw, var = c("PRCP", "TMAX", "TMIN"))

# clinton lake
staid_clinton <- "USC00141612"
df_clinton <-
  rnoaa::meteo_tidy_ghcnd(staid_clinton, var = c("PRCP", "TMAX", "TMIN"))

## combine
df <- df_ku

# fill in missing precip
prcp_missing_dates <- df$date[is.na(df$prcp)]
for (d in prcp_missing_dates){
  if (sum(is.finite(df_river$prcp[df_river$date==d])) > 0){
    df$prcp[df$date==d] <- df_river$prcp[df_river$date==d]
    df$station_prcp[df$date==d] <- staid_river
    
  } else if (sum(is.finite(df_airport$prcp[df_airport$date==d])) > 0){
    df$prcp[df$date==d] <- df_airport$prcp[df_airport$date==d]
    df$station_prcp[df$date==d] <- staid_airport
    
  } else if (sum(is.finite(df_sw$prcp[df_sw$date==d])) > 0){
    df$prcp[df$date==d] <- df_sw$prcp[df_sw$date==d]
    df$station_prcp[df$date==d] <- staid_sw
    
  } else if (sum(is.finite(df_clinton$prcp[df_clinton$date==d])) > 0){
    df$prcp[df$date==d] <- df_clinton$prcp[df_clinton$date==d]
    df$station_prcp[df$date==d] <- staid_clinton
    
  } else {
    df$station_prcp[df$date==d] <- "No Data"
  }
}

tmax_missing_dates <- df$date[is.na(df$tmax)]
for (d in tmax_missing_dates){
  if (sum(is.finite(df_river$tmax[df_river$date==d])) > 0){
    df$tmax[df$date==d] <- df_river$tmax[df_river$date==d]
    df$station_tmax[df$date==d] <- staid_river
    
  } else if (sum(is.finite(df_airport$tmax[df_airport$date==d])) > 0){
    df$tmax[df$date==d] <- df_airport$tmax[df_airport$date==d]
    df$station_tmax[df$date==d] <- staid_airport
    
  } else if (sum(is.finite(df_sw$tmax[df_sw$date==d])) > 0){
    df$tmax[df$date==d] <- df_sw$tmax[df_sw$date==d]
    df$station_tmax[df$date==d] <- staid_sw
    
  } else if (sum(is.finite(df_clinton$tmax[df_clinton$date==d])) > 0){
    df$tmax[df$date==d] <- df_clinton$tmax[df_clinton$date==d]
    df$station_tmax[df$date==d] <- staid_clinton
    
  } else {
    df$station_tmax[df$date==d] <- "No Data"
  }
}

tmin_missing_dates <- df$date[is.na(df$tmin)]
for (d in tmin_missing_dates){
  if (sum(is.finite(df_river$tmin[df_river$date==d])) > 0){
    df$tmin[df$date==d] <- df_river$tmin[df_river$date==d]
    df$station_tmin[df$date==d] <- staid_river
    
  } else if (sum(is.finite(df_airport$tmin[df_airport$date==d])) > 0){
    df$tmin[df$date==d] <- df_airport$tmin[df_airport$date==d]
    df$station_tmin[df$date==d] <- staid_airport
    
  } else if (sum(is.finite(df_sw$tmin[df_sw$date==d])) > 0){
    df$tmin[df$date==d] <- df_sw$tmin[df_sw$date==d]
    df$station_tmin[df$date==d] <- staid_sw
    
  } else if (sum(is.finite(df_clinton$tmin[df_clinton$date==d])) > 0){
    df$tmin[df$date==d] <- df_clinton$tmin[df_clinton$date==d]
    df$station_tmin[df$date==d] <- staid_clinton
    
  } else {
    df$station_tmin[df$date==d] <- "No Data"
  }
}

# inspect data continuity
rnoaa::vis_miss(df)

# set units
df$prcp_mm <- df$prcp/10
df$tmax_c <- df$tmax/10
df$tmin_c <- df$tmin/10

df$prcp <- NULL
df$tmax <- NULL
df$tmin <- NULL

# summarize by year, month
df$year <- lubridate::year(df$date)
df$month <- lubridate::month(df$date)

df_mo <-
  df %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarize(n_prcp = sum(is.finite(prcp_mm)),
                   n_tmax = sum(is.finite(tmax_c)),
                   n_tmin = sum(is.finite(tmin_c)),
                   prcp_mm_sum = sum(prcp_mm, na.rm = T),
                   tmax_c_mean = mean(tmax_c, na.rm = T),
                   tmin_c_mean = mean(tmin_c, na.rm = T)) %>% 
  dplyr::mutate(n_days_mo = lubridate::days_in_month(month)) %>% 
  dplyr::ungroup()

# set months with too many missing days to NA
day_thres <- 3
df_mo$prcp_mm_sum[(df_mo$n_days_mo - df_mo$n_prcp > day_thres)] <- NA
df_mo$tmax_c_mean[(df_mo$n_days_mo - df_mo$n_tmax > day_thres)] <- NA
df_mo$tmin_c_mean[(df_mo$n_days_mo - df_mo$n_tmin > day_thres)] <- NA

## annual totals
df_yr <- 
  df_mo %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm_sum),
                   tmax_c = mean(tmax_c_mean),
                   tmin_c = mean(tmin_c_mean)) %>% 
  dplyr::ungroup()

## annual extremes
df_yr_extreme <-
  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_gt_25mm = sum(prcp_mm > 25),
                   prcp_gt_50mm = sum(prcp_mm > 50),
                   prcp_gt_75mm = sum(prcp_mm > 75))

## plots
# monthly trends
ggplot(df_mo, aes(x = year, y = prcp_mm_sum)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = year, y = tmax_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = year, y = tmin_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

# annual trends
ggplot(df_yr, aes(x = year, y = prcp_mm)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_yr, aes(x = year, y = tmax_c)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_yr, aes(x = year, y = tmin_c)) +
  geom_point() +
  stat_smooth(method = "lm")
