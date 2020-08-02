## NOAA-GHCND_PlotLawrenceMetData.R

library(tidyverse)

# sen function used to put sen's slope onto ggplots
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

## load output from NOAA-GHCND_GetLawrenceMetData.R
df_all <-
  readr::read_csv(file.path("results", "NOAA-GHCND_LawrenceMetData.csv"),
                  col_types = cols(
                    date = col_date(),
                    station_prcp = col_character(),
                    prcp_mm = col_double(),
                    station_tmax = col_character(),
                    tmax_c = col_double(),
                    station_tmin = col_character(),
                    tmin_c = col_double()
                  ))

# summarize by year, month
df_all$year <- lubridate::year(df_all$date)
df_all$month <- lubridate::month(df_all$date)
df_all$year_mo <- paste0(df_all$year, "_", df_all$month)

## first: figure out missing data by year to see where to start analysis
df_NAsByYear <-
  df_all %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(missing_prcp = sum(is.na(prcp_mm)),
                   missing_tmax = sum(is.na(tmax_c)),
                   missing_tmin = sum(is.na(tmin_c)))

# years to include: no more than 10% missing data (<36.5 days)
yr_day_thres <- 36.5
yrs_prcp <- df_NAsByYear$year[df_NAsByYear$missing_prcp < yr_day_thres]
yrs_tmax <- df_NAsByYear$year[df_NAsByYear$missing_tmax < yr_day_thres]
yrs_tmin <- df_NAsByYear$year[df_NAsByYear$missing_tmin < yr_day_thres]

## now: screen out months missing more than 10% of data (> 3 days)
df_NAsByMo <-
  df %>% 
  dplyr::group_by(year_mo) %>% 
  dplyr::summarize(missing_prcp = sum(is.na(prcp_mm)),
                   missing_tmax = sum(is.na(tmax_c)),
                   missing_tmin = sum(is.na(tmin_c))) %>% 
  dplyr::ungroup()

# year-months to include: no more than 10% missing data (<3 days)
yrmo_day_thres <- 3
yrmo_prcp <- df_NAsByMo$year_mo[df_NAsByMo$missing_prcp < yrmo_day_thres]
yrmo_tmax <- df_NAsByMo$year_mo[df_NAsByMo$missing_tmax < yrmo_day_thres]
yrmo_tmin <- df_NAsByMo$year_mo[df_NAsByMo$missing_tmin < yrmo_day_thres]

# set everything you don't want to keep to NA
df <-
  df_all
df$prcp_mm[!(df$year %in% yrs_prcp) & !(df$year_mo %in% yrmo_prcp)] <- NA
df$tmax_c[!(df$year %in% yrs_tmax) & !(df$year_mo %in% yrmo_tmax)] <- NA
df$tmin_c[!(df$year %in% yrs_tmin) & !(df$year_mo %in% yrmo_tmin)] <- NA
df$station_prcp[!(df$year %in% yrs_prcp) & !(df$year_mo %in% yrmo_prcp)] <- NA
df$station_tmax[!(df$year %in% yrs_tmax) & !(df$year_mo %in% yrmo_tmax)] <- NA
df$station_tmin[!(df$year %in% yrs_tmin) & !(df$year_mo %in% yrmo_tmin)] <- NA

### analysis
## annual totals
df_yr <- 
  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm),
                   tmax_c = mean(tmax_c),
                   tmin_c = mean(tmin_c)) %>% 
  dplyr::ungroup()

# annual trends
ggplot(df_yr, aes(x = year, y = prcp_mm)) +
  geom_point() +
  stat_smooth(method = "sen")

ggplot(df_yr, aes(x = year, y = tmax_c)) +
  geom_point() +
  stat_smooth(method = "sen")

ggplot(df_yr, aes(x = year, y = tmin_c)) +
  geom_point() +
  stat_smooth(method = "sen")

## monthly totals
df_mo <- 
  df %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm),
                   tmax_c = mean(tmax_c),
                   tmin_c = mean(tmin_c)) %>% 
  dplyr::ungroup()

# monthly trends
ggplot(df_mo, aes(x = year, y = prcp_mm)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "sen")

ggplot(df_mo, aes(x = year, y = tmax_c)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "sen")

ggplot(df_mo, aes(x = year, y = tmin_c)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "sen")

ggplot(df_mo, aes(x = year, y = (tmax_c - tmin_c))) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "sen")

## annual extremes
df_yr_extreme <-
  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_gt_25mm = sum(prcp_mm > 25),
                   prcp_gt_50mm = sum(prcp_mm > 50),
                   prcp_gt_75mm = sum(prcp_mm > 75)) %>% 
  subset(complete.cases(.))


ggplot(df_yr_extreme, aes(x = year, y = prcp_gt_25mm)) +
  geom_point() +
  stat_smooth(method = "sen")

ggplot(df_yr_extreme, aes(x = year, y = prcp_gt_50mm)) +
  geom_point() +
  stat_smooth(method = "sen")

ggplot(df_yr_extreme, aes(x = year, y = prcp_gt_75mm)) +
  geom_point() +
  stat_smooth(method = "sen")

## variability
# absolute value of difference from previous year
df_yr$prcp_mm_diff <- c(NA, diff(df_yr$prcp_mm))
df_yr$prcp_mm_diff_abs <- abs(df_yr$prcp_mm_diff)

ggplot(df_yr, aes(x = year, y = prcp_mm_diff)) +
  geom_point() +
  stat_smooth(method = "sen")

ggplot(df_yr, aes(x = year, y = prcp_mm_diff_abs)) +
  geom_point() +
  stat_smooth(method = "sen")