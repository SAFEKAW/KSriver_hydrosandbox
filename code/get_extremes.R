## get_extremes.R

# library(tidyverse)
# library(lubridate)
# 
# # load some arbitrary meteorology (from Larned)
# df_daily <- read_csv("https://raw.githubusercontent.com/samzipper/LarnedIntermittencyRegimes/refs/heads/main/data/Meteorology_Daily_Clean.csv")
# 
# # summarize to total annual precipitation
# df_annual <- 
#   df_daily |> 
#   mutate(year = year(date)) |> 
#   group_by(year) |> 
#   summarize(precip_mm = sum(prcp_mm)) |> 
#   mutate(precip_prctl = percent_rank(precip_mm))

# identify extremes
get_extremes <- function(x, dry_thres = 0.2, wet_thres = 0.8, change_thres = 0.6){
  # This function identifies six extreme typologies based on a timeseries of precipitation data
  # x: a numeric vector of precipitation percentile values
  #    IMPORTANT: if there is a year with missing data, it should be retained as an NA, rather than dropped
  # dry_thres: the percentile threshold below which a year is considered dry
  # wet_thres: the percentile threshold above which a year is considered wet
  # change_thres: the absolute value of the percentile threshold for whiplash
  #
  # returns a vector stating the extreme type for each year
  
  # for testing: x <- df_annual$precip_prctl
  x_prctl_change <- c(0, diff(x)) # 0 for first year
  
  # conditions:
  #  - isolated wet = percentile > wet_thres and not in year following another extreme
  #  - isolated dry = percentile < dry_thres and not in year following another extreme
  #  - recurring wet = percentile > wet_thres 2+ years in a row
  #  - recurring dry = percentile < dry_thres 2+ years in a row
  #  - whiplash wet = percentile change > change_thres
  #  - whiplash dry = percentile change < -change_thres
  dry_extreme <- x < dry_thres
  wet_extreme <- x > wet_thres
  dry_recurring <- dry_extreme & lag(dry_extreme)
  wet_recurring <- wet_extreme & lag(wet_extreme)
  dry_whiplash <- x_prctl_change < -change_thres
  wet_whiplash <- x_prctl_change > change_thres
  dry_isolated <- dry_extreme & !dry_whiplash & !wet_whiplash & !dry_recurring & !wet_recurring
  wet_isolated <- wet_extreme & !dry_whiplash & !wet_whiplash & !dry_recurring & !wet_recurring
  
  # any year with NA: should be NA for all extremes. following year should also be NA for whiplash and recurring
  dry_recurring[is.na(x) | is.na(lag(x))] <- NA
  wet_recurring[is.na(x) | is.na(lag(x))] <- NA
  dry_whiplash[is.na(x) | is.na(lag(x))] <- NA
  wet_whiplash[is.na(x) | is.na(lag(x))] <- NA
  dry_isolated[is.na(x)] <- NA
  wet_isolated[is.na(x)] <- NA
  
  # if it is a dry/wet extreme and the prior year is NA, categorize as isolated
  dry_isolated[dry_extreme & is.na(lag(x))] <- TRUE
  wet_isolated[wet_extreme & is.na(lag(x))] <- TRUE
  
  # # for testing: build data frame to inspect
  # df <- data.frame(
  #   x = x, 
  #   x_prctl = x_prctl,
  #   x_prctl_change = x_prctl_change,
  #   dry_recurring = dry_recurring,
  #   wet_recurring = wet_recurring,
  #   dry_whiplash = dry_whiplash,
  #   wet_whiplash = wet_whiplash,
  #   dry_isolated = dry_isolated,
  #   wet_isolated = wet_isolated
  # )
  
  # create output vector that takes the name of whatever extreme is true and calls it normal if no
  extreme_type <- case_when(
    dry_recurring ~ "dry2dry",
    wet_recurring ~ "wet2dry",
    dry_whiplash ~ "wet2dry",
    wet_whiplash ~ "dry2wet",
    dry_isolated ~ "dry",
    wet_isolated ~ "wet",
    TRUE ~ "normal"
  )
  
  # if there is an NA in the input, make sure it is NA in the output for that year
  extreme_type[is.na(x)] <- NA
  
  return(extreme_type)
  
}

# # test
# df_annual$extreme_type <- get_extremes(df_annual$precip_prctl)
# 
# ggplot(df_annual, aes(x = year, y = precip_mm)) +
#   geom_path() +
#   geom_point(aes(color = extreme_type))
# 
# # test with an NA
# df_annual$precip_prctl[7] <- NA
# df_annual$extreme_type_withNA <- get_extremes(df_annual$precip_prctl)
# 
# ggplot(df_annual, aes(x = year, y = precip_mm)) +
#   geom_path() +
#   geom_point(aes(color = extreme_type_withNA))
