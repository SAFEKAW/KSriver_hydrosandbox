## PRISM_LongTermTrends.R
# Plot long-term trends in precipitation and temperature extracted by Archi

source(file.path("code", "paths+packages.R"))

# load data
df_precip <- 
  read_csv(file.path(path_drive, "Data", "Climate", "Watershed_Avg_Precipitation_Prism_EKSRB.csv")) |> 
  dplyr::select(Year_N, avg_Precipitation) |>
  rename(YearMo = Year_N, precip_mm = avg_Precipitation)

df_temp <- read_csv(file.path(path_drive, "Data", "Climate", "Watershed_Avg_Temp_Prism_EKSRB.csv")) |> 
  dplyr::select(Year_N, avg_temp) |>
  rename(YearMo = Year_N, temp_C = avg_temp)

# join
df_clim <- full_join(df_precip, df_temp, by="YearMo") |> 
  mutate(Date = parse_date_time(YearMo, orders = c("mdy", "ymd")),
         Year = year(Date),
         Month = month(Date)) |> 
  subset(Year < 2024)

## annual summaries
df_yr <- 
  df_clim |> 
  group_by(Year) |> 
  summarize(precip_mm_total = sum(precip_mm, na.rm=TRUE),
            temp_C_mean = mean(temp_C, na.rm=TRUE)) |> 
  mutate(precip_prctl = percent_rank(precip_mm_total))

# calculate extremes
df_yr$extremes <- get_extremes(df_yr$precip_prctl)


# plot extreme type
ggplot(df_yr, aes(x = Year, y = precip_mm_total/25.4, fill = extremes)) +
  geom_col() +
  labs(x = "Year", y = "Precipitation (inches/yr)", 
       title = "Extreme Precipitation Years in the Kansas River Watershed") +
  theme_scz() +
  scale_fill_manual(values = c("normal" = col.gray, 
                               "dry" = col.cat.org, "dry2dry" = col.cat.red,
                               "wet" = col.cat.grn, "wet2wet" = col.cat.blu,
                               "wet2dry" = col.cat.yel, "dry2wet" = "purple"))

# extreme type per decade
df_yr$decade <- cut(df_yr$Year, 
                    breaks = seq(1900, 2030, 10),
                    labels = seq(1900, 2020, 10),
                    include.lowest = T)
df_decade <-
  df_yr |> 
  group_by(decade) |> 
  summarize(dry = sum(extremes == "dry", na.rm = TRUE),
            dry2dry = sum(extremes == "dry2dry", na.rm = TRUE),
            wet = sum(extremes == "wet", na.rm = TRUE),
            wet2wet = sum(extremes == "wet2wet", na.rm = TRUE),
            wet2dry = sum(extremes == "wet2dry", na.rm = TRUE),
            dry2wet = sum(extremes == "dry2wet", na.rm = TRUE),
            total_extremes = sum(extremes != "normal", na.rm = TRUE))
df_decade |> 
  pivot_longer(cols = c(dry, dry2dry, wet, wet2wet, wet2dry, dry2wet), names_to = "extreme_type", values_to = "n_extreme") |> 
  subset(!is.na(decade)) |> 
  ggplot(aes(x = decade, y = n_extreme, fill = extreme_type)) +
  geom_col() +
  labs(x = "Decade", y = "Number of Extreme Years", 
       title = "Extreme Precipitation Years in the Kansas River Watershed") +
  theme_scz() +
  scale_fill_manual(values = c("normal" = col.gray, 
                               "dry" = col.cat.org, "dry2dry" = col.cat.red,
                               "wet" = col.cat.grn, "wet2wet" = col.cat.blu,
                               "wet2dry" = col.cat.yel, "dry2wet" = "purple"))

# plot annual totals
lm_precip <- lm(precip_mm_total/25.4 ~ Year, data = df_yr)
summary(lm_precip)

p_precip <-
  ggplot(df_yr, aes(x = Year, y = precip_mm_total/25.4)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Total Precipitation (inches/yr)", 
       #title = "Annual Precipitation in the Kansas River Watershed",
       subtitle = "Slope = ~quarter-inch per decade; p = 0.09") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  theme_scz() +
  stat_smooth(method = "lm", color = col.cat.blu)

lm_temp <- lm((temp_C_mean*9/5 + 32) ~ Year, data = df_yr)
summary(lm_temp)

p_temp <- 
  ggplot(df_yr, aes(x = Year, y = (temp_C_mean*9/5 + 32))) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Temperature (F)", 
       #title = "Temperature in the Kansas River Watershed",
       subtitle = "No trend; line shows smoothed fit") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  theme_scz() +
  stat_smooth(color = col.cat.red)

p_precip + p_temp +
  plot_layout(ncol = 1)
ggsave(file.path(path_drive, "Data", "Climate", "Watershed_Avg_Annual_Plots.png"), 
       width = 8, height = 6, dpi = 300)


## monthly plots
df_clim |> 
  ggplot(aes(x = Year, y = precip_mm/25.4)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Precipitation (inches/mo)", 
       title = "Monthly Precipitation in the Kansas River Watershed") +
  theme_scz() +
  facet_wrap(~Month, scales = "free_x", nrow = 2) +
  plot_layout(ncol = 1)
ggsave(file.path(path_drive, "Data", "Climate", "Watershed_Avg_Monthly_Precip_Plots.png"), 
       width = 12, height = 6, dpi = 300)

df_clim |> 
  ggplot(aes(x = Year, y = (temp_C*9/5 + 32))) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Temperature (F)", 
       title = "Monthly Temperature in the Kansas River Watershed") +
  theme_scz() +
  facet_wrap(~Month, scales = "free", nrow = 2) +
  plot_layout(ncol = 1)
ggsave(file.path(path_drive, "Data", "Climate", "Watershed_Avg_Monthly_Temp_Plots.png"), 
       width = 12, height = 6, dpi = 300)
