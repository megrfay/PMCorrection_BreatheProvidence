
###########################################################
# PM Calibration Paper: Evaluation and Goodness of Fit    #
# by Meg Fay                                              #
###########################################################

# ----------- SET UP ------------ #
# Load libraries
pacman::p_load(dplyr, tidyverse, ggplot2, skimr, data.table, janitor, RColorBrewer)

# Read in calibrated and raw data
offset_rh <- read.csv("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/6month_pm25_offset_humidity.csv")
raw_rh <- read.csv("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/6month_pm25_humidity.csv")

# ----------- DATA MERGE  ------------ #
# change date to POSIXct date format
offset_rh$date <- as.POSIXct(offset_rh$date, format = "%Y-%m-%d %H:%M:%S")
raw_rh$date <- as.POSIXct(raw_rh$date, format = "%Y-%m-%d %H:%M:%S")

# subset raw_rh data to only included needed columns
raw_rh <- raw_rh %>% 
    select(date, code, pm2_5, pm2_5_dry) %>%
    dplyr::rename(pm2_5_raw = pm2_5, pm2_5_dry_raw = pm2_5_dry)

# subset offset_rh data to only include needed columns
offset_rh <- offset_rh %>% 
    dplyr::rename(pm2_5_dry_corrected = pm2_5_dry)

# merge raw and calibrated data
pm_data <- merge(offset_rh, raw_rh, by = c("date", "code"))
rm(offset_rh, raw_rh)

# ----------- DATA COMPLETION ------------ #
# skim data
skimr::skim_without_charts(pm_data)

# find percentage of sub-zero values in all pm variables
pm_data %>% 
  summarise(across(starts_with("pm"), ~sum(.x < 0, na.rm = TRUE)) / nrow(pm_data))

# ----------- DATA CLEANING ------------ #
pm_data <- as.data.table(pm_data) %>%
    janitor::clean_names() 

pm_hourly <- pm_data[pm2_5_raw >= 0 & pm2_5_dry_raw >= 0 & pm2_5_corrected >= 0 & pm2_5_dry_corrected >= 0, 
                   lapply(.SD, mean, na.rm = TRUE), 
                   by = .(date_hourly, code), 
                   .SDcols = startsWith(names(pm_data), "pm")]

pm_hourly <- as.data.frame(pm_data)

# ----------- DATA DISTRIBUTION ------------ #

# density plot of pm2_5_raw and pm2_5_corrected
pm_hourly %>%
  ggplot() +
  geom_density(aes(x = pm2_5_raw), fill = "#FE6100", alpha = 0.5) +
  geom_density(aes(x = pm2_5_corrected), fill = "#648FFF", alpha = 0.5) +
  scale_x_continuous(trans = "log") +
  labs(title = "Density Plot of Raw and Offset Corrected PM2.5 Data",
       x = "Log PM2.5",
       y = "Density") +
  theme_minimal()

# density plot of pm2_5_raw and pm2_5_raw_dry
pm_hourly %>%
    ggplot() +
    geom_density(aes(x = pm2_5_raw), fill = "#FE6100", alpha = 0.5) +
    geom_density(aes(x = pm2_5_dry_raw), fill = "#648FFF", alpha = 0.5) +
    scale_x_continuous(trans = "log") +
    labs(title = "Density Plot of Raw and Humidity Corrected PM2.5 Data",
         x = "Log PM2.5",
         y = "Density") +
    theme_minimal()

# density plot of pm2_5_corrected and pm2_5_dry_corrected
pm_hourly %>%
    ggplot() +
    geom_density(aes(x = pm2_5_corrected), fill = "#FE6100", alpha = 0.5) +
    geom_density(aes(x = pm2_5_dry_corrected), fill = "#648FFF", alpha = 0.5) +
    scale_x_continuous(trans = "log") +
    labs(title = "Density Plot of Offset and Offset-Humidity Corrected PM2.5 Data",
         x = "Log PM2.5",
         y = "Density") +
    theme_minimal()

# ----------- LINEAR REGRESSION ------------ #

# >>> clean for linear regression <<<

pm_hourly_mjf <- pm_hourly %>%
    # filter for collocated sensor data (code = MJF)
    dplyr::filter(code == "MJF") %>%
    # filter out pm values above 81
     dplyr::filter(across(starts_with("pm"), ~.x <= 60)) %>%
    # filter out pm values below 0
    dplyr::filter(across(starts_with("pm"), ~.x >= 0))

# create breaks for AQI sub-index bins
breaks <- c(0, 12, 35, 50, 150)

# create AQI sub-index bins
pm_hourly_mjf <- pm_hourly_mjf %>%
    dplyr::mutate(aqi_bin = cut(pm2_5_ridem, 
                                  breaks = breaks, 
                                  labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy")))

# >>> raw data <<<

# create linear regression model for raw data
lm_raw <- lm(pm2_5_ridem ~ pm2_5_raw, data = pm_hourly_mjf)
summary(lm_raw)

# plot raw data
ggplot(pm_hourly_mjf, aes(x=pm2_5_raw, y=pm2_5_ridem)) +
  geom_bin2d(bins = 50) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# >>> offset corrected data <<<

# create linear regression model for corrected data
lm_corrected <- lm(pm2_5_ridem ~ pm2_5_corrected, data = pm_hourly_mjf)
summary(lm_corrected)

# plot corrected data
ggplot(pm_hourly_mjf, aes(x=pm2_5_corrected, y=pm2_5_ridem)) +
  geom_bin2d(bins = 50) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# >>> humidity corrected data <<<

# create linear regression model for corrected data
lm_dry_raw <- lm(pm2_5_ridem ~ pm2_5_dry_raw, data = pm_hourly_mjf)
summary(lm_dry_raw)

# plot corrected data
ggplot(pm_hourly_mjf, aes(x=pm2_5_dry_raw, y=pm2_5_ridem)) +
  geom_bin2d(bins = 50) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# >>> offset-humidity corrected data <<<

# create linear regression model for corrected data
lm_dry_corrected <- lm(pm2_5_ridem ~ pm2_5_dry_corrected, data = pm_hourly_mjf)
summary(lm_dry_corrected)

# plot corrected data
ggplot(pm_hourly_mjf, aes(x=pm2_5_dry_corrected, y=pm2_5_ridem)) +
  geom_bin2d(bins = 50) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# ----------- LM AQI BINS ------------ #
# create models for each of the aqi bins
lm_raw_good <- lm(pm2_5_ridem ~ pm2_5_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Good"))
lm_raw_moderate <- lm(pm2_5_ridem ~ pm2_5_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Moderate"))
lm_raw_usg <- lm(pm2_5_ridem ~ pm2_5_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy for Sensitive Groups"))
#lm_raw_unhealthy <- lm(pm2_5_ridem ~ pm2_5_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy"))

# print summary of each model
summary(lm_raw_good)
summary(lm_raw_moderate)
summary(lm_raw_usg)
#summary(lm_raw_unhealthy)

# create models for each of the aqi bins
lm_corrected_good <- lm(pm2_5_ridem ~ pm2_5_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Good"))
lm_corrected_moderate <- lm(pm2_5_ridem ~ pm2_5_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Moderate"))
lm_corrected_usg <- lm(pm2_5_ridem ~ pm2_5_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy for Sensitive Groups"))
#lm_corrected_unhealthy <- lm(pm2_5_ridem ~ pm2_5_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy"))

# print summary of each model
summary(lm_corrected_good)
summary(lm_corrected_moderate)
summary(lm_corrected_usg)
#summary(lm_corrected_unhealthy)

# create models for each of the aqi bins
lm_dry_raw_good <- lm(pm2_5_ridem ~ pm2_5_dry_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Good"))
lm_dry_raw_moderate <- lm(pm2_5_ridem ~ pm2_5_dry_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Moderate"))
lm_dry_raw_usg <- lm(pm2_5_ridem ~ pm2_5_dry_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy for Sensitive Groups"))
#lm_dry_raw_unhealthy <- lm(pm2_5_ridem ~ pm2_5_dry_raw, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy"))

# print summary of each model
summary(lm_dry_raw_good)
summary(lm_dry_raw_moderate)
summary(lm_dry_raw_usg)
#summary(lm_dry_raw_unhealthy)

# create models for each of the aqi bins
lm_dry_corrected_good <- lm(pm2_5_ridem ~ pm2_5_dry_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Good"))
lm_dry_corrected_moderate <- lm(pm2_5_ridem ~ pm2_5_dry_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Moderate"))
lm_dry_corrected_usg <- lm(pm2_5_ridem ~ pm2_5_dry_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy for Sensitive Groups"))
#lm_dry_corrected_unhealthy <- lm(pm2_5_ridem ~ pm2_5_dry_corrected, data = pm_hourly_mjf %>% filter(aqi_bin == "Unhealthy"))

# print summary of each model
summary(lm_dry_corrected_good)
summary(lm_dry_corrected_moderate)
summary(lm_dry_corrected_usg)
#summary(lm_dry_corrected_unhealthy)

# ----------- RMSE and MEAN BIAS ------------ #
# create function to calculate RMSE
rmse <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2))
}

# create function to calculate mean bias
mean_bias <- function(observed, predicted) {
  mean(predicted - observed)
}

# calculate RMSE and mean bias for raw data
rmse_raw <- rmse(pm_hourly_mjf$pm2_5_ridem, predict(lm_raw))
mean_bias_raw <- mean_bias(pm_hourly_mjf$pm2_5_ridem, predict(lm_raw))
correlation_raw <- cor(pm_hourly_mjf$pm2_5_ridem, pm_hourly_mjf$pm2_5_raw)

# calculate RMSE and mean bias for corrected data
rmse_corrected <- rmse(pm_hourly_mjf$pm2_5_ridem, predict(lm_corrected))
mean_bias_corrected <- mean_bias(pm_hourly_mjf$pm2_5_ridem, predict(lm_corrected))
correlation_corrected <- cor(pm_hourly_mjf$pm2_5_ridem, pm_hourly_mjf$pm2_5_corrected)

# calculate RMSE and mean bias for dry corrected data
rmse_dry_raw <- rmse(pm_hourly_mjf$pm2_5_ridem, predict(lm_dry_raw))
mean_bias_dry_raw <- mean_bias(pm_hourly_mjf$pm2_5_ridem, predict(lm_dry_raw))
correlation_dry_raw <- cor(pm_hourly_mjf$pm2_5_ridem, pm_hourly_mjf$pm2_5_dry_raw)

# calculate RMSE and mean bias for dry corrected data
rmse_dry_corrected <- rmse(pm_hourly_mjf$pm2_5_ridem, predict(lm_dry_corrected))
mean_bias_dry_corrected <- mean_bias(pm_hourly_mjf$pm2_5_ridem, predict(lm_dry_corrected))
correlation_dry_corrected <- cor(pm_hourly_mjf$pm2_5_ridem, pm_hourly_mjf$pm2_5_dry_corrected)

# create data frame of RMSE and mean bias
lm_eval <- data.frame(
  model = c("Raw", "Corrected", "Dry Raw", "Dry Corrected"),
  rmse = c(rmse_raw, rmse_corrected, rmse_dry_raw, rmse_dry_corrected),
  mean_bias = c(mean_bias_raw, mean_bias_corrected, mean_bias_dry_raw, mean_bias_dry_corrected),
  correlation = c(correlation_raw, correlation_corrected, correlation_dry_raw, correlation_dry_corrected)
)