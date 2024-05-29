
###########################################################
# PM Calibration Paper: Evaluation and Goodness of Fit    #
# by Meg Fay                                              #
###########################################################

# ----------- SET UP ------------ #
# Load libraries
pacman::p_load(dplyr, tidyverse, ggplot2, skimr, data.table, janitor, RColorBrewer, Metrics)

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
    janitor::clean_names() %>% # clean column names
    mutate_at(vars(pm2_5, pm2_5_corrected, pm2_5_dry_raw, pm2_5_dry_corrected, pm2_5_ridem), 
    ~pmax(.x, 0)) %>% # truncate negative values to 0
    mutate_at(vars(pm2_5, pm2_5_corrected, pm2_5_dry_raw, pm2_5_dry_corrected, pm2_5_ridem),
    ~pmin(.x, 81)) # truncate values greater than 81 to 81

pm_hourly <- pm_data[, 
                   lapply(.SD, mean, na.rm = TRUE), 
                   by = .(date_hourly, code), 
                   .SDcols = startsWith(names(pm_data), "pm")]

pm_hourly <- as.data.frame(pm_data)

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