
###########################################################
# PM Calibration Paper: Cross Network Humidity            #
# by Meg Fay                                              #
###########################################################

# ----------- SET UP ------------ #

# clear the environment 
rm()

# Load libraries
pacman::p_load(dplyr, tidyverse, ggplot2, skimr, data.table, janitor, RColorBrewer, Metrics, zoo)

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

# ----------- DATA CLEANING ------------ #

# subset columns to only include sensor code, date, and relative humidity
rh_data <- pm_data %>% 
    select(code, date, rh) %>%  
    filter(!is.na(rh)) %>%
    filter(rh >= 0 & rh <= 100)

rh_long <- rh_data %>% 
    pivot_longer(cols = -c(code, date), names_to = "variable", values_to = "value")
    
# ----------- FIGURES ------------ #

### time series plot of relative humidity by sensor code ###
rh_long %>%
    ggplot(aes(x = date, y = value, color = code)) +
    geom_line() +
    labs(title = "Relative Humidity by Sensor Code",
         x = "Date",
         y = "Relative Humidity (%)",
         color = "Sensor Code") +
  theme_minimal() +
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 20, face = "bold", color = "black"))

### histogram of relative humidity by sensor code ###
rh_long %>%
    ggplot(aes(x = value, fill = code)) +
    geom_histogram(binwidth = 5, position = "dodge") +
    labs(title = "Relative Humidity by Sensor Code",
         x = "Relative Humidity (%)",
         y = "Frequency",
         fill = "Sensor Code") +
    facet_wrap(~ code) +
    theme_minimal() +
    theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

### boxplots of relative humidity by sensor code ###
rh_long %>%
    ggplot(aes(x = code, y = value, fill = code)) +
    geom_boxplot() +
    labs(title = "Relative Humidity by Sensor Code",
         x = "Sensor Code",
         y = "Relative Humidity (%)",
         fill = "Sensor Code") +
    theme_minimal() +
    theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

# retry time series with 4-hour averages

rh_roll <- rh_data %>%
  na.omit() %>%
  arrange(code, date) %>%
  group_by(code) %>%
  mutate(rolling_avg = rollmean(rh, 240, fill = NA, align = "right"))

rh_roll_long <- rh_roll %>% 
  pivot_longer(cols = -c(code, date), names_to = "variable", values_to = "value")

# recreate time series plot with 4-hour averages
rh_roll_long %>%
  ggplot(aes(x = date, y = value, color = code)) +
  geom_line() +
  labs(title = "Relative Humidity by Sensor Code",
       x = "Date",
       y = "Relative Humidity (%)",
       color = "Sensor Code") +
  theme_minimal() +
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 20, face = "bold", color = "black"))