
###########################################################
# PM Calibration Paper: Baseline and Offset Visualization #
# by Meg Fay                                              #
###########################################################

# ----------- SET UP ------------ #

# clear the environment 
rm()

# Load libraries
pacman::p_load(dplyr, tidyverse, ggplot2, skimr, data.table, janitor, RColorBrewer, Metrics)

# read in baseline data
baseline_df <- read.csv("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Field Data/sensor_baselines.csv")

# read in offset data
offset_df <- read.csv("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Field Data/sensor_offsets.csv")

# ----------- BASELINES ------------ #

### data cleaning ###

# change date to POSIXct date format
baseline_df$date <- as.POSIXct(baseline_df$date, format = "%Y-%m-%d %H:%M:%S")
# remove column X
baseline_df <- baseline_df[, -1]
# rename the row_median column
baseline_df <- baseline_df %>% 
  dplyr::rename(MEDIANpm5pct = row_median)

# change columns from being sensorcodepm5pct to two columns - code and pm5pct
# Reshape the data
baseline_long <- baseline_df %>%
  tidyr::pivot_longer(
    cols = ends_with("pm5pct"),
    names_to = "code",
    values_to = "pm5pct"
  ) %>%
  # remove pm5pct from the code column
  mutate(code = gsub("pm5pct", "", code))

# filter for data during June 2023
wildfire_long <- baseline_long %>% 
  filter(date >= "2023-06-01")

# filter for data before May 31st 2023
baseline_long <- baseline_long %>%
  filter(date < "2023-05-30")

### time series ###
# plot baseline time series by sensor code
baseline_timeseries <- baseline_long %>%
  filter(code != "MEDIAN") %>%
  ggplot(aes(x = date, y = pm5pct, color = I("gray"))) +
  geom_line() +
  geom_line(data = filter(baseline_long, code == "MEDIAN"), color = "firebrick") +
  labs(title = "Baseline PM2.5 by Sensor Code",
       x = "Date",
       y = "PM2.5 Baseline",
       color = "Sensor Code") +
  theme_minimal() +
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 20, face = "bold", color = "black"))

# ----------- OFFSETS ------------ #

### data cleaning ###

# change date to POSIXct date format
offset_df$date <- as.POSIXct(offset_df$date, format = "%Y-%m-%d %H:%M:%S")
# remove column X
offset_df <- offset_df[, -1]

# change columns from being sensorcodeoffset to two columns - code and offset
# Reshape the data
offset_long <- offset_df %>%
  tidyr::pivot_longer(
    cols = ends_with("offset"),
    names_to = "code",
    values_to = "offset"
  ) %>%
  # remove offset from the code column
  mutate(code = gsub("offset", "", code))

# filter for data during June 2023
wildfire_offset_long <- offset_long %>% 
  filter(date >= "2023-06-01")

# filter for data before May 31st 2023
offset_long <- offset_long %>%
  filter(date < "2023-05-30")

### descriptive statistics ###
# find mean and standard deviation of offsets by sensor code
offset_long %>%
  group_by(code) %>%
  summarise(mean_offset = mean(offset, na.rm = TRUE),
            sd_offset = sd(offset, na.rm = TRUE))

# load packages to create table (gt and gtextras)
pacman::p_load(gt, gtExtras)

# create a custom color function
color_func <- function(x) {
  ifelse(x >= 0, "#648FFF", "firebrick")
}

# create table
offset_table <- offset_long %>%
  group_by(code) %>%
  summarise(mean_offset = mean(offset, na.rm = TRUE)) %>%
  gt() %>%
  fmt_number(decimals = 2) %>%
  data_color(
    columns = "mean_offset",
    colors = color_func
  ) %>%
  cols_label(mean_offset = "Average Offset") %>%
  tab_spanner(label = "Sensor Offsets", columns = "mean_offset")

# ----------- EXPORT FIGURES ------------ #
out_dir <- "C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Field Data/Figures"
# create output directory if it does not exist
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

filename <- paste0(out_dir, "/baseline_timeseries.png")
# export baseline time series plot
ggsave(filename = filename, plot = baseline_timeseries, width = 18, height = 6, units = "in", dpi = 500)

# export offset table
#gtsave(offset_table, filename = paste0(out_dir, "/offset_table.png"))
