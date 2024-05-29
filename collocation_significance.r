###########################################################
# PM Calibration Paper: Significance Testing on Offset    #
# by Meg Fay                                              #
###########################################################

# ----------- SETUP ------------ #
rm()

#Library
pacman::p_load(lubridate, dplyr, tidyverse, ggpubr, tidyverse, ggplot2, gridExtra, car, Metrics)

# set in directories
raw_dir <- "C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/collocation_pm25_raw.csv"
corrected_dirs <- "C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Collocation/Corrected Data"

# set out directory
out_dir <- "C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Collocation/Figures"
# create out directory if it doesn't exist
if (!dir.exists(out_dir)){
    dir.create(out_dir)
}

# ----------- LOAD DATA ------------ #

raw_pm25 <- read.csv(raw_dir)
file_names <- list.files(corrected_dirs, full.names = TRUE)

for(i in seq_along(file_names)) {
  file_name <- basename(file_names[i])
  file_name <- sub("\\.csv$", "", file_name)
  assign(file_name, read.csv(file_names[i]))
}

# ----------- CLEAN DATA ------------ #
# change all date columns to POSIXct
raw_pm25$date <- as.POSIXct(raw_pm25$date, format = "%Y-%m-%d %H:%M:%S")
collocation_data1_offset$date <- as.POSIXct(collocation_data1_offset$date, format = "%Y-%m-%d %H:%M:%S")
collocation_data2_offset$date <- as.POSIXct(collocation_data2_offset$date, format = "%Y-%m-%d %H:%M:%S")
collocation_data3_offset$date <- as.POSIXct(collocation_data3_offset$date, format = "%Y-%m-%d %H:%M:%S")
collocation_data4_offset$date <- as.POSIXct(collocation_data4_offset$date, format = "%Y-%m-%d %H:%M:%S")
collocation_data5_offset$date <- as.POSIXct(collocation_data5_offset$date, format = "%Y-%m-%d %H:%M:%S")

# merge raw data to corrected data for each sensor by date
collocation_merge1 <- merge(raw_pm25, collocation_data1_offset, by = c("date", "code")) %>%
    dplyr::select(date, code, pm2_5, pm2_5_corrected) %>%
    dplyr::mutate(Period = "1") %>%
    na.omit()
collocation_merge2 <- merge(raw_pm25, collocation_data2_offset, by = c("date", "code")) %>%
    dplyr::select(date, code, pm2_5, pm2_5_corrected) %>%
    dplyr::mutate(Period = "2") %>%
    na.omit()
collocation_merge3 <- merge(raw_pm25, collocation_data3_offset, by = c("date", "code")) %>%
    dplyr::select(date, code, pm2_5, pm2_5_corrected) %>%
    dplyr::mutate(Period = "3") %>%
    na.omit()
collocation_merge4 <- merge(raw_pm25, collocation_data4_offset, by = c("date", "code")) %>%
    dplyr::select(date, code, pm2_5, pm2_5_corrected) %>%
    dplyr::mutate(Period = "4") %>%
    na.omit()
collocation_merge5 <- merge(raw_pm25, collocation_data5_offset, by = c("date", "code")) %>%
    dplyr::select(date, code, pm2_5, pm2_5_corrected) %>%
    dplyr::mutate(Period = "5") %>%
    na.omit()

# ----------- DESCRIPTIVE STATISTICS ------------ #
# calculate the mean pm2_5 for each minute
collocation_merge1_mean <- collocation_merge1 %>%
    group_by(date) %>%
    summarise(mean_pm2_5 = mean(pm2_5, na.rm = TRUE),
              mean_pm2_5_corrected = mean(pm2_5_corrected, na.rm = TRUE)) %>%
    na.omit()
collocation_merge2_mean <- collocation_merge2 %>%
    group_by(date) %>%
    summarise(mean_pm2_5 = mean(pm2_5, na.rm = TRUE),
              mean_pm2_5_corrected = mean(pm2_5_corrected, na.rm = TRUE)) %>%
    na.omit()
collocation_merge3_mean <- collocation_merge3 %>%
    group_by(date) %>%
    summarise(mean_pm2_5 = mean(pm2_5, na.rm = TRUE),
              mean_pm2_5_corrected = mean(pm2_5_corrected, na.rm = TRUE)) %>%
    na.omit()
collocation_merge4_mean <- collocation_merge4 %>%
    group_by(date) %>%
    summarise(mean_pm2_5 = mean(pm2_5, na.rm = TRUE),
              mean_pm2_5_corrected = mean(pm2_5_corrected, na.rm = TRUE)) %>%
    na.omit()
collocation_merge5_mean <- collocation_merge5 %>%
    group_by(date) %>%
    summarise(mean_pm2_5 = mean(pm2_5, na.rm = TRUE),
              mean_pm2_5_corrected = mean(pm2_5_corrected, na.rm = TRUE)) %>%
    na.omit()

# add the mean pm2_5 to the original data
collocation_merge1 <- merge(collocation_merge1, collocation_merge1_mean, by = "date")
collocation_merge2 <- merge(collocation_merge2, collocation_merge2_mean, by = "date")
collocation_merge3 <- merge(collocation_merge3, collocation_merge3_mean, by = "date")
collocation_merge4 <- merge(collocation_merge4, collocation_merge4_mean, by = "date")
collocation_merge5 <- merge(collocation_merge5, collocation_merge5_mean, by = "date")

# find RMSE for each sensor code against mean pm2_5
stat1 <- collocation_merge1 %>%
    group_by(code) %>%
    summarise(RMSE_raw = Metrics::rmse(pm2_5, mean_pm2_5),
              RMSE_corrected = Metrics::rmse(pm2_5_corrected, mean_pm2_5_corrected),
              bias_raw = Metrics::bias(pm2_5, mean_pm2_5),
              bias_corrected = Metrics::bias(pm2_5_corrected, mean_pm2_5_corrected)) %>%
    na.omit()
stat2 <- collocation_merge2 %>%
    group_by(code) %>%
    summarise(RMSE_raw = Metrics::rmse(pm2_5, mean_pm2_5),
              RMSE_corrected = Metrics::rmse(pm2_5_corrected, mean_pm2_5_corrected),
              bias_raw = Metrics::bias(pm2_5, mean_pm2_5),
              bias_corrected = Metrics::bias(pm2_5_corrected, mean_pm2_5_corrected)) %>%
    na.omit()
stat3 <- collocation_merge3 %>%
    group_by(code) %>%
    summarise(RMSE_raw = Metrics::rmse(pm2_5, mean_pm2_5),
              RMSE_corrected = Metrics::rmse(pm2_5_corrected, mean_pm2_5_corrected),
              bias_raw = Metrics::bias(pm2_5, mean_pm2_5),
              bias_corrected = Metrics::bias(pm2_5_corrected, mean_pm2_5_corrected)) %>%
    na.omit()
stat4 <- collocation_merge4 %>%
    group_by(code) %>%
    summarise(RMSE_raw = Metrics::rmse(pm2_5, mean_pm2_5),
              RMSE_corrected = Metrics::rmse(pm2_5_corrected, mean_pm2_5_corrected),
              bias_raw = Metrics::bias(pm2_5, mean_pm2_5),
              bias_corrected = Metrics::bias(pm2_5_corrected, mean_pm2_5_corrected)) %>%
    na.omit()
stat5 <- collocation_merge5 %>%
    group_by(code) %>%
    summarise(RMSE_raw = Metrics::rmse(pm2_5, mean_pm2_5),
              RMSE_corrected = Metrics::rmse(pm2_5_corrected, mean_pm2_5_corrected),
              bias_raw = Metrics::bias(pm2_5, mean_pm2_5),
              bias_corrected = Metrics::bias(pm2_5_corrected, mean_pm2_5_corrected)) %>%
    na.omit()

# create a data frame to store the statistics
stat <- rbind(stat1, stat2, stat3, stat4, stat5)
stat_table <- data.frame(
  RMSE_Raw_Min = min(stat$RMSE_raw),
  RMSE_Raw_Max = max(stat$RMSE_raw),
  RMSE_Raw_Mean = mean(stat$RMSE_raw),
  RMSE_Corrected_Min = min(stat$RMSE_corrected),
  RMSE_Corrected_Max = max(stat$RMSE_corrected),
  RMSE_Corrected_Mean = mean(stat$RMSE_corrected),
  Bias_Raw_Min = min(stat$bias_raw),
  Bias_Raw_Max = max(stat$bias_raw),
  Bias_Raw_Mean = mean(stat$bias_raw),
  Bias_Corrected_Min = min(stat$bias_corrected),
  Bias_Corrected_Max = max(stat$bias_corrected),
  Bias_Corrected_Mean = mean(stat$bias_corrected)
)

# print the statistics table
print(stat_table)

# reshape the data frame to long format
long_stat_table <- stat_table %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
  separate(Statistic, into = c("Measure", "Type", "Statistic"), sep = "_")

# print the long format statistics table
print(long_stat_table)

# ----------- TIME SERIES FIGURES ------------ #
# Reshape the data
collocation_merge1_long <- collocation_merge1 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge2_long <- collocation_merge2 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge3_long <- collocation_merge3 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge4_long <- collocation_merge4 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge5_long <- collocation_merge5 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")

# Define custom appearance
new_labels <- c("pm2_5" = "Raw Data", "pm2_5_corrected" = "Offset Corrected Data")
palette <- c("#648FFF", "#FE6100", "#DC267F", "#FFB000", "#785EF0")

# Plot time series
timeseries1 <- ggplot(collocation_merge1_long) +
    geom_line(aes(x = date, y = value, color = code)) +
    theme_minimal() +
    scale_color_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

timeseries2 <- ggplot(collocation_merge2_long) +
    geom_line(aes(x = date, y = value, color = code)) +
    theme_minimal() +
    scale_color_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

timeseries3 <- ggplot(collocation_merge3_long) +
    geom_line(aes(x = date, y = value, color = code)) +
    theme_minimal() +
    scale_color_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

timeseries4 <- ggplot(collocation_merge4_long) +
    geom_line(aes(x = date, y = value, color = code)) +
    theme_minimal() +
    scale_color_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

timeseries5 <- ggplot(collocation_merge5_long) +
    geom_line(aes(x = date, y = value, color = code)) +
    theme_minimal() +
    scale_color_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))
          
# ----------- BOXPLOT FIGURES ------------ #

# Define a function to remove outliers
remove_outliers <- function(df, var_name) {
  Q1 <- quantile(df[[var_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df <- df[!(df[[var_name]] < (Q1 - 1.5 * IQR) | df[[var_name]] > (Q3 + 1.5 * IQR)), ]
  return(df)
}

# Remove outliers from each dataset
collocation_merge1 <- remove_outliers(collocation_merge1, "pm2_5")
collocation_merge2 <- remove_outliers(collocation_merge2, "pm2_5")
collocation_merge3 <- remove_outliers(collocation_merge3, "pm2_5")
collocation_merge4 <- remove_outliers(collocation_merge4, "pm2_5")
collocation_merge5 <- remove_outliers(collocation_merge5, "pm2_5")

# Remove outliers from each dataset
collocation_merge1 <- remove_outliers(collocation_merge1, "pm2_5_corrected")
collocation_merge2 <- remove_outliers(collocation_merge2, "pm2_5_corrected")
collocation_merge3 <- remove_outliers(collocation_merge3, "pm2_5_corrected")
collocation_merge4 <- remove_outliers(collocation_merge4, "pm2_5_corrected")
collocation_merge5 <- remove_outliers(collocation_merge5, "pm2_5_corrected")


### FACET WRAP ###
# Reshape the data
collocation_merge1_long <- collocation_merge1 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge2_long <- collocation_merge2 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge3_long <- collocation_merge3 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge4_long <- collocation_merge4 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")
collocation_merge5_long <- collocation_merge5 %>%
    pivot_longer(cols = c(pm2_5, pm2_5_corrected),
                 names_to = "measurement_type",
                 values_to = "value")

# Define custom appearance
new_labels <- c("pm2_5" = "Raw Data", "pm2_5_corrected" = "Offset Corrected Data")
palette <- c("#648FFF", "#FE6100", "#DC267F", "#FFB000", "#785EF0")

# Plot
facet_boxplot1 <- ggplot(collocation_merge1_long) +
    geom_boxplot(aes(x = code, y = value, fill = code), alpha = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

facet_boxplot2 <- ggplot(collocation_merge2_long) +
    geom_boxplot(aes(x = code, y = value, fill = code), alpha = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

facet_boxplot3 <- ggplot(collocation_merge3_long) +
    geom_boxplot(aes(x = code, y = value, fill = code), alpha = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

facet_boxplot4 <- ggplot(collocation_merge4_long) +
    geom_boxplot(aes(x = code, y = value, fill = code), alpha = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

facet_boxplot5 <- ggplot(collocation_merge5_long) +
    geom_boxplot(aes(x = code, y = value, fill = code), alpha = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    facet_wrap(~ measurement_type, labeller = as_labeller(new_labels)) + # Use new labels
    theme(legend.position = "none", # No legend
          axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 18, color = "black"),
          strip.text = element_text(size = 20, face = "bold", color = "black"))

# ----------- SAVE FIGURES ------------ #
### TIME SERIES ###
ggsave(filename = paste0(out_dir, "/timeseries1.png"), plot = timeseries1, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries2.png"), plot = timeseries2, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries3.png"), plot = timeseries3, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries4.png"), plot = timeseries4, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries5.png"), plot = timeseries5, width = 12, height = 8, units = "in", dpi = 500)

### BOXPLOTS ###
ggsave(filename = paste0(out_dir, "/facet_boxplot1.png"), plot = facet_boxplot1, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot2.png"), plot = facet_boxplot2, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot3.png"), plot = facet_boxplot3, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot4.png"), plot = facet_boxplot4, width = 12, height = 8, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot5.png"), plot = facet_boxplot5, width = 12, height = 8, units = "in", dpi = 500)

# wide formats
### TIME SERIES ###
ggsave(filename = paste0(out_dir, "/timeseries1_wide.png"), plot = timeseries1, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries2_wide.png"), plot = timeseries2, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries3_wide.png"), plot = timeseries3, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries4_wide.png"), plot = timeseries4, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/timeseries5_wide.png"), plot = timeseries5, width = 18, height = 6, units = "in", dpi = 500)

### BOXPLOTS ###
ggsave(filename = paste0(out_dir, "/facet_boxplot1_wide.png"), plot = facet_boxplot1, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot2_wide.png"), plot = facet_boxplot2, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot3_wide.png"), plot = facet_boxplot3, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot4_wide.png"), plot = facet_boxplot4, width = 18, height = 6, units = "in", dpi = 500)
ggsave(filename = paste0(out_dir, "/facet_boxplot5_wide.png"), plot = facet_boxplot5, width = 18, height = 6, units = "in", dpi = 500)
