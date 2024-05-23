###########################################################
# PM Calibration Paper: Offset Correction on collocation  #
# by Meg Fay                                              #
###########################################################
# ----------- SETUP ------------ #
#Library
pacman::p_load(lubridate, dplyr, tidyverse, ggpubr, openair, openairmaps, ggplot2, worldmet)

# out directory
out_dir <- "C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Collocation"
# create directory if it doesn't exist
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# create Baseline and Corrected Data directories
if (!dir.exists(file.path(out_dir, "Baseline"))) {
  dir.create(file.path(out_dir, "Baseline"))
}
if (!dir.exists(file.path(out_dir, "Corrected Data"))) {
  dir.create(file.path(out_dir, "Corrected Data"))
}

# ----------- DEFINE CORRECTION FUNCTION ------------ #

offset_correction <- function(sensor_data) {
  sensor_hourly_data <- lapply(sensor_data, function(sensor) {
    sensor_hourly <- sensor %>%
      mutate(dateHourly = floor_date(date, unit = "hours"))
    return(sensor_hourly)
  })

  baseline_list <- lapply(seq_along(sensor_data), function(i) {
    sensor_hourly <- sensor_hourly_data[[i]]
    sensor_baseline <- sensor_hourly %>%
      group_by(dateHourly) %>%
      summarise(pm5pct = quantile(pm2_5, probs = c(0.05), na.rm = TRUE))
    colnames(sensor_baseline)[2] <- paste0(sensor_codes[i], "pm5pct")
    return(sensor_baseline)
  })

  baseline_df <- Reduce(function(x, y) full_join(x, y, by = "dateHourly"), baseline_list)
  baseline_df$row_median <- apply(baseline_df[, -1], 1, median, na.rm = TRUE)

  write.csv(baseline_df, file.path(out_dir, "Baseline", paste0(deparse(substitute(sensor_data)), "_baseline.csv")))

  sensor_offsets <- lapply(seq_along(sensor_data), function(i) {
    sensor_offset <- baseline_df[, c("dateHourly", paste0(sensor_codes[i], "pm5pct"))]
    offset_col <- paste0(sensor_codes[[i]], "offset")
    sensor_offset[, offset_col] <- sensor_offset[, paste0(sensor_codes[i], "pm5pct")] - baseline_df$row_median
    return(sensor_offset)
  })

  sensor_merge_list <- lapply(seq_along(sensor_hourly_data), function(i) {
    merge_data <- merge(sensor_hourly_data[[i]], sensor_offsets[[i]], by = "dateHourly")
    offset_col <- paste0(sensor_codes[[i]], "offset")
    merge_data$pm2_5_corrected <- merge_data$pm2_5 - merge_data[, offset_col]
    return(select(merge_data, date, Sensor, code, pm2_5_corrected, rh, temp, lat, lon))
  })

  All_Sensors_Corrected <- Reduce(full_join, sensor_merge_list)
  All_Sensors_Corrected$Sensor <- as.character(All_Sensors_Corrected$Sensor)
  
  All_Sensors_Corrected$code <- sapply(All_Sensors_Corrected$code, as.character)
  
 write.csv(All_Sensors_Corrected, file.path(out_dir, "Corrected Data", paste0(deparse(substitute(sensor_data)), "_offset.csv")))
}

# ----------- LOAD DATA ------------ #
# save lists in colllocation_periods_list as inputs for offset_correction function
collocation_data1 <- collocation_periods_list[[1]]
collocation_data2 <- collocation_periods_list[[2]]
collocation_data3 <- collocation_periods_list[[3]]
collocation_data4 <- collocation_periods_list[[4]]
collocation_data5 <- collocation_periods_list[[5]]

# ----------- OFFSET CORRECTION ------------ #

# apply offset_correction function to each sensor data
offset_correction(collocation_data1)
offset_correction(collocation_data2)
offset_correction(collocation_data3)
offset_correction(collocation_data4)
offset_correction(collocation_data5)

