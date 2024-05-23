###########################################################
# PM Calibration Paper: Data pull for collocation data    #
# by Meg Fay                                              #
###########################################################

# ----------- SETUP ------------ #
#Library
pacman::p_load(lubridate, dplyr, tidyverse, ggpubr, openair, openairmaps, ggplot2, worldmet)

# ----------- DEFINE FUNCTIONS ------------ #

# Function to generate sensor URLs
generate_sensor_url <- function(sensor_name, sensor_node) {
  url <- paste("http://128.32.208.8/node/", sensor_node, "/measurements_all/csv?name=", URLencode(sensor_name), "&interval=1", "&variables=rh,temp,", pollutant, "&start=", URLencode(Start_Date), "%20", URLencode(Start_Time), "&end=", URLencode(End_Date), "%20", URLencode(End_Time), "&chart_type=pre-deployment%20data", sep="")
  return(url)
}

# Function to update data frame
update_data <- function(data, index) {
  data$date <- force_tz(as_datetime(data$datetime, format = "%Y-%m-%d %H:%M:%S"), "GMT") %>% 
    with_tz(tz = "EST")
  if(dst(Sys.time()))data$date <- data$date + hms("01:00:00") #this tries to account for dst (adjusts to present time zone at time of viewing)
  data$Sensor <- sensor_names[index]
  data$code <- sensor_codes[index]
  data$lat <- sensor_info[[index]]$lat #this code could be shortened
  data$lon <- sensor_info[[index]]$lon #this would also be a good place to add in stories or sight hight if wanted
  return(select(data, date, lat, lon, pollutant, rh, temp, Sensor, code))
  }

# Function to correct for time zone 
current_to_PST <- function(date, time){
  dt <- force_tz(as_datetime(paste(date, time), format = "%Y-%m-%d %H:%M:%S"), Sys.timezone()) %>%
    with_tz(tz = "US/Pacific")
  DT <- list()
  DT[1] <- format(dt, "%Y-%m-%d")
  DT[2] <- format(dt, "%H:%M:%S")
  return(DT)
}

# ----------- SENSOR INFO ------------ #

#Sensor Info:
# List of sensor information
sensor_info <- list(
  list(
    ID = 1, node = 250, Location = "Myron J. Francis Elementary",
    lat = 41.84094, lon = -71.36093,
    stories = 1, height_m = 3, Install_Date = "7/11/2022",
    Code = "MJF"
  ),
  list(
    ID = 2, node = 254, Location = "Silver Lake Residence",
    lat = 41.81052, lon = -71.44752,
    stories = 1, height_m = 3, Install_Date = "12/1/2022",
    Code = "SLR"
  ),
  list(
    ID = 3, node = 258, Location = "Reservoir Ave Elementary",
    lat = 41.79157, lon = -71.42788,
    stories = 1.5, height_m = 4.5, Install_Date = "11/17/2022",
    Code = "RAE"
  ),
  list(
    ID = 4, node = 261, Location = "Anthony Carnevale Elementary",
    lat = 41.81678, lon = -71.46458,
    stories = 2, height_m = 6, Install_Date = "12/6/2022",
    Code = "ACE"
  ),
  list(
    ID = 5, node = 264, Location = "E-Cubed Academy Senior High",
    lat = 41.85417, lon = -71.43444,
    stories = 1, height_m = 3, Install_Date = "11/17/2022",
    Code = "ECA"
  ),
  list(
    ID = 6, node = 267, Location = "Rochambeau Library",
    lat = 41.84621, lon = -71.39663,
    stories = 1.5, height_m = 4.5, Install_Date = "8/25/2022",
    Code = "RL"
  ),
  list(
    ID = 7, node = 270, Location = "Smith Hill Library",
    lat = 41.83549, lon = -71.42217,
    stories = 1.5, height_m = 4.5, Install_Date = "8/25/2022",
    Code = "SHL"
  ),
  list(
    ID = 8, node = 274, Location = "Alpert Medical School",
    lat = 41.81898, lon = -71.40826,
    stories = 3.5, height_m = 10.5, Install_Date = "7/28/2022",
    Code = "AMS"
  ),
  list(
    ID = 9, node = 276, Location = "Department of Public Works",
    lat = 41.79520, lon = -71.39784,
    stories = 1, height_m = 3, Install_Date = "8/9/2022",
    Code = "DPW"
  ),
  list(
    ID = 10, node = 251, Location = "Zuccolo Recreation Center",
    lat = 41.82287, lon = -71.43067,
    stories = 1, height_m = 3, Install_Date = "10/4/2022",
    Code = "ZRC"
  ),
  list(
    ID = 11, node = 252, Location = "West End Community Center",
    lat = 41.80439, lon = -71.42825,
    stories = 1, height_m = 3, Install_Date = "9/2/2022",
    Code = "WECC"
  ),
  list(
    ID = 12, node = 255, Location = "United Way",
    lat = 41.81803, lon = -71.44121,
    stories = 1.5, height_m = 4.5, Install_Date = "11/19/2022",
    Code = "UW"
  ),
  list(
    ID = 13, node = 257, Location = "Providence Housing Authority",
    lat = 41.81714, lon = -71.45533,
    stories = 1, height_m = 3, Install_Date = "8/9/2022",
    Code = "PHA"
  ),
  list(
    ID = 14, node = 259, Location = "CCRI - Liston Campus",
    lat = 41.80745, lon = -71.41341,
    stories = 2, height_m = 6, Install_Date = "10/27/2022",
    Code = "CCRI"
  ),
  list(
    ID = 15, node = 262, Location = "Main Street Martial Arts",
    lat = 41.85521, lon = -71.40042,
    stories = 1, height_m = 3, Install_Date = "11/11/2022",
    Code = "MSMA"
  ),
  list(
    ID = 16, node = 263, Location = "South Providence Library",
    lat = 41.80205, lon = -71.41378,
    stories = 1.5, height_m = 4.5, Install_Date = "8/25/2022",
    Code = "SPL"
  ),
  list(
    ID = 19, node = 272, Location = "Rock Spot",
    lat = 41.81509, lon = -71.42216,
    stories = 1.5, height_m = 4.5, Install_Date = "12/15/2022",
    Code = "RS"
  )
)

Sensor_info_df <- do.call(rbind, sensor_info)

# ----------- COLLOCATION PERIOD ------------ #
#ENTER VALUES HERE
pollutant <- "pm2_5" #might be usable to interchange but we still have baseline correction
collocation <- readxl::read_xlsx("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/Collocations.xlsx", sheet = 2)

# Initialize missing_sensor_index and sensor_data
missing_sensor_index <- c()
sensor_data <- list()

# create lists of Dates
Start_Dates <- as.character(collocation$StartDate)
End_Dates <- as.character(collocation$EndDate)

# create list of sensor names
sensor_names <- collocation$Names

# create list of sensor nodes
sensor_nodes <- collocation$Nodes

# create list of sensor codes
sensor_codes <- collocation$Codes

# create a list to store the data
sensor_data <- vector("list", length(sensor_names))

# download sensor data to working directory
missing_sensor_index <- c()
for (i in seq_along(sensor_names)) {
  Start_Date <- Start_Dates[i]
  Start_Time <- "12:00:00"
  End_Date <- End_Dates[i]
  End_Time <- "12:00:00"
  
  #use function to convert and reassign start and end times to be fed to URL
  Start <- current_to_PST(Start_Date, Start_Time)
  Start_Date <- Start[1]
  Start_Time <- Start[2]
  
  End <- current_to_PST(End_Date, End_Time)
  End_Date <- End[1]
  End_Time <- End[2]

  url <- generate_sensor_url(sensor_names[i], sensor_nodes[i])
  print(url)
  sensor_data[[i]] <- try(read.csv(url), silent = TRUE)
  if (inherits(sensor_data[[i]], "try-error")) {
    missing_sensor_index <- append(missing_sensor_index, i)
  } else {
    sensor_data[[i]][sensor_data[[i]] == -999] <- NA # remove missing values
    sensor_data[[i]] <- update_data(sensor_data[[i]], i) # update each sensor dataframe
   }
}

# read in the data for Sensor1
# different url because it is in measurement and not pre-deployment
# Define the different URL for the first sensor
sensor_node <- sensor_nodes[1]
sensor_name <- sensor_names[1]
Start_Date <- Start_Dates[1]
Start_Time <- "12:00:00"
End_Date <- End_Dates[1]
End_Time <- "12:00:00"

sensor1_url <-  paste("http://128.32.208.8/node/", sensor_node, "/measurements_all/csv?name=", URLencode(sensor_name), "&interval=1", "&variables=rh,temp,", pollutant, "&start=", URLencode(Start_Date), "%20", URLencode(Start_Time), "&end=", URLencode(End_Date), "%20", URLencode(End_Time), "&chart_type=measurement", sep="")

# Read in the data from the different URL
sensor1_data <- try(read.csv(sensor1_url), silent = TRUE)
# Check if there was an error reading the data
if (!inherits(sensor1_data, "try-error")) {
  # Remove missing values
  sensor1_data[sensor1_data == -999] <- NA
  # Update the sensor dataframe
  sensor1_data <- update_data(sensor1_data, 1)
  # Assign the updated data to the first element of sensor_data
  sensor_data[[1]] <- sensor1_data
} else {
  print("Error reading data from the different URL")
}

# combine all the data into one dataframe
All_Sensors_df <- do.call(rbind, sensor_data)

# Merge collocation start and end dates with the sensor data
All_Sensors_df <- All_Sensors_df %>%
  merge(collocation, by.x = "code", by.y = "Codes")

# ----------- EXPORT RAW DATA ------------ #
fileName <- paste0("C:/Users/mrfay/OneDrive - University of Vermont/Desktop/BPVD/PM/data/collocation_pm25_raw.csv")

fileName #copy this to use for data visualization script

All_Sensors_df <- as.data.frame(All_Sensors_df)
All_Sensors_df$code <- sapply(All_Sensors_df$code, paste, collapse = ",") 
# remove Sensor.y column and rename Sensor.x to Sensor using dplyr
All_Sensors_df <- All_Sensors_df %>%
  select(-Sensor.x) %>%
  rename(Sensor = Sensor.y)
  
All_Sensors_df$Sensor <- sapply(All_Sensors_df$Sensor, "[[", 1)
write.csv(All_Sensors_df, fileName, row.names = FALSE)


saveRDS(sensor_data, "collocation_data.rds")
saveRDS(sensor_info, "collocation_info.rds")

# ----------- BREAK UP DATA BY COLLOCATION PERIOD ------------ #

# Step 1: Create a list of dataframes for each start date
collocation_periods <- All_Sensors_df %>%
  group_by(StartDate) %>%
  group_split()

# Step 2: Break each of those lists into dataframes by sensor code
collocation_periods_list <- lapply(collocation_periods, function(df) {
  df %>%
    group_by(code) %>%
    group_split()
})