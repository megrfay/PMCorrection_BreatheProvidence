# PMCorrectionBPVD

<div align="center">
    <h1>Data for "Low-Cost Sensor Calibration Methodology and Preservation of Spatial Variability for Fine Particulate Matter in the Breathe Providence Network"</h1>
    <h2>Institute at Brown for Environment and Society</h2>
    <h4> Date: May 2024 </h4>
</div>

## Abstract
Low-cost air quality sensors are increasingly popular for their affordability, compact size, and stability. Monitoring networks utilizing these devices produce data useful for scientific research and community initiatives, particularly those with environmental justice and public health goals. However, sensor responses can vary due to meteorological conditions and zero offsets, making results difficult to interpret. In Providence, Rhode Island, questions have been raised about air quality and its relationship to asthma rates on a neighborhood level. The Breathe Providence network comprises up to 25 multi-pollutant, low-cost air quality and greenhouse gas monitors and has excellent potential to collect data that can make these local comparisons. Here, we describe a two-step calibration methodology for individual fine particulate matter (PM2.5) sensors in the network utilizing one high-quality reference sensor in the study area without necessitating laboratory calibrations of any individual low-cost sensor. This strategy addresses network precision and measurement sensitivity to relative humidity. When applied to PM2.5 observations within the Providence metropolitan area, we observe a measurement error of less than 3 μg/m3 and demonstrate intranetwork comparison capabilities. We show through analysis of spatial variability and local signals that the calibration is 1. well suited to Providence and 2. has the potential to be successful in other networks that have single particulate matter sensors, a limited number of co-location sites, and a broad range of ambient conditions.

## Producers and Sponsors
- **Primary Investigator(s)**: Meredith Hastings, Brown University Department of Earth, Environment, and Planetary Sciences
- **Primary Producers**: Breathe Providence Project and Rhode Island Department of Environmental Mangement
- **Funding Agency/ies**: Clean Air Fund

## Data Overview
### 6month_pm25_raw.csv
- **Source**: Breathe Providence Project
- **Collection Method**: BEACO2N Monitor containing Plantower PMS5003 original version (fine particulate matter) and BME280 digital sensor (ambient conditions)
- **Description**: 1-minute PM2.5 and ambient condition measurements as recorded by low-cost sensors in the Breathe Providence network from 1/1/2023 to 6/13/2023. 
- **Note**: This data can also be generated with raw data directly from the server using the Author's GitHub code 6-Month_DataPull.

| Column Name | Description                                                  |
|-------------|--------------------------------------------------------------|
| date        | Local datetime in DD/MM/YYYY HH:MM:SS                        |
| lat         | Latitude of sensor position                                  |
| lon         | Longitude of sensor position                                 |
| pm2_5       | Fine particulate matter concentration in µg/m³               |
| rh          | BME 280 internal relative humidity in %                      |
| temp        | BME 280 internal temperature in F                            |
| Sensor      | Name of sensor location                                      |
| code        | Letter code identifying sensor                               |

### Compiled_RIDEM_Data_6.13.23.csv
- **Source**: Rhode Island Department of Environmental Management (RIDEM)
- **Collection Method**: BAM 1020 Beta Attenuation Mass Monitor, an EPA-approved Federal Equivalent Method (FEM)
- **Note**: This data was acquired through Breathe Providence contacts at RODEM and RIDOH, with special thanks to Paul Theroux

| Column Name | Description                                                  |
|-------------|--------------------------------------------------------------|
| Date        | Local datetime in DD/MM/YYYY HH:MM:SS format                 |
| Date_Local  | Local date in DD/MM/YYYY format                              |
| CO          | Carbon Monoxide concentration in parts per million           |
| NO          | Nitric Oxide concentration in parts per billion              |
| NO2         | Nitrogen Dioxide concentration in parts per billion          |
| PM2_5       | Fine Particulate Matter concentration in µg/m³               |
| O3          | Ozone concentration in parts per billion                     |