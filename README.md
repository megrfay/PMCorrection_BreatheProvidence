# PMCorrectionBPVD

<div align="center">
    <h1>Code for "Low-Cost Sensor Calibration Methodology and Preservation of Spatial Variability for Fine Particulate Matter in the Breathe Providence Network"</h1>
    <h2>Institute at Brown for Environment and Society</h2>
    <h4>Primary Code Producer: Meg Fay <br />
    Date: May 2024 </h4>
</div>

## Abstract
Low-cost air quality sensors are increasingly popular for their affordability, compact size, and stability. Monitoring networks utilizing these devices produce data useful for scientific research and community initiatives, particularly those with environmental justice and public health goals. However, sensor responses can vary due to meteorological conditions and zero offsets, making results difficult to interpret. In Providence, Rhode Island, questions have been raised about air quality and its relationship to asthma rates on a neighborhood level. The Breathe Providence network comprises up to 25 multi-pollutant, low-cost air quality and greenhouse gas monitors and has excellent potential to collect data that can make these local comparisons. Here, we describe a two-step calibration methodology for individual fine particulate matter (PM2.5) sensors in the network utilizing one high-quality reference sensor in the study area without necessitating laboratory calibrations of any individual low-cost sensor. This strategy addresses network precision and measurement sensitivity to relative humidity. When applied to PM2.5 observations within the Providence metropolitan area, we observe a measurement error of less than 3 μg/m3 and demonstrate intranetwork comparison capabilities. We show through analysis of spatial variability and local signals that the calibration is 1. well suited to Providence and 2. has the potential to be successful in other networks that have single particulate matter sensors, a limited number of co-location sites, and a broad range of ambient conditions.

## Producers and Sponsors
- **Primary Investigator(s)**: Meredith Hastings, Brown University Department of Earth, Environment, and Planetary Sciences
- **Funding Agency/ies**: Clean Air Fund

## Code Overview
### Collocation Batches and Calibration
collocation_DataPull.r 
- Pulls raw low-cost PM2.5 data for all pre-deployment collocation batches at East Providence from server.

collocation_Baseline_5PCT.r 
- Applies the scalar offset correction as detailed in publication to all collocation batches at East Providence. 

collocation_ErrorReduction.r 
- Evaluates the RMSE and bias of the raw low-cost data and offset-ccorrected data. 

### Field Data and Calibration
6-Month_DataPull.Rmd
- Pulls raw low-cost PM2.5 data from 1/1/23 to 6/13/23 for all sensors included in 6-Month field deployment from server.

6-Month_Baseline_5PCT.Rmd
- Applies the scalar offset correction as detailed in publication to all raw field data.

6-Month_Baseline_Humidity.Rmd
- Applies the humidity correction as detailed in publication to all offset-corrected field data.
