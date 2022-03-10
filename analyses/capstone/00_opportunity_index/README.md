# Opportunity Index files

## Purpose

This code is used to create King County standardized opportunity index score for each census tract, based on data and methodology from [Puget Sound Regional Council's (PSRC) Opportunity Mapping](https://www.psrc.org/opportunity-mapping).

Script `create_opportunity_index.R` takes raw data from `PSRC_opp_mapping_all_indicators_raw.xlsx` and writes `kc_opp_indices_scaled.csv`, which contains the King County standardized opportunity index score by census tract.

## Files

### PSRC_opp_mapping_all_indicators_raw.xlsx

-   This xlsx file contains the raw data for all indicators incorporating PSRC's 2018 Opportunity Index, by census tract in the 4-county area (see `opportunitymapping_methodology.pdf` for more information on component indicators)

-   The file is split into sheets: Index-EDU, Index-ECON, Index-HOUSE, Index-TRANS, and Index-Health which contain the sub-components for each of the 5 major components of the opportunity index

-   The final Index-ALL sheet contains the **4-county area standardized score** for each of EDU, ECON, HOUSE, TRANS, and HEALTH, as well as the averaged score- OPP-Z by census tract

### create_opportunity_index.R

-   Script pulls the raw opportunity index indicator data from PSRC (`PSRC_opp_mapping_all_indicators_raw.xlsx`) in the same working directory

-   For each major component (EDU, ECON, HOUSE, TRANS, HEALTH) the script:

1)  standardizes each sub-element (see `opportunitymapping_methodology.pdf` for more information on these)
2)  averages the standardized sub-elements to obtain a z-score for each major component
3)  averages the 5 composite major component z-scores (`EDU_Z`, `ECON_Z`, `HOUSE_Z`, `TRANS_Z`, `HEALTH_Z`) to obtain an opportunity score `OPP_Z`

-   **Note:** This standardization is done both within the **4-county area** AND **just for King County**

-   Script confirms same methodology as PSRC by checking 4-county area standardized scores against that from raw file (`PSRC_opp_mapping_all_indicators_raw.xlsx` sheet `Index-ALL`)

-   Script writes the King County standard opportunity index score to the file `kc_opp_indices_scaled.csv`

### kc_opp_indices_scaled.csv

-   File contains **King County standardized opportunity index scores** for each census tract

-   Created by running the script `create_opportunity_index.R`

### opportunitymapping_methodology.pdf

-   Report containing information on the 5 major components (Education, Economic Health, Housing and Neighborhood Quality, Mobility and Transportation, and Health and Environment) and each of the sub-component indicators comprising Puget Sound Regional Council's opportunity mapping index

-   Also documents updates that were made in 2018 to the original 2012 Opportunity Index
