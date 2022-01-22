## Script name: join_opportunity_index.R
##
## Purpose of script: Join KC-standardized opportunity index scores by census tracts to the Biostat Capstone data by census tract (for Capstone Hudhears project)
##
## Author: Taylor Keating
## Date Created: 1/19/2022
## Email:n-tkeating@kingcounty.gov
##
## Notes: 
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

# Select table that contains cleaned data for Biost Capstone Hudhears project
tth_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_2]"))



#-----------------------
# load opportunity index data (version standardized in King County)
kc_opp_index_data<- read_csv("kc_opp_indices_scaled.csv")

# create variables for state, county, and tract identifies
kc_opp_index_data<- kc_opp_index_data %>%
  mutate(GEO_STATE= substr(kc_opp_index_data$GEOID10, 1, 2),
         GEO_COUNTY= substr(kc_opp_index_data$GEOID10, 3, 5),
         GEO_TRACT= substr(kc_opp_index_data$GEOID10, 6, 11)) %>%
  select(GEOID10, GEO_STATE, GEO_COUNTY, GEO_TRACT, everything()) %>%
  rename(kc_opp_index_score= OPP_Z)
#-----------------------

# join opportunity index scores to housing data by census tract
tth_data<- left_join(tth_data,
                     (kc_opp_index_data %>% select(GEO_TRACT, kc_opp_index_score)),
                     by= c("geo_tractce10"="GEO_TRACT"))

# look at summary of number missing opp score by census tract in housing data
tth_data %>% filter(is.na(kc_opp_index_score)) %>% summarise(total_missing=n())
tth_data %>% filter(is.na(kc_opp_index_score)) %>% group_by(geo_tractce10) %>% summarise(missing=n())

#-----------------------
# write table to SQL database

DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_3"), 
                  value = setDF(copy(tth_data)), 
                  append = F, 
                  overwrite = T)




