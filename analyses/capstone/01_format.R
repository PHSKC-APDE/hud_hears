## Script name: 01_format.R
##
## Purpose of script: 
##    Step 1) Obtain study population of exits with exit reasons and categories
##              (from pha.stage_pha_exit_timevar),
##              join covariates (hudhears.control_match_covariate), 
##              and join homeless status data (from hud.hears.pha_homeless_status)
##    Step 2) Derive the outcome variable: "days to first instance of homelessness
##            within 1 year of exiting public housing"
##    Step 3) Join KC-standardized opportunity index scores to data by census tract
##
## Author: Taylor Keating and Zichen Liu
## Date Created: 3/11/2022
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown)

# Uncomment the following line to create a key (run when you change KCIT pwd)
# Put your KC working email address inside the "" of the username = ""
# When a window pops up, please enter your standard KCIT password
# keyring::key_set(service = "hhsaw", username = "")

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

#--------------------------------------------------
### Step 1) Obtain study population, join covariates, and join homeless status data

#-----
## 1a) Pull Tables 

# Select PHA exit data Table (and for which chooser = chooser_max to take just
# one row for each id_hudhears/exit_date combo)
pha_exit_timevar <- 
  setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT id_hudhears, id_kc_pha, exit_year,
                        act_date, agency, exit_reason_clean, exit_category, true_exit,
                        exit_order_study, exit_order_max_study, exit_type_keep
                        FROM [pha].[stage_pha_exit_timevar] 
                        WHERE chooser=chooser_max"))

# Select Covariate Table and filter for id_type is exit
covariate_data <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT id_hudhears, id_type,
                                        hh_id_kc_pha, exit_date, gender_me, race_eth_me,
                                        age_at_exit, housing_time_at_exit, agency,
                                        major_prog, prog_type, geo_tractce10, hh_size,
                                        hh_disability, n_disability, single_caregiver 
                                        FROM [hudhears].[control_match_covariate]")) %>% 
  filter(id_type=="id_exit")

# Select Homeless Status Table
homeless_status_data <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT id_hudhears,
                                              start_date, housing_status, sourcesystemnm 
                                              FROM [hudhears].[pha_homeless_status]"))

#-----
## 1b) Filter PHA data to obtain dataset of study participants
# Filter for:
# i) total exits
# ii) exits in study period (SHA 01/01/2012-12/31/2018, KCHA 01/01/2016-12/31/2018)
# iii) true exits (i.e., no housing activity within 1 year of the period the exit date falls in 
#           and the exit date is within 1 year of the max_in_period date)
# iv) unique exits (take most recent exit for people with multiple exits)
# v) non-death exits

exit_data <- pha_exit_timevar %>% 
  
  # i) filter for exits
  filter(!is.na(act_date)) %>% 
  
  # ii) filter for study period
  filter((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") | 
           (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31")) %>%
  
  # iii) filter for true exits 
  filter(true_exit==1) %>% 
  
  # iv) filter for unique exits 
  # (take most recent exit for people with multiple exits)
  # (also one exit reason per exit event)
  filter(exit_order_study == exit_order_max_study & exit_type_keep == 1) %>%
  
  # v) filter out deaths  (exit_reason_clean= "Deceased")
  filter(exit_reason_clean != "Deceased" & !is.na(exit_reason_clean)) 

# now exit_data is study population

#-----
## 1c) Join covariates and homeless status data to study population
joined_data <- left_join((exit_data %>% 
                          select(id_hudhears, exit_year, act_date,
                                 exit_reason_clean, exit_category) %>%
                          rename(exit_date=act_date)),
                         (covariate_data %>%
                           select(-id_type)),
                         by=c("id_hudhears", "exit_date"))

joined_data <- left_join(joined_data,
                        homeless_status_data,
                        by="id_hudhears")

#-----
# Write intermediate data to SQL table
DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_1"), 
                  value = setDF(copy(joined_data)), 
                  append = F, 
                  overwrite = T)

#---------------------------------------
### Step 2) Derive time to homelessness variable

#-----
## 2a) Handle dates beyond threshold and non-homeless status

# Define study end date, censoring threshold, and administrative buffer
max_date <- as.Date("2019-12-31")
max_days <- 365
buffer <- 30

# Where homeless date > 30 days before exit date, treat start date as study end date
joined_data$start_date <- if_else(joined_data$start_date + buffer < joined_data$exit_date,
                                  max_date, joined_data$start_date)

# Create dummy date variable to handle instances of non-homeless start dates:
# This is set to the start date if housing status is homeless
# Otherwise, this is set to the study end date
joined_data$dummy_date <- if_else(joined_data$housing_status != "homeless" |
                                  is.na(joined_data$housing_status) |
                                  joined_data$start_date > max_date, max_date,
                                  joined_data$start_date)

#-----
# 2b) Get time to homelessness variable

# For each unique individual/exit_date combination, get the earliest dummy date
tth_data <- joined_data %>% group_by(id_hudhears, exit_date) %>%
  arrange(dummy_date) %>%
  slice(1L) %>%
  ungroup()

# Create time to homelessness variable
tth_data$tt_homeless <- as.numeric(tth_data$dummy_date - tth_data$exit_date,
                                   units = "days")

#-----
# 2c) Create censoring/event indicator

# Create variable to indicate event or date censoring
tth_data$event <- if_else(tth_data$tt_homeless >= max_days, 0, 1)

# Make all censored observations end at the censoring threshold
tth_data$tt_homeless <- if_else(tth_data$tt_homeless >= max_days, max_days,
                                tth_data$tt_homeless)

# Make all negative survival times 0
tth_data$tt_homeless <- if_else(tth_data$tt_homeless < 0, 0, tth_data$tt_homeless)

#-----
# Write intermediate data to SQL table
DBI::dbWriteTable(conn = db_hhsaw,
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_2"),
                  value = setDF(copy(df)),
                  append = F,
                  overwrite = T)

#---------------------------------
### Step 3) Join KC-standardized opportunity index scores to data by census tract

#-----
# Load opportunity index data (version standardized in King County)
kc_opp_index_data <- read_csv(file.path(here::here(), "analyses/capstone",
                                        "00_opportunity_index/kc_opp_indices_scaled.csv"))

# Create variables for state, county, and tract identifies
kc_opp_index_data <- kc_opp_index_data %>%
  mutate(GEO_STATE = substr(kc_opp_index_data$GEOID10, 1, 2),
         GEO_COUNTY = substr(kc_opp_index_data$GEOID10, 3, 5),
         GEO_TRACT = substr(kc_opp_index_data$GEOID10, 6, 11)) %>%
  select(GEOID10, GEO_STATE, GEO_COUNTY, GEO_TRACT, everything()) %>%
  rename(kc_opp_index_score = OPP_Z)

#-----
# Join opportunity index scores to housing data by census tract
tth_data <- left_join(tth_data,
                      (kc_opp_index_data %>% select(GEO_TRACT, kc_opp_index_score)),
                      by = c("geo_tractce10" = "GEO_TRACT"))

# Look at summary of number missing opp score by census tract in housing data
tth_data %>% filter(is.na(kc_opp_index_score)) %>% summarise(total_missing = n())
tth_data %>% filter(is.na(kc_opp_index_score)) %>% group_by(geo_tractce10) %>%
  summarise(missing = n())

#-----
# Write final data to SQL table
DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_3"), 
                  value = setDF(copy(tth_data)), 
                  append = F, 
                  overwrite = T)

