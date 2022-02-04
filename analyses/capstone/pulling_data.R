## Script name: pulling_data.R
##
## Purpose of script: 
##    1) Create dataset of study individuals for Biost Capstone HUDHEARS project
##        - using pha.stage_pha_exit_timevar table
##        - each filtering step is broken down to obtain flowchart of study individuals
##    2) Obtain covariates for study individuals (using hudhears.control_match_covariate table)
##       and obtain homelessness services access records (using hudhears.pha_homeless_status table)
##    3) Merge covariates and homelessness variables to PHA data by hudhears id (and exit_date)
##
## Author: Hantong Hu, Taylor Keating, Zichen Liu, Niki Petrakos
## Date Created: 1/6/2022
## Email: n-hhu@kingcounty.gov, n-tkeating@kingcounty.gov, n-zliu@kingcounty.gov, n-npetrakos@kingcounty.gov
##
## Notes: Each step of the filtering process is broken down to obtain a flowchart of study individuals
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

#--------------------------------------------------
# 1)
##### Filtering PHA data to obtain dataset of study participants
## Note: Each step has been broken apart so that a flowchart of study participants can be made

# Select PHA exit data Table (specific variables)
# (and for which chooser= chooser_max to take just one row for each id_hudhears/exit_date combo)
exit_data_full<- 
  setDT(DBI::dbGetQuery(conn= db_hhsaw, "SELECT id_hudhears, id_kc_pha, exit_year, act_date, 
                        agency, exit_reason_clean, exit_category, true_exit 
                        FROM [pha].[stage_pha_exit_timevar] 
                        WHERE chooser=chooser_max"))

#---
# 1a) Total Exits

# filter for exits
exit_data<- exit_data_full %>%
  filter(!is.na(act_date))
paste("Total Exits", nrow(exit_data))

#---
# 1b) Exits in Study Period

## look at number of exits excluded by study period
#nrow(exit_data %>% filter(agency=="SHA" & (act_date< "2013-01-01" | act_date > "2018-12-31")))
#nrow(exit_data %>% filter(agency=="KCHA" & (act_date< "2016-01-01" | act_date > "2018-12-31")))

# filter for study period (dates for KCHA and SHA)
exit_data<- exit_data %>% 
  filter((agency=="SHA" & act_date >= "2013-01-01" & act_date <= "2018-12-31") |
           (agency=="KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))
paste("Total Exits in study period", nrow(exit_data))

#---
# 1c) True Exits

# filter for true exits (true_exit==1)  
#   (i.e., no housing activity within 1 year of the period the exit date falls in 
#           and the exit date is within 1 year of the max_in_period date)
exit_data<- exit_data %>% 
  filter(true_exit==1)
paste("True Exits", nrow(exit_data))

#---
# 1d) Unique Exits

# # look at number of people with multiple exits
# mult_exit_data<- exit_data %>%
#   arrange(id_kc_pha, act_date) %>%
#   group_by(id_kc_pha) %>%
#   mutate(exit_cnt=n(), exit_order=row_number()) %>% ungroup() %>%
#   filter(exit_cnt > 1)
# paste("Number of People with Multiple Exits", length(unique(mult_exit_data$id_kc_pha)))

# filter for distinct people (take most recent exit for people with multiple exits)
exit_data<- exit_data  %>%
  arrange(id_kc_pha, act_date) %>%
  group_by(id_kc_pha) %>%
  mutate(exit_cnt=n(), exit_order = row_number()) %>% ungroup() %>%
  filter(exit_cnt == exit_order)
paste("Unique Individual Exits", nrow(exit_data))

#---
# 1e) Non-Death Exits

# # look at number of deaths
# nrow(exit_data %>% filter(exit_reason_clean=="Deceased"))

# filter out deaths  (exit_reason_clean= "Deceased")
exit_data<- exit_data %>%
  filter(exit_reason_clean != "Deceased" | is.na(exit_reason_clean)==TRUE)
paste("Non Death Exits", nrow(exit_data))

# now exit_data is study population
#--------------------------------------------------


#--------------------------------------------------
# 2)
###### Get covariate table and homeless status table to add to PHA data

# Select Covariate Table and filter for id_type is exit
covariate_data<- setDT(DBI::dbGetQuery(conn= db_hhsaw, "SELECT id_hudhears, id_type, hh_id_kc_pha, 
                                       exit_date, gender_me, race_eth_me, age_at_exit, 
                                       housing_time_at_exit, agency, major_prog, prog_type, 
                                       geo_tractce10, hh_size, hh_disability, n_disability, single_caregiver 
                                       FROM [hudhears].[control_match_covariate]"))
covariate_data<- covariate_data %>% filter(id_type=="id_exit")

# Select Homeless Status Table
homeless_status_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT id_hudhears, start_date, housing_status, sourcesystemnm 
                                             FROM [hudhears].[pha_homeless_status]"))
#--------------------------------------------------


#--------------------------------------------------
# 3)
##### Joining Data

# join covariate data to PHA data
joined_data<- left_join((exit_data %>% 
                           select(id_hudhears, exit_year, act_date, exit_reason_clean, exit_category) %>%
                           rename(exit_date=act_date)),
                        (covariate_data %>%
                           select(-id_type)),
                        by=c("id_hudhears", "exit_date"))
joined_data<- left_join(joined_data,
                        homeless_status_data,
                        by="id_hudhears")
#--------------------------------------------------



#------------------------------------------------------------
## Write Temporary table to SQL Server

DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_1"), 
                  value = setDF(copy(joined_data)), 
                  append = F, 
                  overwrite = T)
