## Script name: pulling_data.R
##
## Purpose of script: Select and join tables needed for Capstone Hudhears project
##
## Author: Hantong Hu, Taylor Keating, Zichen Liu, Niki Petrakos
## Date Created: 1/6/2022
## Email: n-hhu@kingcounty.gov, n-tkeating@kingcounty.gov, n-zliu@kingcounty.gov, n-npetrakos@kingcounty.gov
##
## Notes: Still a work in progress, first attempt
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

# Select Homeless Status Table
homeless_status_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT id_hudhears, start_date, housing_status, sourcesystemnm 
                                             FROM [hudhears].[pha_homeless_status]"))

# Select Covariate Table and filter for id_type is exit
covariate_data<- setDT(DBI::dbGetQuery(conn= db_hhsaw, "SELECT id_hudhears, id_type, hh_id_kc_pha, 
                                       exit_date, exit_death, gender_me, race_eth_me, age_at_exit, 
                                       housing_time_at_exit, agency, major_prog, prog_type, 
                                       geo_tractce10, hh_size, hh_disability, n_disability, single_caregiver 
                                       FROM [hudhears].[control_match_covariate]"))
covariate_data<- covariate_data %>% filter(id_type=="id_exit")

# Select PHA exit data Table and filter for those with true exits (act_date not NULL and true_exit==1)
exit_data<- setDT(DBI::dbGetQuery(conn= db_hhsaw, "SELECT id_hudhears, act_date, exit_reason_clean, exit_category, true_exit
                                  FROM [pha].[stage_pha_exit_timevar]"))
exit_data<- exit_data[is.na(exit_data$act_date)==FALSE,]
exit_data<- exit_data %>% filter(true_exit==1)


#----------------------------------------------------------
## Joining by id_hudhears and exit_date
joined_data<- left_join(covariate_data, homeless_status_data, by="id_hudhears")
joined_data<- left_join(joined_data, exit_data, by=c("id_hudhears", "exit_date"="act_date"))

#filter for SHA exits after 12/31/2012 and KCHA exits after 12/31/2015
joined_data<- joined_data %>% filter((agency=="SHA" & exit_date > as.Date("2012/12/31")) |
                                       (agency=="KCHA" & exit_date > as.Date("2015/12/31")))
#filter out exits due to deceased
joined_data<- joined_data %>% filter(exit_death==0)


#------------------------------------------------------------
## Write Temporary table to SQL Server

DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_1"), 
                  value = setDF(copy(joined_data)), 
                  append = F, 
                  overwrite = T)
