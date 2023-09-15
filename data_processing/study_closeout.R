## Script name: study_closeout
##
## Purpose of script: Delete all HUD HEARS tables for study closeout
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2023-09-15
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 18 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")


# Get list of tables in HUD HEARS schema
tables <- dbGetQuery(db_hhsaw, "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = 'hudhears'")


# Also add in xwalk in person schema
tables <- rbind(tables,
                c("hhs_analytics_workspace", "amatheson", "hudhears_xwalk_ids", "BASE TABLE"))

# Delete the tables
map2(.x = tables$TABLE_SCHEMA, .y = tables$TABLE_NAME, 
     ~ dbRemoveTable(db_hhsaw, name = DBI::Id(schema = .x, table = .y)))

