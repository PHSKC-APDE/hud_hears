/## Script name: esd_ssn_pull.R
##
## Purpose of script: Pull out SSNs of people in HUD HEARS to send to ESD for linkings to wage data
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2022-07-29
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")


# Pull SSNs
# Ideally include controls who remain in housing for full comparisons
ssns <- dbGetQuery(db_hhsaw,
                   "SELECT id.id_hudhears, x.ssn
                   FROM
                   (SELECT distinct id_hudhears FROM hudhears.control_match_long) id
                   LEFT JOIN
                   (SELECT distinct id_hudhears, ssn FROM amatheson.hudhears_xwalk_ids
                     WHERE ssn IS NOT NULL) x
                   ON id.id_hudhears = x.id_hudhears")

ssns_output <- ssns %>% filter(!is.na(ssn))


# Write out
write.csv(ssns_output,
          file = "//dchs-shares01/dchsdata/DCHSPHClaimsData/Analyses/Alastair/HUD HEARS/esd_for_linkage.csv",
          na = "", row.names = F)
