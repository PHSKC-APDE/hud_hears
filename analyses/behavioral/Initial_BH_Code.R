##01/21/2022
# #Initial Code--Behavioral Analysis
# Making some descriptive tables!
# 
# # #Steps
# # 1) Connect to HHSAW SQL server
# # 2) Read in appropriate data tables
#     a) Table that includes full Medicaid coverage for 7 and 11 months with
#        exits and matched non-exits
# # 3) Modify Alastair's code to make demographic comparisons
#     a) Descriptives by exit type
#     b) Differences in Medicaid coverage by exit type
#     c) Differences in Medicaid continuation after coverage

#Connect to HHSAW Server
library(DBI)
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

#Load necessary packages
pacman::p_load(data.table, DBI, glue, dplyr, tidyr, lubridate)

#Load data table 
#Load some data in to test
#Load some data in to test
##Relevant tables to work with
control_match_covariate <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_covariate]"))

control_match_long <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_long]"))

control_match <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match]"))

##Left join this table with Medicaid ID table
control_match_id_mcaid <- dbGetQuery(db_hhsaw,
                                     "SELECT a.*, b.id_mcaid
                                     FROM
(SELECT * FROM hudhears.control_match_long) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid
FROM claims.hudhears_id_xwalk WHERE id_mcaid is not null) b
ON a.id_hudhears = b.id_hudhears") %>%
  mutate(exit_1yr_prior = exit_date - years(1) + days(1),
         exit_1yr_after = exit_date + years(1) - days(1))


## Observations off by 300
# Do antijoin to see why this is happening

mis_match<- anti_join(control_match_id_mcaid, control_match_covariate, by=c("id_hudhears"))
mis_match2<- as.data.frame(mis_match)




control_match_covariate %>% select(id_hudhears, starts_with("full_")) %>% head() %>% mutate(chk = full_cov_7_prior * full_cov_7_after)
