# Initial set-up
# Steps:
# 1) Load required package.skeleton
# 2) Connect to HHSAW database in SQL
# 3) Read in relevant data
# 4) Join datasets as needed

#Load packages and change settings
options(scipen = 6, digits = 4, warning.length = 8170)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, lubridate)

#Connect to SQL server where HHSAW data are stored
# db_hhsaw <- DBI::dbConnect(odbc::odbc(),
#                            driver = "ODBC Driver 17 for SQL Server",
#                            server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
#                            database = "hhs_analytics_workspace",
#                            uid = keyring::key_list("hhsaw")[["username"]],
#                            pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
#                            Encrypt = "yes",
#                            TrustServerCertificate = "yes",
#                            Authentication = "ActiveDirectoryPassword")

#Load some data in to test
control_match_id_mcaid <- dbGetQuery(db_hhsaw,
                                     "SELECT a.*, b.id_mcaid
                                     FROM
(SELECT * FROM hudhears.control_match_long) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid
FROM claims.hudhears_id_xwalk) b
ON a.id_hudhears = b.id_hudhears") %>%
  mutate(exit_1yr_prior = exit_date - years(1) + days(1),
         exit_1yr_after = exit_date + years(1) - days(1))


control_match_id_mcaid %>% arrange(exit_uid, id_type, id_hudhears) %>% head(12)

##

