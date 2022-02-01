##01/21/2022
#Initial Code--Behavioral Analysis

# #Steps
# 1) Connect to HHSAW SQL server
# 2) Read in appropriate data tables
# 3) Use cross-walk tables to join tables of interest
#   a) Study ID to KCID
#   b) KCID to auth_id file (for crisis events)
#   c) Also joint with Medicaid ID (If applicable)
# 4) Demographic comparisons


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

#Read in tables of interest



