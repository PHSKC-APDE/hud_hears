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






