## Script name: 05_types_of_homelessness
##
## Purpose of script: Look at the categories of homeless experienced
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2022-09-08
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table)


db_hhsaw <- DBI::dbConnect(odbc::odbc(), 
                           driver = "ODBC Driver 17 for SQL Server", 
                           server = "kcitazrhpasqlprp16.azds.kingcounty.gov", 
                           database = "hhs_analytics_workspace", 
                           uid = keyring::key_list("hhsaw")[["username"]], 
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]), 
                           Encrypt = "yes", 
                           TrustServerCertificate = "yes", 
                           Authentication = "ActiveDirectoryPassword")

# BRING IN DATA ----
# Select the table developed for time to homelessness analysis
exits <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))

# Pull in all homelessness events that might be relevant
peh <- dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[pha_homeless_status]")

# Add status code if available
exits_code <- exits %>% select(id_hudhears, exit_date, exit_category, start_date, tt_homeless, event) %>%
  left_join(., distinct(peh), by = c("id_hudhears", "start_date"))


# ANALYSES ----
## Source system (~50 people appear in 2+ systems, ignoring that for now)
exits_code %>% filter(event == 1) %>% 
  distinct(id_hudhears, sourcesystemnm) %>% 
  count(sourcesystemnm) %>%
  mutate(tot = sum(n), pct = round(n / tot * 100, 1))

exits_code %>% filter(event == 1) %>% 
  distinct(id_hudhears, exit_category, sourcesystemnm) %>% 
  count(exit_category, sourcesystemnm) %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n), pct = round(n / tot * 100, 1))


exits_code %>% filter(event == 1) %>% 
  distinct(id_hudhears, sourcesystemnm, housing_status) %>% 
  count(sourcesystemnm, housing_status) %>%
  mutate(tot = sum(n), pct = round(n / tot * 100, 1))

## Exit codes
# See codes here: https://kc1.sharepoint.com/:x:/r/teams/DCHSPublicHealthDataIntegrationDataStewardship/_layouts/15/Doc.aspx?sourcedoc=%7B6E63F55C-52BF-4A29-8881-089DD3ED7BB3%7D&file=IDH%20Demography%20Crosswalk_updated.xlsx&action=default&mobileredirect=true&cid=f6e0b026-9c0c-41ab-896c-4997de946169
# And descriptions of codes in appendix here: https://kingcounty.gov/~/media/depts/community-human-services/department/documents/KC_DCHS_Cross_Systems_Homelessness_Analysis_Brief_12_16_2021_FINAL.ashx
exits_code %>% filter(event == 1) %>% 
  distinct(id_hudhears, sourcesystemnm, status_code) %>% 
  count(sourcesystemnm, status_code) %>%
  mutate(tot = sum(n), pct = round(n / tot * 100, 1)) %>%
  arrange(-n)


## Medicaid
# Since so many homeless events come from Medicaid, look into this
set.seed(98104)
exits_code %>% filter(event == 1 & sourcesystemnm == "medicaid") %>% slice_sample(n = 10)
